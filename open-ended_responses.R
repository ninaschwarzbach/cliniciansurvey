# ===========================================================================
# Clinician Survey Project - Open-ended Responses -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 03.11.2026
#
# Prepares the open-ended responses of both waves for the qualitative
# analysis: availability per wave, response counts and lengths (Appendix C)
# and a coding sheet.
# Output: outputs/open_item_availability.csv, outputs/open_response_summary.csv,
#         outputs/open_response_coding.csv, outputs/open_response_feedback.csv
# ===========================================================================


# ---------------------------------------------------------------------------
# 1. Open-ended items
# ---------------------------------------------------------------------------
# role: substantive = analysed in the Results; admin = "other, namely" fields;
# feedback = questionnaire feedback (Appendix C)
# conditional: TRUE if the item was shown only after a specific answer

item_registry <- tribble(
  ~item,                            ~role,          ~construct,                          ~conditional,
  "UseEBMH1_TEXT",                  "substantive",  "How EBMH is used in practice",       FALSE,
  "MainObstacleEBMH_TEXT",          "substantive",  "Main obstacle to EBMH",              FALSE,
  "NeedsEMBH_PsychDisorders_TEXT",  "substantive",  "Needs for EBMH in psychosis",        FALSE,
  "ReasonsNotEBMH_TEXT",            "substantive",  "Reasons for rejecting EBMH",         FALSE,
  "EBMHUnderstandingSimilar_TEXT",  "substantive",  "Own understanding of EBMH",          TRUE,
  "SchoolSliderOther_TEXT",         "admin",        "Missing therapeutic orientations",   TRUE,
  "ResearchResources_TEXT",         "admin",        "Additional research resources",      TRUE,
  "OccationsContact_TEXT",          "admin",        "Additional contact occasions",       TRUE,
  "OccationsDiscussing_TEXT",       "admin",        "Additional discussion occasions",    TRUE,
  "UpdatedOther_TEXT",              "admin",        "Feeling updated - other occasion",   TRUE,
  "EBMHEndorsementOther_TEXT",      "admin",        "Endorsement - other occasion",       TRUE,
  "WorkSetting_Text",               "admin",        "Work setting - other",               TRUE,
  "ProfessionalBackground_TEXT",    "admin",        "Professional background - other",    TRUE,
  "GenderIdentity_TEXT",            "admin",        "Gender identity - other",            TRUE,
  "Country_TEXT",                   "admin",        "Country - other",                    TRUE,
  "Feedback_Text1",                 "feedback",     "Questionnaire feedback 1",           FALSE,
  "Feedback_Text2",                 "feedback",     "Questionnaire feedback 2",           FALSE
)

# ---------------------------------------------------------------------------
# 2. Classifying responses
# ---------------------------------------------------------------------------

placeholder_pat <- regex(
  paste0("^(",
         "[-./,;:_*]+|",
         "n\\.?v\\.?t\\.?|nvt|nihil|",
         "geen toelichting|geen opmerking|geen commentaar|",
         "k\\.?a\\.?|keine angabe|",
         "x+|\\?+",
         ")$"),
  ignore_case = TRUE
)

crossref_pat <- regex(
  "^(s\\.?\\s?o\\.?|siehe oben\\.?|zie boven\\.?|s\\.o\\.|see above\\.?)$",
  ignore_case = TRUE
)

classify_response <- function(x) {
  case_when(
    is.na(x) | str_trim(x) == ""            ~ "blank",
    str_detect(str_trim(x), placeholder_pat) ~ "placeholder",
    str_detect(str_trim(x), crossref_pat)    ~ "cross_ref",
    TRUE                                     ~ "informative"
  )
}

# Rough language flag based on frequent words (German, Dutch, English);
# checked by hand in the coding sheet.
guess_language <- function(x) {
  xl <- str_to_lower(paste0(" ", x, " "))
  de <- str_count(xl, "\\b(ich|nicht|und|der|die|das|mit|eine|f\u00fcr|von|zu|auf|bei|mehr|zeit|keine)\\b") +
    str_count(xl, "[\u00e4\u00f6\u00fc\u00df]")
  nl <- str_count(xl, "\\b(ik|niet|en|de|het|een|van|met|voor|meer|tijd|geen|dat|dit|wordt|bij)\\b")
  en <- str_count(xl, "\\b(the|and|of|to|in|is|for|with|more|time|not|my|it)\\b")
  case_when(
    is.na(x) | str_trim(x) == ""      ~ NA_character_,
    de >= nl & de >= en & de > 0      ~ "DE",
    nl >= de & nl >= en & nl > 0      ~ "NL",
    en > 0                            ~ "EN",
    TRUE                              ~ "unclear"
  )
}

# ---------------------------------------------------------------------------
# 3. Which items exist in which wave?
# ---------------------------------------------------------------------------

availability <- item_registry %>%
  mutate(
    in_wave1 = item %in% names(quali_wave1),
    in_wave2 = item %in% names(quali_wave2)
  ) %>%
  select(item, role, construct, in_wave1, in_wave2)

print(as.data.frame(availability), row.names = FALSE)
write.csv(availability, "outputs/open_item_availability.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# 4. Long-format extraction (one row per respondent x item)
# ---------------------------------------------------------------------------

extract_long <- function(df, wave_label) {
  vars <- intersect(item_registry$item, names(df))
  if (!length(vars)) return(NULL)
  df %>%
    mutate(row_id = row_number()) %>%
    select(row_id, all_of(vars)) %>%
    mutate(across(all_of(vars), as.character)) %>%
    pivot_longer(-row_id, names_to = "item", values_to = "response") %>%
    mutate(
      wave     = wave_label,
      response = str_squish(response),
      status   = classify_response(response),
      n_words  = if_else(status == "blank", NA_integer_,
                         as.integer(str_count(response, "\\S+"))),
      language = guess_language(response)
    )
}

open_long <- bind_rows(
  extract_long(quali_wave1, "Wave 1"),
  extract_long(quali_wave2, "Wave 2")
) %>%
  left_join(item_registry, by = "item") %>%
  select(wave, row_id, item, role, construct, conditional,
         response, status, n_words, language)

# ---------------------------------------------------------------------------
# 5. Summary table (Appendix C)
# ---------------------------------------------------------------------------

wave_n <- tibble(
  wave      = c("Wave 1", "Wave 2"),
  n_in_wave = c(nrow(quali_wave1), nrow(quali_wave2))
)

open_summary <- open_long %>%
  group_by(wave, item, role, construct, conditional) %>%
  summarise(
    n_answered    = sum(status != "blank"),
    n_placeholder = sum(status == "placeholder"),
    n_crossref    = sum(status == "cross_ref"),
    n_informative = sum(status == "informative"),
    median_words  = if (any(status == "informative"))
      median(n_words[status == "informative"]) else NA_real_,
    q1_words      = if (any(status == "informative"))
      quantile(n_words[status == "informative"], .25, names = FALSE) else NA_real_,
    q3_words      = if (any(status == "informative"))
      quantile(n_words[status == "informative"], .75, names = FALSE) else NA_real_,
    max_words     = if (any(status == "informative"))
      max(n_words[status == "informative"]) else NA_real_,
    n_DE = sum(language == "DE", na.rm = TRUE),
    n_NL = sum(language == "NL", na.rm = TRUE),
    n_EN = sum(language == "EN", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(wave_n, by = "wave") %>%
  mutate(
    # Percentage of the whole wave is only interpretable for items shown to
    # everyone. For conditional items the denominator is unknown from the
    # data alone, so it is deliberately left missing.
    pct_of_wave = if_else(conditional, NA_real_,
                          round(100 * n_informative / n_in_wave, 1))
  ) %>%
  arrange(wave, role, item)

print(as.data.frame(open_summary), row.names = FALSE)
write.csv(open_summary, "outputs/open_response_summary.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# 6. Coding sheet
# ---------------------------------------------------------------------------

coding_sheet <- open_long %>%
  filter(role %in% c("substantive", "admin"), status == "informative") %>%
  arrange(wave, item, row_id) %>%
  mutate(
    translation_en = NA_character_,
    category_1     = NA_character_,
    category_2     = NA_character_,
    coder_note     = NA_character_
  )

write.csv(coding_sheet, "outputs/open_response_coding.csv", row.names = FALSE)
cat("\nCoding sheet rows to code:", nrow(coding_sheet), "\n")

# Questionnaire feedback is kept separate (Appendix C)
feedback_sheet <- open_long %>%
  filter(role == "feedback", status == "informative") %>%
  arrange(wave, item, row_id)

write.csv(feedback_sheet, "outputs/open_response_feedback.csv", row.names = FALSE)

# ---------------------------------------------------------------------------
# 7. Optional: print all informative responses of one item, e.g.
#    show_item("MainObstacleEBMH_TEXT", "Wave 2")
# ---------------------------------------------------------------------------

show_item <- function(item_name, wave_label = NULL) {
  d <- open_long %>%
    filter(item == item_name, status == "informative")
  if (!is.null(wave_label)) d <- filter(d, wave == wave_label)
  cat("\n=== ", item_name,
      if (!is.null(wave_label)) paste0(" (", wave_label, ")") else "",
      " | n = ", nrow(d), " ===\n", sep = "")
  walk2(d$row_id, d$response, ~ cat(.x, ": ", .y, "\n", sep = ""))
  invisible(d)
}


# ---------------------------------------------------------------------------
# 8. Clean environment (show_item() is kept for inspecting responses)
# ---------------------------------------------------------------------------
rm(list = intersect(c(
  "item_registry", "placeholder_pat", "crossref_pat", "classify_response",
  "guess_language", "availability", "extract_long", "open_long", "wave_n",
  "open_summary", "coding_sheet", "feedback_sheet"
), ls()))