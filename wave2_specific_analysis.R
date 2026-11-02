# ===========================================================================
# Clinician Survey Project - Occasions Discussion Evidence -
# Author: Nina Schwarzbach
# Start date: 31.09.2025
# Date last updated: 13.08.2026
# ===========================================================================


# Occasions Discussion Evidence (wave 2 specific analyses)

## 6.1 Data   ---------------------------------------------------------------

# Get lists from wave_info
vars_both        <- wave_info$Variable[wave_info$Wave == "Both waves"]
vars_wave2_only  <- wave_info$Variable[wave_info$Wave == "Wave 2 only"]

# Wave 2 full = both + wave2-only (but only keep columns that actually exist in data)
vars_wave2_full  <- intersect(c(vars_both, vars_wave2_only), names(data))
data_wave2       <- data[, vars_wave2_full, drop = FALSE]

# Wave 2 exclusive = variables that are listed as "Wave 2 only" and present in data
vars_wave2_only_present <- intersect(vars_wave2_only, names(data))
data_wave2_only         <- data[, vars_wave2_only_present, drop = FALSE]

## 6.2. Occasions for cTreatmentPsychDisorders## 6.3. Occasions for contact / discussing evidence ---------------------------------------------------------------

tab_occations <- table(unlist(strsplit(data_wave2_only$OccationsContact, ","))); tab_occations
tab_updated <- table(unlist(strsplit(data_wave2_only$OccationsDiscussing, ","))); tab_updated

# Prepare long data for plot
updated_long <- data_wave2_only %>%
  select(UpdatedConferences, UpdatedWorkplace, UpdatedSupervision, UpdatedPrivate, UpdatedOther) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = everything(), names_to = "Occasion", values_to = "Updatedness") %>%
  mutate(
    Occasion = gsub("^Updated", "", Occasion),
    Updatedness = str_trim(Updatedness),
    Updatedness = ifelse(Updatedness == "", NA, Updatedness)
  )

# Count updatedness responses per occasion
plot_df <- updated_long %>%
  filter(!is.na(Updatedness)) %>%
  group_by(Occasion, Updatedness) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Occasion) %>%
  mutate(Total_n = sum(Count)) %>%
  ungroup()

# Bring in total number of people who reported each occasion
totals_contact <- tibble(
  Occasion = c("Conferences", "Supervision", "Workplace", "Private", "Other"),
  TotalOcc = as.numeric(tab_occations[c("At conferences", 
                                        "During supervision meetings", 
                                        "In my direct work environment", 
                                        "In private", 
                                        "Other")])
)

# Join total contacts and calculate "No response"
stack_df <- totals_contact %>%
  left_join(plot_df, by = "Occasion") %>%
  group_by(Occasion) %>%
  mutate(NoResponse = TotalOcc[1] - sum(Count, na.rm = TRUE)) %>%
  ungroup()

# Add "No response" rows
noresp_df <- stack_df %>%
  distinct(Occasion, TotalOcc, NoResponse) %>%
  mutate(Updatedness = "No response", Count = NoResponse) %>%
  select(Occasion, Updatedness, Count, TotalOcc)

# Combine and calculate percents
stack_df <- stack_df %>%
  select(Occasion, Updatedness, Count, TotalOcc) %>%
  bind_rows(noresp_df) %>%
  group_by(Occasion) %>%
  mutate(Percent = Count / sum(Count, na.rm = TRUE) * 100) %>%
  ungroup()

# Clean and relabel Updatedness 
stack_df <- stack_df %>%
  mutate(
    Updatedness = case_when(
      Updatedness == "2" ~ "Rather not updated",
      Updatedness == "3" ~ "Neutral",
      Updatedness == "4" ~ "Rather updated",
      TRUE ~ Updatedness
    )
  )

# Order factors
likert_levels <- c("No response", "Rather not updated", "Neutral", "Rather updated")
stack_df$Updatedness <- factor(stack_df$Updatedness, levels = rev(likert_levels), ordered = TRUE)

# --- Colors: blue gradient + grey
pal_colors <- c(
  "Rather not updated" = "#AF97A2",
  "Neutral" = "#4A6C6F",
  "Rather updated" = "#7FB685",
  "No response" = "#708090"
)

# --- Plot
gg <- ggplot(stack_df, aes(x = Occasion, y = Count, fill = Updatedness)) +
  geom_col(color = "white") +
  geom_text(aes(label = ifelse(Percent >= 4, sprintf("%.0f%%", Percent), "")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 3) +
  coord_flip() +
  labs(
    title = "Feeling Updated About EBMH by Context",
    x = "",
    y = "Number of respondents (selected occasion)",
    fill = "Feeling updated"
  ) +
  scale_fill_manual(values = pal_colors) +
  theme_minimal(base_size = 13)

# Add n labels
totals <- totals_contact %>%
  mutate(ypos = TotalOcc + max(TotalOcc, na.rm = TRUE) * 0.03,
         lab = paste0("n=", TotalOcc))
gg <- gg + geom_text(data = totals, aes(x = Occasion, y = ypos, label = lab),
                     inherit.aes = FALSE, hjust = 0, size = 3)
gg <- gg + expand_limits(y = max(totals$ypos) * 1.05)

print(gg)


ggsave("plots/occasions_updated.png", gg, width = 11, height = 5, dpi = 300)


## 6.3 EBMH endorsement  ---------------------------------------------------------------

# Prepare long data
endorse_long <- data_wave2_only %>%
  select(EBMHEndorsementConferences, EBMHEndorsementWorkplace,
         EBMHEndorsementSupervision, EBMHEndorsementPrivate, EBMHEndorsementOther) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = everything(), names_to = "Occasion", values_to = "Endorsement") %>%
  mutate(
    Occasion = gsub("^EBMHEndorsement", "", Occasion),
    Endorsement = str_trim(Endorsement),
    Endorsement = ifelse(Endorsement == "", NA, Endorsement)
  )

# Count endorsement responses per occasion
plot_df <- endorse_long %>%
  filter(!is.na(Endorsement)) %>%
  group_by(Occasion, Endorsement) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Occasion) %>%
  mutate(Total_n = sum(Count)) %>%
  ungroup()

# Bring in total number of people who reported each occasion
totals_contact <- tibble(
  Occasion = c("Conferences", "Supervision", "Workplace", "Private", "Other"),
  TotalOcc = as.numeric(tab_occations[c("At conferences",
                                        "During supervision meetings",
                                        "In my direct work environment",
                                        "In private",
                                        "Other")])
)

# Join total contacts and calculate "No response"
stack_df <- totals_contact %>%
  left_join(plot_df, by = "Occasion") %>%
  group_by(Occasion) %>%
  mutate(NoResponse = TotalOcc[1] - sum(Count, na.rm = TRUE)) %>%
  ungroup()

# Add "No response" rows
noresp_df <- stack_df %>%
  distinct(Occasion, TotalOcc, NoResponse) %>%
  mutate(Endorsement = "No response", Count = NoResponse) %>%
  select(Occasion, Endorsement, Count, TotalOcc)

# Combine and calculate percentages
stack_df <- stack_df %>%
  select(Occasion, Endorsement, Count, TotalOcc) %>%
  bind_rows(noresp_df) %>%
  group_by(Occasion) %>%
  mutate(Percent = Count / sum(Count, na.rm = TRUE) * 100) %>%
  ungroup()

# Relabel endorsement values
stack_df <- stack_df %>%
  mutate(
    Endorsement = case_when(
      Endorsement == "1" ~ "Not at all endorsed",
      Endorsement == "2" ~ "Rather not endorsed",
      Endorsement == "3" ~ "Neutral",
      Endorsement == "4" ~ "Rather endorsed",
      Endorsement == "5" ~ "Very endorsed",
      TRUE ~ Endorsement
    )
  )

# Order factors
levels_endorsed <- c("No response", "Not at all endorsed", "Rather not endorsed",
                     "Neutral", "Rather endorsed", "Very endorsed")
stack_df$Endorsement <- factor(stack_df$Endorsement, levels = rev(levels_endorsed), ordered = TRUE)
stack_df$Occasion <- factor(stack_df$Occasion,
                            levels = c("Conferences", "Workplace", "Supervision", "Private", "Other"))

# Colors


pal_colors <- c(
  "Not at all endorsed" = "#AF97A2",
  "Rather not endorsed" = "#C1AEB7",
  "Neutral" = "#4A6C6F",
  "Rather endorsed" = "#7FB685",
  "Very endorsed" = "#437549",
  "No response" = "#708090"
)

# Plot
gg2 <- ggplot(stack_df, aes(x = Occasion, y = Count, fill = Endorsement)) +
  geom_col(color = "white") +
  geom_text(aes(label = ifelse(Percent >= 4, sprintf("%.0f%%", Percent), "")),
            position = position_stack(vjust = 0.5),
            color = "black", size = 3) +
  coord_flip() +
  labs(
    title = "Colleagues’ Endorsement of EBMH by Context",
    x = "",
    y = "Number of respondents (selected occasion)",
    fill = "EBMH endorsement"
  ) +
  scale_fill_manual(values = pal_colors) +
  theme_minimal(base_size = 13)

# Add total n labels
totals <- totals_contact %>%
  mutate(ypos = TotalOcc + max(TotalOcc, na.rm = TRUE) * 0.03,
         lab = paste0("n=", TotalOcc))

gg2 <- gg2 +
  geom_text(data = totals, aes(x = Occasion, y = ypos, label = lab),
            inherit.aes = FALSE, hjust = 0, size = 3) +
  expand_limits(y = max(totals$ypos) * 1.05)

print(gg2)

ggsave("plots/endorsed.png", gg2, width = 11, height = 5, dpi = 300)


## 6.4 Use of evidence, research resources,  and treatment manuals   ---------------------------------------------------------------

# use of evidence
tab_evidence <- table(data_wave2_only$UseEvidence_YesNo); tab_evidence


n_evidence_users <- sum(data_wave2_only$UseEvidence_YesNo == "Yes", na.rm = TRUE)
n_evidence_users 

resource_items <- strsplit(data_wave2_only$ResearchResources, ",")
resource_items <- lapply(resource_items, function(x) {
  x <- trimws(x)
  x[x != ""]
})
tab_resource <- table(unlist(resource_items))

cbind(Count   = tab_resource,
      Percent = round(100 * tab_resource / n_evidence_users, 1))
# using treatment manuals and frequency of it
tab_manuals <- table(data_wave2_only$TreatmentManuals); tab_manuals
desc_manuals <- describe(data_wave2_only$TreatmentManuals_Freq); desc_manuals



# Clean environment ---------------------------------------------------------------
rm(
  list = c(
    "data_wave2", "data_wave2_only", "gg", "gg2", 
    "noresp_df",  "stack_df", "totals", "desc_manuals", 
    "endorse_long", "plot_df", "totals_contact", "updated_long", 
     "likert_levels", 
     "pal_colors", ls(pattern = "^vars_"), ls(pattern = "^tab")
  )
)

