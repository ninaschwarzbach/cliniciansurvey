# ===========================================================================
# Clinician Survey Project - Public Dataset -
# Author: Nina Schwarzbach
# Start date: 03.11.2026
# Date last updated: 03.11.2026
#
# Creates the anonymized dataset for publication and its codebook from the
# raw Qualtrics exports. Run this script on its own (Source); it runs the
# data cleaning itself.
#
# As stated in the participant information of both waves, age and gender are
# not shared. Years of experience and graduation year are not shared either,
# because they allow age to be estimated. Open-ended responses, participant
# codes and Qualtrics metadata are not shared. The complete dataset is
# available on reasonable request.
#
# Input:  data/raw/wave1_data.csv, data/raw/wave2_data.csv
# Output: data/public/EBMH-ClinicianSurvey_data_anonymized.csv
#         data/public/EBMH-ClinicianSurvey_codebook.csv
# ===========================================================================

rm(list = ls())

### 1. Setup (as in analysis_main.R) -----------------------------------------
packages <- c("tidyverse", "psych", "rlang")
missing_packages <- setdiff(c(packages, "rstudioapi"), rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)
invisible(lapply(packages, library, character.only = TRUE))

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

if (".func_env" %in% search()) detach(".func_env")
.func_env <- new.env(parent = .GlobalEnv)
source("functions.R", local = .func_env)
source("maps.R",      local = .func_env)
attach(.func_env, pos = 2, name = ".func_env")


### 2. Data cleaning ---------------------------------------------------------
source("data_cleaning.R")


### 3. Variables that are shared ----------------------------------------------
# Only the variables listed here are shared (everything else is left out)
public_vars <- c(
  # Data collection and background
  "Wave", "Country", "Degree", "ProfessionalBackground", "WorkSetting",
  "SelfEmployed", "LengthTrajectory",
  # Therapeutic orientation
  grep("^SchoolSlider", names(data), value = TRUE), "SchoolForcedChoice",
  # EBMH items (both waves)
  "FamiliarityEBMH", "UseEBMH1", "UseEBMH2",
  "EBMHSkills1", "EBMHSkills2", "EBMHSkills3",
  "PerceptionGap1", "PerceptionGap2",
  "AttitudeEBMH1", "AttitudeEBMH2", "AttitudeEBMH3", "AttitudeEBMH4",
  "ResearchExperience1", "ResearchExperience2",
  paste0("WorkplaceInfluences", 1:5),
  paste0("EmployerInfluences", 1:7),
  "UniversityEducation",
  # Wave 2 only
  "WorkplaceInfluences2_PsychDisorders",
  "EBMHPsychDisorders1", "EBMHPsychDisorders2",
  "ComparisonEBMHPsychDisordersOtherDisorders",
  "UseEvidence_YesNo", "ResearchResources",
  "TreatmentManuals", "TreatmentManuals_Freq",
  "EBMHUnderstandingSimilar",
  "OccationsContact", "OccationsDiscussing",
  grep("^Updated", names(data), value = TRUE),
  grep("^EBMHEndorsement", names(data), value = TRUE)
)
public_vars <- intersect(unique(public_vars), names(data))

# Check: variables that are NOT shared (look through this list before publishing)
cat("\nVariables not included in the public dataset:\n")
print(setdiff(names(data), public_vars))


### 4. Build the public dataset ------------------------------------------------
data_public <- data[, public_vars]

# Likert answers as numbers (1 = strongly disagree / disagree totally to
# 5 = strongly agree / agree totally). AttitudeEBMH3 and PerceptionGap2 are
# already reverse-coded (data_cleaning.R, section 6).
likert_cols <- names(data_public)[sapply(data_public, function(x) {
  any(str_detect(tolower(na.omit(as.character(x))), "agree|disagree"))
})]
data_public <- data_public %>%
  mutate(across(all_of(likert_cols), likert_to_num))

# Random row order and a new ID, so rows cannot be linked to the raw export
set.seed(20261103)
data_public <- data_public[sample(nrow(data_public)), ]
data_public <- data.frame(ID = seq_len(nrow(data_public)), data_public,
                          row.names = NULL)

# Check: categories with fewer than 5 participants (possible identification)
cat("\nCategories with fewer than 5 participants:\n")
for (v in c("Country", "Degree", "ProfessionalBackground", "WorkSetting",
            "SelfEmployed", "LengthTrajectory", "SchoolForcedChoice")) {
  if (!v %in% names(data_public)) next
  tab <- table(data_public[[v]])
  small <- tab[tab < 5]
  if (length(small) > 0) {
    cat(v, ":", paste0(names(small), " (n = ", small, ")", collapse = "; "), "\n")
  }
}


### 5. Codebook ------------------------------------------------------------------
codebook <- bind_rows(lapply(names(data_public), function(v) {
  x <- data_public[[v]]
  in_w1 <- any(!is.na(x[data_public$Wave == "Wave 1"]))
  in_w2 <- any(!is.na(x[data_public$Wave == "Wave 2"]))
  values <- if (v %in% likert_cols) {
    "1 = strongly disagree to 5 = strongly agree"
  } else if (is.numeric(x)) {
    if (all(is.na(x))) "" else paste0(min(x, na.rm = TRUE), " to ", max(x, na.rm = TRUE))
  } else {
    vals <- sort(unique(na.omit(as.character(x))))
    if (length(vals) > 12) paste0(length(vals), " different answers (multiple choice)")
    else paste(vals, collapse = " | ")
  }
  data.frame(
    Variable = v,
    Waves    = if (in_w1 && in_w2) "Both" else if (in_w1) "Wave 1" else if (in_w2) "Wave 2" else "None",
    Type     = if (is.numeric(x)) "numeric" else "text",
    N_valid  = sum(!is.na(x)),
    Values   = values,
    Note     = case_when(
      v %in% c("AttitudeEBMH3", "PerceptionGap2") ~ "reverse-coded",
      v == "ID" ~ "random ID, not linked to the raw data",
      TRUE ~ ""
    ),
    Question = ""   # question wording added from the questionnaires
  )
}))


### 6. Save ----------------------------------------------------------------------
if (!dir.exists("data/public")) dir.create("data/public", recursive = TRUE)
write.csv(data_public, "data/public/EBMH-ClinicianSurvey_data_anonymized.csv",
          row.names = FALSE, na = "")
write.csv(codebook, "data/public/EBMH-ClinicianSurvey_codebook.csv",
          row.names = FALSE, na = "")
cat("\nPublic dataset:", nrow(data_public), "participants,",
    ncol(data_public), "variables\n")


### Clean up ---------------------------------------------------------------------
detach(".func_env")
rm(list = ls())