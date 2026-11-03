# ===========================================================================
# Clinician Survey Project - Scale Creation -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 03.11.2026
#
# Converts the Likert answers to numbers (1-5) and creates the final
# variables used in the analyses. The item analyses behind the scale
# decisions are in level-item_stats.R (Appendix B).
# ===========================================================================


### 1. Likert answers from text to numbers -----------------------------------
# All columns that contain Likert answers (agree/disagree labels)
likert_cols <- names(data)[sapply(data, function(x) {
  any(str_detect(tolower(na.omit(as.character(x))), "agree|disagree"))
})]
likert_cols

data <- data %>%
  mutate(across(all_of(likert_cols), likert_to_num))


### 2. Final variables -------------------------------------------------------
## 2.1 Single items (Appendix B)
# The two original EBMH use items correlated only weakly, so they are
# analysed as two separate variables.
data$EBMH_use      <- data$UseEBMH1
data$EBMH_openness <- data$UseEBMH2

# AttitudeEBMH3 was reverse-coded in data_cleaning.R to match the attitude
# scale. As a separate variable it keeps its original direction
# (high = clinical experience valued over research).
data$EBMH_importance_clinical_expertise <- 6 - data$AttitudeEBMH3

## 2.2 Scales (mean of the answered items)
# EBMH attitude: Items 1 and 2 only (Items 3 and 4 removed, see Appendix B)
data$EBMH_attitude_mean <- rowMeans(
  data[, c("AttitudeEBMH1", "AttitudeEBMH2")], na.rm = TRUE)

data$EBMH_skills_mean <- rowMeans(
  data[, c("EBMHSkills1", "EBMHSkills2", "EBMHSkills3")], na.rm = TRUE)

data$perception_gap_mean <- rowMeans(
  data[, c("PerceptionGap1", "PerceptionGap2")], na.rm = TRUE)

data$workplace_influences_mean <- rowMeans(
  data[, c("WorkplaceInfluences1", "WorkplaceInfluences2",
           "WorkplaceInfluences3", "WorkplaceInfluences4",
           "WorkplaceInfluences5")], na.rm = TRUE)

data$employer_influences_mean <- rowMeans(
  data[, c("EmployerInfluences1", "EmployerInfluences2",
           "EmployerInfluences3", "EmployerInfluences4",
           "EmployerInfluences5", "EmployerInfluences6",
           "EmployerInfluences7")], na.rm = TRUE)

data$research_experience_mean <- rowMeans(
  data[, c("ResearchExperience1", "ResearchExperience2")], na.rm = TRUE)

data$EBMH_PsychDisorders_mean <- rowMeans(
  data[, c("EBMHPsychDisorders1", "EBMHPsychDisorders2")], na.rm = TRUE)

# rowMeans() returns NaN when all items are missing; set these to NA
mean_vars <- c("EBMH_attitude_mean", "EBMH_skills_mean", "perception_gap_mean",
               "workplace_influences_mean", "employer_influences_mean",
               "research_experience_mean", "EBMH_PsychDisorders_mean")
data[mean_vars] <- lapply(data[mean_vars], function(x) replace(x, is.nan(x), NA))


### 3. Add the new variables to wave_info -------------------------------------
wave_info <- rbind(
  wave_info,
  data.frame(
    Variable = c("EBMH_use", "EBMH_openness",
                 "EBMH_importance_clinical_expertise",
                 "EBMH_attitude_mean", "EBMH_skills_mean",
                 "perception_gap_mean", "workplace_influences_mean",
                 "employer_influences_mean", "research_experience_mean",
                 "EBMH_PsychDisorders_mean"),
    Wave = c(rep("Both waves", 9), "Wave 2 only"),
    stringsAsFactors = FALSE
  )
)


# Clean environment ---------------------------------------------------------------
rm(list = intersect(c("likert_cols", "mean_vars"), ls()))
