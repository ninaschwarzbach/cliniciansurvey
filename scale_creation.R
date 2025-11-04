
# ===========================================================================
# Clinician Survey Project - Scale Creation -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 04.11.2025
# ===========================================================================

## Transform Likert to numbers 
# define vector of Likert item names  ### continue here looking to identify likerts
likert_cols <- names(data)[sapply(data, function(x) {
  any(str_detect(tolower(na.omit(as.character(x))),
                 "agree|disagree"))
})]; likert_cols

# convert text -> numeric using your likert_to_num function
data <- data %>%
  mutate(across(all_of(likert_cols), ~ likert_to_num(.x)))

## Merge items to variables
data$EBMH_use_mean <- rowMeans(data[, c("UseEBMH1", "UseEBMH2")], na.rm = TRUE)
data$EBMH_skills_mean <- rowMeans(data[, c("EBMHSkills1", "EBMHSkills2", "EBMHSkills3")], na.rm = TRUE)
data$perception_gap_mean <- rowMeans(data[, c("PerceptionGap1", "PerceptionGap2" )], na.rm = TRUE)
data$EBMH_attitude_mean <- rowMeans(data[, c( "AttitudeEBMH1", "AttitudeEBMH2", "AttitudeEBMH3" , "AttitudeEBMH4")], na.rm = TRUE)
data$workplace_influences_mean <- rowMeans(data[, c("WorkplaceInfluences1", "WorkplaceInfluences2", 
                                                    "WorkplaceInfluences3", "WorkplaceInfluences4")], na.rm = TRUE)
data$employer_influences_mean <- rowMeans(data[, c("EmployerInfluences1", "EmployerInfluences2",
                                                   "EmployerInfluences3", "EmployerInfluences4",                
                                                   "EmployerInfluences5", "EmployerInfluences6",
                                                   "EmployerInfluences7")], na.rm = TRUE)
data$research_experience_mean <- rowMeans(data[, c( "ResearchExperience1",  "ResearchExperience2")], na.rm = TRUE)
data$EBMH_PsychDisorders_mean <- rowMeans(data[, c("EBMHPsychDisorders1", "EBMHPsychDisorders2")], na.rm = TRUE)

# Add this to wave_info df
new_wave_info <- data.frame(
  Variable = c(
    "EBMH_openness",
    "EBMH_skills_mean",
    "EBMH_use",
    "employer_influences_mean",
    "perception_gap_mean",
    "research_experience_mean",
    "workplace_influences_mean",
    "EBMH_attitude_mean",
    "EBMH_PsychDisorders_mean"
  ),
  Wave = c(
    rep("Both waves", 8),
    "Wave 2 only"
  ),
  stringsAsFactors = FALSE
)

# Combine the new rows with your existing wave_info
wave_info <- rbind(wave_info, new_wave_info)


# Clean environment ---------------------------------------------------------------
rm(likert_cols, new_wave_info)
