# ===========================================================================
# Clinician Survey Project - Correlational Analysis -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 
# ===========================================================================


# Correlational analyses
# Define and check predictors 
### Are these all variables?? Check analysis plan
### Where is university education?? also check analysis above

# Dependent variables
DVs <- c("EBMH_use", "EBMH_attitude_mean", "EBMH_openness")  # Fixed 'EBMH Openness' to 'EBMH_openness'

# Continuous/interval-like predictors
continuous_preds <- c("EBMH_skills_mean", "perception_gap_mean", "workplace_influences_mean",
                      "employer_influences_mean", "research_experience_mean",
                      "EBMH_PsychDisorders_mean",
                      "Age", "YearsExperience", grep("^SchoolSlider", names(data), value = TRUE))

# Ordinal predictors
ordinal_preds <- c("FamiliarityEBMH", "UpdatedConferences", "UpdatedWorkplace", "UpdatedSupervision",
                   "UpdatedPrivate", "UpdatedOther")

# Binary predictors
binary_preds <- c("Country", "Wave")

# Nominal predictors
nominal_preds <- c("GenderIdentity", "Degree", "WorkSetting", "LengthTrajectory", "ProfessionalBackground")  

# Combine all variables
all_vars <- c(DVs, continuous_preds, ordinal_preds, binary_preds, nominal_preds)

# Check classes of the variables in the dataset
class_info <- sapply(data[all_vars], class)

# Print the results
print(class_info)

# Fix issues 
for (var in ordinal_preds) {
  data[[var]] <- as.ordered(data[[var]])  # Use as.ordered for ordinal data
}

for (var in binary_preds) {
  data[[var]] <- as.factor(data[[var]])
}

for (var in nominal_preds) {
  data[[var]] <- as.factor(data[[var]])
}


# Which ones are wave 1 or wave 2 specific 
wave2_preds <- c("EBMH_PsychDisorders_mean", "SchoolSliderPsychodynamic_PsychDisorders", 
                 "SchoolSliderHumanistic", "SchoolSliderMBT", "UpdatedConferences", 
                 "UpdatedWorkplace", "UpdatedSupervision", "UpdatedPrivate", 
                 "UseEvidence_YesNo", "ProfessionalBackground")
wave1_preds <- c("SchoolSliderAnalytical") ### why is this wave 1 specific??? 

## Correlations with EBMH use
## Correlations with EBMH attitudes
## Correlations among  other variables