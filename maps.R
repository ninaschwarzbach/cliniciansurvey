# ===========================================================================
# Clinician Survey Project - Maps
# Author: Nina Schwarzbach
# Start date: 21.10.2025
# Date last updated: 03.11.2026
#
# Colours and variable labels used by the plot functions (functions.R)
# and the descriptive figures (descriptives.R).
# ===========================================================================


### 1. Plot colours -------------------------------------------------------------
wave_colors   <- c("Wave 1" = "#7FB685", "Wave 2" = "#AF97A2")
likert_colors <- c("#735965", "#AF97A2", "#D9D9D9", "#7FB685", "#437549")


### 2. Variable labels ----------------------------------------------------------
## Scale means (Figure E2, boxplots)
labels_means <- c(
  EBMH_skills_mean          = "EBMH Skills",
  perception_gap_mean       = "Perceived Research–Practice Gap",
  EBMH_attitude_mean        = "EBMH Attitude",
  workplace_influences_mean = "Colleague Support",
  employer_influences_mean  = "Employer Support",
  research_experience_mean  = "Research Experience",
  EBMH_PsychDisorders_mean  = "Importance of EBMH for \nPsychotic Disorders"
)

## Single items (Figure E1, stacked bars)
labels_items <- c(
  EBMH_use                           = "EBMH Use",
  EBMH_openness                      = "EBMH Openness",
  EBMH_importance_clinical_expertise = "Clinical Expertise Valued Over Research",
  FamiliarityEBMH                    = "Familiarity with EBMH"
)
  
  
  