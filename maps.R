# ===========================================================================
# Clinician Survey Project - Maps
# Author: Nina Schwarzbach
# Start date: 21.10.2025
# Date last updated: 03.11.2026
# ===========================================================================

# plot colours 
wave_colors <- c("Wave 1" = "#7FB685", "Wave 2" = "#AF97A2")

likert_colors <- c("#735965", "#AF97A2", "#D9D9D9", "#7FB685", "#437549")

group_colors_2 <- c("#7FB685", "#AF97A2")

# Likert Levels 
  likert_levels <- c(
    "Strongly disagree",
    "Disagree",
    "Neither Agree nor Disagree",
    "Agree",
    "Strongly agree"
  )

# Define reversed order
  likert_reversed <- rev(likert_levels)

# Likert map (customize for your scale)
  likert_map <- c(
    "Strongly disagree" = 1,
    "Disagree" = 2,
    "Neither Agree nor Disagree" = 3,
    "Agree" = 4,
    "Strongly agree" = 5
  )
  # Multi-item scale means -> boxplots
  labels_means <- c(
    EBMH_skills_mean          = "EBMH Skills",
    perception_gap_mean       = "Perceived Research–Practice Gap",
    EBMH_attitude_mean        = "EBMH Attitude",
    workplace_influences_mean = "Colleague Support",
    employer_influences_mean  = "Employer Support",
    research_experience_mean  = "Research Experience",
    EBMH_PsychDisorders_mean  = "Importance of EBMH\nfor Psychotic Disorders")
  
  # Single Likert items -> stacked bars
  labels_items <- c(
    EBMH_use                           = "EBMH Use",
    EBMH_openness                      = "EBMH Openness",
    EBMH_importance_clinical_expertise = "Clinical Expertise Valued Over Research",
    FamiliarityEBMH                    = "Familiarity with EBMH"
  )
  
  labels_updated <- c(
    UpdatedConferences = "Conferences",
    UpdatedWorkplace   = "Workplace",
    UpdatedSupervision = "Supervision",
    UpdatedPrivate     = "Private study",
    UpdatedOther       = "Other"
  )
  
  labels_endorsement <- c(
    EBMHEndorsementConferences = "Conferences",
    EBMHEndorsementWorkplace   = "Workplace",
    EBMHEndorsementSupervision = "Supervision",
    EBMHEndorsementPrivate     = "Private",
    EBMHEndorsementOther       = "Other"
  )
  
  
  