# ===========================================================================
# Clinician Survey Project - Maps
# Author: Nina Schwarzbach
# Start date: 21.10.2025
# Date last updated: 
# ===========================================================================

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

  # Label map (UPDATED for split Use -> violin_EBMH_use vs openness; attitude uses items 1&2; item3 = importance)
  label_map <- list(
    "EBMH_use" = list(
      title = "EBMH Use",
      y = "EBMH Use",
      filename = "violin_EBMH_use.png"
    ),
    "EBMH_openness" = list(
      title = "EBMH Openness",
      y = "EBMH Openness",
      filename = "violin_EBMH_use_openness.png"
    ),
    "EBMH_skills_mean" = list(
      title = "Skills EBMH",
      y = "EBMH Skills (mean)",
      filename = "violin_EBMH_skills_mean.png"
    ),
    "perception_gap_mean" = list(
      title = "Perception Gap Between Science and Practice",
      y = "Perception Gap (mean)",
      filename = "violin_perception_gap_mean.png"
    ),
    "EBMH_attitude_mean" = list(
      title = "Attitudes Toward EBMH",
      y = "Attitude Toward EBMH (mean)",
      filename = "violin_EBMH_attitude_mean.png"
    ),
    "EBMH_importance_clinical_expertise" = list(
      title = "Importance: clinical expertise",
      y = "Importance: clinical expertise",
      filename = "violin_EBMH_importance_clinical_expertise.png"
    ),
    "workplace_influences_mean" = list(
      title = "Workplace Influences EBMH Implementation",
      y = "Workplace Influences (mean)",
      filename = "violin_workplace_influences_mean.png"
    ),
    "employer_influences_mean" = list(
      title = "Employer Influences EBMH Implementation",
      y = "Employer Influences (mean)",
      filename = "violin_employer_influences_mean.png"
    ),
    "research_experience_mean" = list(
      title = "Research Experience",
      y = "Research Experience (mean)",
      filename = "violin_research_experience_mean.png"
    ),
    "EBMH_PsychDisorders_mean" = list(
      title = "Importance EBMH for Psychotic Dis.",
      y = "Importance EBMH (mean)",
      filename = "violin_EBMH_PsychDisorders_mean.png"
    ),
    "FamiliarityEBMH" = list(
      title = "Familiarity EBMH Concept",
      y = "Familiarity with EBMH",
      filename = "violin_FamiliarityEBMH.png"
    ),
    "UpdatedConferences" = list(
      title = "Updated Through Conferences",
      y = "Updated Conferences",
      filename = "violin_UpdatedConferences.png"
    ),
    "UpdatedWorkplace" = list(
      title = "Updated Through Workplace",
      y = "Updated Workplace",
      filename = "violin_UpdatedWorkplace.png"
    ),
    "UpdatedSupervision" = list(
      title = "Updated Through Supervision",
      y = "Updated Supervision",
      filename = "violin_UpdatedSupervision.png"
    ),
    "UpdatedPrivate" = list(
      title = "Updated Through Private Study",
      y = "Updated Private Study",
      filename = "violin_UpdatedPrivate.png"
    ),
    "UpdatedOther" = list(
      title = "Updated Through Other Means",
      y = "Updated Other Methods",
      filename = "violin_UpdatedOther.png"
    ),
    "EBMHEndorsementConferences" = list(
      title = "Endorsement of EBMH Conferences",
      y = "EBMH Endorsement Conferences",
      filename = "violin_EBMHEndorsementConferences.png"
    ),
    "EBMHEndorsementWorkplace" = list(
      title = "Endorsement of EBMH Workplace",
      y = "EBMH Endorsement Workplace",
      filename = "violin_EBMHEndorsementWorkplace.png"
    ),
    "EBMHEndorsementSupervision" = list(
      title = "Endorsement of EBMH Supervision",
      y = "EBMH Endorsement Supervision",
      filename = "violin_EBMHEndorsementSupervision.png"
    ),
    "EBMHEndorsementPrivate" = list(
      title = "Endorsement of EBMH Private",
      y = "EBMH Endorsement Private Settings",
      filename = "violin_EBMHEndorsementPrivate.png"
    ),
    "EBMHEndorsementOther" = list(
      title = "Endorsement of EBMH Other Contexts",
      y = "EBMH Endorsement (Other Contexts)",
      filename = "violin_EBMHEndorsementOther.png"
    )
  )
  