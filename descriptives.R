# ===========================================================================
# Clinician Survey Project - Descriptives -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 03.11.2026
#
# Descriptive statistics of all analysed variables, by wave and in total
# (Tables 3 and 4), and Figures E1 and E2 (Appendix E).
# Output: outputs/descriptives_continuous_main..., plots/FigureE1..., plots/FigureE2...
# ===========================================================================


### 1. Descriptive statistics by wave and in total ---------------------------
desc_vars <- c(
  "EBMH_use", "EBMH_openness", "EBMH_attitude_mean",
  "EBMH_importance_clinical_expertise", "EBMH_skills_mean",
  "perception_gap_mean", "workplace_influences_mean",
  "employer_influences_mean", "research_experience_mean",
  "EBMH_PsychDisorders_mean", "FamiliarityEBMH","UniversityEducation",
  "UpdatedConferences", "UpdatedWorkplace", "UpdatedSupervision",
  "UpdatedPrivate", "UpdatedOther",
  "EBMHEndorsementConferences", "EBMHEndorsementWorkplace",
  "EBMHEndorsementSupervision", "EBMHEndorsementPrivate",
  "EBMHEndorsementOther"
)

desc_table_1 <- describe_continuous_by_group(
  data, desc_vars, group = "Wave", save = TRUE,
  file_prefix = "outputs/descriptives_continuous_main"
)
desc_table_1


### 2. Figures (Appendix E) ---------------------------------------------------
## Figure E1: single items (stacked bars)
plot_outcomes <- likert_panel(
  data, names(labels_items), labels = labels_items, ncol = 2
)
save_plot(plot_outcomes, "FigureE1_single_items", width = 11, height = 6)

## Figure E2: scale means (boxplots)
plot_means <- box_panel(
  data, names(labels_means), labels = labels_means, ncol = 2
)
save_plot(plot_means, "FigureE2_scale_means", width = 8, height = 10)


# Clean environment ---------------------------------------------------------------
rm(list = intersect(c(
  "desc_vars", "desc_table_1", "plot_outcomes", "plot_means"
), ls()))