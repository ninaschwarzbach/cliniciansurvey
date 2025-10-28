# ===========================================================================
# Clinician Survey Project - Sample Characteristics -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 
# ===========================================================================


# Sample characteristics ### self employed in wave 2
# Continuous vars
data$GraduationYear <- as.numeric(data$GraduationYear) # was class character
cont_vars <- c("Age", "YearsExperience", "GraduationYear")
# compute descriptives by Wave and total
dir.create("outputs", showWarnings = FALSE)
desc_table <- describe_continuous_by_group(data, cont_vars, group = "Wave", save = TRUE, file_prefix = "outputs/descriptives_continuous"); desc_table


# Plots

# Remove plots before adding new ones 
plots_folder <- "plots"
if (dir.exists(plots_folder)) {
  unlink(file.path(plots_folder, "*"), recursive = FALSE)  # Deletes all files inside
  message("All old plots removed from ", plots_folder)
}

violin_age <- split_violin_total(data, "Age", title = "Distribution of Age by Wave")
save_violin(violin_age, "violin_age")
violin_experience <- split_violin_total(data, "YearsExperience", title = "Years of Work Experience by Wave")
save_violin(violin_experience, "violin_experience")
combined_violin <- grid.arrange(violin_age, violin_experience, ncol = 2)
ggsave(
  "plots/violin_combined.png",
  combined_violin,
  width = 10,  
  height = 5,   
  dpi = 300
)

# Categorical vars
cat_vars <- c("GenderIdentity", "Degree", "Country", "WorkSetting", "LengthTrajectory", "ProfessionalBackground")
cat_table <- describe_categorical(data, cat_vars,group = "Wave", include_na = FALSE, file_prefix = "Table2_Categorical",out_folder = "outputs", pct_digits = 1); cat_table     


