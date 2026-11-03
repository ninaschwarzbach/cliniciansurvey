# ===========================================================================
# Clinician Survey Project - Sample Characteristics -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 03.11.2026
# ===========================================================================


# Sample characteristics ### self employed in wave 2
# Continuous vars
data$GraduationYear <- as.numeric(data$GraduationYear) # was class character


orientation_vars <- c(
  "SchoolSliderCBT",
  "SchoolSliderPsychodynamic",
  "SchoolSliderAnalytical",
  "SchoolSliderSystemic",
  "SchoolSliderNeuropsychology",
  "SchoolSliderPersonCentered",
  "SchoolSliderACT",
  "SchoolSliderOther"
)

cont_vars <- c("Age", "YearsExperience", "GraduationYear", orientation_vars)

# compute descriptives by Wave and total
dir.create("outputs", showWarnings = FALSE)

desc_table <- describe_continuous_by_group(
  data,
  cont_vars,
  group = "Wave",
  save = FALSE
)

# Ensure columns are numeric
numeric_columns <- c(
  "Mean",
  "SD",
  "Min",
  "Max",
  "Skewness",
  "Kurtosis"
)

desc_table[numeric_columns] <- lapply(
  desc_table[numeric_columns],
  as.numeric
)

# Round all descriptive statistics to two decimals
desc_table[numeric_columns] <- lapply(
  desc_table[numeric_columns],
  round,
  digits = 2
)

# Inspect the table in R
print(desc_table)

writexl::write_xlsx(
  desc_table,
  "outputs/descriptives_continuous.xlsx"
)

# Plots

# Plots
box_age <- wave_box(data, "Age",
                    title = "Age by Wave", ylab = "Age",
                    y_limits = NULL)

box_experience <- wave_box(data, "YearsExperience",
                           title = "Years of Work Experience by Wave",
                           ylab = "Years of experience",
                           y_limits = NULL)

save_plot(box_age, "box_age", width = 5, height = 5)
save_plot(box_experience, "box_experience", width = 5, height = 5)

# Combined via faceting (no grid.arrange, no duplicated legend)
demo_long <- data %>%
  select(Wave, Age, YearsExperience) %>%
  pivot_longer(c(Age, YearsExperience), names_to = "Variable", values_to = "value",
               values_drop_na = TRUE) %>%
  mutate(Variable = recode(Variable,
                           Age = "Age (years)",
                           YearsExperience = "Work experience (years)"))

combined_demo <- ggplot(demo_long, aes(x = Wave, y = value, fill = Wave)) +
  geom_boxplot(width = 0.5, colour = "black", alpha = 0.85,
               outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(width = 0.12, alpha = 0.2, size = 0.8, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5,
               fill = "white", na.rm = TRUE) +
  facet_wrap(~ Variable, scales = "free_y") +
  scale_fill_manual(values = wave_colors) +
  labs(x = NULL, y = NULL) +
  theme_plots() +
  theme(legend.position = "none")

save_plot(combined_demo, "FigureD1_age_experience", 
          width = 9, height = 4.5)

# Clean environment ---------------------------------------------------------------
rm(box_age, box_experience, combined_demo, demo_long, desc_table, cont_vars, 
   installed, numeric_columns, orientation_vars, packages)

