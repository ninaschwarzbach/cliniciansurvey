# ===========================================================================
# Clinician Survey Project - Sample Characteristics
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 03.11.2026
#
# Output: Table 1 (outputs/Table1_continuous.xlsx),
#         Table 2 (outputs/Table2_categorical_wide.csv),
#         Figure D1 (plots/FigureD1_age_experience.png)
# ===========================================================================

# Graduation year is stored as text in the raw data
data$GraduationYear <- as.numeric(data$GraduationYear)


### 1. Continuous variables (Table 1) ----------------------------------------
orientation_vars <- c(
  "SchoolSliderCBT",
  "SchoolSliderPsychodynamic",
  "SchoolSliderAnalytical",
  "SchoolSliderSystemic",
  "SchoolSliderNeuropsychology",
  "SchoolSliderPersonCentered",
  "SchoolSliderACT",
  "SchoolSliderHumanistic",
  "SchoolSliderMBT",
  "SchoolSliderOther"
)

cont_vars <- c("Age", "YearsExperience", orientation_vars)

desc_table <- describe_continuous_by_group(data, cont_vars, group = "Wave", save = FALSE)

numeric_columns <- c("Mean", "SD", "Min", "Max", "Skewness", "Kurtosis")
desc_table[numeric_columns] <- lapply(desc_table[numeric_columns], round, digits = 2)

print(desc_table, n = Inf)

writexl::write_xlsx(desc_table, "outputs/Table1_continuous.xlsx")


### 2. Categorical variables (Table 2) ---------------------------------------
# Percentages are based on participants who answered the question
cat_vars <- c("Country", "Degree", "GenderIdentity", "LengthTrajectory",
              "ProfessionalBackground", "WorkSetting")

cat_table <- describe_categorical(data, cat_vars, group = "Wave",
                                  file_prefix = "Table2_categorical")

print(cat_table, n = Inf)


### 3. Age and years of work experience by wave (Figure D1) ------------------
demo_long <- data %>%
  select(Wave, Age, YearsExperience) %>%
  pivot_longer(c(Age, YearsExperience), names_to = "Variable", values_to = "value",
               values_drop_na = TRUE) %>%
  mutate(Variable = recode(Variable,
                           Age             = "Age (years)",
                           YearsExperience = "Work experience (years)"))

FigureD1 <- ggplot(demo_long, aes(x = Wave, y = value, fill = Wave)) +
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

save_plot(FigureD1, "FigureD1_age_experience", width = 9, height = 4.5)


### Clean environment --------------------------------------------------------
rm(orientation_vars, cont_vars, desc_table, numeric_columns,
   cat_vars, cat_table, demo_long, FigureD1)