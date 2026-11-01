#========================================================================
# Clinician Survey Project - Wave 2 Descriptives -
# Author: Nina Schwarzbach
# Start date: 19.11.2025
# Date last updated: 21.11.2025
# ===========================================================================

# Use of Evidence
cat("\nUse of Evidence (Wave 2)\n")
print(table(wave2$UseEvidence_YesNo, useNA = "ifany"))
print(round(100 * prop.table(table(wave2$UseEvidence_YesNo, useNA = "ifany")), 1))

# Use of Treatment Manuals
cat("\nUse of Treatment Manuals (Wave 2)\n")
print(table(wave2$TreatmentManuals, useNA = "ifany"))
print(round(100 * prop.table(table(wave2$TreatmentManuals, useNA = "ifany")), 1))

# Frequency of Treatment Manual Use
cat("\nFrequency Use of Treatment Manuals (Wave 2)\n")
print(psych::describe(as.numeric(data$TreatmentManuals_Freq)))

# Research Resources Used
cat("\nResearch Resources Used (Wave 2)\n")
clean <- wave2$ResearchResources |>
  str_remove_all("Other, namely") |>      # remove "Other, namely"
  str_split(",") |>                       # split into choices
  unlist() |>                             # flatten
  str_trim() |>                           # remove spaces
  discard(~ .x == "" | is.na(.x))         # drop empties and NA

counts <- table(clean); counts
percent <- round(100 * counts / nrow(wave2), 1); percent

# EBMH Understanding Similar
cat("\nEBMH Understanding Similar (Wave 2)\n")
print(table(wave2$EBMHUnderstandingSimilar, useNA = "ifany"))
print(round(100 * prop.table(table(wave2$EBMHUnderstandingSimilar, useNA = "ifany")), 1))


# Clean environment ---------------------------------------------------------------

rm(clean, counts, percent)
