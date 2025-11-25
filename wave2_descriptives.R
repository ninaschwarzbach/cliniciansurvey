# ===========================================================================
# Clinician Survey Project - Wave 2 Descriptives -
# Author: Nina Schwarzbach
# Start date: 19.11.2025
# Date last updated: 21.11.2025
# ===========================================================================

# Use of Evidence
cat("\n=== Use of Evidence (Wave 2) ===\n")
print(table(wave2$UseEvidence_YesNo, useNA = "ifany"))
print(round(100 * prop.table(table(wave2$UseEvidence_YesNo, useNA = "ifany")), 1))

# Use of Treatment Manuals
cat("\n=== Use of Treatment Manuals (Wave 2) ===\n")
print(table(wave2$TreatmentManuals, useNA = "ifany"))
print(round(100 * prop.table(table(wave2$TreatmentManuals, useNA = "ifany")), 1))

# Frequency of Treatment Manual Use
cat("\n=== Frequency Use of Treatment Manuals (Wave 2) ===\n")
print(psych::describe(as.numeric(data$TreatmentManuals_Freq)))

# Research Resources Used
cat("\n=== Research Resources Used (Wave 2) ===\n")
clean <- wave2$ResearchResources |>
  str_remove_all("Other, namely") |>      # remove "Other, namely"
  str_split(",") |>                       # split into choices
  unlist() |>                             # flatten
  str_trim() |>                           # remove spaces
  discard(~ .x == "" | is.na(.x))         # drop empties and NA

table(clean)
round(prop.table(table(clean)) * 100, 1)


# EBMH Understanding Similar
cat("\n=== EBMH Understanding Similar (Wave 2) ===\n")
print(table(wave2$EBMHUnderstandingSimilar, useNA = "ifany"))
print(round(100 * prop.table(table(wave2$EBMHUnderstandingSimilar, useNA = "ifany")), 1))


# Clean environment ---------------------------------------------------------------

rm(clean)
