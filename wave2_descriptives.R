# ===========================================================================
# Clinician Survey Project - Wave 2 Descriptives -
# Author: Nina Schwarzbach
# Start date: 19.11.2025
# Date last updated: 03.11.2026
#
# Frequencies of the Wave 2-only questions on the use of evidence,
# treatment manuals, research resources and EBMH understanding (Results).
# ===========================================================================


### 1. Use of evidence ----------------------------------------------------------
cat("\nUse of evidence (Wave 2)\n")
print(table(wave2$UseEvidence_YesNo, useNA = "ifany"))
print(round(100 * prop.table(table(wave2$UseEvidence_YesNo, useNA = "ifany")), 1))


### 2. Treatment manuals --------------------------------------------------------
cat("\nUse of treatment manuals (Wave 2)\n")
print(table(wave2$TreatmentManuals, useNA = "ifany"))
print(round(100 * prop.table(table(wave2$TreatmentManuals, useNA = "ifany")), 1))

cat("\nFrequency of treatment manual use (Wave 2)\n")
print(psych::describe(as.numeric(data$TreatmentManuals_Freq)))


### 3. Research resources used ---------------------------------------------------
# Asked only of participants who use evidence; multiple answers possible.
# Percentages refer to the participants who use evidence.
cat("\nResearch resources used (Wave 2)\n")
n_evidence_users <- sum(wave2$UseEvidence_YesNo == "Yes", na.rm = TRUE)
cat("Participants who use evidence:", n_evidence_users, "\n")
resources <- wave2$ResearchResources |>
  str_remove_all("Other, namely") |>
  str_split(",") |>
  unlist() |>
  str_trim() |>
  discard(~ is.na(.x) | .x == "")

resource_counts  <- table(resources)
resource_percent <- round(100 * resource_counts / n_evidence_users, 1)
print(resource_counts)
print(resource_percent)


### 4. Similar understanding of EBMH ----------------------------------------------
cat("\nEBMH understanding similar (Wave 2)\n")
print(table(wave2$EBMHUnderstandingSimilar, useNA = "ifany"))
print(round(100 * prop.table(table(wave2$EBMHUnderstandingSimilar, useNA = "ifany")), 1))


# Clean environment ---------------------------------------------------------------
rm(list = intersect(c("n_evidence_users", "resources", "resource_counts",
                      "resource_percent"), ls()))