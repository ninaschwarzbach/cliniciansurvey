# ===========================================================================
# Clinician Survey Project - Level-Item Statistics -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 03.11.2026
#
# Reliability of the final scales (Method), item analysis of the original
# EBMH use and EBMH attitude items (Appendix B, Table B2) and item-level
# descriptive statistics (Table B1).
# Output: outputs/TableB1_item_stats.xlsx, outputs/TableB2_attitude_reliability.xlsx
# ===========================================================================


### 1. Reliability of the final scales (Method) -----------------------------
# Three or more items: Cronbach's alpha and Guttman's lambda 2
# Two items: inter-item correlation
scales_multi <- list(
  EBMH_skills          = c("EBMHSkills1", "EBMHSkills2", "EBMHSkills3"),
  workplace_influences = c("WorkplaceInfluences1", "WorkplaceInfluences2",
                           "WorkplaceInfluences3", "WorkplaceInfluences4",
                           "WorkplaceInfluences5"),
  employer_influences  = c("EmployerInfluences1", "EmployerInfluences2",
                           "EmployerInfluences3", "EmployerInfluences4",
                           "EmployerInfluences5", "EmployerInfluences6",
                           "EmployerInfluences7")
)

scales_two <- list(
  perception_gap      = c("PerceptionGap1", "PerceptionGap2"),
  research_experience = c("ResearchExperience1", "ResearchExperience2"),
  EBMH_attitude       = c("AttitudeEBMH1", "AttitudeEBMH2"),
  EBMH_PsychDisorders = c("EBMHPsychDisorders1", "EBMHPsychDisorders2")
)

reliability_multi <- bind_rows(lapply(names(scales_multi), function(s) {
  items <- data[, scales_multi[[s]]]
  data.frame(
    Scale    = s,
    N_Items  = ncol(items),
    Alpha    = round(psych::alpha(items, check.keys = FALSE,
                                  warnings = FALSE)$total$raw_alpha, 2),
    Lambda2  = round(guttman_lambda2(items), 2)
  )
}))

reliability_two <- bind_rows(lapply(names(scales_two), function(s) {
  items <- scales_two[[s]]
  data.frame(
    Scale = s,
    r     = round(cor(data[[items[1]]], data[[items[2]]],
                      use = "pairwise.complete.obs"), 2)
  )
}))

cat("\nReliability of the final scales (3+ items):\n")
print(reliability_multi, row.names = FALSE)
cat("\nInter-item correlation of the final two-item scales:\n")
print(reliability_two, row.names = FALSE)


### 2. Item analysis behind the scale decisions (Appendix B) ----------------
## 2.1 Original EBMH use items
cat("\nCorrelation of the two original EBMH use items (UseEBMH1, UseEBMH2):\n")
print(round(cor(data$UseEBMH1, data$UseEBMH2, use = "pairwise.complete.obs"), 2))

## 2.2 Original EBMH attitude items
attitude_full <- data[, c("AttitudeEBMH1", "AttitudeEBMH2",
                          "AttitudeEBMH3", "AttitudeEBMH4")]

# Item-total correlations of the four-item scale
# r.drop = corrected item-total correlation (item vs. sum of the other items)
# r.cor  = item-total correlation corrected for item overlap and reliability
cat("\nEBMH attitude, four items: item-total correlations\n")
print(round(psych::alpha(attitude_full, check.keys = FALSE,
                         warnings = FALSE)$item.stats[, c("r.drop", "r.cor")], 2))

# Ceiling effect of Item 4
item4 <- na.omit(data$AttitudeEBMH4)
cat("\nAttitudeEBMH4: M =", round(mean(item4), 2),
    "| SD =", round(sd(item4), 2),
    "| % at 4-5 =", round(100 * mean(item4 >= 4), 1), "\n")

## 2.3 Table B2: sequential removal of Items 4 and 3
attitude_configs <- list(
  "Full scale (Items 1–4)" = c("AttitudeEBMH1", "AttitudeEBMH2",
                               "AttitudeEBMH3", "AttitudeEBMH4"),
  "Without Item 4"         = c("AttitudeEBMH1", "AttitudeEBMH2",
                               "AttitudeEBMH3"),
  "Without Items 3 and 4"  = c("AttitudeEBMH1", "AttitudeEBMH2")
)

tableB2 <- bind_rows(lapply(names(attitude_configs), function(cfg) {
  items     <- data[, attitude_configs[[cfg]]]
  alpha_out <- psych::alpha(items, check.keys = FALSE, warnings = FALSE)
  data.frame(
    Configuration = cfg,
    Items         = ncol(items),
    Alpha         = round(alpha_out$total$raw_alpha, 2),
    Lambda2       = round(guttman_lambda2(items), 2),
    Average_r     = round(alpha_out$total$average_r, 2)
  )
}))

# Correlation of the two remaining items, with df = pairs - 2
n_pairs <- sum(complete.cases(data$AttitudeEBMH1, data$AttitudeEBMH2))
r_att   <- cor(data$AttitudeEBMH1, data$AttitudeEBMH2,
               use = "pairwise.complete.obs")
tableB2$Correlation <- c("—", "—",
                         sprintf("r(%d) = %.2f", n_pairs - 2, r_att))

cat("\nTable B2: EBMH attitude reliability\n")
print(tableB2, row.names = FALSE)
writexl::write_xlsx(tableB2, "outputs/TableB2_attitude_reliability.xlsx")


### 3. Table B1: item-level descriptive statistics ---------------------------
# AttitudeEBMH3 is shown in the scored direction of importance of clinical
# expertise (EBMH_importance_clinical_expertise).
items_B1 <- c(
  UseEBMH1 = "EBMH use", UseEBMH2 = "EBMH openness",
  EBMHSkills1 = "EBMH skills", EBMHSkills2 = "EBMH skills",
  EBMHSkills3 = "EBMH skills",
  PerceptionGap1 = "perception gap", PerceptionGap2 = "perception gap",
  AttitudeEBMH1 = "EBMH attitude", AttitudeEBMH2 = "EBMH attitude",
  EBMH_importance_clinical_expertise = "Importance clinical expertise",
  WorkplaceInfluences1 = "colleague support",
  WorkplaceInfluences2 = "colleague support",
  WorkplaceInfluences3 = "colleague support",
  WorkplaceInfluences4 = "colleague support",
  WorkplaceInfluences5 = "colleague support",
  EmployerInfluences1 = "employer support", EmployerInfluences2 = "employer support",
  EmployerInfluences3 = "employer support", EmployerInfluences4 = "employer support",
  EmployerInfluences5 = "employer support", EmployerInfluences6 = "employer support",
  EmployerInfluences7 = "employer support",
  ResearchExperience1 = "research experience",
  ResearchExperience2 = "research experience",
  EBMHPsychDisorders1 = "EBMH psychotic", EBMHPsychDisorders2 = "EBMH psychotic"
)

tableB1 <- bind_rows(lapply(names(items_B1), function(item) {
  x <- na.omit(data[[item]])
  data.frame(
    Item        = ifelse(item == "EBMH_importance_clinical_expertise",
                         "AttitudeEBMH3", item),
    Scale       = items_B1[[item]],
    N           = length(x),
    M           = round(mean(x), 2),
    SD          = round(sd(x), 2),
    Floor_pct   = round(100 * mean(x == 1), 1),
    Ceiling_pct = round(100 * mean(x == 5), 1),
    Min         = min(x),
    Max         = max(x)
  )
}))

cat("\nTable B1: item-level descriptive statistics\n")
print(tableB1, row.names = FALSE)
writexl::write_xlsx(tableB1, "outputs/TableB1_item_stats.xlsx")


### 4. Floor and ceiling effects of the analysed variables -------------------
# Check only (not reported as a table): proportion of answers at 1 and 5,
# and SD, for all variables on a 1-5 scale
desc_vars <- c(
  "EBMH_use", "EBMH_openness", "EBMH_attitude_mean",
  "EBMH_importance_clinical_expertise", "EBMH_skills_mean",
  "perception_gap_mean", "workplace_influences_mean",
  "employer_influences_mean", "research_experience_mean",
  "EBMH_PsychDisorders_mean", "FamiliarityEBMH",
  "UpdatedConferences", "UpdatedWorkplace", "UpdatedSupervision",
  "UpdatedPrivate", "UpdatedOther",
  "EBMHEndorsementConferences", "EBMHEndorsementWorkplace",
  "EBMHEndorsementSupervision", "EBMHEndorsementPrivate",
  "EBMHEndorsementOther"
)

floor_ceiling_check <- data.frame(
  t(sapply(data[desc_vars], function(x)
    floor_ceiling(as.numeric(x), min_val = 1, max_val = 5))),
  SD = sapply(data[desc_vars], function(x) sd(as.numeric(x), na.rm = TRUE))
)

cat("\nFloor and ceiling effects (proportions) and SD:\n")
print(round(floor_ceiling_check, 3))


# Clean environment ---------------------------------------------------------------
rm(list = intersect(c(
  "scales_multi", "scales_two", "reliability_multi", "reliability_two",
  "attitude_full", "item4", "attitude_configs", "tableB2", "n_pairs", "r_att",
  "items_B1", "tableB1", "desc_vars", "floor_ceiling_check"
), ls()))

