# ===========================================================================
# Clinician Survey Project - Level-Item Statistics -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 19.08.2026
# ===========================================================================


## Item-level descriptive stats: mean, SD, skew, kurtosis
# Define the items for each scale 
scale_items <- list(
  EBMH_use = c("UseEBMH1", "UseEBMH2"),
  EBMH_skills = c("EBMHSkills1", "EBMHSkills2", "EBMHSkills3"),
  perception_gap = c("PerceptionGap1", "PerceptionGap2"),
  EBMH_attitude = c("AttitudeEBMH1", "AttitudeEBMH2", "AttitudeEBMH3", "AttitudeEBMH4"),
  workplace_influences = c("WorkplaceInfluences1", "WorkplaceInfluences2", 
                           "WorkplaceInfluences3", "WorkplaceInfluences4", 
                           "WorkplaceInfluences5"),
  employer_influences = c("EmployerInfluences1", "EmployerInfluences2", 
                          "EmployerInfluences3", "EmployerInfluences4", 
                          "EmployerInfluences5", "EmployerInfluences6", 
                           "EmployerInfluences7"),
  research_experience = c("ResearchExperience1", "ResearchExperience2"), # Corrected from duplicate
  EBMH_PsychDisorders = c("EBMHPsychDisorders1", "EBMHPsychDisorders2")
)

# Reliability


reliability_results <- lapply(names(scale_items), function(scale_name) {
  
  items <- intersect(scale_items[[scale_name]], names(data))
  n_items <- length(items)
  
  if (n_items < 2) {
    return(data.frame(
      Scale = scale_name,
      N_Items = n_items,
      Cronbach_Alpha = NA_real_,
      Guttman_Lambda2 = NA_real_,
      Mean_r = NA_real_,
      Note = "Not enough items"
    ))
  }
  
  item_data <- data[, items, drop = FALSE]
  
  # Cronbach's alpha
  alpha_out <- psych::alpha(
    item_data,
    check.keys = FALSE,
    warnings = FALSE
  )
  
  # Guttman's lambda 2
  lambda2 <- guttman_lambda2(item_data)
  
  data.frame(
    Scale = scale_name,
    N_Items = n_items,
    Cronbach_Alpha = round(alpha_out$total$raw_alpha, 3),
    Guttman_Lambda2 = round(lambda2, 3),
    Mean_r = round(alpha_out$total$average_r, 3),
    Note = ifelse(
      n_items == 2,
      "Two-item scale; interpret reliability cautiously",
      ""
    )
  )
}) %>%
  bind_rows()

cat("\nReliability Results:\n")
print(reliability_results)

# Correlation for 2 item scales 
cor(data$UseEBMH1, data$UseEBMH2, use = "pairwise.complete.obs")
cor(data$PerceptionGap1, data$PerceptionGap2, use = "pairwise.complete.obs")

# Checking if attitude is improved with less items
full <- data[, c("AttitudeEBMH1", "AttitudeEBMH2",
                 "AttitudeEBMH3", "AttitudeEBMH4")]

alpha(full)$total
guttman_lambda2(full)

# Without item 4
no4 <- data[, c("AttitudeEBMH1", "AttitudeEBMH2",
                "AttitudeEBMH3")]

alpha(no4)$total
guttman_lambda2(no4)

# Without items 3 and 4
no34 <- data[, c("AttitudeEBMH1", "AttitudeEBMH2")]

alpha(no34)$total
guttman_lambda2(no34)
# Change items with low internal consistency
data$EBMH_use <- data$UseEBMH1 # only one item for use
data$EBMH_openness <- data$UseEBMH2 # making this a separate item
data$EBMH_attitude_mean <- rowMeans(data[, c("AttitudeEBMH1", "AttitudeEBMH2")],
                                    na.rm = TRUE) # two items for attitude
# Decided to change this (see manuscript)
# AttitudeEBMH3 was reverse-coded at wave level to align with the attitude
# scale. As a standalone variable it should carry its original meaning
# (high = clinical experience valued over research), so reverse it back.
data$EBMH_importance_clinical_expertise <- 6 - data$AttitudeEBMH3

# Changing this also in wave_info
wave_info <- rbind(wave_info,
                   data.frame(Variable = "EBMH_importance_clinical_expertise",
                              Wave     = "Both waves"))
# Updated scale items 
scale_items <- list(
  EBMH_skills = c("EBMHSkills1", "EBMHSkills2", "EBMHSkills3"),
  perception_gap = c("PerceptionGap1", "PerceptionGap2"),
  EBMH_attitude = c("AttitudeEBMH1", "AttitudeEBMH2"), 
  workplace_influences = c("WorkplaceInfluences1", "WorkplaceInfluences2", 
                           "WorkplaceInfluences3", "WorkplaceInfluences4", 
                           "WorkplaceInfluences5"),
  employer_influences = c("EmployerInfluences1", "EmployerInfluences2", 
                          "EmployerInfluences3", "EmployerInfluences4", 
                          "EmployerInfluences5", "EmployerInfluences6", 
                          "EmployerInfluences7"),
  research_experience = c("ResearchExperience1", "ResearchExperience2"),
  EBMH_PsychDisorders = c("EBMHPsychDisorders1", "EBMHPsychDisorders2")
)

# Combine all items into a single vector
all_items <- unlist(scale_items)
extra_single_items <- c("UseEBMH1", "UseEBMH2", 
                        "EBMH_importance_clinical_expertise")  # Keep item4 out
all_items <- unique(c(unlist(scale_items), extra_single_items))


# Define scale range (adjust if not 1–5)
min_scale <- 1
max_scale <- 5

# Calculate item-level descriptive stats
item_stats <- data.frame(
  Item = all_items,
  Mean = sapply(data[all_items], function(x) round(mean(x, na.rm = TRUE), 3)),
  Std_Dev = sapply(data[all_items], function(x) round(sd(x, na.rm = TRUE), 3)),
  Floor_Prop = sapply(data[all_items], function(x) round(sum(x == min_scale, na.rm = TRUE) / length(na.omit(x)), 3)),
  Ceiling_Prop = sapply(data[all_items], function(x) round(sum(x == max_scale, na.rm = TRUE) / length(na.omit(x)), 3)),
  Min = sapply(data[all_items], function(x) round(min(x, na.rm = TRUE), 3)),
  Max = sapply(data[all_items], function(x) round(max(x, na.rm = TRUE), 3)),
  Missing_Prop = sapply(data[all_items], function(x) round(mean(is.na(x)), 3))
)

# Print results
cat("Item-Level Descriptive Statistics:\n")
print(item_stats)

## Identify floor/ceiling effects or low variance items

# Vector of mean variables ### university education missing
desc_vars <- c(
  "EBMH_use",            
  "EBMH_openness",          
  "EBMH_skills_mean",
  "perception_gap_mean",
  "EBMH_attitude_mean",         
  "workplace_influences_mean",
  "employer_influences_mean",
  "research_experience_mean",
  "EBMH_PsychDisorders_mean",
  "EBMH_importance_clinical_expertise", 
  "FamiliarityEBMH", 
  "UpdatedConferences", 
  "UpdatedWorkplace",
  "UpdatedSupervision", 
  "UpdatedPrivate", 
  "UpdatedOther",
  "EBMHEndorsementConferences", 
  "EBMHEndorsementWorkplace",
  "EBMHEndorsementSupervision", 
  "EBMHEndorsementPrivate",
  "EBMHEndorsementOther"
)

# Calculate floor/ceiling effects
floor_ceiling_results <- t(sapply(data[desc_vars], floor_ceiling, min_val = min_scale, max_val = max_scale))
cat("Floor and Ceiling Effects (Proportions):\n")
print(round(floor_ceiling_results, 3))

# Calculate variance and standard deviation for each variable
variance_stats <- data.frame(
  Variance = sapply(data[desc_vars], var, na.rm = TRUE),
  Std_Dev = sapply(data[desc_vars], sd, na.rm = TRUE)
)

cat("\nVariance and Standard Deviation:\n")
print(round(variance_stats, 3))

# Identify low variance items (e.g., SD < 1 for a 1-5 scale)
low_variance_threshold <- 1  
low_variance_items <- variance_stats[variance_stats$Std_Dev < low_variance_threshold, ]
cat("\nLow Variance Items (SD < 1):\n")
print(low_variance_items)



# Clean environment ---------------------------------------------------------------
rm(floor_ceiling_results, item_stats, low_variance_items, scale_items, variance_stats, 
   all_items, extra_single_items, low_variance_threshold, max_scale, min_scale, 
   reliability_results, no34, no4, full)

