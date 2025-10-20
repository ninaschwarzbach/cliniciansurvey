
# Clinician Survey Project - Analysis Script-
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 

# Load libraries
library(tidyverse)
library(psych)      
library(ggplot2)  
library(readxl) 
library(dplyr)
library(gghalves)
library(rlang)
library(gridExtra)
library(stringr)


# Set working directory 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load functions script
source("functions.R")

# Load cleaned data 
source("data_cleaning.R")

# Sample characteristics
  # Continuous vars
    data$GraduationYear <- as.numeric(data$GraduationYear) # was class character
    cont_vars <- c("Age", "YearsExperience", "GraduationYear")
    # compute descriptives by Wave and total
      dir.create("outputs", showWarnings = FALSE)
      desc_table <- describe_continuous_by_group(data, cont_vars, group = "Wave", save = TRUE, file_prefix = "outputs/descriptives_continuous"); desc_table
    
    # Plots
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

# Descriptive statistics
    
  ## Transform Likert to numbers 
    # define vector of Likert item names  ### continue here looking to identify likerts
        likert_cols <- names(data)[sapply(data, function(x) {
          any(str_detect(tolower(na.omit(as.character(x))),
                         "agree|disagree"))
        })]; likert_cols
    
    # convert text -> numeric using your likert_to_num function
        data <- data %>%
          mutate(across(all_of(likert_cols), ~ likert_to_num(.x)))
        
  ## Merge items to variables
        data$EBMH_use_mean <- rowMeans(data[, c("UseEBMH1", "UseEBMH2")], na.rm = TRUE)
        data$EBMH_skills_mean <- rowMeans(data[, c("EBMHSkills1", "EBMHSkills2", "EBMHSkills3")], na.rm = TRUE)
        data$perception_gap_mean <- rowMeans(data[, c("PerceptionGap1", "PerceptionGap2" )], na.rm = TRUE)
        data$EBMH_attitude_mean <- rowMeans(data[, c( "AttitudeEBMH1", "AttitudeEBMH2", "AttitudeEBMH3" , "AttitudeEBMH4")], na.rm = TRUE)
        data$workplace_influences_mean <- rowMeans(data[, c("WorkplaceInfluences1", "WorkplaceInfluences2", 
                                                            "WorkplaceInfluences3", "WorkplaceInfluences4")], na.rm = TRUE)
        data$employer_influences_mean <- rowMeans(data[, c("EmployerInfluences1", "EmployerInfluences2",
                                                           "EmployerInfluences3", "EmployerInfluences4",                
                                                           "EmployerInfluences5", "EmployerInfluences6",
                                                           "EmployerInfluences7")], na.rm = TRUE)
        data$research_experience_mean <- rowMeans(data[, c( "ResearchExperience1",  "ResearchExperience2")], na.rm = TRUE)
        data$EBMH_PsychDisorders_mean <- rowMeans(data[, c("EBMHPsychDisorders1", "EBMHPsychDisorders2")], na.rm = TRUE)
        
        
        # mean variables
        mean_vars <- c("EBMH_use_mean", "EBMH_skills_mean", "perception_gap_mean", 
                       "EBMH_attitude_mean", "workplace_influences_mean", 
                       "employer_influences_mean", "research_experience_mean", 
                       "EBMH_PsychDisorders_mean")
        
    
  ## Item-level descriptive stats: mean, SD, skew, kurtosis
      # Define the items for each scale 
        scale_items <- list(
          EBMH_use = c("UseEBMH1", "UseEBMH2"),
          EBMH_skills = c("EBMHSkills1", "EBMHSkills2", "EBMHSkills3"),
          perception_gap = c("PerceptionGap1", "PerceptionGap2"),
          EBMH_attitude = c("AttitudeEBMH1", "AttitudeEBMH2", "AttitudeEBMH3", "AttitudeEBMH4"),
          workplace_influences = c("WorkplaceInfluences1", "WorkplaceInfluences2", 
                                   "WorkplaceInfluences3", "WorkplaceInfluences4"),
          employer_influences = c("EmployerInfluences1", "EmployerInfluences2", 
                                  "EmployerInfluences3", "EmployerInfluences4", 
                                  "EmployerInfluences5", "EmployerInfluences6", 
                                  "WorkplaceInfluences5", "EmployerInfluences7"),
          research_experience = c("ResearchExperience1", "ResearchExperience2"), # Corrected from duplicate
          EBMH_PsychDisorders = c("EBMHPsychDisorders1", "EBMHPsychDisorders2")
        )
        
      # Combine all items into a single vector
        all_items <- unlist(scale_items)
        
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
        
        ### Still cronbach's alpha or so??? 
        
        
  ## Identify floor/ceiling effects or low variance items
        
      # Calculate floor/ceiling effects
        floor_ceiling_results <- t(sapply(data[mean_vars], floor_ceiling, min_val = min_scale, max_val = max_scale))
        cat("Floor and Ceiling Effects (Proportions):\n")
        print(round(floor_ceiling_results, 3))
        
      # Calculate variance and standard deviation for each variable
        variance_stats <- data.frame(
          Variance = sapply(data[mean_vars], var, na.rm = TRUE),
          Std_Dev = sapply(data[mean_vars], sd, na.rm = TRUE)
        )
        
        cat("\nVariance and Standard Deviation:\n")
        print(round(variance_stats, 3))
        
      # Identify low variance items (e.g., SD < 1 for a 1-5 scale)
        low_variance_threshold <- 1  
        low_variance_items <- variance_stats[variance_stats$Std_Dev < low_variance_threshold, ]
        cat("\nLow Variance Items (SD < 1):\n")
        print(low_variance_items)
        
        ### rm from environment what you don't need anymore 
        
# Descriptives Core outcomes ### continue here all descriptive variables by wave with plots
  ## EBMH use (composite)
  ## EBMH attitudes (composite)
  ## Violin/raincloud plots by wave and country

# Correlational analyses
  ## Correlations with EBMH use
  ## Correlations with EBMH attitudes
  ## Correlations among  other variables

# Wave 2 specific analyses
  ## Importance of EBMH in psychotic disorder treatment
  ## Professional interactions & evidence exchange
  ## Use of treatment manuals
  ## Descriptive percentages, stacked bar plots

# Open-ended responses
  ## Extract answers so they can be analysed qualitatively outside of this


