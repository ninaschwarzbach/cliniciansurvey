
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

# Hidden environment for functions
  .func_env <- new.env(parent = .GlobalEnv)

# Source to hidden environment
  source("functions.R", local = .func_env)
  source("maps.R", local = .func_env)

# Attach environment
  attach(.func_env, pos = 2)

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

  # Remove from environment 
    rm(cat_vars, cont_vars, violin_age, violin_experience)
    
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
        
      # Remove from environment 
        rm(likert_cols, scale_items, all_items, item_stats) ### Check more items to remove :) 
        
        
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
        
      # Remove from environment 
        rm(min_scale, max_scale)
        
# Descriptives Core outcomes 
  
  ## Vector with all variables            
    desc_vars <- c(mean_vars,
      "FamiliarityEBMH", 
      "UpdatedConferences", "UpdatedWorkplace",
      "UpdatedSupervision", "UpdatedPrivate", "UpdatedOther",
      "EBMHEndorsementConferences", "EBMHEndorsementWorkplace",
      "EBMHEndorsementSupervision", "EBMHEndorsementPrivate",
      "EBMHEndorsementOther")
    
  ## compute descriptives by Wave and total
    desc_table_1 <- describe_continuous_by_group(data, desc_vars, group = "Wave", save = TRUE, file_prefix = "outputs/descriptives_continuous_main"); desc_table_1
    
  ## Plots
      
    # Rename Columns so it works with previous functions 
        plot_data <- data
        plot_data <- plot_data %>% 
          rename(
            `Use of EBMH (mean)` = EBMH_use_mean,
            `EBMH Skills (mean)` = EBMH_skills_mean,
            `Perception Gap (mean)` = perception_gap_mean,
            `Attitude Toward EBMH (mean)` = EBMH_attitude_mean,
            `Workplace Influences (mean)` = workplace_influences_mean,
            `Employer Influences (mean)` = employer_influences_mean,
            `Research Experience (mean)` = research_experience_mean,
            `Importance EBMH (mean)` = EBMH_PsychDisorders_mean,
            `Familiarity with EBMH` = FamiliarityEBMH,
            `Updated Conferences` = UpdatedConferences,
            `Updated Workplace` = UpdatedWorkplace,
            `Updated Supervision` = UpdatedSupervision,
            `Updated Private Study` = UpdatedPrivate,
            `Updated Other Methods` = UpdatedOther,
            `EBMH Endorsement Conferences` = EBMHEndorsementConferences,
            `EBMH Endorsement Workplace` = EBMHEndorsementWorkplace,
            `EBMH Endorsement Supervision` = EBMHEndorsementSupervision,
            `EBMH Endorsement Private Settings` = EBMHEndorsementPrivate,
            `EBMH Endorsement (Other Contexts)` = EBMHEndorsementOther
          )
        
      
    # Loop through all variables plots OLD
        for (var_old in names(label_map)) {
          info <- label_map[[var_old]]
          v <- info$y
          violin_plot <- split_violin_total(plot_data, v, title = info$title)
          obj_name <- gsub(" ", "_", paste0("violin_", v))
          assign(obj_name, violin_plot, envir = .GlobalEnv)  
          print(violin_plot)
          ggsave(filename = file.path("plots", info$filename), plot = violin_plot, width = 6, height = 4, dpi = 300)
          message("Saved: ", info$filename)
        }
        
        
    # Combined plots 
        combined_violin_dependentvars <- grid.arrange(`violin_Use_of_EBMH_(mean)`, `violin_Attitude_Toward_EBMH_(mean)`, ncol = 2)
        ggsave(
          "plots/combined_violin_dependentvars.png",
          combined_violin_dependentvars,
          width = 10,  
          height = 5,   
          dpi = 300
        ); combined_violin_dependentvars
        
        combined_violin_independentvars <- grid.arrange(`violin_EBMH_Skills_(mean)`, `violin_Perception_Gap_(mean)`, 
                                                        `violin_Workplace_Influences_(mean)`, `violin_Employer_Influences_(mean)`, 
                                                        `violin_Research_Experience_(mean)`, violin_Familiarity_with_EBMH,
                                                        ncol = 2)
        ggsave(
          "plots/combined_violin_independentvars.png",
          combined_violin_independentvars,
          width = 10,  
          height = 15,   
          dpi = 300
        ); combined_violin_independentvars
          
        combined_violin_endorsement <- grid.arrange(violin_EBMH_Endorsement_Workplace, violin_EBMH_Endorsement_Supervision,
                                                    violin_EBMH_Endorsement_Conferences, violin_EBMH_Endorsement_Private_Settings, 
                                                    `violin_EBMH_Endorsement_(Other_Contexts)`,
                                                    ncol = 2)
        ggsave(
          "plots/combined_violin_endorsement.png",
          combined_violin_endorsement,
          width = 10,  
          height = 15,   
          dpi = 300
        ); combined_violin_endorsement 
        
        combined_violin_updated <- grid.arrange(violin_Updated_Conferences, violin_Updated_Private_Study, 
                                                violin_Updated_Supervision, violin_Updated_Workplace, 
                                                `violin_Updated_Other_Methods`,
                                                ncol = 2)
        ggsave(
          "plots/combined_violin_updated.png",
          combined_violin_updated,
          width = 10,  
          height = 15,   
          dpi = 300
        ); combined_violin_updated
        
  ## EBMH use (composite)
  ## EBMH attitudes (composite)
  ## Violin/raincloud plots by wave and country
        
        # remove here desc_vars, desc_table1, plot_data and all plots
        ### continue here: examine all plots if they look good and 

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


# Remove local environment 
  detach(.func_env)
  rm(.func_env)