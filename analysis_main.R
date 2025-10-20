
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
      likert_df <- data %>%
        select(where(~ any(str_detect(tolower(as.character(.x)),
                                      "agree|disagree|neither agree nor disagree"))))
      
    # Check what got selected
    names(likert_df)
      likert_items <- c("AttitudeEBMH1", "AttitudeEBMH2", "AttitudeEBMH3",
                      "AttitudeEBMH4","EBMHSkills1", "EBMHSkills2", "EBMHSkills3")
   
    
    # convert text -> numeric using your likert_to_num function
    data <- data %>%
      mutate(across(all_of(likert_items), ~ likert_to_num(.x)))
    
  ## Item-level descriptive stats: mean, SD, skew, kurtosis
  ## Identify floor/ceiling effects or low variance items

# Descriptives Core outcomes
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


