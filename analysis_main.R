
# Clinician Survey Project - Analysis Script-
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 

# Load libraries
library(tidyverse)
library(psych)      
library(ggplot2)  
library(readxl) 

# Set working directory 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load functions script
source("functions.R")

# Load cleaned data 
source("data_cleaning.R")


    

  ## Consent and eligibility checks
    ## Consent 
      freq_table(wave1[-c(1:2), ], "Q5") ### change names questions here
      freq_table(wave1[-c(1:2), ], "Q6")
      freq_table(wave2[-c(1:2), ], "Q5")
      freq_table(wave2[-c(1:2), ], "Q6")
      
    ## Delete participants not from NL or DE 
      freq_table(wave1[-c(1:2), ], "Q29")
      freq_table(wave2[-c(1:2), ], "Q15")
    
  ## Save dataset excluding those who do not consent
      
      
  ## Outlier inspection
  ## Data preparation 

### Note: Check levels of variables, Maybe make lists with different levels of variables? Also merge items into variables. 

  ## Duplicate participants

# Sample characteristics
  ## Age, gender, education, years of experience, country
  ## Work setting, therapy length, profession
  ## Compare waves

# Descriptive statistics
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


