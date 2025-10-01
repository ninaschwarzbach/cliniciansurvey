
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

# Upload Data
wave1 <- read.csv("data/raw/wave1_data.csv", stringsAsFactors = FALSE)
wave2 <- read.csv("data/raw/wave2_data.csv", stringsAsFactors = FALSE)


### Note: Both data sets, merge datasets (or merge after?)

# Data cleaning / exclusion
  ## Consent and eligibility checks
    ## Consent wave 1
    table(wave1$Q5, useNA = "ifany")  # infany includes NA's
    table(wave1$Q6, useNA = "ifany")
    ## Consent wave 2
    table(wave2$Q5, useNA = "ifany")
    table(wave2$Q6, useNA = "ifany")
    
    ### Note: Still write something about this. 
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


