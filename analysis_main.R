
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


# Set working directory 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load functions script
source("functions.R")

# Load cleaned data 
source("data_cleaning.R")

## Outlier inspection

### Note: Check levels of variables, Maybe make lists with different levels of variables? Also merge items into variables. 

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


