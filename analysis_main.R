
# ===========================================================================
# Clinician Survey Project - Analysis Script Main -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 03.11.2026
# ===========================================================================

rm(list = ls())

### 1. Setup ---------------------------------------------------------------

## 1.1 Install and Load Required Packages

packages <- c(
  "tidyverse",
  "psych",
  "ggplot2",
  "readxl",
  "dplyr",
  "gghalves",
  "rlang",
  "gridExtra",
  "patchwork",
  "stringr",
  "reshape2",
  "ggrain",
  "writexl", 
  "purrr", 
  "tibble"
)

# Install packages that are not yet installed
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed])
}

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))

## 1.2 Set working directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## 1.3 Make hidden environment for functions
# Add hidden environment
.func_env <- new.env(parent = .GlobalEnv)

# Source to hidden environment
source("functions.R", local = .func_env)
source("maps.R", local = .func_env)

# Attach environment
attach(.func_env, pos = 2)

# Clean previously saved tables and plots
  if (dir.exists("plots")) {
    unlink(file.path("plots", "*"), recursive = FALSE)  
    message("All old plots removed from ", "plots")
  }
  if (dir.exists("outputs")) {
    unlink(file.path("outputs", "*"), recursive = FALSE)  
    message("All old tables removed from ", "outputs")
  }


### 2. Data cleaning ---------------------------------------------------------------
# Load cleaned data
source("data_cleaning.R")

### 3. Sample Characteristics ---------------------------------------------------------------
source("sample_characteristics.R")

### 4. Scale Creation and Item-Level Statistics ---------------------------------------------------------------
## 4.1. Scale Creation
source("scale_creation.R")

## 4.2. Item-Level Statistics
source("level-item_stats.R")

### 5. Descriptives ---------------------------------------------------------------
source("descriptives.R")
source("wave2_descriptives.R")

### 6. Correlational analysis ---------------------------------------------------------------
source("correlational_analysis.R")

### 7. Wave2-specific analysis ---------------------------------------------------------------
source("wave2_specific_analysis.R")

### 8. Open-ended responses ---------------------------------------------------------------
 source("open-ended_responses.R")

### 9. Clean Environment ---------------------------------------------------------------
# Remove local environment 
  detach(.func_env)
  rm(.func_env)
  
  