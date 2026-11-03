# ===========================================================================
# Clinician Survey Project - Main Analysis Script
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 03.11.2026
# ===========================================================================

rm(list = ls())

### 1. Setup ---------------------------------------------------------------

## 1.1 Packages
packages <- c(
  "tidyverse",  # dplyr, tidyr, ggplot2, stringr, purrr, tibble, forcats
  "psych",      # descriptive statistics, Cronbach's alpha
  "gghalves",   # half-violin plots (Figure 3)
  "gridExtra",  # combining plots
  "patchwork",  # combining plots (Figure 5)
  "rlang",      # tidy evaluation inside the plot functions
  "writexl"     # Excel export of tables
)

# Only called as package::function(), so installed but not loaded
packages_no_load <- c(
  "effectsize", # confidence intervals for eta-squared
  "rstudioapi"  # setting the working directory
)

# Install packages that are not yet installed
missing_packages <- setdiff(c(packages, packages_no_load),
                            rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

# Load packages
invisible(lapply(packages, library, character.only = TRUE))

## 1.2 Working directory (the folder this script is saved in; needs RStudio)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## 1.3 Functions and labels in a hidden environment
if (".func_env" %in% search()) detach(".func_env")

.func_env <- new.env(parent = .GlobalEnv)
source("functions.R", local = .func_env)
source("maps.R",      local = .func_env)
attach(.func_env, pos = 2, name = ".func_env")

## 1.4 Output folders (emptied first, so every file in them comes from this run)
dir_outputs <- "outputs"
dir_plots   <- "plots"

for (folder in c(dir_plots, dir_outputs)) {
  if (dir.exists(folder)) {
    unlink(file.path(folder, "*"))
    message("Old files removed from ", folder, "/")
  } else {
    dir.create(folder)
  }
}


### 2. Data cleaning ---------------------------------------------------------
source("data_cleaning.R")

### 3. Sample characteristics ------------------------------------------------
# Tables 1 and 2, Figure D1
source("sample_characteristics.R")

### 4. Scale creation and item-level statistics ------------------------------
## 4.1 Scale creation
source("scale_creation.R")

## 4.2 Item-level statistics
source("level-item_stats.R")

### 5. Descriptives ----------------------------------------------------------
# Figures E1 and E2
source("descriptives.R")
source("wave2_descriptives.R")

### 6. Correlational analysis ------------------------------------------------
# Figures 2, 3, 4 and F1; correlation table (Appendix F)
source("correlational_analysis.R")

### 7. Wave 2-specific analysis ----------------------------------------------
# Figure 5
source("wave2_specific_analysis.R")

### 8. Open-ended responses --------------------------------------------------
source("open-ended_responses.R")


### 9. Clean up --------------------------------------------------------------
detach(".func_env")
rm(.func_env, folder, packages, packages_no_load, missing_packages,
   dir_outputs, dir_plots)

# R and package versions used for this run
sessionInfo()
  