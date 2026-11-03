# ===========================================================================
# Clinician Survey Project - Main Analysis Script (Public Dataset)
# Author: Nina Schwarzbach
# Start date: 03.11.2026
# Date last updated: 03.11.2026
#
# Runs the analysis on the anonymized public dataset. Use this script if you
# do not have the raw data. Age, gender, years of experience and the
# open-ended responses are not in the public dataset, so the sample
# characteristics (Tables 1 and 2, Figure D1), the open-ended responses and
# the associations involving age, years of experience and gender cannot be
# reproduced. All other tables and figures are the same as in the article.
#
# Input:  data/public/EBMH-ClinicianSurvey_data_anonymized.csv
# Output: tables in outputs_public_data/, figures in plots_public_data/
#         (both folders are emptied first; the results of the full analysis
#         in outputs/ and plots/ are not touched)
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

missing_packages <- setdiff(c(packages, packages_no_load),
                            rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)

invisible(lapply(packages, library, character.only = TRUE))

## 1.2 Working directory (the folder this script is saved in; needs RStudio)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## 1.3 Functions and labels in a hidden environment
if (".func_env" %in% search()) detach(".func_env")

.func_env <- new.env(parent = .GlobalEnv)
source("functions.R", local = .func_env)
source("maps.R",      local = .func_env)
attach(.func_env, pos = 2, name = ".func_env")

## 1.4 Output folders (separate from the full analysis; emptied first, so
##     every file in them comes from this run)
dir_outputs <- "outputs_public_data"
dir_plots   <- "plots_public_data"

for (folder in c(dir_plots, dir_outputs)) {
  if (dir.exists(folder)) {
    unlink(file.path(folder, "*"))
    message("Old files removed from ", folder, "/")
  } else {
    dir.create(folder)
  }
}


### 2. Load the public dataset ------------------------------------------------
# (replaces data_cleaning.R)
data <- read.csv("data/public/EBMH-ClinicianSurvey_data_anonymized.csv",
                 stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Wave 2 participants (used in wave2_descriptives.R)
wave2 <- data %>% filter(Wave == "Wave 2")

# Which variables were assessed in both waves or in one wave only
# (as in data_cleaning.R, section 15.3)
waves <- sort(unique(na.omit(data$Wave)))
wave_info <- data.frame(
  Variable = setdiff(names(data), "Wave"),
  Wave = sapply(setdiff(names(data), "Wave"), function(var) {
    present <- sapply(waves, function(w) any(!is.na(data[[var]][data$Wave == w])))
    if (all(present)) "Both waves"
    else if (present[1]) paste0(waves[1], " only")
    else if (present[2]) paste0(waves[2], " only")
    else "No valid data"
  }),
  stringsAsFactors = FALSE,
  row.names = NULL
)
rm(waves)


### 3. Scale creation and item-level statistics ------------------------------
source("scale_creation.R")
source("level-item_stats.R")

### 4. Descriptives ----------------------------------------------------------
# Tables 3, 4 and E1; Figures E1 and E2
source("descriptives.R")
source("wave2_descriptives.R")

### 5. Correlational analysis ------------------------------------------------
# Table F1 and Figures 2, 3, 4 and F1 (without age, years of experience and
# gender; Figure 3 without the gender panel)
source("correlational_analysis.R")

### 6. Wave 2-specific analysis ----------------------------------------------
# Figure 5
source("wave2_specific_analysis.R")


### 7. Clean up --------------------------------------------------------------
detach(".func_env")
rm(.func_env, folder, packages, packages_no_load, missing_packages,
   dir_outputs, dir_plots)

sessionInfo()