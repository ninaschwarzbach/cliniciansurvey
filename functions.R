# Clinician Survey Project - Functions
# Author: Nina Schwarzbach
# Start date: 01.10.2025
# Date last updated: 

library(dplyr)

# Data Cleaning
    
  ## Function for transforming Likert (5 point) to numbers
    likert_to_num <- function(x) {
      x <- trimws(tolower(x))  # remove spaces, lowercase
      # Only map valid Likert responses
      case_when(
        x %in% c("strongly agree") ~ 5,
        x %in% c("agree") ~ 4,
        x %in% c("neither agree nor disagree") ~ 3,
        x %in% c("disagree") ~ 2,
        x %in% c("strongly disagree") ~ 1,
        TRUE ~ NA_real_  # everything else becomes NA
      )
    }

# Descriptives
  ##  Function for frequency table
    freq_table <- function(data, col) {
      data %>%
        count(!!sym(col), name = "n") %>%
        mutate(percent = n / sum(n) * 100) %>%
        bind_rows(summarise(., !!sym(col) := "Total", n = sum(n), percent = sum(percent)))
    }
  
  
# Plots? 