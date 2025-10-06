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
    
  ## Merge 2 Likert Variables (Duplicates)
    merge_likert_numeric <- function(df, var1, var2, new_var_name, likert_map) {
      # Convert both to numeric using likert_map
      num1 <- likert_map[as.character(df[[var1]])]
      num2 <- likert_map[as.character(df[[var2]])]
      
      # Merge: take mean if both present, otherwise take non-NA
      merged <- ifelse(!is.na(num1) & !is.na(num2),
                       rowMeans(cbind(num1, num2), na.rm = TRUE),
                       ifelse(!is.na(num1), num1, num2))
      
      # Assign to new variable
      df[[new_var_name]] <- merged
      return(df)
    }
    
  # Likert map (customize for your scale)
    likert_map <- c(
      "Strongly disagree" = 1,
      "Disagree" = 2,
      "Neutral" = 3,
      "Agree" = 4,
      "Strongly agree" = 5
    )
    

# Descriptives
  ##  Function for frequency table
    freq_table <- function(data, col) {
      data %>%
        count(!!sym(col), name = "n") %>%
        mutate(percent = n / sum(n) * 100) %>%
        bind_rows(summarise(., !!sym(col) := "Total", n = sum(n), percent = sum(percent)))
    }
  
  
# Plots? 