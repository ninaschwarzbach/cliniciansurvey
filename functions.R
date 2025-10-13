# Clinician Survey Project - Functions
# Author: Nina Schwarzbach
# Start date: 01.10.2025
# Date last updated: 

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
      "Neither Agree nor Disagree" = 3,
      "Agree" = 4,
      "Strongly agree" = 5
    )
  
  # Likert Levels 
    likert_levels <- c(
      "Strongly disagree",
      "Disagree",
      "Neither Agree nor Disagree",
      "Agree",
      "Strongly agree"
    )
    
  # Define reversed order
    likert_reversed <- rev(likert_levels)
    
  # Function to reverse text-based Likert scales
    reverse_likert_text <- function(x) {
      factor(x, levels = likert_levels, labels = likert_reversed)
    }
    
  # Function to replace NA -> 0 only when forced-choice is answered
      fill_na <- function(df, slider_cols, forced_col) {
        slider_cols <- slider_cols[slider_cols %in% colnames(df)]
        if (length(slider_cols) == 0) {
          message("No slider columns found.")
          return(df)
        }
        if (! (forced_col %in% colnames(df))) stop("Forced column not found: ", forced_col)
        
        # Flag rows where forced choice was answered
        forced_answered <- !is.na(df[[forced_col]]) & df[[forced_col]] != ""
        
        # Report before
        before <- sapply(df[slider_cols], function(x) sum(is.na(x)))
        message("NAs before (per slider):")
        print(before)
        
        # Loop slider-by-slider to avoid assignment shape issues
        for (colname in slider_cols) {
          # convert to numeric safely (if already numeric, this keeps it)
          df[[colname]] <- suppressWarnings(as.numeric(as.character(df[[colname]])))
          # Only replace NA -> 0 where SchoolForcedChoice was answered
          df[[colname]][forced_answered & is.na(df[[colname]])] <- 0
        }
        
        # Report after
        after <- sapply(df[slider_cols], function(x) sum(is.na(x)))
        message("NAs after (per slider):")
        print(after)
        
        # return modified df
        df
      }
      
    
# Descriptives
  ##  Function for frequency table
    freq_table <- function(data, col) {
      data %>%
        count(!!sym(col), name = "n") %>%
        mutate(percent = n / sum(n) * 100) %>%
        bind_rows(summarise(., !!sym(col) := "Total", n = sum(n), percent = sum(percent)))
    }
  
  
# Plots
  # Theme
    theme_plots <- function() {
      theme_minimal(base_size = 13, base_family = "sans") +
        theme(
          plot.title = element_text(size = 16, hjust = 0.5),
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          legend.position = "none",
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.margin = margin(10, 10, 10, 10)
        )
    }
    
  # Save plots
    save_plot <- function(plot, filename, folder = "plots", width = 8, height = 6, dpi = 300) {
      # Create folder if it doesn’t exist
      if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
      
      # Apply consistent theme and save
      final_plot <- plot + theme_plots()
      
      # Save to file (PNG by default)
      ggsave(
        filename = file.path(folder, paste0(filename, ".png")),
        plot = final_plot,
        width = width,
        height = height,
        dpi = dpi
      )
      
      message("✅ Plot saved to: ", file.path(folder, paste0(filename, ".png")))
      return(final_plot)
    }
    