# ===========================================================================
# Clinician Survey Project - Functions
# Author: Nina Schwarzbach
# Start date: 01.10.2025
# Date last updated: 
# ===========================================================================

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
  
  ## Describe cont variables by group and in total
    
    describe_continuous_by_group <- function(df, 
                                             vars, 
                                             group = "Wave", 
                                             save = FALSE, 
                                             file_prefix = "descriptives") { 
      stopifnot(is.data.frame(df)) 
      vars <- intersect(vars, names(df)) 
      if (length(vars) == 0) stop("No valid variables found in data.") 
      
      out <- list() 
      
      groups <- if (!is.null(group) && group %in% names(df)) { 
        unique(na.omit(as.character(df[[group]]))) 
      } else { 
        character(0) 
      } 
      
      # helper to compute stats for one vector 
      stats_for <- function(x) { 
        x_num <- suppressWarnings(as.numeric(x)) 
        n <- sum(!is.na(x_num)) 
        if (n == 0) { 
          return(tibble(
            N = 0, Mean = NA_real_, SD = NA_real_, 
            Min = NA_real_, Max = NA_real_, 
            Skewness = NA_real_, Kurtosis = NA_real_
          )) 
        } 
        d <- psych::describe(x_num) 
        tibble(
          N = as.integer(d$n), 
          Mean = unname(d$mean), 
          SD = unname(d$sd), 
          Min = unname(d$min), 
          Max = unname(d$max), 
          Skewness = unname(d$skew), 
          Kurtosis = unname(d$kurtosis)
        ) 
      } 
      
      # by each group 
      if (length(groups) > 0) { 
        for (g in groups) {
          df_g <- df %>% filter((.data[[group]] == g)) 
          for (v in vars) { 
            s <- stats_for(df_g[[v]]) 
            out[[length(out) + 1]] <- tibble(Group = g, Variable = v) %>% bind_cols(s) 
          } 
        } 
      } 
      
      # total 
      for (v in vars) { 
        s <- stats_for(df[[v]]) 
        out[[length(out) + 1]] <- tibble(Group = "Total", Variable = v) %>% bind_cols(s) 
      } 
      
      # combine everything
      res <- bind_rows(out) %>% 
        mutate(
          Mean = round(Mean, 3), 
          SD = round(SD, 3), 
          Skewness = round(Skewness, 3), 
          Kurtosis = round(Kurtosis, 3)
        ) %>% 
        arrange(Variable, Group) 
      
      if (save) { 
        fname <- paste0(file_prefix, "_continuous_by_group.csv") 
        write.csv(res, fname, row.names = FALSE) 
        message("Saved descriptives to: ", fname) 
      } 
      
      return(res) 
    }

 ## Describe cat variables by group and in total
      
      describe_categorical <- function(df,
                                          vars,
                                          group = "Wave",
                                          include_na = FALSE,
                                          file_prefix = "Table2_Categorical",
                                          out_folder = "outputs",
                                          pct_digits = 1) {
      stopifnot(is.data.frame(df))
      vars <- intersect(vars, names(df))
      if (length(vars) == 0) stop("No valid variables found in data.")
      
      # Determine groups and preserve their order; will append "Total" later
      groups <- if (!is.null(group) && group %in% names(df)) {
        unique(na.omit(as.character(df[[group]])))
      } else {
        character(0)
      }
      
      # compute counts & percents for one variable and one data subset
      compute_table <- function(vec) {
        if (!include_na) vec <- vec[!is.na(vec)]
        if (length(vec) == 0) return(tibble(Category = character(0), n = integer(0), percent = numeric(0)))
        tab <- as.data.frame(table(vec), stringsAsFactors = FALSE)
        names(tab) <- c("Category", "n")
        tab <- as_tibble(tab) %>%
          mutate(n = as.integer(n),
                 percent = round(100 * n / sum(n), pct_digits))
        return(tab)
      }
      
      # Build long-format entries for each group
      rows <- list()
      if (length(groups) > 0) {
        for (g in groups) {
          df_g <- df %>% filter((.data[[group]] == g))
          for (v in vars) {
            tbl <- compute_table(df_g[[v]])
            if (nrow(tbl) == 0) {
              rows[[length(rows) + 1]] <- tibble(Variable = v, Category = NA_character_, Group = g, n = 0L, percent = NA_real_)
            } else {
              rows[[length(rows) + 1]] <- tbl %>%
                mutate(Variable = v, Group = g) %>%
                select(Variable, Category, Group, n, percent)
            }
          }
        }
      }
      
      # Add totals
      for (v in vars) {
        tbl_tot <- compute_table(df[[v]])
        if (nrow(tbl_tot) == 0) {
          rows[[length(rows) + 1]] <- tibble(Variable = v, Category = NA_character_, Group = "Total", n = 0L, percent = NA_real_)
        } else {
          rows[[length(rows) + 1]] <- tbl_tot %>%
            mutate(Variable = v, Group = "Total") %>%
            select(Variable, Category, Group, n, percent)
        }
      }
      
      long_tbl <- bind_rows(rows) %>%
        mutate(Group = factor(Group, levels = c(groups, "Total"))) %>%
        arrange(Variable, Category, Group) %>%
        mutate(percent = round(percent, pct_digits))
      
      # Build wide table: n (pct%) strings, pivot so each Group is a column
      wide_tbl <- long_tbl %>%
        mutate(n_pct = ifelse(is.na(percent), 
                              as.character(n), 
                              paste0(n, " (", percent, "%)"))) %>%
        select(Variable, Category, Group, n_pct) %>%
        pivot_wider(names_from = Group, values_from = n_pct) %>%
        arrange(Variable, Category)
      
      # Ensure output folder exists
      if (!dir.exists(out_folder)) dir.create(out_folder, recursive = TRUE)
      
      # Write CSV (outputs/<file_prefix>_wide.csv)
      out_fname <- file.path(out_folder, paste0(file_prefix, "_wide.csv"))
      
      # If file exists, try to remove it first (e.g., if Excel had it open previously)
      if (file.exists(out_fname)) {
        tryCatch({
          file.remove(out_fname)
        }, warning = function(w) {
          message("Could not remove existing file (perhaps it's open in Excel?): ", out_fname)
        }, error = function(e) {
          message("Error removing existing file: ", e$message)
        })
      }
      
      write.csv(wide_tbl, out_fname, row.names = FALSE)
      message("Wde categorical table saved to: ", out_fname)
      
      return(wide_tbl)
      }
    
    # Function to identify floor and ceiling effects
      floor_ceiling <- function(x, min_val, max_val) {
        n <- length(na.omit(x))  # Number of non-NA values
        floor_prop <- sum(x == min_val, na.rm = TRUE) / n
        ceiling_prop <- sum(x == max_val, na.rm = TRUE) / n
        return(c(floor = floor_prop, ceiling = ceiling_prop))
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
      
      message("Plot saved to: ", file.path(folder, paste0(filename, ".png")))
      return(final_plot)
    }
    
  # Half violin function
    split_violin_total <- function(data,
                                   var,
                                   title = NULL,
                                   wave1 = "Wave 1",
                                   wave2 = "Wave 2",
                                   total_name = "Total",
                                   palette = "Set2",
                                   box_fill = "grey80",
                                   box_width = 0.15,
                                   violin_alpha = 0.6,
                                   show_legend = TRUE,
                                   ...) {
      
      # Handle quoted/unquoted variable
      var_sym <- if (is.character(var) && length(var) == 1) sym(var) else enquo(var)
      
      # Sanity checks
      if (!"Wave" %in% names(data))
        stop("`data` must contain a column named 'Wave'.")
      if (!all(c(wave1, wave2) %in% unique(data$Wave)))
        stop(paste0("Please ensure both '", wave1, "' and '", wave2, "' exist in data$Wave."))
      
      # Prepare datasets
      data_total <- data %>% mutate(Wave = total_name)
      d1 <- data %>% filter(Wave == wave1)
      d2 <- data %>% filter(Wave == wave2)
      
      # Build plot
      p <- ggplot() +
        geom_half_violin(
          data = d1,
          aes(x = 1, y = !!var_sym, fill = Wave),
          side = "l", trim = FALSE, alpha = violin_alpha, ...
        ) +
        geom_half_violin(
          data = d2,
          aes(x = 1, y = !!var_sym, fill = Wave),
          side = "r", trim = FALSE, alpha = violin_alpha, ...
        ) +
        geom_boxplot(
          data = data_total,
          aes(x = 1, y = !!var_sym),
          width = box_width,
          fill = box_fill,
          color = "black",
          alpha = 0.9,
          outlier.shape = NA
        ) +
        scale_fill_brewer(palette = palette, name = "Wave") +
        labs(title = title, x = NULL, 
             y = as_label(var_sym)) +
        coord_cartesian(clip = "off") +
        theme_plots() + 
        theme(
          legend.position = if (show_legend) "right" else "none",
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 12)
        )
      
      return(p)
    }

    
    # Save violin plots
    save_violin <- function(plot,
                            filename,
                            folder = "plots",
                            legend_position = "right",
                            width = 5,
                            height = 6,
                            dpi = 300) {
      # Create folder if needed
      if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
      
      # Apply consistent theme and legend position
      final_plot <- plot +
        theme_plots() +
        theme(legend.position = legend_position)
      
      # Save to file
      ggsave(
        filename = file.path(folder, paste0(filename, ".png")),
        plot = final_plot,
        width = width,
        height = height,
        dpi = dpi
      )
      message("Violin plot saved to: ", file.path(folder, paste0(filename, ".png")))
      return(final_plot)
    }
    