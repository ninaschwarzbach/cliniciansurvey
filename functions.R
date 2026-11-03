# ===========================================================================
# Clinician Survey Project - Functions
# Author: Nina Schwarzbach
# Start date: 01.10.2025
# Date last updated: 03.11.2026
# ===========================================================================

# Data Cleaning
    
 ## Function for transforming Likert (5 point) to numbers
    likert_to_num <- function(x) {
      x <- trimws(tolower(x))  # remove spaces, lowercase
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
      
# Function for Guttman's lambda
      guttman_lambda2 <- function(x, use = "pairwise.complete.obs") {
      
      x <- as.data.frame(x)
      
      # Remove variables with no variance
      keep <- vapply(x, function(z) {
        stats::var(z, na.rm = TRUE) > 0
      }, logical(1))
      
      x <- x[, keep, drop = FALSE]
      
      if (ncol(x) < 2) {
        return(NA_real_)
      }
      
      S <- stats::cov(x, use = use)
      
      k <- ncol(S)
      
      # Variance of the total score
      total_variance <- sum(S)
      
      # Guttman's lambda 1
      lambda1 <- 1 - sum(diag(S)) / total_variance
      
      # Sum of squared off-diagonal covariances
      off_diagonal <- S[row(S) != col(S)]
      C2 <- sum(off_diagonal^2)
      
      # Guttman's lambda 2
      lambda2 <- lambda1 +
        sqrt((k / (k - 1)) * C2) / total_variance
      
      lambda2
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
      theme_plots <- function(base_size = 13) {
        theme_minimal(base_size = base_size, base_family = "sans") +
          theme(
            plot.title      = element_text(size = base_size + 3, hjust = 0.5),
            axis.title      = element_text(size = base_size + 1),
            axis.text       = element_text(size = base_size),
            strip.text      = element_text(size = base_size, face = "bold"),
            panel.grid.major = element_line(color = "gray90"),
            panel.grid.minor = element_blank(),
            panel.border    = element_blank(),
            plot.margin     = margin(10, 10, 10, 10)
          )
      }
    
  # Save plots
      save_plot <- function(plot, filename, folder = "plots",
                            width = 8, height = 6, dpi = 300) {
        if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
        ggsave(
          filename = file.path(folder, paste0(filename, ".png")),
          plot = plot, width = width, height = height, dpi = dpi, 
          bg = "white"
        )
        message("Plot saved to: ", file.path(folder, paste0(filename, ".png")))
        invisible(plot)
      }
      
      # Boxplot + raw points by wave. For continuous vars and scale means.
      wave_box <- function(data, var,
                           title = NULL, ylab = NULL,
                           y_limits = c(0.5, 5.5),
                           show_points = TRUE,
                           show_legend = FALSE) {
        
        var_sym  <- if (is.character(var) && length(var) == 1) rlang::sym(var) else rlang::ensym(var)
        var_name <- rlang::as_name(var_sym)
        
        if (!"Wave" %in% names(data)) stop("`data` must contain a column named 'Wave'.")
        if (!var_name %in% names(data)) stop("Variable not found in `data`: ", var_name)
        
        d <- data %>%
          dplyr::select(Wave, value = !!var_sym) %>%
          dplyr::filter(!is.na(Wave), !is.na(value), is.finite(value)) %>%
          dplyr::mutate(Wave = factor(Wave))
        
        if (nrow(d) == 0) stop("No valid observations for `", var_name, "`.")
        
        p <- ggplot(d, aes(x = Wave, y = value, fill = Wave)) +
          geom_boxplot(width = 0.5, colour = "black", alpha = 0.85,
                       outlier.shape = NA, na.rm = TRUE)
        
        if (show_points) {
          p <- p + geom_jitter(width = 0.12, height = 0.06, alpha = 0.25,
                               size = 1, na.rm = TRUE)
        }
        
        p <- p +
          stat_summary(fun = mean, geom = "point", shape = 23,
                       size = 2.5, fill = "white", colour = "black", na.rm = TRUE) +
          scale_fill_manual(values = wave_colors, drop = FALSE) +
          labs(title = title, x = NULL,
               y = if (is.null(ylab)) var_name else ylab) +
          theme_plots() +
          theme(legend.position = if (show_legend) "right" else "none")
        
        if (!is.null(y_limits)) p <- p + coord_cartesian(ylim = y_limits)
        p
      }
      
      # Faceted boxplots for several scale means at once
      box_panel <- function(data, vars, labels = NULL, ncol = 3,
                            y_limits = c(1, 5), title = NULL,
                            show_points = TRUE) {
        
        vars <- intersect(vars, names(data))
        if (!length(vars)) stop("None of `vars` found in data.")
        labs_vec <- if (is.null(labels)) stats::setNames(vars, vars) else labels[vars]
        
        d <- data %>%
          dplyr::select(Wave, dplyr::all_of(vars)) %>%
          tidyr::pivot_longer(dplyr::all_of(vars),
                              names_to = "Variable", values_to = "value",
                              values_drop_na = TRUE) %>%
          dplyr::filter(!is.na(Wave), is.finite(value)) %>%
          dplyr::mutate(
            Wave     = factor(Wave),
            Variable = factor(unname(labs_vec[Variable]), levels = unname(labs_vec))
          )
        
        p <- ggplot(d, aes(x = Wave, y = value, fill = Wave)) +
          geom_boxplot(width = 0.5, colour = "black", alpha = 0.85,
                       outlier.shape = NA, na.rm = TRUE)
        
        if (show_points) {
          p <- p + geom_jitter(width = 0.12, height = 0.06, alpha = 0.2,
                               size = 0.8, na.rm = TRUE)
        }
        
        p <- p +
          stat_summary(fun = mean, geom = "point", shape = 23,
                       size = 2, fill = "white", colour = "black", na.rm = TRUE) +
          facet_wrap(~ Variable, ncol = ncol) +
          scale_fill_manual(values = wave_colors, drop = FALSE) +
          labs(title = title, x = NULL, y = "Score") +
          theme_plots() +
          theme(legend.position = "none")
        
        if (!is.null(y_limits)) p <- p + coord_cartesian(ylim = y_limits)
        p
      }
      
      # Stacked percent bars for single Likert items
      likert_panel <- function(data, vars, labels = NULL, ncol = 2,
                               response_labels = c("Strongly disagree", "Disagree",
                                                   "Neutral", "Agree", "Strongly agree"),
                               title = NULL, min_pct_label = 7) {
        
        vars <- intersect(vars, names(data))
        if (!length(vars)) stop("None of `vars` found in data.")
        labs_vec <- if (is.null(labels)) stats::setNames(vars, vars) else labels[vars]
        
        d <- data %>%
          dplyr::select(Wave, dplyr::all_of(vars)) %>%
          tidyr::pivot_longer(dplyr::all_of(vars),
                              names_to = "Variable", values_to = "value") %>%
          dplyr::filter(!is.na(value), !is.na(Wave)) %>%
          dplyr::mutate(
            Variable = factor(unname(labs_vec[Variable]), levels = unname(labs_vec)),
            Response = factor(round(as.numeric(value)),
                              levels = 1:5, labels = response_labels)
          ) %>%
          dplyr::count(Variable, Wave, Response) %>%
          dplyr::group_by(Variable, Wave) %>%
          dplyr::mutate(pct = 100 * n / sum(n), n_group = sum(n)) %>%
          dplyr::ungroup()
        
        # Panel titles with n per wave, e.g. "EBMH Use\n(Wave 1: n = 129; Wave 2: n = 93)"
        panel_labs <- d %>%
          dplyr::distinct(Variable, Wave, n_group) %>%
          dplyr::arrange(Variable, Wave) %>%
          dplyr::group_by(Variable) %>%
          dplyr::summarise(
            Panel = paste0(Variable[1], "\n(",
                           paste0(Wave, ": n = ", n_group, collapse = "; "), ")"),
            .groups = "drop"
          )
        
        d <- d %>%
          dplyr::left_join(panel_labs, by = "Variable") %>%
          dplyr::mutate(
            Panel = factor(Panel, levels = panel_labs$Panel),
            Wave  = factor(Wave, levels = rev(sort(unique(as.character(Wave)))))  # Wave 1 on top
          )
        
        fills <- stats::setNames(likert_colors, response_labels)
        
        ggplot(d, aes(x = Wave, y = pct, fill = Response)) +
          geom_col(width = 0.65, colour = "white") +
          geom_text(aes(label = ifelse(pct >= min_pct_label, sprintf("%.0f%%", pct), "")),
                    position = position_stack(vjust = 0.5), size = 3, colour = "black") +
          facet_wrap(~ Panel, ncol = ncol) +
          coord_flip() +
          scale_fill_manual(values = fills, drop = FALSE) +
          scale_y_continuous(expand = expansion(mult = c(0, 0.02))) +
          labs(title = title, x = NULL, y = "% of respondents", fill = NULL) +
          theme_plots() +
          theme(legend.position = "bottom",
                panel.grid.major.y = element_blank())
      }
      
# Correlation functions 
    # Cramer's V 
    cramers_v <- function(x, y) {
      tab <- table(x, y)
      if (any(dim(tab) == 0)) return(NA)
      chi <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
      n <- sum(tab)
      k <- min(nrow(tab), ncol(tab))
      if (n == 0 || k == 1) return(NA)
      as.numeric(sqrt(chi / (n * (k - 1))))
    }
    
## CI helper functions
    # Eta-squared (for nominal predictor vs continuous DV)
    eta_squared <- function(dv, group) {
      # require at least two groups
      if (length(unique(group[!is.na(group)])) < 2) return(NA)
      df <- data.frame(dv = dv, group = as.factor(group))
      df <- df[complete.cases(df), ]
      if (nrow(df) < 3) return(NA)
      a <- aov(dv ~ group, data = df)
      ss <- summary(a)[[1]][, "Sum Sq"]
      # ss[1] = between, ss[2] = within
      eta2 <- ss[1] / sum(ss)
      pval <- summary(a)[[1]][, "Pr(>F)"][1]
      list(eta2 = as.numeric(eta2), p = as.numeric(pval), n = nrow(df))
    }
    
    # decide Pearson vs Spearman for two numeric-ish vectors
    choose_corr <- function(x, y, force_method = NULL) {
      # Track whether each input was ordinal *before* converting to numeric,
      # since that's what should drive the Pearson/Spearman decision.
      x_is_ordinal <- is.ordered(x)
      y_is_ordinal <- is.ordered(y)
      
      # Convert ordered factors to their numeric ranks.
      # Numeric variables remain unchanged.
      to_numeric <- function(z) {
        if (is.ordered(z)) {
          return(as.numeric(z))
        }
        
        if (is.factor(z)) {
          stop(
            "Unordered factors cannot be used in choose_corr(). ",
            "Use an association measure appropriate for nominal variables."
          )
        }
        
        as.numeric(z)
      }
      
      x_num <- to_numeric(x)
      y_num <- to_numeric(y)
      
      # Decide the method:
      # - Pearson only if BOTH variables are continuous (not ordinal).
      # - Spearman if either variable is ordinal.
      # - force_method overrides the automatic choice if supplied.
      method <- if (!is.null(force_method)) {
        match.arg(force_method, c("pearson", "spearman"))
      } else if (x_is_ordinal || y_is_ordinal) {
        "spearman"
      } else {
        "pearson"
      }
      
      # Use pairwise complete observations.
      ok <- complete.cases(x_num, y_num)
      n <- sum(ok)
      
      if (n < 3L) {
        return(list(method = method, cor = NA_real_, p = NA_real_, n = n))
      }
      
      x_complete <- x_num[ok]
      y_complete <- y_num[ok]
      
      # A correlation cannot be calculated when either variable is constant.
      if (
        length(unique(x_complete)) < 2L ||
        length(unique(y_complete)) < 2L
      ) {
        return(list(method = method, cor = NA_real_, p = NA_real_, n = n))
      }
      
      test <- tryCatch(
        cor.test(
          x_complete,
          y_complete,
          method = method,
          exact = if (method == "spearman") FALSE else NULL
        ),
        error = function(e) NULL
      )
      
      if (is.null(test)) {
        return(list(method = method, cor = NA_real_, p = NA_real_, n = n))
      }
      
      list(
        method = method,
        cor = unname(as.numeric(test$estimate)),
        p = as.numeric(test$p.value),
        n = n
      )
    }
    
    # point-biserial: correlate continuous with binary (factor with 2 levels)
    point_biserial <- function(cont, bin) {
      # convert bin to numeric 0/1
      ok <- complete.cases(cont, bin)
      n <- sum(ok)
      if (n < 3) return(list(r = NA, p = NA, n = n))
      binf <- as.factor(bin[ok])
      if (nlevels(binf) != 2) return(list(r = NA, p = NA, n = n))
      bin_num <- as.numeric(binf) - 1 # 0/1
      ct <- tryCatch(cor.test(cont[ok], bin_num, method = "pearson"), error = function(e) NULL)
      if (is.null(ct)) return(list(r = NA, p = NA, n = n))
      list(r = as.numeric(ct$estimate), p = as.numeric(ct$p.value), n = n)
    }
    
    
    # Fisher z transformation: used for Pearson r and point-biserial r
    ci_fisher <- function(r, n, conf = .95) {
      if (is.na(r) || is.na(n) || n < 4 || abs(r) >= 1) return(c(NA_real_, NA_real_))
      z    <- atanh(r)
      se   <- 1 / sqrt(n - 3)
      crit <- qnorm(1 - (1 - conf) / 2)
      tanh(c(z - crit * se, z + crit * se))
    }
    
    # Bonett & Wright (2000) standard error for Spearman's rho.
    ci_spearman <- function(rho, n, conf = .95) {
      if (is.na(rho) || is.na(n) || n < 5 || abs(rho) >= 1) return(c(NA_real_, NA_real_))
      z    <- atanh(rho)
      se   <- sqrt((1 + rho^2 / 2) / (n - 3))
      crit <- qnorm(1 - (1 - conf) / 2)
      tanh(c(z - crit * se, z + crit * se))
    }
    
    # Noncentral F confidence interval for eta-squared.
    ci_eta2 <- function(x, g, conf = .90) {
      ok <- complete.cases(x, g)
      x  <- x[ok]
      g  <- droplevels(as.factor(g[ok]))
      if (nlevels(g) < 2 || length(x) < 3) return(c(NA_real_, NA_real_))
      
      es <- effectsize::eta_squared(stats::aov(x ~ g), ci = conf,
                                    alternative = "two.sided", verbose = FALSE)
      
      lo <- es$CI_low[1]
      hi <- es$CI_high[1]
      c(if (is.na(lo)) 0 else lo, if (is.na(hi)) 0 else hi)
    }
    
    
    #  functions to check variable levels
    is_continuous_var <- function(xname) {
      xname %in% continuous_preds && xname %in% names(data)
    }
    
    is_ordinal_var <- function(xname) {
      xname %in% ordinal_preds && xname %in% names(data)
    }
    
    is_binary_var <- function(xname) {
      xname %in% binary_preds && xname %in% names(data)
    }
    
    is_nominal_var <- function(xname) {
      xname %in% nominal_preds && xname %in% names(data)
    }
    
    # Effect size estimation 
    categorize_effect <- function(stat, method) {
      if (is.na(stat)) return(NA)
      abs_stat <- abs(stat)
      
      # use different cutoffs by method type
      if (grepl("eta", method, ignore.case = TRUE)) {
        if (abs_stat < 0.01) return("very small")
        else if (abs_stat < 0.06) return("small")
        else if (abs_stat < 0.14) return("medium")
        else return("large")
      } else if (grepl("cramer", method, ignore.case = TRUE)) {
        if (abs_stat < 0.1) return("very small")
        else if (abs_stat < 0.3) return("small")
        else if (abs_stat < 0.5) return("medium")
        else return("large")
      } else { # for correlations (r, rho, point-biserial)
        if (abs_stat < 0.1) return("very small")
        else if (abs_stat < 0.3) return("small")
        else if (abs_stat < 0.5) return("medium")
        else return("large")
      }
    }
    

    # Make split plots for correlations

    make_split_raincloud <- function(
    data, DVs, group_var,
    violin_alpha = 0.5,
    box_width = 0.08,
    box_fill = "white",
    show_legend = TRUE,
    plot_title = NULL,
    legend_title = NULL,
    y_limits = c(1, 5),
    group_colors = c("#7FB685", "#AF97A2")
    ) {
      
      group_var  <- rlang::ensym(group_var)
      group_name <- rlang::as_name(group_var)
      if (is.null(legend_title)) legend_title <- group_name
      
      
      # Check requested columns
      missing_cols <- setdiff(
        c(DVs, group_name),
        names(data)
      )
      
      if (length(missing_cols) > 0) {
        stop(
          "Columns not found in `data`: ",
          paste(missing_cols, collapse = ", ")
        )
      }
      
      
      # Reshape data
      df <- data %>%
        dplyr::select(
          dplyr::all_of(DVs),
          !!group_var
        ) %>%
        dplyr::filter(
          !is.na(!!group_var)
        ) %>%
        dplyr::mutate(
          !!group_var := factor(!!group_var)
        ) %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(DVs),
          names_to = "DV",
          values_to = "value",
          values_drop_na = TRUE
        ) %>%
        dplyr::mutate(
          DV = dplyr::recode(
            DV,
            "EBMH_attitude_mean" = "Attitude",
            "EBMH_openness" = "Openness",
            "EBMH_use" = "Use"
          )
        )
      
      
      # Identify grouping levels
      groups <- levels(
        droplevels(df[[group_name]])
      )
      
      if (length(groups) != 2) {
        stop(
          "`", group_name,
          "` must contain exactly two observed levels. Found: ",
          paste(groups, collapse = ", ")
        )
      }
      
      
      # Automatically assign colors to the two groups
      colors_named <- stats::setNames(
        group_colors,
        groups
      )
      
      
      # First group
      d1 <- df %>%
        dplyr::filter(
          .data[[group_name]] == groups[1]
        ) %>%
        dplyr::group_by(DV) %>%
        dplyr::filter(
          dplyr::n() >= 2,
          dplyr::n_distinct(value) >= 2
        ) %>%
        dplyr::ungroup()
      
      
      # Second group
      d2 <- df %>%
        dplyr::filter(
          .data[[group_name]] == groups[2]
        ) %>%
        dplyr::group_by(DV) %>%
        dplyr::filter(
          dplyr::n() >= 2,
          dplyr::n_distinct(value) >= 2
        ) %>%
        dplyr::ungroup()
      
      
      # Build plot
      p <- ggplot2::ggplot()
      
      
      # Left half violin: group 1
      if (nrow(d1) > 0) {
        
        p <- p +
          gghalves::geom_half_violin(
            data = d1,
            ggplot2::aes(
              x = DV,
              y = value,
              fill = .data[[group_name]]
            ),
            side = "l",
            trim = F,
            alpha = violin_alpha,
            na.rm = TRUE
          )
      }
      
      
      # Right half violin: group 2
      if (nrow(d2) > 0) {
        
        p <- p +
          gghalves::geom_half_violin(
            data = d2,
            ggplot2::aes(
              x = DV,
              y = value,
              fill = .data[[group_name]]
            ),
            side = "r",
            trim = F,
            alpha = violin_alpha,
            na.rm = TRUE
          )
      }
      
      
      # Group 1 boxplot
      if (nrow(d1) > 0) {
        
        p <- p +
          ggplot2::geom_boxplot(
            data = d1,
            ggplot2::aes(
              x = DV,
              y = value
            ),
            width = box_width,
            fill = box_fill,
            color = "black",
            alpha = 0.9,
            outlier.shape = NA,
            position = ggplot2::position_nudge(x = -0.1),
            na.rm = TRUE
          )
      }
      
      
      # Group 2 boxplot
      if (nrow(d2) > 0) {
        
        p <- p +
          ggplot2::geom_boxplot(
            data = d2,
            ggplot2::aes(
              x = DV,
              y = value
            ),
            width = box_width,
            fill = box_fill,
            color = "black",
            alpha = 0.9,
            outlier.shape = NA,
            position = ggplot2::position_nudge(x = 0.1),
            na.rm = TRUE
          )
      }
      
      
      # Colors, labels, and theme
      p <- p +
        ggplot2::scale_fill_manual(
          values = colors_named,
          name = legend_title,
          drop = FALSE
        ) +
        ggplot2::scale_y_continuous(breaks = 1:5) +
        ggplot2::labs(
          title = if (!is.null(plot_title)) {
            plot_title
          } else {
            group_name
          },
          x = NULL,
          y = "Score"
        ) +
        ggplot2::theme_classic() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(size = 12),
          legend.position = if (show_legend) "right" else "none",
          legend.title = ggplot2::element_text(size = 13),
          legend.text = ggplot2::element_text(size = 12)
        )
      
      return(p)
    }