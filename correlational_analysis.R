# ===========================================================================
# Clinician Survey Project - Correlational Analysis -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 19.08.2026
# ===========================================================================


### 5. Descriptives 

## 5.1 Defining important  variables ---------------------------------------------------------------

# "Dependent" variables
DVs <- c("EBMH_use", "EBMH_attitude_mean", "EBMH_openness")  

# Continuous/interval-like predictors
continuous_preds <- c("EBMH_skills_mean", "perception_gap_mean", "workplace_influences_mean",
                      "employer_influences_mean", "research_experience_mean",
                      "EBMH_PsychDisorders_mean",
                      "Age", "YearsExperience", 
                     "EBMH_importance_clinical_expertise", 
                      grep("^SchoolSlider", names(data), value = TRUE))

# Ordinal predictors
ordinal_preds <- c("FamiliarityEBMH", "UniversityEducation")

# Binary predictors
binary_preds <- c("Country", "Wave")

# Nominal predictors
nominal_preds <- c("GenderIdentity", "Degree", "WorkSetting", "LengthTrajectory", "ProfessionalBackground")  

# Combine all variables
all_vars <- c(DVs, continuous_preds, ordinal_preds, binary_preds, nominal_preds)

## 5.2 Fixing classes for variables ---------------------------------------------------------------

for (var in ordinal_preds) {
  data[[var]] <- as.ordered(data[[var]])  # Use as.ordered for ordinal data
}

for (var in binary_preds) {
  data[[var]] <- as.factor(data[[var]])
}

for (var in nominal_preds) {
  data[[var]] <- as.factor(data[[var]])
}

## 5.3 Correlations ------------------------------------------------------------


results <- list()
rowi <- 1

# Variables that should be analysed using Spearman correlations
spearman_vars <- c(DVs, continuous_preds, ordinal_preds)

for (dv in DVs) {
  
  if (!dv %in% names(data)) next
  
  for (pred in setdiff(all_vars, dv)) {
    
    if (!pred %in% names(data)) next
    
    x <- data[[dv]]
    y <- data[[pred]]
    
    # Spearman or Pearson correlation (choose_corr decides)
    if (pred %in% spearman_vars) {
      
      res <- choose_corr(x, y)
      
      method <- res$method
      stat   <- res$cor
      pval   <- res$p
      nobs   <- res$n
      ci     <- if (grepl("spearman", method, ignore.case = TRUE)) {
        ci_spearman(stat, nobs)
      } else {
        ci_fisher(stat, nobs)
      }
      conf   <- 0.95
    }
    
    # Point-biserial correlation
    else if (pred %in% binary_preds) {
      
      res <- point_biserial(x, y)
      
      method <- "point-biserial"
      stat   <- res$r
      pval   <- res$p
      nobs   <- res$n
      ci     <- ci_fisher(stat, nobs)
      conf   <- 0.95
    }
    
    # Eta-squared
    else if (pred %in% nominal_preds) {
      
      res <- eta_squared(x, y)
      
      method <- "eta-squared"
      stat   <- res$eta2
      pval   <- res$p
      nobs   <- res$n
      ci     <- ci_eta2(x, y)
      conf   <- 0.90
    }
    
    # Unsupported variable type
    else {
      
      method <- "unsupported"
      stat   <- NA_real_
      pval   <- NA_real_
      nobs   <- sum(complete.cases(x, y))
      ci     <- c(NA_real_, NA_real_)
      conf   <- NA_real_
    }
    
    results[[rowi]] <- data.frame(
      Predictor = pred,
      DV        = dv,
      Method    = method,
      Statistic = as.numeric(stat),
      CI_low    = as.numeric(ci[1]),
      CI_high   = as.numeric(ci[2]),
      CI_level  = as.numeric(conf),
      P.value   = as.numeric(pval),
      N         = as.integer(nobs),
      stringsAsFactors = FALSE
    )
    
    rowi <- rowi + 1
  }
}

# Combine results
results_df <- do.call(rbind, results)

# Add effect-size categories before formatting
results_df$EffectSize <- mapply(
  categorize_effect,
  stat   = results_df$Statistic,
  method = results_df$Method,
  USE.NAMES = FALSE
)

# Add wave information
results_df <- merge(
  results_df,
  wave_info,
  by.x  = "Predictor",
  by.y  = "Variable",
  all.x = TRUE,
  sort  = FALSE
)

# Select and order columns
results_df <- results_df[
  c(
    "Predictor",
    "DV",
    "Method",
    "Statistic",
    "CI_low",
    "CI_high",
    "CI_level",
    "P.value",
    "N",
    "EffectSize",
    "Wave"
  )
]

# Sort by predictor and DV
results_df <- results_df[
  order(results_df$Predictor, results_df$DV),
]

# Keep this version numeric for plots and further analyses
results_df$Statistic <- round(results_df$Statistic, 2)
results_df$CI_low    <- round(results_df$CI_low, 2)
results_df$CI_high   <- round(results_df$CI_high, 2)


cor_table <- results_df

# Format statistic with two decimal places
cor_table$Statistic <- ifelse(
  is.na(results_df$Statistic),
  "",
  sprintf("%.2f", results_df$Statistic)
)

# Format confidence interval as a single bracketed column
cor_table$CI <- ifelse(
  is.na(results_df$CI_low) | is.na(results_df$CI_high),
  "",
  sprintf("[%.2f, %.2f]", results_df$CI_low, results_df$CI_high)
)

# Format CI level as a percentage label
cor_table$CI_level <- ifelse(
  is.na(results_df$CI_level),
  "",
  sprintf("%d%%", as.integer(round(results_df$CI_level * 100)))
)

# Format p-values using the original, unrounded values
cor_table$P.value <- ifelse(
  is.na(results_df$P.value),
  "",
  ifelse(
    results_df$P.value < .001,
    "< .001",
    sprintf("%.3f", results_df$P.value)
  )
)

# Final column order for the exported table
cor_table <- cor_table[
  c(
    "Predictor",
    "DV",
    "Method",
    "Statistic",
    "CI",
    "CI_level",
    "P.value",
    "N",
    "EffectSize",
    "Wave"
  )
]

# Save directly as Excel file
writexl::write_xlsx(
  cor_table,
  "outputs/cor_table_by_predictor.xlsx"
)


## 5.4 Correlation forest plot -------------------------------------------------

## ---- Select the associations to display -----------------------------------
# Continuous/ordinal predictors only; outcome-on-outcome pairs are dropped
# (reported in the text instead).

cont_ord_vars <- unique(c(continuous_preds, ordinal_preds, DVs))

corr_vis <- results_df %>%
  filter(
    DV %in% DVs,
    Predictor %in% cont_ord_vars,
    !Predictor %in% DVs,
    grepl("pearson|spearman", Method, ignore.case = TRUE)
  ) %>%
  mutate(
    Statistic = as.numeric(trimws(Statistic)),
    CI_low    = as.numeric(CI_low),
    CI_high   = as.numeric(CI_high)
  )

## ---- Labels ---------------------------------------------------------------
name_map <- c(
  "Age"                                = "Age",
  "YearsExperience"                    = "Years of experience",
  "FamiliarityEBMH"                    = "Familiarity with EBMH",
  "EBMH_skills_mean"                   = "EBMH-related skills",
  "research_experience_mean"           = "Previous research experience",
  "UniversityEducation"                = "EBMH-endorsing university education",
  "perception_gap_mean"                = "Perceived research\u2013practice gap",
  "EBMH_importance_clinical_expertise" = "Clinical expertise valued over research",
  "SchoolSliderCBT"                    = "CBT orientation",
  "SchoolSliderPsychodynamic"          = "Psychodynamic orientation",
  "SchoolSliderAnalytical"             = "Analytical orientation",
  "SchoolSliderSystemic"               = "Systemic orientation",
  "SchoolSliderPersonCentered"         = "Person-centred orientation",
  "SchoolSliderNeuropsychology"        = "Neuropsychological orientation",
  "SchoolSliderACT"                    = "ACT orientation",
  "SchoolSliderHumanistic"             = "Humanistic orientation",
  "SchoolSliderMBT"                    = "MBT orientation",
  "SchoolSliderOther"                  = "Other orientation",
  "workplace_influences_mean"          = "Workplace influences",
  "employer_influences_mean"           = "Employer inluences",
  "EBMH_PsychDisorders_mean"           = "Importance of EBMH for psychotic disorders"
)

dv_map <- c(
  "EBMH_attitude_mean" = "EBMH attitude",
  "EBMH_openness"      = "EBMH openness",
  "EBMH_use"           = "EBMH use"
)

family_map <- c(
  "Age"                                = "Demographic",
  "YearsExperience"                    = "Demographic",
  "FamiliarityEBMH"                    = "EBMH background",
  "EBMH_skills_mean"                   = "EBMH background",
  "research_experience_mean"           = "EBMH background",
  "UniversityEducation"                = "EBMH background",
  "perception_gap_mean"                = "EBMH background",
  "EBMH_importance_clinical_expertise" = "EBMH background",
  "SchoolSliderCBT"                    = "Therapeutic orientation",
  "SchoolSliderPsychodynamic"          = "Therapeutic orientation",
  "SchoolSliderAnalytical"             = "Therapeutic orientation",
  "SchoolSliderSystemic"               = "Therapeutic orientation",
  "SchoolSliderPersonCentered"         = "Therapeutic orientation",
  "SchoolSliderNeuropsychology"        = "Therapeutic orientation",
  "SchoolSliderACT"                    = "Therapeutic orientation",
  "SchoolSliderHumanistic"             = "Therapeutic orientation",
  "SchoolSliderMBT"                    = "Therapeutic orientation",
  "SchoolSliderOther"                  = "Therapeutic orientation",
  "workplace_influences_mean"          = "Context",
  "employer_influences_mean"           = "Context",
  "EBMH_PsychDisorders_mean"           = "Clinical focus"
)

corr_vis <- corr_vis %>%
  mutate(
    Family     = factor(family_map[Predictor],
                        levels = c("Demographic", "EBMH background",
                                   "Therapeutic orientation", "Context",
                                   "Clinical focus")),
    PredLabel  = name_map[Predictor],
    DVLabel    = factor(dv_map[DV],
                        levels = c("EBMH attitude", "EBMH openness", "EBMH use")),
    SingleWave = grepl("Wave 1|Wave 2", Wave)
  )

## ---- Order predictors by largest absolute association ---------------------
# One shared ordering across all three panels so rows can be scanned across.
order_predictors <- function(df) {
  lev <- df %>%
    group_by(PredLabel) %>%
    summarise(MaxAbs = max(abs(Statistic), na.rm = TRUE), .groups = "drop") %>%
    arrange(MaxAbs) %>%          # ascending, so the largest ends up at the top
    pull(PredLabel)
  df$PredLabel <- factor(df$PredLabel, levels = lev)
  df
}

corr_vis <- order_predictors(corr_vis)

## ---- Plot builder ---------------------------------------------------------
# A function rather than ggplot2's %+%, which is masked by psych.
build_forest <- function(df, breaks_x, labels_x, base_size = 11) {
  
  ggplot(df, aes(x = Statistic, y = PredLabel)) +
    
    annotate("rect", xmin = -0.10, xmax = 0.10, ymin = -Inf, ymax = Inf,
             fill = "grey85", alpha = 0.35) +
    geom_vline(xintercept = 0, linewidth = 0.4, colour = "grey40") +
    
    geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                   height = 0, linewidth = 0.5, colour = "grey30") +
    geom_point(aes(shape = SingleWave),
               size = 2.1, fill = "white", colour = "#2F4F4F") +
    
    scale_shape_manual(
      values = c(`FALSE` = 16, `TRUE` = 21),
      labels = c(`FALSE` = "Both data collections",
                 `TRUE`  = "One data collection only"),
      name = NULL
    ) +
    
    facet_grid(Family ~ DVLabel, scales = "free_y", space = "free_y",
               switch = "y") +
    
    scale_x_continuous(
      limits = c(-0.75, 0.85),
      breaks = breaks_x,
      labels = labels_x,
      expand = expansion(mult = 0.02)
    ) +
    
    labs(x = "Correlation with 95% confidence interval", y = NULL) +
    
    theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major.y = element_line(colour = "grey93", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.spacing.x    = unit(1, "lines"),
      strip.text.y.left  = element_text(angle = 0, hjust = 0, face = "italic",
                                        size = base_size - 2),
      strip.text.x       = element_text(face = "bold", size = base_size - 1),
      strip.placement    = "outside",
      axis.text.x        = element_text(size = base_size - 2, colour = "grey20"),
      axis.text.y        = element_text(size = base_size - 2),
      axis.title.x       = element_text(size = base_size - 1,
                                        margin = margin(t = 8)),
      legend.position    = "bottom",
      plot.margin        = margin(6, 14, 6, 6)
    )
}

## ---- Full version (appendix) ----------------------------------------------
# Three ticks per panel; more than that is unreadable at this panel width.
cor_forest <- build_forest(
  corr_vis,
  breaks_x = c(-0.5, 0, 0.5),
  labels_x = c("-.50", "0", ".50")
)

cor_forest

ggsave("plots/cor_forest.png", cor_forest,
       width = 13, height = 8.5, dpi = 300)

## ---- Compact version (main text) ------------------------------------------
# Predictors whose largest association reaches |.15|.
corr_main <- corr_vis %>%
  group_by(PredLabel) %>%
  filter(max(abs(Statistic), na.rm = TRUE) >= 0.15) %>%
  ungroup() %>%
  order_predictors()

cor_forest_main <- build_forest(
  corr_main,
  breaks_x = c(-0.5, -0.25, 0, 0.25, 0.5, 0.75),
  labels_x = c("-.50", "-.25", "0", ".25", ".50", ".75")
)

cor_forest_main

ggsave("plots/cor_forest_main.png", cor_forest_main,
       width = 12, height = 5.5, dpi = 300)

## ---- Categorical predictors -----------------------------------

# Country
plot_country <- make_split_raincloud(
  data = data, DVs = DVs, group_var = Country,
  plot_title = "Country", legend_title = "Country"
)


# Professional background
plot_background <- make_split_raincloud(
  data, DVs, ProfessionalBackground,
  plot_title = "Professional Background", legend_title = "Background"
)

# Gender - recoded because the other categories have very few cases
data$GenderIdentity_recode <- ifelse(
  data$GenderIdentity %in% c("Male", "Female"),
  as.character(data$GenderIdentity),
  NA
)


plot_gender <- make_split_raincloud(
  data, DVs, GenderIdentity_recode,
  plot_title = "Gender Identity", legend_title = "Gender"
)

# Degree
  plot_degree <- make_split_raincloud(
    data, DVs, Degree_plot,
    plot_title   = "Degree",
    legend_title = "Degree"
  )

data$Degree_plot <- factor(
  data$Degree_plot,
  levels = c("Master's Degree", "PhD Degree")
)

plot_degree <- make_split_raincloud(
  data, DVs, Degree_plot,
  plot_title   = "Degree",
  legend_title = "Degree"
)

# Combine
combined_cat_cor <- grid.arrange(plot_country, plot_background,
                                 plot_gender, plot_degree, ncol = 2)

ggsave("plots/combined_cat_cor.png", combined_cat_cor,
       width = 11, height = 9, dpi = 300)

# Work setting plot
stack_df <- stack_df %>%
  mutate(WorkSetting = forcats::fct_reorder(WorkSetting, value, .fun = median, na.rm = TRUE))

gg <- ggplot(stack_df, aes(x = WorkSetting, y = value, fill = WorkSetting)) +
  geom_boxplot(width = 0.5, color = "black", outlier.shape = NA, alpha = 0.8) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 1, na.rm = TRUE) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2,
               fill = "white", na.rm = TRUE) +
  facet_wrap(~DV, ncol = 1) +
  coord_flip(ylim = c(1, 5)) +
  scale_fill_manual(values = pal_colors_ws) +
  labs(title = "EBMH Scores by Work Setting", x = NULL, y = "Score") +
  theme_plots() +
  theme(legend.position = "none")

save_plot(gg, "EBMH_WorkSetting", width = 10, height = 9)

# Clean environment ---------------------------------------------------------------
rm(corr_vis, cor_plot, res, results, results_df, 
   all_vars, binary_preds, continuous_preds, dv, 
   DVs, method, nobs, nominal_preds, ordinal_preds, 
   pred, pval, rowi, stat, var, x, y, 
   pal_colors_ws, cont_ord_vars, stack_df, plot_background, 
   plot_country, plot_degree, plot_gender, data_filtered, 
   combined_cat_cor, gg, name_map, cor_table)

data$GenderIdentity_recode <- NULL

