# ===========================================================================
# Clinician Survey Project - Correlational Analysis -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 03.11.2026
#
# Associations of therapist and contextual characteristics with EBMH
# attitude, openness and use (Table F1), and Figures 2, 3, 4 and F1.
# Output: outputs/cor_table_by_predictor.xlsx, plots/Figure2..., Figure3...,
#         Figure4..., FigureF1...
# ===========================================================================


### 1. Variables ----------------------------------------------------------------
# Outcomes
DVs <- c("EBMH_use", "EBMH_attitude_mean", "EBMH_openness")

# Continuous predictors (scale means, 5-point single items, VAS, age, years)
continuous_preds <- c("EBMH_skills_mean", "perception_gap_mean",
                      "workplace_influences_mean", "employer_influences_mean",
                      "research_experience_mean", "EBMH_PsychDisorders_mean",
                      "Age", "YearsExperience",
                      "EBMH_importance_clinical_expertise",
                      grep("^SchoolSlider", names(data), value = TRUE))

# Ordinal predictors (Spearman)
ordinal_preds <- c("FamiliarityEBMH", "UniversityEducation")

# Dichotomous predictors (point-biserial)
binary_preds <- c("Country", "Wave")

# Categorical predictors (eta squared)
nominal_preds <- c("GenderIdentity", "Degree", "WorkSetting",
                   "LengthTrajectory", "ProfessionalBackground")

all_vars <- c(DVs, continuous_preds, ordinal_preds, binary_preds, nominal_preds)

# Variable classes
for (var in ordinal_preds) data[[var]] <- as.ordered(data[[var]])
for (var in c(binary_preds, nominal_preds)) data[[var]] <- as.factor(data[[var]])


### 2. Associations with the three outcomes -----------------------------------
# Pearson's r between continuous variables; Spearman's rho if one variable is
# ordinal (decided in choose_corr()); point-biserial r for dichotomous and
# eta squared for categorical predictors.
corr_vars <- c(DVs, continuous_preds, ordinal_preds)

results <- list()
rowi <- 1

for (dv in DVs) {
  for (pred in setdiff(all_vars, dv)) {
    
    if (!pred %in% names(data)) next
    
    x <- data[[dv]]
    y <- data[[pred]]
    
    if (pred %in% corr_vars) {
      res    <- choose_corr(x, y)
      method <- res$method
      stat   <- res$cor
      nobs   <- res$n
      ci     <- if (grepl("spearman", method, ignore.case = TRUE)) {
        ci_spearman(stat, nobs)
      } else {
        ci_fisher(stat, nobs)
      }
      conf   <- 0.95
      
    } else if (pred %in% binary_preds) {
      res    <- point_biserial(x, y)
      method <- "point-biserial"
      stat   <- res$r
      nobs   <- res$n
      ci     <- ci_fisher(stat, nobs)
      conf   <- 0.95
      
    } else if (pred %in% nominal_preds) {
      res    <- eta_squared(x, y)
      method <- "eta-squared"
      stat   <- res$eta2
      nobs   <- res$n
      ci     <- ci_eta2(x, y)   # 90% CI, see Method
      conf   <- 0.90
    }
    
    results[[rowi]] <- data.frame(
      Predictor = pred,
      DV        = dv,
      Method    = method,
      Statistic = as.numeric(stat),
      CI_low    = as.numeric(ci[1]),
      CI_high   = as.numeric(ci[2]),
      CI_level  = as.numeric(conf),
      N         = as.integer(nobs),
      stringsAsFactors = FALSE
    )
    rowi <- rowi + 1
  }
}

results_df <- do.call(rbind, results)

# Effect-size labels (based on values rounded to two decimals)
results_df$EffectSize <- mapply(
  categorize_effect,
  stat   = results_df$Statistic,
  method = results_df$Method,
  USE.NAMES = FALSE
)

# Which data collection(s) each predictor comes from
results_df <- merge(results_df, wave_info,
                    by.x = "Predictor", by.y = "Variable",
                    all.x = TRUE, sort = FALSE)

results_df <- results_df[order(results_df$Predictor, results_df$DV),
                         c("Predictor", "DV", "Method", "Statistic", "CI_low",
                           "CI_high", "CI_level", "N", "EffectSize", "Wave")]

# "+ 0" turns -0.00 into 0.00
results_df$Statistic <- round(results_df$Statistic, 2) + 0
results_df$CI_low    <- round(results_df$CI_low, 2) + 0
results_df$CI_high   <- round(results_df$CI_high, 2) + 0


### 3. Table F1 (Excel) -----------------------------------------------------------
cor_table <- results_df %>%
  mutate(
    Statistic = ifelse(is.na(Statistic), "", sprintf("%.2f", Statistic)),
    CI        = ifelse(is.na(CI_low) | is.na(CI_high), "",
                       sprintf("[%.2f, %.2f]", CI_low, CI_high)),
    CI_level  = ifelse(is.na(CI_level), "",
                       sprintf("%d%%", as.integer(round(CI_level * 100))))
  ) %>%
  select(Predictor, DV, Method, Statistic, CI, CI_level, N, EffectSize, Wave)

writexl::write_xlsx(cor_table, "outputs/cor_table_by_predictor.xlsx")


### 4. Figures 2 and F1: correlations of continuous and ordinal predictors -----
# Outcome-with-outcome correlations are reported in the text, not plotted.
corr_vis <- results_df %>%
  filter(Predictor %in% c(continuous_preds, ordinal_preds),
         grepl("pearson|spearman", Method, ignore.case = TRUE))

name_map <- c(
  "Age"                                = "Age",
  "YearsExperience"                    = "Years of experience",
  "FamiliarityEBMH"                    = "Familiarity with EBMH",
  "EBMH_skills_mean"                   = "EBMH-related skills",
  "research_experience_mean"           = "Previous research experience",
  "UniversityEducation"                = "EBMH-endorsing university education",
  "perception_gap_mean"                = "Perceived research–practice gap",
  "EBMH_importance_clinical_expertise" = "Clinical expertise valued over research",
  "SchoolSliderCBT"                    = "CBT orientation",
  "SchoolSliderPsychodynamic"          = "Psychodynamic orientation",
  "SchoolSliderAnalytical"             = "Analytical orientation",
  "SchoolSliderSystemic"               = "Systemic orientation",
  "SchoolSliderPersonCentered"         = "Person-centered orientation",
  "SchoolSliderNeuropsychology"        = "Neuropsychological orientation",
  "SchoolSliderACT"                    = "ACT orientation",
  "SchoolSliderHumanistic"             = "Humanistic orientation",
  "SchoolSliderMBT"                    = "MBT orientation",
  "SchoolSliderOther"                  = "Other orientation",
  "workplace_influences_mean"          = "Colleague support",
  "employer_influences_mean"           = "Employer support",
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

# Same predictor order in all three panels: largest absolute association on top
order_predictors <- function(df) {
  lev <- df %>%
    group_by(PredLabel) %>%
    summarise(MaxAbs = max(abs(Statistic), na.rm = TRUE), .groups = "drop") %>%
    arrange(MaxAbs) %>%
    pull(PredLabel)
  df$PredLabel <- factor(df$PredLabel, levels = lev)
  df
}

corr_vis <- order_predictors(corr_vis)

# Forest plot (a function instead of ggplot2's %+%, which psych masks)
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
    scale_x_continuous(limits = c(-0.75, 0.85), breaks = breaks_x,
                       labels = labels_x, expand = expansion(mult = 0.02)) +
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

## Figure F1: all predictors (Appendix F)
cor_forest <- build_forest(corr_vis,
                           breaks_x = c(-0.5, 0, 0.5),
                           labels_x = c("-.50", "0", ".50"))

ggsave("plots/FigureF1_all_correlations.png", cor_forest,
       width = 13, height = 8.5, dpi = 600, bg = "white")

## Figure 2: predictors with at least one association of |r| >= .15
corr_main <- corr_vis %>%
  group_by(PredLabel) %>%
  filter(max(abs(Statistic), na.rm = TRUE) >= 0.15) %>%
  ungroup() %>%
  order_predictors()

Figure2_correlations <- build_forest(
  corr_main,
  breaks_x = c(-0.5, -0.25, 0, 0.25, 0.5, 0.75),
  labels_x = c("-.50", "-.25", "0", ".25", ".50", ".75")
)

ggsave("plots/Figure2_correlations.png", Figure2_correlations,
       width = 12, height = 5.5, dpi = 600, bg = "white")


### 5. Figure 3: dichotomous predictors ------------------------------------------
plot_country <- make_split_raincloud(
  data, DVs, Country,
  plot_title = "Country", legend_title = "Country"
)

plot_background <- make_split_raincloud(
  data, DVs, ProfessionalBackground,
  plot_title = "Professional Background", legend_title = "Background"
)

# Gender: only female and male shown (other categories n = 1 each)
data$GenderIdentity_recode <- ifelse(
  data$GenderIdentity %in% c("Male", "Female"),
  as.character(data$GenderIdentity), NA
)

plot_gender <- make_split_raincloud(
  data, DVs, GenderIdentity_recode,
  plot_title = "Gender Identity", legend_title = "Gender"
)

# Degree: shortened labels
data$Degree_plot <- factor(
  ifelse(is.na(data$Degree), NA,
         ifelse(grepl("PhD", data$Degree), "PhD Degree", "Master's Degree")),
  levels = c("Master's Degree", "PhD Degree")
)

plot_degree <- make_split_raincloud(
  data, DVs, Degree_plot,
  plot_title = "Degree", legend_title = "Degree"
)

combined_cat_cor <- arrangeGrob(plot_country, plot_background,
                                plot_gender, plot_degree, ncol = 2)

ggsave("plots/Figure3_categorical_variables.png", combined_cat_cor,
       width = 11, height = 7, dpi = 600, bg = "white")


### 6. Figure 4: work setting ------------------------------------------------------
# "Other" is left out; examples in brackets are removed from the labels
ws_data <- data %>%
  filter(!is.na(WorkSetting), !grepl("^Other", WorkSetting)) %>%
  mutate(WorkSetting = sub(" \\(e\\.g\\..*\\)$", "", as.character(WorkSetting)))

ws_n <- ws_data %>%
  count(WorkSetting, name = "n_setting")

ws_long <- ws_data %>%
  select(WorkSetting, all_of(DVs)) %>%
  pivot_longer(all_of(DVs), names_to = "DV", values_to = "value",
               values_drop_na = TRUE) %>%
  left_join(ws_n, by = "WorkSetting") %>%
  mutate(
    DV          = factor(dv_map[DV],
                         levels = c("EBMH attitude", "EBMH openness", "EBMH use")),
    Setting     = factor(WorkSetting),
    WorkSetting = paste0(WorkSetting, " (n = ", n_setting, ")"),
    WorkSetting = forcats::fct_reorder(WorkSetting, value, .fun = median)
  )

# One colour per work setting (alphabetical order of the settings)
pal_colors_ws <- c("#5B8DB8", "#735965", "#C1AEB7", "#4A6C6F",
                   "#AF97A2", "#7FB685", "#437549", "#708090")

gg <- ggplot(ws_long, aes(x = WorkSetting, y = value, fill = Setting)) +
  geom_boxplot(width = 0.75, colour = "black", alpha = 0.85,
               outlier.shape = NA) +
  geom_jitter(width = 0.2, height = 0.05, alpha = 0.3, size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5,
               fill = "white") +
  facet_wrap(~ DV, ncol = 1) +
  coord_flip(ylim = c(1, 5)) +
  scale_fill_manual(values = pal_colors_ws) +
  labs(x = NULL, y = "Score") +
  theme_plots() +
  theme(legend.position = "none")

save_plot(gg, "Figure4_work_setting", width = 12, height = 10)


# Clean environment ---------------------------------------------------------------
rm(list = intersect(c(
  "DVs", "continuous_preds", "ordinal_preds", "binary_preds", "nominal_preds",
  "all_vars", "corr_vars", "var", "results", "rowi", "dv", "pred", "x", "y",
  "res", "method", "stat", "nobs", "ci", "conf", "results_df", "cor_table",
  "corr_vis", "corr_main", "name_map", "dv_map", "family_map",
  "order_predictors", "build_forest", "cor_forest", "Figure2_correlations",
  "plot_country", "plot_background", "plot_gender", "plot_degree",
  "combined_cat_cor", "ws_data", "ws_n", "ws_long", "pal_colors_ws", "gg"
), ls()))

data$GenderIdentity_recode <- NULL
data$Degree_plot <- NULL
