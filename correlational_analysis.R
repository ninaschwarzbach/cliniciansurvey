# ===========================================================================
# Clinician Survey Project - Correlational Analysis -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 04.11.2025
# ===========================================================================


### 5. Descriptives 

## 5.1 Defining important  variables ---------------------------------------------------------------

# "Dependent" variables
DVs <- c("EBMH_use", "EBMH_attitude_mean", "EBMH_openness")  # Fixed 'EBMH Openness' to 'EBMH_openness'

# Continuous/interval-like predictors
continuous_preds <- c("EBMH_skills_mean", "perception_gap_mean", "workplace_influences_mean",
                      "employer_influences_mean", "research_experience_mean",
                      "EBMH_PsychDisorders_mean",
                      "Age", "YearsExperience", 
                      "TreatmentManuals_Freq", 
                      grep("^SchoolSlider", names(data), value = TRUE))

# Ordinal predictors
ordinal_preds <- c("FamiliarityEBMH", "UpdatedConferences", "UpdatedWorkplace", "UpdatedSupervision",
                   "UpdatedPrivate", "UpdatedOther", "UniversityEducation")

# Binary predictors
binary_preds <- c("Country", "Wave", "UseEvidence_YesNo", "TreatmentManuals")

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

## 5.3 Correlations   ---------------------------------------------------------------

# Make results list
results <- list()
rowi <- 1

# Calculate correlations
for (dv in DVs) {
  if (!dv %in% names(data)) next
  for (pred in setdiff(all_vars, dv)) {
    if (!pred %in% names(data)) next
    
    x <- data[[dv]]
    y <- data[[pred]]

    method <- NA; stat <- NA; pval <- NA; nobs <- NA
    
    # continuous DV vs continuous predictor -> Pearson or Spearman (normality check)
    if (is.numeric(x) && is_continuous_var(pred)) {
      res <- choose_corr(x, as.numeric(y))
      method <- res$method
      stat <- res$cor
      pval <- res$p
      nobs <- res$n
    }
    # continuous DV vs ordinal predictor -> Spearman
    else if (is.numeric(x) && is_ordinal_var(pred)) {
      res <- choose_corr(x, as.numeric(as.ordered(y)))
      method <- ifelse(res$method == "pearson", "pearson (both normal)", "spearman")
      stat <- res$cor; pval <- res$p; nobs <- res$n
    }
    # continuous DV vs binary predictor -> point-biserial (pearson using binary numeric)
    else if (is.numeric(x) && is_binary_var(pred)) {
      res <- point_biserial(x, y)
      method <- "point-biserial (pearson on 0/1)"
      stat <- res$r; pval <- res$p; nobs <- res$n
    }
    # continuous DV vs nominal predictor -> eta-squared (ANOVA)
    else if (is.numeric(x) && is_nominal_var(pred)) {
      eta <- eta_squared(x, y)
      method <- "eta-squared (ANOVA)"
      stat <- if (!is.null(eta$eta2)) eta$eta2 else NA
      pval <- if (!is.null(eta$p)) eta$p else NA
      nobs <- if (!is.null(eta$n)) eta$n else NA
    }
    # ordinal DV vs continuous predictor -> Spearman
    else if (is.ordered(x) && is_continuous_var(pred)) {
      res <- choose_corr(as.numeric(x), as.numeric(y))
      method <- "spearman (ordinal DV vs continuous pred)"
      stat <- res$cor; pval <- res$p; nobs <- res$n
    }
    # ordinal DV vs ordinal predictor -> Spearman (or polychoric but we use Spearman)
    else if (is.ordered(x) && is_ordinal_var(pred)) {
      res <- choose_corr(as.numeric(x), as.numeric(as.ordered(y)))
      method <- "spearman"
      stat <- res$cor; pval <- res$p; nobs <- res$n
    }
    # ordinal DV vs binary predictor -> point-biserial (Spearman could be used; we use Pearson on ranks)
    else if (is.ordered(x) && is_binary_var(pred)) {
      # correlate numeric ranks with 0/1
      res <- point_biserial(as.numeric(x), y)
      method <- "point-biserial (on ordinal as numeric)"
      stat <- res$r; pval <- res$p; nobs <- res$n
    }
    # nominal vs nominal -> Cramer's V
    else if (is_nominal_var(dv) && is_nominal_var(pred)) {
      v <- cramers_v(x, y)
      # p-value from chisq
      t <- tryCatch(chisq.test(table(x, y), correct = FALSE), error = function(e) NULL)
      pv <- if (!is.null(t)) as.numeric(t$p.value) else NA
      method <- "Cramer's V"
      stat <- v; pval <- pv; nobs <- sum(table(x, y))
    }
    # nominal DV vs continuous predictor -> eta-squared (swap order)
    else if (is_nominal_var(dv) && is.numeric(y)) {
      eta <- eta_squared(y, x)
      method <- "eta-squared (ANOVA, nominal DV as group)"
      stat <- if (!is.null(eta$eta2)) eta$eta2 else NA
      pval <- if (!is.null(eta$p)) eta$p else NA
      nobs <- if (!is.null(eta$n)) eta$n else NA
    }
    # other combos fallback to Spearman or try cor.test if numeric
    else {
      if (is.numeric(x) && is.numeric(y)) {
        res <- choose_corr(x, y)
        method <- res$method; stat <- res$cor; pval <- res$p; nobs <- res$n
      } else {
        method <- "unsupported combo - skipped"
        stat <- NA; pval <- NA; nobs <- sum(complete.cases(x, y))
      }
    }
    
    results[[rowi]] <- data.frame(
      DV = dv,
      Predictor = pred,
      Method = method,
      Statistic = stat,
      P.value = pval,
      N = nobs,
      stringsAsFactors = FALSE
    )
    rowi <- rowi + 1
  }
}

results_df <- do.call(rbind, results)

# tidy up numeric columns
results_df$Statistic <- as.numeric(results_df$Statistic)
results_df$P.value <- as.numeric(results_df$P.value)
results_df$N <- as.integer(results_df$N)

#  merge
results_df <- merge(
  results_df,
  wave_info,
  by.x = "Predictor",
  by.y = "Variable",
  all.x = TRUE
)

# sort columns
results_df <- results_df[order(results_df$DV, results_df$Predictor), ]


# Then round
results_df$Statistic <- format(round(results_df$Statistic, 2), nsmall = 2)
results_df$P.value   <- format(round(results_df$P.value, 3), nsmall = 3)

# Add effect size estimation

# add column 
results_df$EffectSize <- mapply(
  categorize_effect,
  stat = results_df$Statistic,   # make sure the capitalization matches your column name
  method = results_df$Method
)

# Save table 
write.csv(results_df, "outputs/cor_table.csv", row.names = FALSE, quote = TRUE)


# Reorder columns so Predictor comes first
results_df_reversed <- results_df[c("Predictor", "DV", setdiff(names(results_df), c("Predictor", "DV")))]

# Sort by Predictor, then DV
results_df_reversed <- results_df_reversed[order(results_df_reversed$Predictor, results_df_reversed$DV), ]

# Save as a new CSV
write.csv(results_df_reversed, "outputs/cor_table_by_predictor.csv", row.names = FALSE, quote = TRUE)

## 5.4 Plot   ---------------------------------------------------------------
corr_vis <- results_df %>%
  filter(grepl("pearson|spearman|point-biserial", Method, ignore.case = TRUE))

# Optional: take absolute value if you want to emphasize magnitude
corr_vis$AbsStat <- abs(corr_vis$Statistic)

cor_plot <- ggplot(corr_vis, aes(x = Predictor, y = DV, fill = Statistic)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "darkblue", high = "darkred", mid = "white", midpoint = 0,
                       name = "r / ρ") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()) +
  labs(title = "Correlation heatmap of continuous/ordinal variables",
       x = "Variable 1", y = "Variable 2")

ggsave(
  "plots/cor_plot.png",
  cor_plot,
  width = 20,  
  height = 5,   
  dpi = 300
)


# Exploring different correlations in more detail 
describe.by(data$EBMH_attitude_mean, group = data$Country)

# Clean environment ---------------------------------------------------------------
rm(corr_vis, cor_plot, eta, res, results, results_df, 
   wave_info1, all_vars, binary_preds, continuous_preds, dv, 
   DVs, method, nobs, nominal_preds, ordinal_preds, 
   pred, pval, rowi, stat, var, x, y, results_df_reversed)
