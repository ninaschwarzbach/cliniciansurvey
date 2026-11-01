# ===========================================================================
# Clinician Survey Project - Descriptives -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 20.08.2026
# ===========================================================================



# Descriptives Core outcomes 
## compute descriptives by Wave and total
desc_table_1 <- describe_continuous_by_group(data, desc_vars, group = "Wave", save = TRUE, file_prefix = "outputs/descriptives_continuous_main"); desc_table_1

## Plots -------------------------------------------------------------------

# 5.1 Core outcomes (single items) -> stacked bars
plot_outcomes <- likert_panel(
  data, names(labels_items), labels = labels_items, ncol = 2,
  title = "Core EBMH outcomes"
)
save_plot(plot_outcomes, "likert_outcomes", width = 11, height = 6)

# 5.2 Scale means -> boxplots
plot_means <- box_panel(
  data, names(labels_means), labels = labels_means, ncol = 2,
  title = "EBMH scale means by wave"
)
save_plot(plot_means, "box_scale_means", width = 8, height = 7)

# 5.3 Feeling updated (Wave 2 only) -> stacked bars
plot_updated <- likert_panel(
  data %>% filter(Wave == "Wave 2"),
  names(labels_updated), labels = labels_updated, ncol = 1,
  response_labels = c("Not at all updated", "Rather not updated",
                      "Neutral", "Rather updated", "Very updated"),
  title = "Feeling updated about EBMH, by context (Wave 2)"
)
save_plot(plot_updated, "likert_updated", width = 10, height = 7)

# 5.4 Colleagues' endorsement (Wave 2 only) -> stacked bars
plot_endorsement <- likert_panel(
  data %>% filter(Wave == "Wave 2"),
  names(labels_endorsement), labels = labels_endorsement, ncol = 1,
  response_labels = c("Not at all endorsed", "Rather not endorsed",
                      "Neutral", "Rather endorsed", "Very endorsed"),
  title = "Colleagues' endorsement of EBMH, by context (Wave 2)"
)
save_plot(plot_endorsement, "likert_endorsement", width = 10, height = 7)

# Clean environment ---------------------------------------------------------------
rm(desc_vars, desc_table_1, plot_outcomes, plot_means,
   plot_updated, plot_endorsement)
