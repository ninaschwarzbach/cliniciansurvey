# ===========================================================================
# Clinician Survey Project - Wave 2-Specific Analysis -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 03.11.2026
#
# Contexts in which Wave 2 participants encounter colleagues: colleagues'
# endorsement of EBMH and feeling updated about EBMH per context (Figure 5).
# Means and SDs per context are in Table 4 (descriptives.R).
# Output (folder set in analysis_main.R): Figure5_endorsement_updated.png
# ===========================================================================


### 1. Contexts selected ---------------------------------------------------------
# Multiple answers per participant, separated by commas
data_wave2 <- data %>% filter(Wave == "Wave 2")

tab_contact <- table(unlist(strsplit(data_wave2$OccationsContact, ",")))
tab_discussing <- table(unlist(strsplit(data_wave2$OccationsDiscussing, ",")))

cat("\nContexts in which participants encounter colleagues (Wave 2):\n")
print(tab_contact)
cat("\nContexts in which participants discuss evidence (Wave 2):\n")
print(tab_discussing)

# Number of participants who selected each context
# (the follow-up ratings were shown only for selected contexts)
contexts <- c("Conferences", "Workplace", "Supervision", "Private", "Other")

totals_contact <- tibble(
  Context  = contexts,
  TotalOcc = as.numeric(tab_contact[c("At conferences",
                                      "In my direct work environment",
                                      "During supervision meetings",
                                      "In private",
                                      "Other")])
)


### 2. Figure 5 -------------------------------------------------------------------
# Response scale of both questions: 1 = disagree totally to 5 = agree totally
response_labels <- c("Disagree totally", "Disagree",
                     "Neither agree nor disagree", "Agree", "Agree totally")
response_levels <- c(response_labels, "No response")

response_colors <- setNames(c(likert_colors, "#708090"), response_levels)

# Stacked bar chart for one question (prefix = start of the variable names)
make_context_panel <- function(prefix) {
  
  ratings <- data_wave2 %>%
    select(all_of(paste0(prefix, contexts))) %>%
    pivot_longer(everything(), names_to = "Context", values_to = "Rating",
                 values_drop_na = TRUE) %>%
    mutate(Context  = sub(paste0("^", prefix), "", Context),
           Response = response_labels[Rating]) %>%
    count(Context, Response, name = "Count")
  
  # Participants who selected a context but did not rate it
  no_response <- ratings %>%
    group_by(Context) %>%
    summarise(Rated = sum(Count), .groups = "drop") %>%
    right_join(totals_contact, by = "Context") %>%
    mutate(Response = "No response",
           Count    = TotalOcc - coalesce(Rated, 0L)) %>%
    select(Context, Response, Count)
  
  plot_df <- bind_rows(ratings, no_response) %>%
    left_join(totals_contact, by = "Context") %>%
    mutate(
      Percent  = 100 * Count / TotalOcc,
      Label    = ifelse(Count >= 3, sprintf("%.0f%%", Percent), ""),
      # White text on the two dark colours
      LabelCol = ifelse(Response %in% c("Disagree totally", "Agree totally"),
                        "white", "black"),
      Context  = factor(Context, levels = rev(contexts)),
      Response = factor(Response, levels = rev(response_levels))
    )
  
  n_labels <- totals_contact %>%
    mutate(Context = factor(Context, levels = rev(contexts)),
           ypos    = TotalOcc + max(TotalOcc) * 0.02,
           lab     = paste0("n = ", TotalOcc))
  
  ggplot(plot_df, aes(x = Context, y = Count, fill = Response, group = Response)) +
    geom_col(colour = "white") +
    geom_text(aes(label = Label, colour = LabelCol),
              position = position_stack(vjust = 0.5), size = 3) +
    geom_text(data = n_labels, aes(x = Context, y = ypos, label = lab),
              inherit.aes = FALSE, hjust = 0, size = 3) +
    coord_flip() +
    scale_fill_manual(values = response_colors, breaks = response_levels,
                      drop = FALSE, name = "Response") +
    scale_colour_identity() +
    expand_limits(y = max(n_labels$ypos) * 1.08) +
    labs(x = NULL, y = "Number of participants who selected the context") +
    theme_minimal(base_size = 13)
}

panel_endorsement <- make_context_panel("EBMHEndorsement")
panel_updated     <- make_context_panel("Updated")

# Panel A: colleagues' endorsement of EBMH; Panel B: feeling updated about EBMH
Figure5 <- (panel_endorsement / panel_updated) +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 16, face = "bold"),
        legend.position = "right")

ggsave(file.path(dir_plots, "Figure5_endorsement_updated.png"), Figure5,
       width = 11, height = 9, dpi = 600, bg = "white")


# Clean environment ---------------------------------------------------------------
rm(list = intersect(c(
  "data_wave2", "tab_contact", "tab_discussing", "contexts", "totals_contact",
  "response_labels", "response_levels", "response_colors",
  "make_context_panel", "panel_endorsement", "panel_updated", "Figure5"
), ls()))
