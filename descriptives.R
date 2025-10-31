# ===========================================================================
# Clinician Survey Project - Descriptives -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 28.10.2025
# ===========================================================================



# Descriptives Core outcomes 
## compute descriptives by Wave and total
desc_table_1 <- describe_continuous_by_group(data, desc_vars, group = "Wave", save = TRUE, file_prefix = "outputs/descriptives_continuous_main"); desc_table_1

## Plots
### do we really need to plot "other" options??
# Rename Columns so it works with previous functions ### university education missing
plot_data <- data
plot_data <- plot_data %>% 
  rename(
    `EBMH Use` = EBMH_use,
    `EBMH Openness` = EBMH_openness,
    `EBMH Skills (mean)` = EBMH_skills_mean,
    `Perception Gap (mean)` = perception_gap_mean,
    `Attitude Toward EBMH (mean)` = EBMH_attitude_mean,
    `Importance: clinical expertise` = EBMH_importance_clinical_expertise,
    `Workplace Influences (mean)` = workplace_influences_mean,
    `Employer Influences (mean)` = employer_influences_mean,
    `Research Experience (mean)` = research_experience_mean,
    `Importance EBMH (mean)` = EBMH_PsychDisorders_mean,
    `Familiarity with EBMH` = FamiliarityEBMH,
    `Updated Conferences` = UpdatedConferences,
    `Updated Workplace` = UpdatedWorkplace,
    `Updated Supervision` = UpdatedSupervision,
    `Updated Private Study` = UpdatedPrivate,
    `Updated Other Methods` = UpdatedOther,
    `EBMH Endorsement Conferences` = EBMHEndorsementConferences,
    `EBMH Endorsement Workplace` = EBMHEndorsementWorkplace,
    `EBMH Endorsement Supervision` = EBMHEndorsementSupervision,
    `EBMH Endorsement Private Settings` = EBMHEndorsementPrivate,
    `EBMH Endorsement (Other Contexts)` = EBMHEndorsementOther
  )

### maybe histogram for single items? 
### Also think about if you want to plot those for only wave 2 differently 
### Stagged barplots for the single item variables 
### Change plots again  

# Loop through all variables plots OLD
for (var_old in names(label_map)) {
  info <- label_map[[var_old]]
  v <- info$y
  violin_plot <- split_violin_total(plot_data, v, title = info$title)
  obj_name <- gsub(" ", "_", paste0("violin_", v))
  assign(obj_name, violin_plot, envir = .GlobalEnv)  
  print(violin_plot)
  ggsave(filename = file.path("plots", info$filename), plot = violin_plot, width = 6, height = 4, dpi = 300)
  message("Saved: ", info$filename)
}


# Combined plots ### update combined plots, think about which ones are actually important
combined_violin_dependentvars <- grid.arrange(`violin_EBMH_Use`, `violin_Attitude_Toward_EBMH_(mean)`, ncol = 2)
ggsave(
  "plots/combined_violin_dependentvars.png",
  combined_violin_dependentvars,
  width = 10,  
  height = 5,   
  dpi = 300
); combined_violin_dependentvars

combined_violin_independentvars <- grid.arrange(`violin_EBMH_Skills_(mean)`, `violin_Perception_Gap_(mean)`, 
                                                `violin_Workplace_Influences_(mean)`, `violin_Employer_Influences_(mean)`, 
                                                `violin_Research_Experience_(mean)`, violin_Familiarity_with_EBMH,
                                                ncol = 2)
ggsave(
  "plots/combined_violin_independentvars.png",
  combined_violin_independentvars,
  width = 10,  
  height = 15,   
  dpi = 300
); combined_violin_independentvars

combined_violin_endorsement <- grid.arrange(violin_EBMH_Endorsement_Workplace, violin_EBMH_Endorsement_Supervision,
                                            violin_EBMH_Endorsement_Conferences, violin_EBMH_Endorsement_Private_Settings, 
                                            `violin_EBMH_Endorsement_(Other_Contexts)`,
                                            ncol = 2)
ggsave(
  "plots/combined_violin_endorsement.png",
  combined_violin_endorsement,
  width = 10,  
  height = 15,   
  dpi = 300
); combined_violin_endorsement 

combined_violin_updated <- grid.arrange(violin_Updated_Conferences, violin_Updated_Private_Study, 
                                        violin_Updated_Supervision, violin_Updated_Workplace, 
                                        `violin_Updated_Other_Methods`,
                                        ncol = 2)
ggsave(
  "plots/combined_violin_updated.png",
  combined_violin_updated,
  width = 10,  
  height = 15,   
  dpi = 300
); combined_violin_updated

### Still by country for supplements??
### look at warnings
### check what you can remove from environment
### in environment to be deleted desc_table_1, plot_data

# Clean environment ---------------------------------------------------------------
rm(list = c("desc_vars", "info", "plot_data", "desc_table_1", "obj_name", "v", "var_old",
            ls(pattern = "^violin_"), 
            ls(pattern = "^combined_violin_")))

