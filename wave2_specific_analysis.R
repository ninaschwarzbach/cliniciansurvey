# ===========================================================================
# Clinician Survey Project - Wave 2 specific analysis -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 28.10.2025
# ===========================================================================


# Wave 2 specific analyses

## 6.1 Data   ---------------------------------------------------------------

# Clean variable names (safe if wave_info came from Excel)
wave_info$Variable <- trimws(as.character(wave_info$Variable))
names(data)      <- trimws(names(data))

# Get lists from wave_info
vars_both        <- wave_info$Variable[wave_info$Wave == "Both waves"]
vars_wave2_only  <- wave_info$Variable[wave_info$Wave == "Wave 2 only"]

# Wave 2 full = both + wave2-only (but only keep columns that actually exist in data)
vars_wave2_full  <- intersect(c(vars_both, vars_wave2_only), names(data))
data_wave2       <- data[, vars_wave2_full, drop = FALSE]

# Wave 2 exclusive = variables that are listed as "Wave 2 only" and present in data
vars_wave2_only_present <- intersect(vars_wave2_only, names(data))
data_wave2_only         <- data[, vars_wave2_only_present, drop = FALSE]

## 6.2 Descriptive Wave 2 Specific analyses  ---------------------------------------------------------------


## 6.3. Occasions for contact / discussing evidence ---------------------------------------------------------------



### Also add here evidence yes/no, as it was wave specific, also  "EBMH_PsychDisorders_mean",
### check missing variables
## Importance of EBMH in psychotic disorder treatment
## Professional interactions & evidence exchange
## Use of treatment manuals
## Descriptive percentages, stacked bar plots


# freq manuals, and whether they use manuals, the occasions of contact and of those occations, when did they discuss evidence
# what research resources they are using 
# Whether the understanding of EBMH is similar
# whether they are self-employed

## 6.x Plots   ---------------------------------------------------------------

# Clean environment ---------------------------------------------------------------
rm(data_wave2, data_wave2_only, vars_wave2, vars_wave2_only)

