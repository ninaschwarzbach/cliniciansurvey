# ===========================================================================
# Clinician Survey Project - Open-ended Responses -
# Author: Nina Schwarzbach
# Start date: 30.09.2025
# Date last updated: 07.11.2025
# ===========================================================================

# Open-ended responses
## Both Waves

# Gender 
unlist(quali_wave1$GenderIdentity_TEXT, use.names = FALSE)
unlist(quali_wave2$GenderIdentity_TEXT, use.names = FALSE)

# Country 
unlist(quali_wave1$Country_TEXT, use.names = FALSE)
unlist(quali_wave2$Country_TEXT, use.names = FALSE)

# Schoolslider 
unlist(quali_wave1$SchoolSliderOther_TEXT, use.names = FALSE)
unlist(quali_wave2$SchoolSliderOther_TEXT, use.names = FALSE)

# Feedback 1
unlist(quali_wave1$Feedback_Text1, use.names = FALSE)
unlist(quali_wave2$Feedback_Text1, use.names = FALSE)

# Feedback2
unlist(quali_wave1$Feedback_Text2, use.names = FALSE)
unlist(quali_wave2$Feedback_Text2, use.names = FALSE)

## Wave 2 only 

# professional background
unlist(quali_wave2$ProfessionalBackground_TEXT, use.names = FALSE)

# work setting
unlist(quali_wave2$WorkSetting_Text, use.names = FALSE)

# how implementing EBMH into daily practice
unlist(quali_wave2$UseEBMH1_TEXT, use.names = FALSE) 

# which resources are used
unlist(quali_wave2$ResearchResources_TEXT,  use.names = FALSE) 

# is the EBMH understanding similar
unlist(quali_wave2$EBMHUnderstandingSimilar_TEXT, use.names = FALSE) 

# which occasions contact with others 
unlist(quali_wave2$OccationsContact_TEXT, use.names = FALSE) 

# occasions for discussing evidence
unlist(quali_wave2$OccationsDiscussing_TEXT, use.names = FALSE) 

# occasions for feeling updated
unlist(quali_wave2$UpdatedOther_TEXT, use.names = FALSE) 

# occasions for where it's endorsed
unlist(quali_wave2$EBMHEndorsementOther_TEXT, use.names = FALSE) 

# main obstacel EBMH
unlist(quali_wave2$MainObstacleEBMH_TEXT, use.names = FALSE)

# what would you need to apply EBMH interventions 
unlist(quali_wave2$NeedsEMBH_PsychDisorders_TEXT, use.names = FALSE) 

# main reasons for rejecting EBMH 
unlist(quali_wave2$ReasonsNotEBMH_TEXT, use.names = FALSE) 



# Clean environment ---------------------------------------------------------------

