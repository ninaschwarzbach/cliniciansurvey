# Clinician Survey Project - Data Cleaning Script
# Author: Nina Schwarzbach
# Start date: 01.10.2025
# Date last updated: 

# Set working directory 
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Load functions script
  source("functions.R")

# Upload Data
  wave1 <- read.csv("data/raw/wave1_data.csv", stringsAsFactors = FALSE)
  wave2 <- read.csv("data/raw/wave2_data.csv", stringsAsFactors = FALSE)

### Note: Both data sets, merge datasets (or merge after?)

# Data cleaning / exclusion
  ## Changes empty spaces to Na's 
    wave1 <- wave1 %>% mutate(across(where(is.character), ~na_if(trimws(.), "")))
    wave2 <- wave2 %>% mutate(across(where(is.character), ~na_if(trimws(.), "")))
  ## Delete preview rows
    wave1 <- wave1[wave1$DistributionChannel != "preview", ]
    wave2 <- wave2[wave2$DistributionChannel != "preview", ]
  ## Delete items that are not needed
    wave1 <- wave1[, -c(1,2,3,4,8,9,10,11,12,13,14,15,16,53,68)] ### make this simpler later
    wave2 <- wave2[, -c(1,2,3,4,8,9,10,11,12,13,14,15,16, 20, 109, 110, 111, 112)] ### make this simpler later, also check deleted items analysis plan


  ## Extract first row (questions) from wave1 and wave2
    questions_wave1 <- wave1[1, ]
    questions_wave2 <- wave2[1, ]
    
  ## Delete Questions from dataset
    wave1 <- wave1[-c(1:2), ]
    wave2 <- wave2[-c(1:2), ]
    
  ## Remove those that did not consent 
    # Frequencies 
      freq_table(wave1, "Q5") 
      freq_table(wave1, "Q6")
      freq_table(wave2, "Q5")
      freq_table(wave2, "Q6")
      
      n_beforeconsent_wave1 <- nrow(wave1); n_beforeconsent_wave1 ### delete these again from environment
      n_beforeconsent_wave2 <- nrow(wave2); n_beforeconsent_wave2
    
    # Relabel options
      wave1$Q5 <- ifelse(grepl("^\\s*Yes", wave1$Q5, ignore.case = TRUE), "yes", wave1$Q5)
      wave1$Q6 <- ifelse(grepl("^\\s*Yes", wave1$Q6, ignore.case = TRUE), "yes", wave1$Q6)
      wave2$Q5 <- ifelse(grepl("^\\s*Yes", wave2$Q5, ignore.case = TRUE), "yes", wave2$Q5)
      wave2$Q6 <- ifelse(grepl("^\\s*Yes", wave2$Q6, ignore.case = TRUE), "yes", wave2$Q6)
      
    # Remove participants 
      wave1 <- wave1 %>%
        filter(Q5 == "yes" & Q6 == "yes")
      wave2 <- wave2 %>%
        filter(Q5 == "yes" & Q6 == "yes")
      
      n_afterconsent_wave1 <- nrow(wave1); n_afterconsent_wave1
      n_afterconsent_wave2 <- nrow(wave2); n_afterconsent_wave2
      
    # Check how many were removed
      cat("Wave 1 removed:", n_beforeconsent_wave1 - n_afterconsent_wave1, "cases\n")
      cat("Wave 2 removed:", n_beforeconsent_wave2 - n_afterconsent_wave2, "cases\n")
      
      
  ## Exclude those that only filled in demographics
    # Mark participants to exclude
      wave1$exclude <- is.na(wave1$Q11_1) & is.na(wave1$Q11_2) & is.na(wave1$Q11_3)
      wave2$exclude <- is.na(wave2$Q16_1) & is.na(wave2$Q16_2) & is.na(wave2$Q16_3)
      
    # Count how many are incomplete
      n_incomplete_wave1 <- sum(wave1$exclude);  n_incomplete_wave1 
      n_incomplete_wave2 <- sum(wave2$exclude);  n_incomplete_wave2
      n_complete_wave1 <- nrow(wave1[!wave1$exclude, ]);  n_complete_wave1 
      n_complete_wave2 <- nrow(wave1[!wave2$exclude, ]);  n_complete_wave2
      
    # Remove incomplete participants
      wave1 <- wave1[!wave1$exclude, ]
      wave2 <- wave2[!wave2$exclude, ]
      
    # Check how many were removed
      cat("Wave 1 removed:", n_incomplete_wave1, "cases\n")
      cat("Wave 2 removed:", n_incomplete_wave2, "cases\n")
    
  ## Delete participants not from NL or DE 
    # Frequencies 
      freq_table(wave1, "Q29")
      freq_table(wave2, "Q15")
      
    # Remove participants 
      wave1 <- wave1 %>%
        filter(Q29 %in% c("Netherlands", "Germany"))
      wave2 <- wave2 %>%
        filter(Q15 %in% c("Netherlands", "Germany"))
      
      n_aftercountry_wave1 <- nrow(wave1);  n_aftercountry_wave1
      n_aftercountry_wave2 <- nrow(wave2);  n_aftercountry_wave2
      
    # Check how many were removed
      cat("Wave 1 removed:", n_complete_wave1 - n_aftercountry_wave1, "cases\n")
      cat("Wave 2 removed:", n_complete_wave2 - n_aftercountry_wave2, "cases\n")
      
      
  ## Exclude those in wave 2 that participated in the previous wave
    # Combine the two columns into one vector
      combined <- c(wave1$Q23, wave2$Q48)
      
    # Remove NAs 
      combined <- combined[!is.na(combined)]
      
    # Check for duplicates
      duplicated_values <- combined[duplicated(combined)]
      unique_duplicates <- unique(duplicated_values); unique_duplicates
      
    # Find indices of each duplicated value
      # For "1234567"
        which(wave1$Q23 == "1234567")   
        which(wave2$Q48 == "1234567")  
        
        # Check demographics
          wave1[which(wave1$Q23 == "1234567"), c("Q8","Q9", "Q10", "Q29", "Q16")] 
          wave2[which(wave2$Q48 == "1234567"), c("Q12","Q13","Q14", "Q15")] 
      
      # For "nvt"
        which(wave1$Q23 == "nvt")
        which(wave2$Q48 == "nvt")
        
        # Check demographics
          wave1[which(wave1$Q23 == "nvt"), c("Q8","Q9", "Q10", "Q29", "Q16")] 
      
      # For "1111111"
        which(wave1$Q23 == "1111111")
        which(wave2$Q48 == "1111111")
        
        # Check demographics
          wave2[which(wave2$Q48 == "1111111"), c("Q12","Q13","Q14", "Q15")] 
      
  ## Remove from environment
      rm(combined, duplicated_values, n_afterconsent_wave1, n_afterconsent_wave2, n_aftercountry_wave1, n_aftercountry_wave2, 
         n_beforeconsent_wave1, n_beforeconsent_wave2, n_complete_wave1, n_complete_wave2, n_incomplete_wave1, n_incomplete_wave2,
         unique_duplicates)
      
  ## Remove from dataset
      wave1 <- wave1 %>% select(-exclude)
      wave2 <- wave2 %>% select(-exclude)
      
  ## Rename rows for consistency
    ## Wave 1 
      wave1 <- wave1 %>%
        rename(
          Consent = Q5,
          DataProcessing = Q6, 
          WorkAsPsychologist = Q7, 
          Age = Q8, 
          GenderIdentity = Q9, 
          DurationSeconds = Duration..in.seconds., 
          GenderIdentity_TEXT = Q9_6_TEXT, 
          Degree= Q10, 
          Country = Q29, 
          Country_TEXT = Q29_4_TEXT, 
          SchoolSliderCBT = Q11_1, 
          SchoolSliderPsychodynamic = Q11_2, 
          SchoolSliderAnalytical = Q11_3,          
          SchoolSliderSystemic = Q11_4,          
          SchoolSliderNeuropsychology = Q11_5,          
          SchoolSliderPersonCentered = Q11_6,          
          SchoolSliderACT = Q11_7,          
          SchoolSliderOther = Q11_8,          
          SchoolSliderOther_TEXT = Q11_8_TEXT,          
          SchoolForcedChoice = Q12,          
          LengthTrajectory = Q13,          
          GraduationYear = Q14,          
          YearsExperience = Q15,          
          WorkSetting = Q16,          
          FamiliarityEBMH = Q17_1,          
          UseEBMH1 = Q17_2,          
          UseEBMH2 = Q18_1,          
          EBMHSkills1 = Q18_2,          
          EBMHSkills2 = Q18_3,         
          PerceptionGap1 = Q18_4,          
          PerceptionGap2 = Q18_5,          
          AttitudeEBMH1 = Q18_6,          
          AttitudeEBMH2 = Q18_7,          
          EBMHSkills3 = Q18_8, 
          AttitudeEBMH3 = Q18_9,          
          ResearchExperience1 = Q19_1,          
          WorkplaceInfluences1 = Q19_3,          
          WorkplaceInfluences2 = Q19_4,          
          AttitudeEBMH4 = Q19_5,          
          WorkplaceInfluences3 = Q19_6,          
          WorkplaceInfluences4 = Q19_7,          
          EmployerInfluences1 = Q20_1,          
          EmployerInfluences2 = Q20_2,          
          EmployerInfluences3 = Q20_3,         
          UniversityEducation = Q20_4,          
          EmployerInfluences4 = Q20_5,          
          EmployerInfluences5 = Q20_6,          
          EmployerInfluences6 = Q20_7,          
          WorkplaceInfluences5 = Q20_8,         
          EmployerInfluences7 = Q20_9,          
          Feedback_Text1 = Q21, 
          Feedback_Text2 = Q22, 
          Participant_ID = Q23
        )
        # Create a named vector for already renamed wave1 columns
          wave1_names <- colnames(wave1)
          wave1_questions <- as.character(questions_wave1)
          names(wave1_questions) <- wave1_names
      
      # Loop through wave2 questions, compare with wave1, and assign names if similar
          wave2_new_names <- map_chr(as.character(questions_wave2), function(q2) {
            match_idx <- which(wave1_questions == q2)
            if(length(match_idx) > 0) {
              # Use the wave1 column name
              names(wave1_questions)[match_idx]
            } else {
              NA_character_  # Mark as not matched yet
            }
          })
      
      # Assign new column names to wave2 where matched
        matched_idx <- which(!is.na(wave2_new_names))
        colnames(wave2)[matched_idx] <- wave2_new_names[matched_idx]
      
      # Show columns that still need renaming
        unmatched_cols <- colnames(wave2)[is.na(wave2_new_names)]
        
        ## Remove from environment
          rm(matched_idx, unmatched_cols, wave1_names, wave2_new_names, wave1_questions)
        
      # Rename unmatched cols
        wave2 <- wave2 %>%
          rename(
            DataProcessing = Q6, 
            WorkAsPsychologist = Q8,
            ProfessionalBackground = Q58, 
            ProfessionalBackground_TEXT = Q58_4_TEXT, 
            TreatmentPsychDisorders = Q9, 
            Experience1Year = Q10, 
            ParticipationPreviousStudy = Q11, 
            SchoolSliderCBT = Q16_1, 
            SchoolSliderPsychodynamic = Q16_2,              
            SchoolSliderPsychodynamic_PsychDisorders = Q16_3,              
            SchoolSliderSystemic = Q16_4, 
            SchoolSliderNeuropsychology = Q16_5,              
            SchoolSliderPersonCentered = Q16_6,              
            SchoolSliderACT = Q16_7,              
            SchoolSliderHumanistic = Q16_9,              
            SchoolSliderMBT = Q16_10, 
            SchoolSliderOther = Q16_8,          
            SchoolSliderOther_TEXT = Q16_8_TEXT,   
            SchoolForcedChoice = Q17, 
            GraduationYear = Q19, 
            WorkSetting = Q21, 
            WorkSetting_Text = Q21_10_TEXT, 
            SelfEmployed = Q22, 
            FamiliarityEBMH = Q24_1,              
            UseEBMH1 = Q24_2,              
            UseEBMH1_TEXT = Q25, 
            UseEBMH2 = Q26,              
            ResearchResources = Q27,              
            ResearchResources_TEXT = Q27_5_TEXT, 
            TreatmentManuals = Q28,              
            TreatmentManuals_Freq = Q29_1,              
            EBMHPsychDisorders1 = Q30_1,              
            EBMHPsychDisorders2 = Q30_2,              
            ComparisonEBMHPsychDisordersOtherDisorders = Q31_1,              
            AdjustingPracticesEBMH = Q32_1, 
            EBMHSkills1 = Q32_2,              
            EBMHSkills2 = Q32_3,              
            PerceptionGap1 = Q32_4,              
            PerceptionGap2 = Q32_5,              
            AttitudeEBMH1 = Q32_6,              
            AttitudeEBMH2 = Q32_7,              
            EBMHSkills3 = Q32_8,       
            AttitudeEBMH3 = Q32_9,       
            EBMHUnderstandingSimilar = Q33, 
            EBMHUnderstandingSimilar_TEXT = Q34, 
            ResearchExperience1 = Q35_1,              
            ResearchExperience2 = Q35_2, 
            AttitudeEBMH4 = Q35_3,              
            OccationsContact = Q36, 
            OccationsContact_TEXT = Q36_5_TEXT, 
            OccationsDiscussing = Q37, 
            OccationsDiscussing_TEXT = Q37_5_TEXT, 
            UpdatedConferences = Q38_1,              
            UpdatedWorkplace = Q38_2,              
            UpdatedSupervision = Q38_3,              
            UpdatedPrivate = Q38_4,              
            UpdatedOther = Q38_5,              
            UpdatedOther_TEXT = Q38_5_TEXT, 
            UpdatedNone = Q38_6, 
            EBMHEndorsementConferences = Q39_1,              
            EBMHEndorsementWorkplace = Q39_2,              
            EBMHEndorsementSupervision = Q39_3,              
            EBMHEndorsementPrivate = Q39_4,              
            EBMHEndorsementOther = Q39_5,              
            EBMHEndorsementOther_TEXT = Q39_5_TEXT, 
            EBMHEndorsementNone = Q39_6,  
            WorkplaceInfluences1 = Q40_1,              
            WorkplaceInfluences2 = Q40_2,              
            WorkplaceInfluences2_PsychDisorders = Q40_3,              
            AttitudeEBMH4_1 = Q40_4,              
            WorkplaceInfluences4 = Q40_5, 
            WorkplaceInfluences3 = Q40_6,              
            EmployerInfluences1 = Q41_1,              
            EmployerInfluences2 = Q41_2,              
            EmployerInfluences3 = Q41_3,              
            UniversityEducation_1 = Q41_4,              
            EmployerInfluences4 = Q41_5,              
            EmployerInfluences5 = Q41_6,              
            EmployerInfluences6 = Q41_7,              
            WorkplaceInfluences5 = Q41_8,              
            EmployerInfluences7 = Q41_9,              
            UniversityEducation_2 = Q42_3, 
            MainObstacleEBMH = Q43, 
            NeedsEMBH_PsychDisorders = Q44, 
            ReasonsNotEBMH = Q45, 
            Participant_ID = Q48
          )
        
  ## Double questions in Wave 2: University Education and AttitudeEBMH4
    # Convert to numbers
        # AttitudeEBMH4
          wave2$AttitudeEBMH4_num <- likert_to_num(wave2$AttitudeEBMH4)
          wave2$AttitudeEBMH4_1_num <- likert_to_num(wave2$AttitudeEBMH4_1)
          # UniversityEducation
          wave2$UniversityEducation_1_num <- likert_to_num(wave2$UniversityEducation_1)
          wave2$UniversityEducation_2_num <- likert_to_num(wave2$UniversityEducation_2)
    # Check differences
        # AttitudeEBMH4
          # Calculate absolute difference for AttitudeEBMH4
            wave2$AttitudeEBMH4_diff <- abs(wave2$AttitudeEBMH4_num - wave2$AttitudeEBMH4_1_num)
          # Show rows where both are not NA and difference > 1
            AttitudeEBMH4_discrepancies <- wave2 %>%
              filter(!is.na(AttitudeEBMH4_num) & !is.na(AttitudeEBMH4_1_num) & AttitudeEBMH4_diff > 0) %>%
              select(AttitudeEBMH4, AttitudeEBMH4_1, AttitudeEBMH4_num, AttitudeEBMH4_1_num, AttitudeEBMH4_diff)
            
        # UniversityEducation 
          # Calculate absolute difference for AttitudeEBMH4
            wave2$UniversityEducation_diff <- abs(wave2$UniversityEducation_1_num - wave2$UniversityEducation_2_num)
          # Show rows where both are not NA and difference > 1
            UniversityEducation_discrepancies <- wave2 %>%
              filter(!is.na(UniversityEducation_1_num) & !is.na(UniversityEducation_2_num) & UniversityEducation_diff > 0) %>%
              select(UniversityEducation_1, UniversityEducation_2, UniversityEducation_1_num, UniversityEducation_2_num, UniversityEducation_diff)
            
    # Merge Variables
        # AttitudeEBMH4
            wave2 <- merge_likert_numeric(wave2, "AttitudeEBMH4", "AttitudeEBMH4_1", "AttitudeEBMH4_merged", likert_map)
            
        # UniversityEducation 
            wave2 <- merge_likert_numeric(wave2, "UniversityEducation_1", "UniversityEducation_2", "UniversityEducation_merged", likert_map)  
            
    # Remove from environment
            rm(AttitudeEBMH4_discrepancies, UniversityEducation_discrepancies) 
            
    # Remove extra columns from wave2
            wave2 <- wave2 %>%
              select(-c(
                AttitudeEBMH4_num,
                AttitudeEBMH4_1_num,
                UniversityEducation_1_num,
                UniversityEducation_2_num,
                AttitudeEBMH4_diff,
                UniversityEducation_diff
              ))      
            
# Reverse Coding 
  wave1$AttitudeEBMH3 <- reverse_likert_text(wave1$AttitudeEBMH3)
  wave2$AttitudeEBMH3 <- reverse_likert_text(wave2$AttitudeEBMH3)
  wave2$PerceptionGap2 <- reverse_likert_text(wave2$PerceptionGap2)

            
# Filling in Slider Values for NA's
  # Define slider columns 
    sliders_w1 <- c(
      "SchoolSliderCBT","SchoolSliderPsychodynamic","SchoolSliderAnalytical",
      "SchoolSliderSystemic","SchoolSliderNeuropsychology","SchoolSliderPersonCentered",
      "SchoolSliderACT","SchoolSliderOther"
    )
    
    sliders_w2 <- c(
      "SchoolSliderCBT","SchoolSliderPsychodynamic","SchoolSliderPsychodynamic_PsychDisorders",
      "SchoolSliderSystemic","SchoolSliderNeuropsychology","SchoolSliderPersonCentered",
      "SchoolSliderACT","SchoolSliderHumanistic","SchoolSliderMBT","SchoolSliderOther"
    )
    
  # Change NA's into 0's
    wave1 <- fill_na(wave1, sliders_w1, forced_col = "SchoolForcedChoice")
    wave2 <- fill_na(wave2, sliders_w2, forced_col = "SchoolForcedChoice")
    
  # Check
    cat("Wave1: forced-choice answered:", sum(!is.na(wave1$SchoolForcedChoice)), "rows\n")
    cat("Wave2: forced-choice answered:", sum(!is.na(wave2$SchoolForcedChoice)), "rows\n")

# Cleaning work experience, Graduation year
    # Graduation Year non-numeric values
        
      ### continue here