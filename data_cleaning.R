# ===========================================================================
# Clinician Survey Project - Data Cleaning Script
# Author: Nina Schwarzbach
# Start date: 01.10.2025
# Date last updated: 20.11.2025
# ===========================================================================

# Upload Data
  wave1 <- read.csv("data/raw/wave1_data.csv", stringsAsFactors = FALSE)
  wave2 <- read.csv("data/raw/wave2_data.csv", stringsAsFactors = FALSE)

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
      
      n_beforeconsent_wave1 <- nrow(wave1); n_beforeconsent_wave1 
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
      # Did they say they participated before?
        table(wave2$Q11)
      
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
            SchoolSliderPsychodynamic_Depth_Analytical = Q16_3,              
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
            UseEBMH2 = Q32_1,  
            UseEvidence_YesNo = Q26,
            ResearchResources = Q27,              
            ResearchResources_TEXT = Q27_5_TEXT, 
            TreatmentManuals = Q28,              
            TreatmentManuals_Freq = Q29_1,              
            EBMHPsychDisorders1 = Q30_1,              
            EBMHPsychDisorders2 = Q30_2,              
            ComparisonEBMHPsychDisordersOtherDisorders = Q31_1,              
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
            MainObstacleEBMH_TEXT = Q43, 
            NeedsEMBH_PsychDisorders_TEXT = Q44, 
            ReasonsNotEBMH_TEXT = Q45, 
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
                UniversityEducation_diff, 
                UniversityEducation_1, 
                UniversityEducation_2, 
                AttitudeEBMH4_1, 
                AttitudeEBMH4
              ))%>% 
            
    # Rename colums 
            rename(
              UniversityEducation = UniversityEducation_merged,
              AttitudeEBMH4 = AttitudeEBMH4_merged
            )   
            
# Reverse Coding 
  wave1$AttitudeEBMH3 <- reverse_likert_text(wave1$AttitudeEBMH3)
  wave2$AttitudeEBMH3 <- reverse_likert_text(wave2$AttitudeEBMH3)
  wave2$PerceptionGap2 <- reverse_likert_text(wave2$PerceptionGap2)

# Sliders 
  # Delete double item 
    wave2 <- wave2 %>% select(-`SchoolSliderPsychodynamic_Depth_Analytical`)
  
# Filling in Slider Values for NA's
  # Define slider columns 
    sliders_w1 <- c(
      "SchoolSliderCBT","SchoolSliderPsychodynamic","SchoolSliderAnalytical",
      "SchoolSliderSystemic","SchoolSliderNeuropsychology","SchoolSliderPersonCentered",
      "SchoolSliderACT","SchoolSliderOther"
    )
    
    sliders_w2 <- c(
      "SchoolSliderCBT","SchoolSliderPsychodynamic",
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
      # Wave 1
        non_numeric_graduation_wave1 <- wave1[!grepl("^\\d+$", trimws(wave1$GraduationYear)) & 
                                          !is.na(wave1$GraduationYear), "GraduationYear"]
        
        # Remame individually
          wave1 <- wave1 %>%
            mutate(
              GraduationYear = case_when(
                GraduationYear == "Studium Ende: 2018" ~ "2018",
                GraduationYear == "GZ opleiding in 2020, master in 2016" ~ "2016",
                GraduationYear == "2016 (Approbation)" ~ "2016",
                GraduationYear == "2009 Universiteit, 2014 als GZ-psycholoog" ~ "2009",
                GraduationYear == "2013 aan de uni, 2020 GZ-psycholoog" ~ "2013",
                GraduationYear == "2022 (Psychologische Psychotherapie Weiterbildung)" ~ "2022",
                GraduationYear == "2025/ Noch nicht beendet" ~ "2025",
                GraduationYear == "Orthopedagoog 2015, GZ-psycholoog 2022" ~ "2015",
                TRUE ~ GraduationYear
              )
            )
      
      # Wave 2
        non_numeric_graduation_wave2 <- wave2[!grepl("^\\d+$", trimws(wave2$GraduationYear)) & 
                                          !is.na(wave2$GraduationYear), "GraduationYear"]
        
        # Remame individually 
          wave2 <- wave2 %>%
            mutate(
              GraduationYear = case_when(
                GraduationYear == "1997 / 2010 / 2023" ~ "1997",  
                GraduationYear == "unklar" ~ NA_character_,
                GraduationYear == "2015 als GZ, 2008 als psycholoog" ~ "2008",
                GraduationYear == "2014 (GZ), 2021 (PT)" ~ "2014",
                GraduationYear == "Deutschland" ~ NA_character_,
                trimws(GraduationYear) %in% c("", "NA", "N/A", "na") ~ NA_character_,
                TRUE ~ GraduationYear
              )
            )
      
    # Years of Experience non-numeric values
      # Wave 1
        non_numeric_experience_wave1 <- wave1[!grepl("^\\d+$", trimws(wave1$YearsExperience)) & 
                                          !is.na(wave1$YearsExperience), "YearsExperience"]
        
        # Remame individually 
          wave1 <- wave1 %>%
            mutate(
              YearsExperience = gsub(",", ".", YearsExperience),  # Replace comma with dot
              YearsExperience = case_when(
                YearsExperience %in% c("7 jaar") ~ "7",
                YearsExperience %in% c("4.5 jaar") ~ "4.5",
                YearsExperience == "13 (waarvan 9 als GZ-psycholoog)" ~ "13",
                YearsExperience == "'+/- 25" ~ "25",
                YearsExperience == "16 jaar" ~ "16",
                YearsExperience == "21 jaar" ~ "21",
                YearsExperience == "30 jaar" ~ "30",
                trimws(YearsExperience) %in% c("", "NA", "N/A", "na") ~ NA_character_,
                TRUE ~ YearsExperience  # Keep other values as-is
              ),
              YearsExperience = as.numeric(YearsExperience)  # Convert to numeric
            )
        
      # Wave 2  
        non_numeric_experience_wave2 <- wave2[!grepl("^\\d+$", trimws(wave2$YearsExperience)) & 
                                          !is.na(wave2$YearsExperience), "YearsExperience"]
        
        # Remame individually 
          wave2 <- wave2 %>%
            mutate(
              YearsExperience = gsub(",", ".", YearsExperience),  # Replace comma with dot for decimal numbers
              YearsExperience = case_when(
                YearsExperience %in% c("23 jaar") ~ "23",
                YearsExperience %in% c("30 Jahr", "> 30 J.") ~ "30",
                YearsExperience %in% c("ongeveer 15 jaar") ~ "15",
                YearsExperience %in% c("37 Jahre") ~ "37",
                YearsExperience %in% c("mehr als 20", "Über 25") ~ "25",  # Approximate to a reasonable value
                trimws(YearsExperience) %in% c("", "NA", "N/A", "na") ~ NA_character_,
                TRUE ~ YearsExperience  # Keep other values (e.g., "2.5", "1.5", "4.5") as-is
              ),
              YearsExperience = as.numeric(YearsExperience)  # Convert to numeric
            )
          
      # Remove from environment
        rm(non_numeric_experience_wave1, non_numeric_experience_wave2, non_numeric_graduation_wave1, non_numeric_graduation_wave2, sliders_w1, sliders_w2)

# Remove other professions wave 2
  # Check "other" option
    wave2 %>%
      filter(ProfessionalBackground == "other, namely") %>%
      select(ProfessionalBackground_TEXT)
    
  # Delete cases 
    wave2 <- wave2 %>%
      filter(
        !ProfessionalBackground_TEXT %in% c(
          "Erziehungswissenschaften",
          "GGZ VS",
          "verpleegkundig specialist ggz (MANP)",
          "verpleegkundig specialist GGZ",
          "Psychiatrisch Verpleegkundige"
        )
      )
  # Rename others
    wave2 <- wave2 %>%
      mutate(
        ProfessionalBackground = case_when(
          ProfessionalBackground == "other, namely" & ProfessionalBackground_TEXT == "gz psycholoog" ~ "Psychologist",
          ProfessionalBackground == "other, namely" & ProfessionalBackground_TEXT == "Psychologie Studium Bachelor, Systemische Familientherapeutin, Ergotherapeutin" ~ "Psychologist",
          ProfessionalBackground == "other, namely" & ProfessionalBackground_TEXT == "GZ-psycholoog in opleiding tot klinisch psycholoog" ~ "Psychologist",
          ProfessionalBackground == "other, namely" & ProfessionalBackground_TEXT == "Gz psychologe-psychotherapeute" ~ "Psychologist",
          ProfessionalBackground == "other, namely" & ProfessionalBackground_TEXT == "Psychologin und approbierte psycholog. Psychotherapeutin" ~ "Psychologist",
          ProfessionalBackground == "other, namely" & ProfessionalBackground_TEXT == "Psychologische Psychotherapeutin" ~ "Psychologist",
          ProfessionalBackground == "other, namely" & ProfessionalBackground_TEXT == "Ärztin" ~ "Medical doctor",
          TRUE ~ ProfessionalBackground
        )
      )
    
  # Delete social workers
    wave2 <- wave2 %>%
      filter(is.na(ProfessionalBackground) | ProfessionalBackground != "Social worker")
    
  # Impute Psychologist as ProfessionalBackground for wave 1 as we only sampled psychologists 
    wave1$ProfessionalBackground <- "Psychologist"
    
# Remove those with Bachelor's Degree or equivalent and High School Degree or equivalent  
    wave1 <- wave1 %>%
      filter(!Degree %in% c("Bachelor's Degree or equivalent", "High School Degree or equivalent"))
    wave2 <- wave2 %>%
      filter(!Degree %in% c("Bachelor's Degree or equivalent", "High School Degree or equivalent"))
    
# Fix age 
  unique(wave1$Age)
  unique(wave2$Age)
  wave1$Age <- as.numeric(gsub(" ?Jahre", "", trimws(wave1$Age)))
  wave2$Age <- as.numeric(gsub(" ?Jahre", "", trimws(wave2$Age)))
  
# Fix length therapy 
  unique(wave1$LengthTrajectory)
  unique(wave2$LengthTrajectory)
  
  # Keep original variable 
    wave2$LengthTrajectory_original <- wave2$LengthTrajectory
    
  # Recode to match wave1 categories
    wave2 <- wave2 %>%
      mutate(
        LengthTrajectory = case_when(
          LengthTrajectory == "Prolonged short-term therapy (up to around 25 sessions or up to a year)" ~ 
            "Short-term therapy (up to 25 sessions or up to a year)",
          
          LengthTrajectory == "Short-term therapy ( around 1 to 16 sessions)" ~ 
            "Short-term therapy (up to 25 sessions or up to a year)",
          
          LengthTrajectory == "Long-term therapy (more than 25 sessions or longer than a year)" ~ 
            "Long-term therapy (more than 25 sessions or longer than a year)",
          
          LengthTrajectory == "It varies" ~ 
            "It varies",
          
          TRUE ~ NA_character_   # Keep missing or any unmatched categories as NA
        )
      )
               
# Outlier detection sample characteristics
  # Combine wave1 and wave2 into one dataset
        combined <- bind_rows(
          wave1 %>% select(Participant_ID, YearsExperience, GraduationYear, Age) %>% mutate(Wave = "Wave 1"),
          wave2 %>% select(Participant_ID, YearsExperience, GraduationYear, Age) %>% mutate(Wave = "Wave 2")
        )
        
  # Compute z-scores
        combined <- combined %>%
          mutate(YearsExperience_z = scale(YearsExperience)[,1],
                 YearsExperience_outlier = abs(YearsExperience_z) > 3.29) %>%
          mutate(GraduationYear = suppressWarnings(as.numeric(as.character(GraduationYear)))) %>%
          mutate(GraduationYear_z = scale(GraduationYear)[,1],
                 GraduationYear_outlier = ifelse(is.na(GraduationYear_z), FALSE, abs(GraduationYear_z) > 3.29)) %>%
          mutate(Age = suppressWarnings(as.numeric(as.character(Age)))) %>%
          mutate(Age_z = scale(Age)[,1],
                 Age_outlier = ifelse(is.na(Age_z), FALSE, abs(Age_z) > 3.29))
        
    # Years Experience
      #Boxplot
        boxplot_experience <-ggplot(combined, aes(x = Wave, y = YearsExperience, fill = Wave)) +
          geom_boxplot(color = "darkblue") +
          labs(
            title = "Boxplot of Years of Experience by Wave",
            y = "Years of Experience",
            x = "Wave"
          ) +
          theme_minimal() +
          scale_fill_manual(values = c("skyblue", "lightgreen")); boxplot_experience 
        
        save_plot(boxplot_experience, "boxplot_years_experience")
        
      # Inspect flagged cases
        outliers <- combined %>% filter(YearsExperience_outlier); outliers 
        wave1 %>%
          filter(YearsExperience == 50) %>%
          select(Participant_ID, Age, GraduationYear, YearsExperience)
        
      # Check if everyone had at least one year of experience
        unique(wave2$Experience1Year)
        which(wave1$YearsExperience < 1)
        wave1$YearsExperience[c(38, 152) ]
        # Exclude those with less than 1 year
          wave1 <- wave1[-c(38, 152), ]
        
        
    # Year Graduation 
      # Inspect flagged cases
        outliers <- combined %>% filter(GraduationYear_outlier); outliers 
      
      # Replace 21 and 33
        wave1$GraduationYear <- with(wave1, ifelse(GraduationYear == 21, 2021, GraduationYear))        
        wave2$GraduationYear <- with(wave2, ifelse(GraduationYear == 33, NA, GraduationYear))     
        combined$GraduationYear <- with(combined, ifelse(GraduationYear == 33, NA, GraduationYear)) 
        combined$GraduationYear <- with(combined, ifelse(GraduationYear == 21, 2021, GraduationYear))    
        
      #Boxplot
        boxplot_grad <-ggplot(combined, aes(x = Wave, y = GraduationYear, fill = Wave)) +
          geom_boxplot(color = "darkblue") +
          labs(
            title = "Boxplot of Graduation Year by Wave",
            y = "Graduation Year",
            x = "Wave"
          ) +
          theme_minimal() +
          scale_fill_manual(values = c("skyblue", "lightgreen")); boxplot_grad
        
        save_plot(boxplot_grad, "boxplot_grad")
        
    # Age 
      # Inspect flagged cases
        outliers <- combined %>% filter(Age_outlier); outliers 
      
      #Boxplot
        boxplot_age <-ggplot(combined, aes(x = Wave, y = Age, fill = Wave)) +
          geom_boxplot(color = "darkblue") +
          labs(
            title = "Age by Wave",
            y = "Age",
            x = "Wave"
          ) +
          theme_minimal() +
          scale_fill_manual(values = c("skyblue", "lightgreen")); boxplot_age

        save_plot(boxplot_age, "boxplot_age")
        
  # Remove 
    rm(combined, outliers, boxplot_age, boxplot_experience, boxplot_grad)
        
# Separate Qualitative Data
  quali_wave1 <- wave1 %>%
    select(contains("TEXT"))
  quali_wave2 <- wave2 %>%
    select(contains("TEXT"))
  
# Separate Quantitative Data
  wave1 <- wave1 %>%
    select(-contains("TEXT"))
  wave2 <- wave2 %>%
    select(-contains("TEXT"))

  
# Merge datasets
  # Dissolve discrepancies
    wave1$AttitudeEBMH4 <- likert_to_num(wave1$AttitudeEBMH4)
    wave1$UniversityEducation <- likert_to_num(wave1$UniversityEducation)
  
  # Merge
    data <- bind_rows(
      wave1 %>% mutate(Wave = "Wave 1"),
      wave2 %>% mutate(Wave = "Wave 2")
    )
    
  # Identify from which wave data is 
    
    # --- Identify which variables have data in both waves, only one, or none ----
    
    if ("Wave" %in% names(data)) {
      
      # standardize wave labels
      waves <- sort(unique(na.omit(data$Wave)))
      
      # all variables except Wave
      vars_to_check <- setdiff(names(data), "Wave")
      
      # compute availability
      wave_presence <- sapply(vars_to_check, function(var) {
        present_by_wave <- sapply(waves, function(w) {
          any(!is.na(data[[var]][data$Wave == w]))
        })
        n_present <- sum(present_by_wave)
        
        if (all(present_by_wave)) {
          "Both waves"
        } else if (present_by_wave[1] && !present_by_wave[2]) {
          paste0(waves[1], " only")
        } else if (!present_by_wave[1] && present_by_wave[2]) {
          paste0(waves[2], " only")
        } else if (!any(present_by_wave)) {
          "No valid data"
        } else {
          paste(names(present_by_wave)[present_by_wave], collapse = ", ")
        }
      }, USE.NAMES = TRUE)
      
      wave_info <- data.frame(
        Variable = names(wave_presence),
        Wave = unname(wave_presence),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
      
      # show quick summary
      print(table(wave_info$Wave, useNA = "ifany"))
      
      # Remove from environment 
      rm(vars_to_check, wave_presence, waves)
      
    } else {
      stop("No column named 'Wave' found in data.")
    }
    
  # Delete participant ID 
    data <- data %>% select(-c(Participant_ID, ParticipationPreviousStudy)) ### In the end, think if you need to remove more if you want to make data public 
    
    
    