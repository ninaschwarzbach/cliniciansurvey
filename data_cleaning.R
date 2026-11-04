# ===========================================================================
# Clinician Survey Project - Data Cleaning Script
# Author: Nina Schwarzbach
# Start date: 01.10.2025
# Date last updated: 04.11.2026
#
# Input:  data/raw/wave1_data.csv, data/raw/wave2_data.csv (Qualtrics exports)
# Output: data (cleaned, both waves combined), quali_wave1, quali_wave2
#         (open-ended responses), wave_info (which variables were assessed
#         in which wave)
# ===========================================================================


### 1. Load data -------------------------------------------------------------
wave1 <- read.csv("data/raw/wave1_data.csv", stringsAsFactors = FALSE)
wave2 <- read.csv("data/raw/wave2_data.csv", stringsAsFactors = FALSE)


### 2. Basic cleaning --------------------------------------------------------

## 2.1 Empty cells to NA
wave1 <- wave1 %>% mutate(across(where(is.character), ~ na_if(trimws(.), "")))
wave2 <- wave2 %>% mutate(across(where(is.character), ~ na_if(trimws(.), "")))

## 2.2 Remove Qualtrics preview responses
wave1 <- wave1[wave1$DistributionChannel != "preview", ]
wave2 <- wave2[wave2$DistributionChannel != "preview", ]

## 2.3 Remove Qualtrics metadata and items not used in the analysis
# Column positions refer to the raw Qualtrics exports
wave1 <- wave1[, -c(1, 2, 3, 4, 8, 9, 10, 11, 12, 13, 14, 15, 16, 53, 68)]
wave2 <- wave2[, -c(1, 2, 3, 4, 8, 9, 10, 11, 12, 13, 14, 15, 16, 20, 109, 110, 111, 112)]

## 2.4 Store question texts (first row of the export), then remove the two
##     Qualtrics header rows
questions_wave1 <- wave1[1, ]
questions_wave2 <- wave2[1, ]

wave1 <- wave1[-c(1:2), ]
wave2 <- wave2[-c(1:2), ]


### 3. Exclusions (Figure 1) -------------------------------------------------

## 3.1 No consent to participation (Q5) or data processing (Q6)
freq_table(wave1, "Q5")
freq_table(wave1, "Q6")
freq_table(wave2, "Q5")
freq_table(wave2, "Q6")

n_beforeconsent_wave1 <- nrow(wave1)
n_beforeconsent_wave2 <- nrow(wave2)

# Harmonise the answer labels
wave1$Q5 <- ifelse(grepl("^\\s*Yes", wave1$Q5, ignore.case = TRUE), "yes", wave1$Q5)
wave1$Q6 <- ifelse(grepl("^\\s*Yes", wave1$Q6, ignore.case = TRUE), "yes", wave1$Q6)
wave2$Q5 <- ifelse(grepl("^\\s*Yes", wave2$Q5, ignore.case = TRUE), "yes", wave2$Q5)
wave2$Q6 <- ifelse(grepl("^\\s*Yes", wave2$Q6, ignore.case = TRUE), "yes", wave2$Q6)

wave1 <- wave1 %>% filter(Q5 == "yes" & Q6 == "yes")
wave2 <- wave2 %>% filter(Q5 == "yes" & Q6 == "yes")

n_afterconsent_wave1 <- nrow(wave1)
n_afterconsent_wave2 <- nrow(wave2)

cat("No consent - Wave 1 removed:", n_beforeconsent_wave1 - n_afterconsent_wave1, "cases\n")
cat("No consent - Wave 2 removed:", n_beforeconsent_wave2 - n_afterconsent_wave2, "cases\n")

## 3.2 Only demographics completed (all orientation sliders empty)
wave1$exclude <- is.na(wave1$Q11_1) & is.na(wave1$Q11_2) & is.na(wave1$Q11_3)
wave2$exclude <- is.na(wave2$Q16_1) & is.na(wave2$Q16_2) & is.na(wave2$Q16_3)

n_incomplete_wave1 <- sum(wave1$exclude)
n_incomplete_wave2 <- sum(wave2$exclude)
n_complete_wave1   <- sum(!wave1$exclude)
n_complete_wave2   <- sum(!wave2$exclude)

wave1 <- wave1[!wave1$exclude, ]
wave2 <- wave2[!wave2$exclude, ]

wave1 <- wave1 %>% select(-exclude)
wave2 <- wave2 %>% select(-exclude)

cat("Only demographics - Wave 1 removed:", n_incomplete_wave1, "cases\n")
cat("Only demographics - Wave 2 removed:", n_incomplete_wave2, "cases\n")

## 3.3 Not working in the Netherlands or Germany
freq_table(wave1, "Q29")
freq_table(wave2, "Q15")

wave1 <- wave1 %>% filter(Q29 %in% c("Netherlands", "Germany"))
wave2 <- wave2 %>% filter(Q15 %in% c("Netherlands", "Germany"))

cat("Other country - Wave 1 removed:", n_complete_wave1 - nrow(wave1), "cases\n")
cat("Other country - Wave 2 removed:", n_complete_wave2 - nrow(wave2), "cases\n")

## 3.4 Participation in both waves (check of participant-generated codes)
participant_codes <- c(wave1$Q23, wave2$Q48)
participant_codes <- participant_codes[!is.na(participant_codes)]
unique(participant_codes[duplicated(participant_codes)])

# Duplicated codes: compare demographics
# "1234567"
which(wave1$Q23 == "1234567")
which(wave2$Q48 == "1234567")
wave1[which(wave1$Q23 == "1234567"), c("Q8", "Q9", "Q10", "Q29", "Q16")]
wave2[which(wave2$Q48 == "1234567"), c("Q12", "Q13", "Q14", "Q15")]

# "nvt"
which(wave1$Q23 == "nvt")
which(wave2$Q48 == "nvt")
wave1[which(wave1$Q23 == "nvt"), c("Q8", "Q9", "Q10", "Q29", "Q16")]

# "1111111"
which(wave1$Q23 == "1111111")
which(wave2$Q48 == "1111111")
wave2[which(wave2$Q48 == "1111111"), c("Q12", "Q13", "Q14", "Q15")]

rm(participant_codes,
   n_beforeconsent_wave1, n_beforeconsent_wave2,
   n_afterconsent_wave1, n_afterconsent_wave2,
   n_incomplete_wave1, n_incomplete_wave2,
   n_complete_wave1, n_complete_wave2)


### 4. Rename variables ------------------------------------------------------

## 4.1 Wave 1
wave1 <- wave1 %>%
  rename(
    Consent                     = Q5,
    DataProcessing              = Q6,
    WorkAsPsychologist          = Q7,
    Age                         = Q8,
    GenderIdentity              = Q9,
    DurationSeconds             = Duration..in.seconds.,
    GenderIdentity_TEXT         = Q9_6_TEXT,
    Degree                      = Q10,
    Country                     = Q29,
    Country_TEXT                = Q29_4_TEXT,
    SchoolSliderCBT             = Q11_1,
    SchoolSliderPsychodynamic   = Q11_2,
    SchoolSliderAnalytical      = Q11_3,
    SchoolSliderSystemic        = Q11_4,
    SchoolSliderNeuropsychology = Q11_5,
    SchoolSliderPersonCentered  = Q11_6,
    SchoolSliderACT             = Q11_7,
    SchoolSliderOther           = Q11_8,
    SchoolSliderOther_TEXT      = Q11_8_TEXT,
    SchoolForcedChoice          = Q12,
    LengthTrajectory            = Q13,
    GraduationYear              = Q14,
    YearsExperience             = Q15,
    WorkSetting                 = Q16,
    FamiliarityEBMH             = Q17_1,
    UseEBMH1                    = Q17_2,
    UseEBMH2                    = Q18_1,
    EBMHSkills1                 = Q18_2,
    EBMHSkills2                 = Q18_3,
    PerceptionGap1              = Q18_4,
    PerceptionGap2              = Q18_5,
    AttitudeEBMH1               = Q18_6,
    AttitudeEBMH2               = Q18_7,
    EBMHSkills3                 = Q18_8,
    AttitudeEBMH3               = Q18_9,
    ResearchExperience1         = Q19_1,
    WorkplaceInfluences1        = Q19_3,
    WorkplaceInfluences2        = Q19_4,
    AttitudeEBMH4               = Q19_5,
    WorkplaceInfluences3        = Q19_6,
    WorkplaceInfluences4        = Q19_7,
    EmployerInfluences1         = Q20_1,
    EmployerInfluences2         = Q20_2,
    EmployerInfluences3         = Q20_3,
    UniversityEducation         = Q20_4,
    EmployerInfluences4         = Q20_5,
    EmployerInfluences5         = Q20_6,
    EmployerInfluences6         = Q20_7,
    WorkplaceInfluences5        = Q20_8,
    EmployerInfluences7         = Q20_9,
    Feedback_Text1              = Q21,
    Feedback_Text2              = Q22,
    Participant_ID              = Q23
  )

## 4.2 Wave 2: questions worded identically to Wave 1 get the Wave 1 name
wave1_questions <- as.character(questions_wave1)
names(wave1_questions) <- colnames(wave1)

wave2_new_names <- map_chr(as.character(questions_wave2), function(q2) {
  match_idx <- which(wave1_questions == q2)
  if (length(match_idx) > 0) names(wave1_questions)[match_idx] else NA_character_
})

matched_idx <- which(!is.na(wave2_new_names))
colnames(wave2)[matched_idx] <- wave2_new_names[matched_idx]

rm(matched_idx, wave2_new_names, wave1_questions)

## 4.3 Wave 2: remaining questions
wave2 <- wave2 %>%
  rename(
    DataProcessing                             = Q6,
    WorkAsPsychologist                         = Q8,
    ProfessionalBackground                     = Q58,
    ProfessionalBackground_TEXT                = Q58_4_TEXT,
    TreatmentPsychDisorders                    = Q9,
    Experience1Year                            = Q10,
    ParticipationPreviousStudy                 = Q11,
    SchoolSliderCBT                            = Q16_1,
    SchoolSliderPsychodynamic                  = Q16_2,
    SchoolSliderPsychodynamic_Depth_Analytical = Q16_3,
    SchoolSliderSystemic                       = Q16_4,
    SchoolSliderNeuropsychology                = Q16_5,
    SchoolSliderPersonCentered                 = Q16_6,
    SchoolSliderACT                            = Q16_7,
    SchoolSliderHumanistic                     = Q16_9,
    SchoolSliderMBT                            = Q16_10,
    SchoolSliderOther                          = Q16_8,
    SchoolSliderOther_TEXT                     = Q16_8_TEXT,
    SchoolForcedChoice                         = Q17,
    GraduationYear                             = Q19,
    WorkSetting                                = Q21,
    WorkSetting_Text                           = Q21_10_TEXT,
    SelfEmployed                               = Q22,
    FamiliarityEBMH                            = Q24_1,
    UseEBMH1                                   = Q24_2,
    UseEBMH1_TEXT                              = Q25,
    UseEBMH2                                   = Q32_1,
    UseEvidence_YesNo                          = Q26,
    ResearchResources                          = Q27,
    ResearchResources_TEXT                     = Q27_5_TEXT,
    TreatmentManuals                           = Q28,
    TreatmentManuals_Freq                      = Q29_1,
    EBMHPsychDisorders1                        = Q30_1,
    EBMHPsychDisorders2                        = Q30_2,
    ComparisonEBMHPsychDisordersOtherDisorders = Q31_1,
    EBMHSkills1                                = Q32_2,
    EBMHSkills2                                = Q32_3,
    PerceptionGap1                             = Q32_4,
    PerceptionGap2                             = Q32_5,
    AttitudeEBMH1                              = Q32_6,
    AttitudeEBMH2                              = Q32_7,
    EBMHSkills3                                = Q32_8,
    AttitudeEBMH3                              = Q32_9,
    EBMHUnderstandingSimilar                   = Q33,
    EBMHUnderstandingSimilar_TEXT              = Q34,
    ResearchExperience1                        = Q35_1,
    ResearchExperience2                        = Q35_2,
    AttitudeEBMH4                              = Q35_3,
    OccationsContact                           = Q36,
    OccationsContact_TEXT                      = Q36_5_TEXT,
    OccationsDiscussing                        = Q37,
    OccationsDiscussing_TEXT                   = Q37_5_TEXT,
    UpdatedConferences                         = Q38_1,
    UpdatedWorkplace                           = Q38_2,
    UpdatedSupervision                         = Q38_3,
    UpdatedPrivate                             = Q38_4,
    UpdatedOther                               = Q38_5,
    UpdatedOther_TEXT                          = Q38_5_TEXT,
    UpdatedNone                                = Q38_6,
    EBMHEndorsementConferences                 = Q39_1,
    EBMHEndorsementWorkplace                   = Q39_2,
    EBMHEndorsementSupervision                 = Q39_3,
    EBMHEndorsementPrivate                     = Q39_4,
    EBMHEndorsementOther                       = Q39_5,
    EBMHEndorsementOther_TEXT                  = Q39_5_TEXT,
    EBMHEndorsementNone                        = Q39_6,
    WorkplaceInfluences1                       = Q40_1,
    WorkplaceInfluences2                       = Q40_2,
    WorkplaceInfluences2_PsychDisorders        = Q40_3,
    AttitudeEBMH4_1                            = Q40_4,
    WorkplaceInfluences4                       = Q40_5,
    WorkplaceInfluences3                       = Q40_6,
    EmployerInfluences1                        = Q41_1,
    EmployerInfluences2                        = Q41_2,
    EmployerInfluences3                        = Q41_3,
    UniversityEducation_1                      = Q41_4,
    EmployerInfluences4                        = Q41_5,
    EmployerInfluences5                        = Q41_6,
    EmployerInfluences6                        = Q41_7,
    WorkplaceInfluences5                       = Q41_8,
    EmployerInfluences7                        = Q41_9,
    UniversityEducation_2                      = Q42_3,
    MainObstacleEBMH_TEXT                      = Q43,
    NeedsEMBH_PsychDisorders_TEXT              = Q44,
    ReasonsNotEBMH_TEXT                        = Q45,
    Participant_ID                             = Q48
  )


### 5. Items asked twice in Wave 2 (AttitudeEBMH4, UniversityEducation) -------

## 5.1 Convert to numbers
wave2$AttitudeEBMH4_num         <- likert_to_num(wave2$AttitudeEBMH4)
wave2$AttitudeEBMH4_1_num       <- likert_to_num(wave2$AttitudeEBMH4_1)
wave2$UniversityEducation_1_num <- likert_to_num(wave2$UniversityEducation_1)
wave2$UniversityEducation_2_num <- likert_to_num(wave2$UniversityEducation_2)

## 5.2 Check for different answers to the two versions
wave2$AttitudeEBMH4_diff <- abs(wave2$AttitudeEBMH4_num - wave2$AttitudeEBMH4_1_num)
AttitudeEBMH4_discrepancies <- wave2 %>%
  filter(!is.na(AttitudeEBMH4_num) & !is.na(AttitudeEBMH4_1_num) & AttitudeEBMH4_diff > 0) %>%
  select(AttitudeEBMH4, AttitudeEBMH4_1, AttitudeEBMH4_num, AttitudeEBMH4_1_num, AttitudeEBMH4_diff)

wave2$UniversityEducation_diff <- abs(wave2$UniversityEducation_1_num - wave2$UniversityEducation_2_num)
UniversityEducation_discrepancies <- wave2 %>%
  filter(!is.na(UniversityEducation_1_num) & !is.na(UniversityEducation_2_num) & UniversityEducation_diff > 0) %>%
  select(UniversityEducation_1, UniversityEducation_2, UniversityEducation_1_num,
         UniversityEducation_2_num, UniversityEducation_diff)

# Kept until section 12, to count the answers in the final sample
wave2$dup_AttitudeEBMH4_diff <- ifelse(
  !is.na(wave2$AttitudeEBMH4_num) & !is.na(wave2$AttitudeEBMH4_1_num),
  wave2$AttitudeEBMH4_diff, NA)
wave2$dup_UniversityEducation_both <- !is.na(wave2$UniversityEducation_1_num) &
  !is.na(wave2$UniversityEducation_2_num)

## 5.3 Merge the two versions (mean if both answered, otherwise the one answered)
wave2 <- merge_likert_numeric(wave2, "AttitudeEBMH4", "AttitudeEBMH4_1",
                              "AttitudeEBMH4_merged")
wave2 <- merge_likert_numeric(wave2, "UniversityEducation_1", "UniversityEducation_2",
                              "UniversityEducation_merged")

wave2 <- wave2 %>%
  select(-c(AttitudeEBMH4_num, AttitudeEBMH4_1_num,
            UniversityEducation_1_num, UniversityEducation_2_num,
            AttitudeEBMH4_diff, UniversityEducation_diff,
            UniversityEducation_1, UniversityEducation_2,
            AttitudeEBMH4_1, AttitudeEBMH4)) %>%
  rename(UniversityEducation = UniversityEducation_merged,
         AttitudeEBMH4       = AttitudeEBMH4_merged)

rm(AttitudeEBMH4_discrepancies, UniversityEducation_discrepancies)


### 6. Reverse coding --------------------------------------------------------
wave1$AttitudeEBMH3  <- reverse_likert_text(wave1$AttitudeEBMH3)
wave2$AttitudeEBMH3  <- reverse_likert_text(wave2$AttitudeEBMH3)
wave2$PerceptionGap2 <- reverse_likert_text(wave2$PerceptionGap2)


### 7. Therapeutic orientation sliders ---------------------------------------

## 7.1 Remove duplicate slider (Wave 2)
wave2 <- wave2 %>% select(-SchoolSliderPsychodynamic_Depth_Analytical)

## 7.2 Unused sliders count as 0 if the forced-choice question was answered
sliders_w1 <- c("SchoolSliderCBT", "SchoolSliderPsychodynamic", "SchoolSliderAnalytical",
                "SchoolSliderSystemic", "SchoolSliderNeuropsychology",
                "SchoolSliderPersonCentered", "SchoolSliderACT", "SchoolSliderOther")

sliders_w2 <- c("SchoolSliderCBT", "SchoolSliderPsychodynamic",
                "SchoolSliderSystemic", "SchoolSliderNeuropsychology",
                "SchoolSliderPersonCentered", "SchoolSliderACT",
                "SchoolSliderHumanistic", "SchoolSliderMBT", "SchoolSliderOther")

wave1 <- fill_na(wave1, sliders_w1, forced_col = "SchoolForcedChoice")
wave2 <- fill_na(wave2, sliders_w2, forced_col = "SchoolForcedChoice")

cat("Wave 1: forced-choice answered:", sum(!is.na(wave1$SchoolForcedChoice)), "rows\n")
cat("Wave 2: forced-choice answered:", sum(!is.na(wave2$SchoolForcedChoice)), "rows\n")

rm(sliders_w1, sliders_w2)


### 8. Graduation year and years of experience -------------------------------
# Free-text answers recoded to a single number

## 8.1 Graduation year: earliest (university) degree
wave1 <- wave1 %>%
  mutate(
    GraduationYear = case_when(
      GraduationYear == "Studium Ende: 2018"                                 ~ "2018",
      GraduationYear == "GZ opleiding in 2020, master in 2016"               ~ "2016",
      GraduationYear == "2016 (Approbation)"                                 ~ "2016",
      GraduationYear == "2009 Universiteit, 2014 als GZ-psycholoog"          ~ "2009",
      GraduationYear == "2013 aan de uni, 2020 GZ-psycholoog"                ~ "2013",
      GraduationYear == "2022 (Psychologische Psychotherapie Weiterbildung)" ~ "2022",
      GraduationYear == "2025/ Noch nicht beendet"                           ~ "2025",
      GraduationYear == "Orthopedagoog 2015, GZ-psycholoog 2022"             ~ "2015",
      TRUE ~ GraduationYear
    )
  )

wave2 <- wave2 %>%
  mutate(
    GraduationYear = case_when(
      GraduationYear == "1997 / 2010 / 2023"               ~ "1997",
      GraduationYear == "unklar"                           ~ NA_character_,
      GraduationYear == "2015 als GZ, 2008 als psycholoog" ~ "2008",
      GraduationYear == "2014 (GZ), 2021 (PT)"             ~ "2014",
      GraduationYear == "Deutschland"                      ~ NA_character_,
      trimws(GraduationYear) %in% c("", "NA", "N/A", "na") ~ NA_character_,
      TRUE ~ GraduationYear
    )
  )

## 8.2 Years of experience (approximate answers such as "more than 20" are
##     set to the stated or a nearby value)
wave1 <- wave1 %>%
  mutate(
    YearsExperience = gsub(",", ".", YearsExperience),
    YearsExperience = case_when(
      YearsExperience == "7 jaar"                          ~ "7",
      YearsExperience == "4.5 jaar"                        ~ "4.5",
      YearsExperience == "13 (waarvan 9 als GZ-psycholoog)" ~ "13",
      YearsExperience == "'+/- 25"                         ~ "25",
      YearsExperience == "16 jaar"                         ~ "16",
      YearsExperience == "21 jaar"                         ~ "21",
      YearsExperience == "30 jaar"                         ~ "30",
      trimws(YearsExperience) %in% c("", "NA", "N/A", "na") ~ NA_character_,
      TRUE ~ YearsExperience
    ),
    YearsExperience = as.numeric(YearsExperience)
  )

wave2 <- wave2 %>%
  mutate(
    YearsExperience = gsub(",", ".", YearsExperience),
    YearsExperience = case_when(
      YearsExperience == "23 jaar"                          ~ "23",
      YearsExperience %in% c("30 Jahr", "> 30 J.")          ~ "30",
      YearsExperience == "ongeveer 15 jaar"                 ~ "15",
      YearsExperience == "37 Jahre"                         ~ "37",
      YearsExperience %in% c("mehr als 20", "Über 25")      ~ "25",
      trimws(YearsExperience) %in% c("", "NA", "N/A", "na") ~ NA_character_,
      TRUE ~ YearsExperience
    ),
    YearsExperience = as.numeric(YearsExperience)
  )


### 9. Professional background (Wave 2) --------------------------------------

## 9.1 Answers under "other, namely"
wave2 %>%
  filter(ProfessionalBackground == "other, namely") %>%
  select(ProfessionalBackground_TEXT)

## 9.2 Remove professions outside the target groups
wave2 <- wave2 %>%
  filter(!ProfessionalBackground_TEXT %in% c(
    "Erziehungswissenschaften",
    "GGZ VS",
    "verpleegkundig specialist ggz (MANP)",
    "verpleegkundig specialist GGZ",
    "Psychiatrisch Verpleegkundige"
  ))

wave2 <- wave2 %>%
  filter(is.na(ProfessionalBackground) | ProfessionalBackground != "Social worker")

## 9.3 Assign psychologists and medical doctors from "other, namely"
psychologist_text <- c(
  "gz psycholoog",
  "Psychologie Studium Bachelor, Systemische Familientherapeutin, Ergotherapeutin",
  "GZ-psycholoog in opleiding tot klinisch psycholoog",
  "Gz psychologe-psychotherapeute",
  "Psychologin und approbierte psycholog. Psychotherapeutin",
  "Psychologische Psychotherapeutin"
)

wave2 <- wave2 %>%
  mutate(
    ProfessionalBackground = case_when(
      ProfessionalBackground == "other, namely" &
        ProfessionalBackground_TEXT %in% psychologist_text ~ "Psychologist",
      ProfessionalBackground == "other, namely" &
        ProfessionalBackground_TEXT == "Ärztin"            ~ "Medical doctor",
      TRUE ~ ProfessionalBackground
    )
  )

rm(psychologist_text)

## 9.4 Wave 1 sampled psychologists only
wave1$ProfessionalBackground <- "Psychologist"


### 10. Further exclusions and recoding --------------------------------------

## 10.1 Degree below Master's level
wave1 <- wave1 %>%
  filter(!Degree %in% c("Bachelor's Degree or equivalent", "High School Degree or equivalent"))
wave2 <- wave2 %>%
  filter(!Degree %in% c("Bachelor's Degree or equivalent", "High School Degree or equivalent"))

## 10.2 Age as a number
unique(wave1$Age)
unique(wave2$Age)
wave1$Age <- as.numeric(gsub(" ?Jahre", "", trimws(wave1$Age)))
wave2$Age <- as.numeric(gsub(" ?Jahre", "", trimws(wave2$Age)))

## 10.3 Length of treatment trajectory: Wave 2 categories recoded to Wave 1
unique(wave1$LengthTrajectory)
unique(wave2$LengthTrajectory)

wave2$LengthTrajectory_original <- wave2$LengthTrajectory

wave2 <- wave2 %>%
  mutate(
    LengthTrajectory = case_when(
      LengthTrajectory == "Prolonged short-term therapy (up to around 25 sessions or up to a year)" ~
        "Short-term therapy (up to 25 sessions or up to a year)",
      LengthTrajectory == "Short-term therapy ( around 1 to 16 sessions)" ~
        "Short-term therapy (up to 25 sessions or up to a year)",
      LengthTrajectory == "Long-term therapy (more than 25 sessions or longer than a year)" ~
        "Long-term therapy (more than 25 sessions or longer than a year)",
      LengthTrajectory == "It varies" ~ "It varies",
      TRUE ~ NA_character_
    )
  )


### 11. Outliers and experience criterion ------------------------------------

## 11.1 z-scores for years of experience, graduation year and age (|z| > 3.29)
outlier_check <- bind_rows(
  wave1 %>% select(Participant_ID, YearsExperience, GraduationYear, Age) %>% mutate(Wave = "Wave 1"),
  wave2 %>% select(Participant_ID, YearsExperience, GraduationYear, Age) %>% mutate(Wave = "Wave 2")
) %>%
  mutate(
    YearsExperience_z       = scale(YearsExperience)[, 1],
    YearsExperience_outlier = abs(YearsExperience_z) > 3.29,
    GraduationYear          = suppressWarnings(as.numeric(as.character(GraduationYear))),
    GraduationYear_z        = scale(GraduationYear)[, 1],
    GraduationYear_outlier  = ifelse(is.na(GraduationYear_z), FALSE, abs(GraduationYear_z) > 3.29),
    Age                     = suppressWarnings(as.numeric(as.character(Age))),
    Age_z                   = scale(Age)[, 1],
    Age_outlier             = ifelse(is.na(Age_z), FALSE, abs(Age_z) > 3.29)
  )

## 11.2 Years of experience: 50 years checked against age and graduation year
##      and kept as plausible
outlier_check %>% filter(YearsExperience_outlier)
wave1 %>%
  filter(YearsExperience == 50) %>%
  select(Participant_ID, Age, GraduationYear, YearsExperience)

## 11.3 At least one year of work experience
# Wave 2: screening question (Experience1Year)
unique(wave2$Experience1Year)

# Wave 1: reported years of experience
n_before <- nrow(wave1)
wave1 <- wave1 %>% filter(is.na(YearsExperience) | YearsExperience >= 1)
cat("Less than 1 year of experience - Wave 1 removed:", n_before - nrow(wave1), "cases\n")

# Unfinished questionnaires without years of experience
n_before_w1 <- nrow(wave1)
n_before_w2 <- nrow(wave2)
wave1 <- wave1 %>% filter(!(Finished == "False" & is.na(YearsExperience)))
wave2 <- wave2 %>% filter(!(Finished == "False" & is.na(YearsExperience)))
cat("Unfinished without years of experience - Wave 1 removed:", n_before_w1 - nrow(wave1),
    "cases; Wave 2 removed:", n_before_w2 - nrow(wave2), "cases\n")
rm(n_before_w1, n_before_w2)


### 12. Final inclusion: at least two of the three outcomes ------------------
# Based on the items of the final outcome measures: EBMH use (UseEBMH1),
# EBMH openness (UseEBMH2) and EBMH attitude (AttitudeEBMH1, AttitudeEBMH2;
# available if at least one item is answered)
count_outcomes <- function(df) {
  df %>%
    mutate(
      use_completed        = !is.na(UseEBMH1),
      openness_completed   = !is.na(UseEBMH2),
      attitude_completed   = !is.na(AttitudeEBMH1) | !is.na(AttitudeEBMH2),
      n_outcomes_completed = use_completed + openness_completed + attitude_completed
    )
}

wave1 <- count_outcomes(wave1)
wave2 <- count_outcomes(wave2)

outcome_counts <- bind_rows(
  wave1 %>% summarise(Wave = "Wave 1", Before = n(),
                      Included = sum(n_outcomes_completed >= 2),
                      Excluded = sum(n_outcomes_completed < 2)),
  wave2 %>% summarise(Wave = "Wave 2", Before = n(),
                      Included = sum(n_outcomes_completed >= 2),
                      Excluded = sum(n_outcomes_completed < 2))
)
print(outcome_counts)

wave1 <- wave1 %>% filter(n_outcomes_completed >= 2)
wave2 <- wave2 %>% filter(n_outcomes_completed >= 2)

## 12.1 Final Wave 2 sample: items asked twice (section 5) and
##      self-reported participation in the earlier wave
cat("AttitudeEBMH4 asked twice - both versions answered:",
    sum(!is.na(wave2$dup_AttitudeEBMH4_diff)),
    "| different answers:", sum(wave2$dup_AttitudeEBMH4_diff > 0, na.rm = TRUE),
    "| 2 or more points apart:", sum(wave2$dup_AttitudeEBMH4_diff >= 2, na.rm = TRUE), "\n")
cat("UniversityEducation asked twice - both versions answered:",
    sum(wave2$dup_UniversityEducation_both), "\n")
cat("Participation in the previous study (Wave 2):\n")
print(table(wave2$ParticipationPreviousStudy, useNA = "ifany"))
wave2 <- wave2 %>% select(-dup_AttitudeEBMH4_diff, -dup_UniversityEducation_both)

rm(count_outcomes)


### 13. Remaining outliers ---------------------------------------------------

## 13.1 Graduation year: "21" recoded to 2021, "33" set to missing
outlier_check %>% filter(GraduationYear_outlier)
wave1$GraduationYear <- with(wave1, ifelse(GraduationYear == 21, 2021, GraduationYear))
wave2$GraduationYear <- with(wave2, ifelse(GraduationYear == 33, NA, GraduationYear))

## 13.2 Age
outlier_check %>% filter(Age_outlier)


### 14. Split open-ended and closed answers ----------------------------------
quali_wave1 <- wave1 %>% select(contains("TEXT"))
quali_wave2 <- wave2 %>% select(contains("TEXT"))

wave1 <- wave1 %>% select(-contains("TEXT"))
wave2 <- wave2 %>% select(-contains("TEXT"))


### 15. Combine both waves ---------------------------------------------------

## 15.1 Wave 1 items stored as text to numbers (already numeric in Wave 2, see 5.3)
wave1$AttitudeEBMH4       <- likert_to_num(wave1$AttitudeEBMH4)
wave1$UniversityEducation <- likert_to_num(wave1$UniversityEducation)

## 15.2 Combine
data <- bind_rows(
  wave1 %>% mutate(Wave = "Wave 1"),
  wave2 %>% mutate(Wave = "Wave 2")
)

## 15.3 Which variables were assessed in both waves, one wave, or not at all
waves <- sort(unique(na.omit(data$Wave)))

wave_presence <- sapply(setdiff(names(data), "Wave"), function(var) {
  present_by_wave <- sapply(waves, function(w) any(!is.na(data[[var]][data$Wave == w])))
  if (all(present_by_wave)) {
    "Both waves"
  } else if (present_by_wave[1]) {
    paste0(waves[1], " only")
  } else if (present_by_wave[2]) {
    paste0(waves[2], " only")
  } else {
    "No valid data"
  }
})

wave_info <- data.frame(
  Variable         = names(wave_presence),
  Wave             = unname(wave_presence),
  stringsAsFactors = FALSE,
  row.names        = NULL
)

print(table(wave_info$Wave, useNA = "ifany"))

## 15.4 Remove identifiers
data <- data %>% select(-c(Participant_ID, ParticipationPreviousStudy))


### Clean environment --------------------------------------------------------
rm(waves, wave_presence, outlier_check, outcome_counts)