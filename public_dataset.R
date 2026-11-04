# ===========================================================================
# Clinician Survey Project - Public Dataset -
# Author: Nina Schwarzbach
# Start date: 03.11.2026
# Date last updated: 04.11.2026
#
# Creates the anonymized dataset for publication and its codebook from the
# raw Qualtrics exports. Run this script on its own (Source); it runs the
# data cleaning itself.
#
# As stated in the participant information of both waves, age and gender are
# not shared. Years of experience and graduation year are not shared either,
# because they allow age to be estimated. Open-ended responses, participant
# codes and Qualtrics metadata are not shared. The complete dataset is
# available on reasonable request.
#
# Input:  data/raw/wave1_data.csv, data/raw/wave2_data.csv
# Output: data/public/EBMH-ClinicianSurvey_data_anonymized.csv
#         data/public/EBMH-ClinicianSurvey_codebook.csv
# ===========================================================================

rm(list = ls())

### 1. Setup (as in analysis_main.R) -----------------------------------------
packages <- c("tidyverse", "psych", "rlang")
missing_packages <- setdiff(c(packages, "rstudioapi"), rownames(installed.packages()))
if (length(missing_packages) > 0) install.packages(missing_packages)
invisible(lapply(packages, library, character.only = TRUE))

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

if (".func_env" %in% search()) detach(".func_env")
.func_env <- new.env(parent = .GlobalEnv)
source("functions.R", local = .func_env)
source("maps.R",      local = .func_env)
attach(.func_env, pos = 2, name = ".func_env")


### 2. Data cleaning ---------------------------------------------------------
source("data_cleaning.R")


### 3. Variables that are shared ----------------------------------------------
# Only the variables listed here are shared (everything else is left out)
public_vars <- c(
  # Data collection and background
  "Wave", "Country", "Degree", "ProfessionalBackground", "WorkSetting",
  "SelfEmployed", "LengthTrajectory",
  # Therapeutic orientation
  grep("^SchoolSlider", names(data), value = TRUE), "SchoolForcedChoice",
  # EBMH items (both waves)
  "FamiliarityEBMH", "UseEBMH1", "UseEBMH2",
  "EBMHSkills1", "EBMHSkills2", "EBMHSkills3",
  "PerceptionGap1", "PerceptionGap2",
  "AttitudeEBMH1", "AttitudeEBMH2", "AttitudeEBMH3", "AttitudeEBMH4",
  "ResearchExperience1", "ResearchExperience2",
  paste0("WorkplaceInfluences", 1:5),
  paste0("EmployerInfluences", 1:7),
  "UniversityEducation",
  # Wave 2 only
  "WorkplaceInfluences2_PsychDisorders",
  "EBMHPsychDisorders1", "EBMHPsychDisorders2",
  "ComparisonEBMHPsychDisordersOtherDisorders",
  "UseEvidence_YesNo", "ResearchResources",
  "TreatmentManuals", "TreatmentManuals_Freq",
  "EBMHUnderstandingSimilar",
  "OccationsContact", "OccationsDiscussing",
  grep("^Updated", names(data), value = TRUE),
  grep("^EBMHEndorsement", names(data), value = TRUE)
)
public_vars <- intersect(unique(public_vars), names(data))

# Check: variables that are NOT shared (look through this list before publishing)
cat("\nVariables not included in the public dataset:\n")
print(setdiff(names(data), public_vars))


### 4. Build the public dataset ------------------------------------------------
data_public <- data[, public_vars]

# Likert answers as numbers (1 = strongly disagree / disagree totally to
# 5 = strongly agree / agree totally). AttitudeEBMH3 and the Wave 2 version of
# PerceptionGap2 are already reverse-coded (data_cleaning.R, section 6).
likert_cols <- names(data_public)[sapply(data_public, function(x) {
  any(str_detect(tolower(na.omit(as.character(x))), "agree|disagree"))
})]
data_public <- data_public %>%
  mutate(across(all_of(likert_cols), likert_to_num))

# Random row order and a new ID, so rows cannot be linked to the raw export
set.seed(20261103)
data_public <- data_public[sample(nrow(data_public)), ]
data_public <- data.frame(ID = seq_len(nrow(data_public)), data_public,
                          row.names = NULL)

# Check: categories with fewer than 5 participants (possible identification)
cat("\nCategories with fewer than 5 participants:\n")
for (v in c("Country", "Degree", "ProfessionalBackground", "WorkSetting",
            "SelfEmployed", "LengthTrajectory", "SchoolForcedChoice")) {
  if (!v %in% names(data_public)) next
  tab <- table(data_public[[v]])
  small <- tab[tab < 5]
  if (length(small) > 0) {
    cat(v, ":", paste0(names(small), " (n = ", small, ")", collapse = "; "), "\n")
  }
}


### 5. Codebook ------------------------------------------------------------------

## 5.1 Question wording (English versions of the questionnaires; Wave 1 wording
##     unless the item was only asked in Wave 2; differences between the waves
##     in brackets)
slider_q <- paste0("Indicate the degree to which your therapy/interventions ",
                   "include elements of the following movement (school), 0 to 100 ",
                   "(Wave 2: The therapies/interventions I provide to patients with ",
                   "psychotic disorders use elements of the following therapy approaches): ")
updated_q <- paste0("When me and other mental healthcare professionals discuss the ",
                    "latest scientific evidence ______ I feel updated: ")
endorsed_q <- "The application of EBMH is endorsed by my colleagues ______: "

question_text <- c(
  # Background
  Country                = "In what country do you work?",
  Degree                 = "What is your highest (academic) degree?",
  ProfessionalBackground = "What is your professional background? (Wave 2 only; all Wave 1 participants were psychologists)",
  WorkSetting            = "What is your current work setting? (Wave 2: In which setting do you work mostly?)",
  SelfEmployed           = "I am self-employed and don't have more colleagues working in my practice.",
  LengthTrajectory       = "How long is your average treatment trajectory? (Wave 2 answer categories merged into the Wave 1 categories)",
  # Therapeutic orientation
  SchoolSliderCBT             = paste0(slider_q, "CBT (Cognitive Behavioural Therapy)"),
  SchoolSliderPsychodynamic   = paste0(slider_q, "Psychodynamic Psychotherapy"),
  SchoolSliderAnalytical      = paste0(slider_q, "Analytical Psychotherapy"),
  SchoolSliderSystemic        = paste0(slider_q, "Systemic Therapy"),
  SchoolSliderNeuropsychology = paste0(slider_q, "Neuropsychology"),
  SchoolSliderPersonCentered  = paste0(slider_q, "Person-centered Psychotherapy"),
  SchoolSliderACT             = paste0(slider_q, "Acceptance and Commitment Therapy (ACT)"),
  SchoolSliderHumanistic      = paste0(slider_q, "Humanistic Therapy"),
  SchoolSliderMBT             = paste0(slider_q, "Mindfulness-based CBT"),
  SchoolSliderOther           = paste0(slider_q, "Other"),
  SchoolForcedChoice          = "If you would need to choose, which therapeutic movement (school) did most of your trainings primarily follow? (Wave 2: ... did your most recent training primarily follow?)",
  # EBMH items (both waves)
  FamiliarityEBMH     = "I am familiar with the concept of EBMH.",
  UseEBMH1            = "EBMH is an essential approach in my clinical practice.",
  UseEBMH2            = "I am open to adjusting my practices when I encounter new scientific evidence.",
  EBMHSkills1         = "My research knowledge is sufficient in order to understand the scientific literature.",
  EBMHSkills2         = "My skills in the English language are sufficient to understand English scientific literature.",
  EBMHSkills3         = "I know how to use the databases to find scientific literature.",
  PerceptionGap1      = "I think there is a gap between science and practice in clinical psychology. (Wave 2: ... in psychotherapy.)",
  PerceptionGap2      = "I don't think clinical science accurately reflects clinical practice. (Wave 2: I think clinical science accurately reflects clinical practice.)",
  AttitudeEBMH1       = "I think only evidence-based treatments should be used in clinical practice.",
  AttitudeEBMH2       = "I want to use more evidence-based treatments in my practice.",
  AttitudeEBMH3       = "I think clinical experience is more valuable than clinical research in order to inform my treatment decisions.",
  AttitudeEBMH4       = "The application and adherence to EBMH is a personal responsibility in my professional practice.",
  ResearchExperience1 = "I am conducting scientific research.",
  ResearchExperience2 = "I have conducted scientific research in the past (after my graduation).",
  WorkplaceInfluences1 = "There is a collaborative atmosphere among me and my colleagues.",
  WorkplaceInfluences2 = "In my work environment, I feel comfortable to try (new) EBMH interventions.",
  WorkplaceInfluences3 = "The application of EBMH is endorsed by my colleagues.",
  WorkplaceInfluences4 = "The application of EBMH is endorsed by my supervisor.",
  WorkplaceInfluences5 = "I get support from my workplace when I want to make use of an evidence-based treatment I have no prior experience with.",
  EmployerInfluences1  = "My employer provides me with opportunities to learn new academic skills which make it easier for me to apply EBMH.",
  EmployerInfluences2  = "My employer provides me with practical support to get practical training in applying evidence-based treatments (e.g. by providing training in a specific intervention).",
  EmployerInfluences3  = "My current employer emphasizes the importance of applying EBMH.",
  EmployerInfluences4  = "My employer supports me financially in order to educate myself on the newest scientific evidence.",
  EmployerInfluences5  = "My employer recognizes that part of my working time is necessary to educate myself on the newest scientific evidence.",
  EmployerInfluences6  = "My employer provides physical facilities (such as study rooms, libraries, working stations) to educate myself on the latest scientific evidence.",
  EmployerInfluences7  = "My employer recognized EBMH in its official policies.",
  UniversityEducation  = "My university education emphasized the importance of applying EBMH.",
  # Wave 2 only
  WorkplaceInfluences2_PsychDisorders = "In my work environment I feel comfortable to try new EBMH interventions to treat patients with psychotic disorders.",
  EBMHPsychDisorders1  = "I believe that EBMH interventions carry special significance in the treatment of psychotic disorders.",
  EBMHPsychDisorders2  = "I believe that EBMH interventions are essential to the treatment of psychotic disorders.",
  ComparisonEBMHPsychDisordersOtherDisorders = "In comparison to other disorders I use EBMH interventions ____ in the treatment of psychotic disorders.",
  UseEvidence_YesNo    = "Are you consulting research to guide your practice?",
  ResearchResources    = "When you are consulting research, which resources are you using? (multiple answers possible)",
  TreatmentManuals     = "Are you using treatment manuals?",
  TreatmentManuals_Freq = "Please indicate the frequency of treatment manual use in % of total treatments.",
  EBMHUnderstandingSimilar = "My understanding of the concept of EBMH is similar to the understanding of the definition provided.",
  OccationsContact     = "On what occasion are you in contact with other mental healthcare professionals? (multiple answers possible)",
  OccationsDiscussing  = "During which of these occasions do you discuss the latest scientific evidence? (multiple answers possible)",
  UpdatedConferences   = paste0(updated_q, "at conferences"),
  UpdatedWorkplace     = paste0(updated_q, "in my direct work environment"),
  UpdatedSupervision   = paste0(updated_q, "during supervision meetings"),
  UpdatedPrivate       = paste0(updated_q, "in private"),
  UpdatedOther         = paste0(updated_q, "other"),
  UpdatedNone          = paste0(updated_q, "none of the above"),
  EBMHEndorsementConferences = paste0(endorsed_q, "at conferences"),
  EBMHEndorsementWorkplace   = paste0(endorsed_q, "in my direct work environment"),
  EBMHEndorsementSupervision = paste0(endorsed_q, "during supervision meetings"),
  EBMHEndorsementPrivate     = paste0(endorsed_q, "in private"),
  EBMHEndorsementOther       = paste0(endorsed_q, "other"),
  EBMHEndorsementNone        = paste0(endorsed_q, "none of the above")
)

# Check: variables in the dataset without question wording (only ID and Wave)
cat("\nVariables without question wording:\n")
print(setdiff(names(data_public), names(question_text)))

## 5.2 Codebook table
codebook <- bind_rows(lapply(names(data_public), function(v) {
  x <- data_public[[v]]
  in_w1 <- any(!is.na(x[data_public$Wave == "Wave 1"]))
  in_w2 <- any(!is.na(x[data_public$Wave == "Wave 2"]))
  values <- if (v %in% likert_cols) {
    "1 = strongly disagree to 5 = strongly agree"
  } else if (is.numeric(x)) {
    if (all(is.na(x))) "" else paste0(min(x, na.rm = TRUE), " to ", max(x, na.rm = TRUE))
  } else {
    vals <- sort(unique(na.omit(as.character(x))))
    if (length(vals) > 12) paste0(length(vals), " different answers (multiple choice)")
    else paste(vals, collapse = " | ")
  }
  data.frame(
    Variable = v,
    Waves    = if (in_w1 && in_w2) "Both" else if (in_w1) "Wave 1" else if (in_w2) "Wave 2" else "None",
    Type     = if (is.numeric(x)) "numeric" else "text",
    N_valid  = sum(!is.na(x)),
    Values   = values,
    Note     = case_when(
      v == "AttitudeEBMH3" ~ "reverse-coded",
      v == "PerceptionGap2" ~ "Wave 2 answers reverse-coded (positively worded in Wave 2)",
      v == "AttitudeEBMH4" ~ "Wave 2: asked twice by mistake; mean of both answers",
      v == "Wave" ~ "Wave 1: May 2023; Wave 2: June-August 2024",
      v == "ID" ~ "random ID, not linked to the raw data",
      TRUE ~ ""
    ),
    Question = if (v %in% names(question_text)) unname(question_text[[v]]) else ""
  )
}))


### 6. Save ----------------------------------------------------------------------
if (!dir.exists("data/public")) dir.create("data/public", recursive = TRUE)
write.csv(data_public, "data/public/EBMH-ClinicianSurvey_data_anonymized.csv",
          row.names = FALSE, na = "")
write.csv(codebook, "data/public/EBMH-ClinicianSurvey_codebook.csv",
          row.names = FALSE, na = "")
cat("\nPublic dataset:", nrow(data_public), "participants,",
    ncol(data_public), "variables\n")


### Clean up ---------------------------------------------------------------------
detach(".func_env")
rm(list = ls())