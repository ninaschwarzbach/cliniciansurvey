# Clinician Survey Project

Analysis code for the manuscript **"Clinicians' Engagement with Evidence-Based Mental Health: Correlates and Perceived Barriers"**.

The study is an exploratory, preregistered online survey of 249 clinicians (psychologists and medical doctors) in Germany and the Netherlands, collected in two waves (Wave 1: May 2023, n = 156; Wave 2: June–August 2024, n = 93). It examines how individual and contextual characteristics relate to clinicians' attitudes toward, openness to, and use of evidence-based mental health care (EBMH), and how clinicians describe barriers to using research evidence.

All study materials are on the Open Science Framework (OSF): https://osf.io/7eyra/

- Preregistration: https://osf.io/wrp78/
- Analysis plan: `01_Preregistration_and_analysis_plan/AnalysisPlan_2025-09.pdf` on OSF
- Documented deviations from the preregistration: `01_Preregistration_and_analysis_plan/PreregistrationDeviations_2026-11.pdf` on OSF
- Questionnaires (English, German, Dutch, with question numbers, survey flow and logic): `02_Wave1_2023/W1_Questionnaire_EN-DE-NL.pdf`, `03_Wave2_2024/W2_Questionnaire_EN-DE-NL.pdf`, and an item-by-item comparison of both waves (`01_Preregistration_and_analysis_plan/QuestionnaireComparison_W1-W2.xlsx`)
- Participant information, informed consent, stopping rule, data management plan and recruitment texts: folders `02_Wave1_2023` and `03_Wave2_2024` on OSF
- Results of earlier student projects with the Wave 1 data: `06_Supplementary/Wave1_StudentProjects_Results.pdf` on OSF
- Data: shared after publication of the article (see *Data* below)

---

## How to run the analysis

1. Install [R](https://www.r-project.org/) (version 4.5.0 was used) and [RStudio](https://posit.co/download/rstudio-desktop/).
2. Download or clone this repository: https://github.com/ninaschwarzbach/cliniciansurvey
3. Open the script that fits the data you have in RStudio and click **Source**:

| You have | Script | What it reproduces |
|---|---|---|
| The anonymized public dataset (`data/public/EBMH-ClinicianSurvey_data_anonymized.csv`) | `analysis_main_public.R` | All results except those involving age, gender, years of experience and the open-ended responses (see *Data*) |
| The raw Qualtrics exports (`data/raw/wave1_data.csv`, `data/raw/wave2_data.csv`; available on request) | `analysis_main.R` | All results |

Both scripts install missing packages and source the analysis scripts in the order below. `analysis_main.R` writes all tables to `outputs/` and all figures to `plots/`; `analysis_main_public.R` writes them to `outputs_public_data/` and `plots_public_data/`, so the results based on the public dataset can be compared directly with those of the full analysis. Each script empties its own two folders at the start of every run, so every file in them comes from the current run. The scripts need RStudio because they set the working directory to their own folder.

---

## Scripts

| Script | What it does | Output (manuscript) |
|---|---|---|
| `analysis_main.R` | Runs the complete analysis from the raw data | – |
| `analysis_main_public.R` | Runs the analysis on the anonymized public dataset | – |
| `functions.R` | Helper functions (Likert conversion, descriptives, correlations and confidence intervals, plots) | – |
| `maps.R` | Plot colours and variable labels | – |
| `data_cleaning.R` | Reads both Qualtrics exports, applies the exclusion criteria, harmonizes and combines the two waves | Participant flow (Figure 1) |
| `public_dataset.R` | Creates the anonymized public dataset and its codebook from the raw data (run on its own) | – |
| `sample_characteristics.R` | Demographic and professional characteristics | Tables 1 and 2, Figure D1 |
| `scale_creation.R` | Converts Likert answers to numbers (1–5) and creates the final variables | – |
| `level-item_stats.R` | Reliability of the scales and item analyses behind the scale decisions | Method (reliabilities), Tables B1 and B2 |
| `descriptives.R` | Descriptive statistics by wave and in total | Tables 3, 4 and E1, Figures E1 and E2 |
| `wave2_descriptives.R` | Use of evidence, treatment manuals and research resources (Wave 2) | Results |
| `correlational_analysis.R` | Associations with EBMH attitude, openness and use | Table F1, Figures 2, 3, 4 and F1 |
| `wave2_specific_analysis.R` | Colleagues' endorsement of EBMH and feeling updated, by context (Wave 2) | Figure 5 |
| `open-ended_responses.R` | Prepares the open-ended responses for qualitative coding | Appendix C |

### Analysis decisions in brief
- Correlations: Pearson's *r* between continuous variables, Spearman's ρ for the two ordinal items (familiarity with EBMH, EBMH-endorsing university education), point-biserial *r* for country and wave, and η² for the other categorical variables.
- Confidence intervals: 95% for *r* and ρ (Fisher's *z*; Bonett–Wright standard error for ρ), 90% for η² (noncentral *F*).
- No null-hypothesis significance tests (exploratory study); associations are interpreted by their magnitude.
- Effect sizes are labelled based on estimates rounded to two decimals.

Full details are given in the manuscript and in the analysis plan on OSF.

---

## Data

The anonymized dataset (`data/public/EBMH-ClinicianSurvey_data_anonymized.csv`) and its codebook (`data/public/EBMH-ClinicianSurvey_codebook.csv`) will be shared after publication of the article, as announced in the participant information. Answers on 5-point scales are coded from 1 (strongly disagree / disagree totally) to 5 (strongly agree / agree totally); AttitudeEBMH3 and PerceptionGap2 are reverse-coded. Rows are in random order and have a new ID.

To protect participants' anonymity, the public dataset does not include age, gender, years of clinical experience, graduation year, open-ended responses, participant codes, or Qualtrics metadata. Therefore, `analysis_main_public.R` cannot reproduce the sample characteristics (Tables 1 and 2, Figure D1), the associations involving age, years of experience and gender (in Table F1 and Figures 2, 3 and F1), and the summaries of the open-ended responses (Appendix C). All other results are identical to those in the article.

The complete dataset is available from the corresponding author upon reasonable request. Raw data and outputs containing open-ended responses are not part of this repository. `public_dataset.R` documents how the public dataset was created from the raw data.

---

## Software

R 4.5.0 with tidyverse 2.0.0, psych 2.5.6, effectsize 1.0.1, gghalves 0.1.4, gridExtra 2.3, patchwork 1.3.2, rlang 1.1.6, writexl 1.5.4 and rstudioapi 0.17.1. `analysis_main.R` prints the full `sessionInfo()` at the end of each run.

---

## License

The code is licensed under the [MIT License](LICENSE).

## Citation

If you use this code, please cite the article: [reference after publication].

## Contact

Nina R. Schwarzbach, University of Groningen – n.r.schwarzbach@rug.nl