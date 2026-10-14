# Wheelchair Racing Survey — Training Practices and Perspectives

[![OSF](https://img.shields.io/badge/OSF-Open%20Data-blue)](https://osf.io/79yqp)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

## Overview

This repository contains the analysis scripts, anonymized dataset, and exercise classification dictionaries for the study:

> **Antunes D, Coroa ELM, Nascimento EMF, Kons RL, Lucas RD, Turnes T.** *Training Prescriptions and Perspectives in Elite Wheelchair Racing: A Survey of Brazilian Athletes and Coaches.* *Int J Sports Sci Coach*, 2026.

The study investigated training practices, knowledge sources, and challenges among Brazilian elite wheelchair racing (WR) athletes (n = 28) and coaches (n = 16), using a cross-sectional survey design applied during the 2025 Brazilian Paralympic Athletics Championship.

---

## Repository Structure

```
wheelchair_racing_survey/
│
├── data/
│   ├── clean_analysis_data.csv       # Available upon request (not tracked in repo)
│   └── exercises/
│       ├── raw_exercise_list.csv     # Raw open-ended exercise responses
│       ├── E1_Strength_Cleaned.csv
│       ├── E2_Speed_Cleaned.csv
│       ├── E3_Agility_Cleaned.csv
│       ├── E4_Flexibility_Cleaned.csv
│       ├── E5_InjuryPrev_Cleaned.csv
│       ├── E6_SpecificCond_Cleaned.csv
│       ├── E7_Performance_Cleaned.csv
│       └── E8_Mobility_Cleaned.csv
│
├── scripts/
│   ├── 04_analyze_exercises.R        # Semi-automated exercise classification
│   ├── 05_characterization.R         # Sample characterization and descriptives
│   └── 06_importance_data.R          # Perceived importance analysis
│
├── outputs/
│   ├── Figure_1.tiff                 # Knowledge sources and physical components
│   └── Figure_2.tiff                 # Exercise preferences by group
│
└── supplementary/
    ├── SDC1_supplementary_material_surveys.docx    # Survey instruments (Portuguese)
    ├── SDC2_supplementary_material_CHERRIES.docx   # CHERRIES reporting checklist
    └── SDC3_supplementary_material_CROSS.docx      # CROSS reporting checklist
```

---

## Data Pipeline (not shared)

Raw data collection involved three separate Google Forms exports (adult athletes, junior athletes, and coaches). The pre-processing pipeline — including variable renaming, group merging, and anonymization — was conducted prior to public release. The fully anonymized output is `data/clean_analysis_data.csv`, which is the sole input for all shared analysis scripts.

---

## Analysis Scripts

All scripts were written in R and run within the [Positron IDE](https://positron.posit.co/). Scripts are numbered to reflect execution order:

| Script | Description |
|--------|-------------|
| `04_analyze_exercises.R` | Applies the semi-automated classification dictionary to open-ended exercise responses across 8 training categories |
| `05_characterization.R` | Generates descriptive statistics and sample characterization (Table 1, Table 2, Table 3) |
| `06_importance_data.R` | Analyzes perceived importance of physical components and S&C (Figure 1) |

### Exercise Classification Dictionary

Open-ended responses about exercise selection were classified using a standardized dictionary based on expert consensus. Each `E1`–`E8` file in `data/exercises/` contains the cleaned and classified responses for one training category:

| File | Category |
|------|----------|
| E1 | Strength Development |
| E2 | Speed / Power Development |
| E3 | Agility Development |
| E4 | Flexibility Development |
| E5 | Injury Prevention |
| E6 | WR-Specific Conditioning |
| E7 | Performance Enhancement |
| E8 | Mobility Exercises (athletes only) |

---

## Reproducibility

```r
# Suggested execution order
source("scripts/04_analyze_exercises.R")
source("scripts/05_characterization.R")
source("scripts/06_importance_data.R")
```

R version and package dependencies will be added via `renv` in a future update.

---

## Data Availability

The anonymized dataset (`clean_analysis_data.csv`) is available upon reasonable request to the corresponding author. Survey instruments and supplementary materials are available on the OSF project page:

> [https://osf.io/79yqp](https://osf.io/79yqp)

---

## Ethical Approval

This study was approved by the Ethics Committee of the Federal University of Santa Catarina (UFSC), approval number **88809725.6.0000.0121**, and conducted in accordance with the Declaration of Helsinki.

---

## Citation

If you use these data or scripts, please cite:

```
Antunes D, Coroa ELM, Nascimento EMF, Kons RL, Lucas RD, Turnes T. Training Prescriptions and 
Perspectives in Elite Wheelchair Racing: A Survey of Brazilian Athletes and Coaches. 
Int J Sports Sci Coach, 2026.
```

---

## License

This repository is licensed under [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/). 
You are free to share and adapt the material with appropriate attribution.

---

## Contact

Diego Antunes — Federal University of Santa Catarina (UFSC) / Univali  
[GitHub](https://github.com/antunesdiego) · [OSF](https://osf.io/79yqp)
