# Social contact and mobility patterns among adults in four US states

Code for the manuscript analysis of the ACCIDDA contact and travel survey
(Florida, Georgia, Maryland and North Carolina, waves 1 and 2).

## Scripts

Run from the repository root, in order:

| Script | What it does | Main outputs |
|---|---|---|
| `scripts/01_analysis.R` | Cleans the survey, applies exclusions, rakes survey weights to 2023 ACS margins, builds travel variables, clusters respondents by travel, compares clusters, and fits survey-weighted negative binomial models | `data/derived/analysis_data.rds`, `outputs/analysis/*.csv` |
| `scripts/02_tables_figures.R` | Manuscript tables and Figure 4 | `outputs/tables/`, `outputs/figures/` |
| `scripts/03_contact_matrices.R` | Builds the contact matrix data (socialmixr participants/contacts format), the weighted and unweighted contact matrices, Q-indices, and Figures 1-3 | `data/analysis_data/`, `outputs/contact_matrices/`, `outputs/figures/` |
| `scripts/04_supplemental.R` | Supplemental Materials tables and figures. Reruns `01_analysis.R` without exclusions for the full-sample sensitivity analysis (Figure S7, Table S5), so it takes about 10 minutes | `outputs/supplement/` |

```r
source("scripts/01_analysis.R")
source("scripts/02_tables_figures.R")
source("scripts/03_contact_matrices.R")
source("scripts/04_supplemental.R")
```

`03_contact_matrices.R` can be rerun without the survey data from
`data/analysis_data/participants.csv` and `data/analysis_data/contacts.csv`,
which contain only a row ID, age group, survey weight and travel cluster for
each respondent, and the age range, type and setting of each contact.

## Inputs

- `data/analysis_data/wave1_2_analysis.csv`: one row per survey response (23,903 rows), with
  only the variables used in the analysis: wave, panel, state, day of the week
  the survey was started, age, household size, gender, race/ethnicity,
  education, household income, occupation, work outside the home, political
  party and neighborhood type (Figure S3 only), the 72
  contact counts (`{any_physical|non_physical}_{contact age group}_{setting}`),
  and work/school and vacation/other travel in the past two weeks (days per
  week, number of destinations, nights away, and geocoded distances from home
  in km). Destinations and survey timestamps are not included.
- `data/2023 ACS 1 year/ACS_2023_{age,education,sex,race}.csv`: 2023 ACS
  1-year raking margins for Florida, Georgia, Maryland and North Carolina,
  from data.census.gov tables S0101 (age, adults 18+), S1501 (education,
  adults 18+: the 18-24 and 25+ rows combined) and DP05 (sex and race/ethnicity,
  total population).

## Outputs and the manuscript

The `outputs/` folder is not included in the repository; the scripts create it
when run.

| Manuscript | File |
|---|---|
| Table 1 | `outputs/tables/table_1_demographics_by_state.csv` |
| Table 2 | `outputs/tables/table_2_travel_by_cluster.csv` |
| Table 3 | `outputs/tables/table_3_demographics_by_cluster.csv`, `table_3_cluster_tests.csv` |
| Figure 1 | `outputs/figures/figure_1_matrices_by_type.png` |
| Figure 2 | `outputs/figures/figure_2_matrices_by_type_and_setting.png` |
| Figure 3 | `outputs/figures/figure_3_matrices_by_cluster.png` |
| Figure 4 | `outputs/figures/figure_4_forest_plot.png` |
| Table S1, Figure S1 | `outputs/supplement/table_s1_contacts_weighted.csv`, `figure_s1_contacts_by_setting.png` |
| Table S2 | `outputs/supplement/table_s2_travel_by_state.csv` |
| Figure S3 | `outputs/supplement/figure_s3a/b/c_demographics_by_cluster.png` |
| Figures S4-S6 | `outputs/supplement/figure_s4_*.png`, `figure_s5_*.png`, `figure_s6_*.png` |
| Table S3 | `outputs/supplement/table_s3_contacts_by_cluster.csv` |
| Table S4 | `outputs/supplement/table_s4_travel_cluster_models.csv` |
| Table S5, Figure S7 | `outputs/supplement/table_s5_full_sample_sensitivity.csv`, `figure_s7_full_sample_sensitivity.png` |
| Figures S8-S9 | `outputs/supplement/figure_s8_*.png`, `figure_s9_*.png`, `distance_models.csv` |
| Figure S10 | `outputs/supplement/figure_s10_sample_vs_acs.png` |
| Table S7 | `outputs/supplement/table_s7_weights.csv` |
| Figures S11-S12 | `outputs/figures/figure_s_unweighted_matrices_by_type.png`, `figure_s_unweighted_matrices_by_type_and_setting.png` (from `03_contact_matrices.R`) |
| Figures S13-S14 | `outputs/supplement/figure_s13_elbow_full_sample.png`, `figure_s14_elbow_low_mobility.png` |
| Q-indices | `outputs/contact_matrices/q_index.csv` |

Manuscript tables are also collected in `outputs/tables/manuscript_tables.xlsx` and
supplemental tables in `outputs/supplement/supplement_tables.xlsx`. Figure S2
(cluster diagram) and Table S6 (sampling targets) are not made by code.

## socialmixr data

`socialmixr/` holds the survey formatted for socialmixr and
socialcontactdata.org (SOCRATES format) in `socialmixr/data/`, and the script
that makes it, `socialmixr/make_socialmixr_files.R` (run after
`01_analysis.R`). See `socialmixr/README.md`.

## R packages

tidyverse, lubridate, survey, sjstats (`svyglm.nb`), Hmisc, table1, writexl,
socialmixr, patchwork, viridis, reshape2. Package versions used are written
to `outputs/*/session_info.txt` on each run.

