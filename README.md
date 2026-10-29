# Public acceptance of CCUS in rural areas: survey experiment in Germany, Norway and the Netherlands

Replication code for the STORCITO stakeholder survey (Work Package 05): an online survey experiment with 2,979 respondents from rural areas in Germany (DE), Norway (NO) and the Netherlands (NL). Respondents were randomly assigned to a control group or one of five short information texts on local co-benefits of carbon capture, utilisation and storage (CCUS) (jobs, local economic growth, preservation of jobs and industries, energy security) and CCUS in the context of climate change mitigation.

The code produces the descriptive results, the treatment effects, the heterogeneity and machine-learning (targeting) analyses, and aggregated regional data for a map application.

> **Report:** Deliverable D5.3

> **Funding:**
<p>
  <img src="logos/storcito.png" alt="STORCITO" height="70">
  &nbsp;&nbsp;&nbsp;
  <img src="logos/eu_funded.png" alt="Funded by the European Union" height="70">
</p>

> This research was developed within the framework of the STORCITO project, Funded by the European Union under grant agreement Nº101182153. Views and opinions expressed are however those of the author(s) only and do not necessarily reflect those of the European Union or Research Executive Agency (REA). Neither the European Union nor the granting authority can be held responsible for them.

> **Citation:** Deller, N. & Lohbeck, L. (2026). *Replication code for: D5.3 Statistical Package.* Zenodo. https://doi.org/[DOI]
---

## Overview of the Code Files
The code works in two stages: 
1. **Data preparation** (`01_cleaning.R`): reads the raw survey files of the three countries, keeps completed interviews, removes the soft-launch interviews, recodes and harmonises the variables, joins the countries, and saves **one analysis dataset** (`data/analysis_data.rds`). The raw data are not public; this script documents every step and is run by the authors only. A synthetic dataset with the same structure is included. 
2. **Analysis** (`02`–`05`): all analyses start from the analysis dataset. Country analyses use the rows of one country; the pooled analysis uses all rows. 

| File | What it does |
|---|---|
| `00_run_all.R` | Runs everything in the right order. **Start here.** Settings at the top. |
| `01_cleaning.R` | Stage 1: creates the analysis dataset from the raw data (included for documentation purposes only; needs the raw data). Section 9 creates the synthetic, public dataset.|
| `R/load_data.R` | Stage 2 starts here: loads the analysis dataset and creates the pooled and country datasets. |
| `02_analysis.R` | Descriptive results, average treatment effects (OLS with robust standard errors), multiple-testing corrections, robustness checks, cross-country comparisons, correlates of support. |
| `03_targeting.R` | Heterogeneity analyses: support and treatment effects by subgroup, interaction tests, and machine learning (random forest, causal forest, RATE/TOC, policy tree). |
| `04_forest_seeds.R` | Robustness of the machine-learning results to the random seed (50 seeds in total). Optional, slow. |
| `05_app_data.R` | Aggregated indicators and communication suggestions per region (federal states, landsdeler, provinces) per country, for the map application. Included only for documentation purposes. Regional results based on the full survey dataset are provided as aggregated tables (results/app/).|
| `R/forest_features.R` | Alternative coding of the characteristics for the forests (used by `04_forest_seeds.R`). |
| `tools/make_nl_lookup.R` | Documents how the Dutch postcode-to-province table in data/lookup/ was built from public CBS data (only for documentation purposes; requires cbsodataR) |
| `data/` | analysis_data.rds: the cleaned, full analysis dataset (not public). analysis_data_synthetic.rds: synthetic dataset with the same structure, for running the code without the survey data. codebook.xlsx: description of all variables. raw/: raw survey data (not public). lookup/: public lookup tables. |
| `output/` | Empty; all tables and figures are written here. |
| `results/` | Aggregate results from the analyses of the full survey. Contains all tables and figures created within the package.  |
---

## Data

The survey was conducted by Norstat (Norstat Deutschland GmbH and partners in Norway and the Netherlands). The data contain personal information, including political preferences. The original survey data cannot be shared publicly for data protection reasons. The survey was conducted online with access panels using quotas for age and gender; results may not be fully representative of the rural population.

`01_cleaning.R` documents how the analysis dataset was created from the raw files:
- only completed interviews (`status == 3`);
- the earliest interviews of the soft launch are removed (DE: 54, NO: 36, NL: 56);
- support items are reversed so that 7 = strongly support, and "don't know" is set to missing;
- petition answers are turned into binary indicators (sign for / against / would not sign / undecided);
- moderators are recoded (e.g. education: university degree or not; parties grouped into left, centre, right and other);
- the three countries are joined into one dataset.

---

### Synthetic dataset

The original survey data cannot be made public. To allow others to run the code directly, the package includes a synthetic dataset (`data/analysis_data_synthetic.rds`), created by section 9 of `01_cleaning.R`. It has the same structure as the analysis dataset: the same variables, categories and number of respondents per country (2,979 in total), but no identifiers, time stamps, survey metadata or free-text answers. All scripts therefore run on it without changes.

The synthetic dataset contains no real respondents. Within each country, every variable was resampled independently from its own distribution: each column has realistic values, but the rows are random combinations of values from different respondents. As a consequence, relationships between variables, such as the effects of the information treatments or the link between climate concern and support for CCUS, are **not** preserved. **Results based on the synthetic data are meaningless** and do not reproduce the results of the report; these are provided in the folder `results/`.

To further protect respondents, only the variables used by the analysis scripts were kept (no identifiers, time stamps, survey metadata or free-text answers), respondent ids were replaced by random numbers, postcodes were shortened to four digits, and values of personal characteristics (e.g. occupation or region) held by fewer than five respondents per country were set to missing.

The synthetic dataset is the default in `00_run_all.R`. To run the analysis on the full survey data, which is only possible for the authors, set `analysis_file` to `data/analysis_data.rds`. An anonymized version of the survey data may be made available after inquiry. 

### Information treatments 

The survey contained an information experiment. All respondents first read a short general introduction to CCUS. Respondents were then randomly assigned to a control group or to one of five groups that read an additional short text:

| Group (`treat`) | Name | Content of the additional text |
|---|---|---|
| 1 | Control | No additional text |
| 2 | Jobs | Potential creation of new local jobs through a CCUS project |
| 3 | Local economic growth | Potential contribution of CCUS projects to local economic growth |
| 4 | Preservation of jobs and industries | Potential of CCUS to preserve local jobs and industries |
| 5 | Energy security | Potential of CCUS to improve local energy supply security |
| 6 | Climate change | Role of CCUS in climate change mitigation, including the threats of climate change |

Groups 2–5 provide information on potential economic co-benefits of CCUS, group 6 on its direct climate benefit. All questions on the outcome measures (support and petitions) were asked after the texts. 

## How to run

**Requirements:** R (version 4.3 or later) and the packages listed in `00_run_all.R`. An internet connection is needed for the map of Germany (giscoR). The Dutch postcode lookup table is included; rebuilding it with tools/make_nl_lookup.R additionally requires the package cbsodataR and an internet connection.
1. Open the project folder in RStudio (or set it as working directory).
2. Install the packages listed in 00_run_all.R (the script lists any missing ones). The package versions used for the results in results/ are listed in results/sessionInfo.txt.
3. In `00_run_all.R`, choose the analysis dataset:
   - data/analysis_data_synthetic.rds (synthetic public data),
   - data/analysis_data.rds (original data, not public)
4. Run:
   ```r
   source("00_run_all.R")
   ```

All results are written to `output/`, with the dataset as prefix (`DE_`, `NO_`, `NL_`, `ALL_` for the pooled data).

**Runtime**: about 20 minutes for the complete analysis, including 04_forest_seeds.R, on a laptop with an Intel Core Ultra 5 225H processor (Windows 11). Runtimes vary with the hardware, mainly for the machine-learning parts. Running the package on the synthetic data in the default without 04_fores_seeds.R takes about 5 minutes. 

**Reproducibility:** all random steps use fixed seeds: 123 in 03_targeting.R, and 50 seeds in 04_forest_seeds.R (123, which reproduces the results of 03_targeting.R, and 1–49). Machine-learning results can differ slightly with other package versions; use `renv::restore()` to get the same versions. The software versions of each run are saved in `output/sessionInfo.txt`.

**Machine learning:** in section F of `03_targeting.R`, the characteristics used by the causal forests are pre-selected within the training half of the sample split only (`forest_selection = "split"`), so the held-out half used to test for heterogeneity has not influenced the model. The alternative `"pilot"` (selection with all respondents) overstates the evidence for heterogeneity and is kept for comparison only; `04_forest_seeds.R` repeats the analysis for 50 seeds with either option.

**Running single scripts:** load the data with source("R/load_data.R"); by default, this loads the synthetic dataset. Then set the dataset in the console, e.g. label <- "DE" ("DE", "NO", "NL" or "ALL" for the pooled data), and run 02_analysis.R, followed by 03_targeting.R and, optionally, 04_forest_seeds.R.  

---

## Results

The folder `results/` contains the results of the analysis based on the full survey data, as reported in deliverables D5.2 and D5.3. When the code is run, it writes the same files to `output/`; with the synthetic data, these show what the code produces, but the values are meaningless and differ from those in `results/`.

File names start with the dataset: `DE_`, `NO_`, `NL_`, or `ALL_` for the pooled data. The main files are:

| File (prefix = dataset) | Content |
|---|---|
| `*_treatment_effects_indices.docx` | Main result: treatment effects on the support index and the composite preference index |
| `*_treatment_effects_support.docx`, `*_treatment_effects_petition_*.docx` | Treatment effects on single outcomes |
| `*_treatment_effects_BH.csv` | p-values corrected for multiple testing (Benjamini-Hochberg) |
| `*_robustness_*.docx` | Ordered logit models, models with covariates |
| `*_support_*.png`, `*_petition.png`, `*_benefits_Q19.png`, ... | Descriptive figures |
| `ALL_cty_*.png` | Cross-country comparisons |
| `*_targeting_*.png`, `*_heterogeneity_*.csv/.docx` | Subgroup analyses and interaction tests |
| `*_forest_*.png/.csv` | Machine-learning results (random forest, causal forests, policy tree) |
| `seeds/` | Robustness of the forest results to the random seed |
| `app/` | Regional data and suggestions for the map application |
| `sessionInfo.txt` | R and package versions used for the run |

## Using the code with other data

The analysis can be applied to other surveys with a similar design (randomised information treatments, support for a technology on a rating scale, petition questions). Create an analysis dataset in the same format (a list saved with `saveRDS()`, see section 8 of `01_cleaning.R`) and point `analysis_file` in `00_run_all.R` to it. The synthetic dataset (`data/analysis_data_synthetic.rds`) shows the required structure. The most important variables of `df_all`:

| Variable | Content |
|---|---|
| `country` | country (factor) |
| `uuid` | respondent id |
| `treat` | treatment group (factor; `"1"` = control, `"2"`–`"6"` = treatments) |
| `support_gen`, `support_biomass`, `support_wte`, `support_util`, `support_pipe`, `support_stor` | support for CCUS in general and five applications (1 = strongly oppose … 7 = strongly support; don't know = NA) |
| `petition_for_*`, `petition_against_*`, `petition_undecided_*`, `petition_for_incl_undec_*` | petition signing per application (TRUE/FALSE) |
| `Q19r1_num`–`Q19r6_num`, `Q27r1_num`–`Q27r5_num` | answer codes of expected consequences and petitions |
| `age_group`, `Gender_new`, `Q7_f`, `Q11_f`, `Q14_f`, `Q34_f`, `party_bloc`, `income`, `region` | characteristics used for subgroups |
| `Q16r*`, `Q17r*`, `Q20r*`, `Q22`–`Q24`, `Q31NEW`–`Q36` | further questions used in the descriptive analysis |

All variables are described in `data/codebook.xlsx`, with question text, categories and number of missing values; variable names follow the question numbers of the STORCITO questionnaire (e.g. `Q16r3` = question 16, answer option 3). Country names (DE, NO, NL), the map of Germany and the regional variables in `05_app_data.R` are specific to this survey and need adjustment for other countries.

---

## Data protection and ethics

The original analysis uses pseudonymised data without direct identifiers. All published results are aggregated; subgroup results are only shown for groups of at least 30 respondents, and subgroup treatment effects only where each treatment and control group contains at least 10 respondents. The machine-learning methods are used to estimate group-level effects only; no predictions or decisions about individuals are published or stored. See the ethics section of the report for details. 

---
## Responsible use

The machine-learning methods in this package (random forests, causal forests and policy trees) were developed and used for scientific research: to assess whether information interventions on local co-benefits could be targeted at specific groups, not to select or address individuals. Users applying the package to other data are asked to observe the following:

- **Group-level use only.** The forests produce predictions for individual respondents. These are intermediate results for estimating and testing group-level patterns and should not be stored, published or used to make decisions about individuals.
- **No targeting without robust evidence.** A targeting rule should only be considered if it performs clearly and robustly better than providing the same information to everyone, tested on data not used to learn it and across several random seeds (see `04_forest_seeds.R`).
- **Acceptable characteristics.** A targeting rule that is considered for implementation should only be based on characteristics that can ethically justify treating people differently. Sensitive or protected characteristics, such as political orientation or gender, should be excluded (set `restrict_targeting <- TRUE` in section F5 of `03_targeting.R`).
- **Balanced and complete information.** Targeting should only concern *additional* information. Essential information, including on risks and costs, must remain available to everyone, and targeting must not be used to persuade people by withholding relevant information.
- **Legal requirements.** Using a targeting rule in practice, beyond research, may fall under Regulation (EU) 2024/1689 (AI Act), including its prohibition of manipulative techniques (Article 5), and must comply with data protection law (GDPR).
- **Data protection.** When applying the package to other survey data, process personal data in accordance with the GDPR, report only aggregated results, and respect minimum group sizes.
- **Careful interpretation.** Variable importance shows which characteristics a forest uses, not whether they increase or decrease an outcome, and not a causal relationship. Differences between subgroups may be due to chance; rely on the tests corrected for multiple testing and on the robustness checks.

## Licence

Code: MIT licence (see `LICENSE`). Lookup tables in `data/lookup/` are derived from public data of Statistics Netherlands (CBS).

## Contact

Nicola Deller
Ruhr-University, Bochum, Germany
nicola.deller@rub.de
