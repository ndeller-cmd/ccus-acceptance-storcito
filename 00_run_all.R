################################################################################
## 00_run_all.R
## Runs the complete analysis. Run it from the project folder (the folder that
## contains this file), e.g. by opening the RStudio project, or:
##   setwd("path/to/this/folder"); source("00_run_all.R")
##
## Two stages:
##   1. 01_cleaning.R creates data/analysis_data.rds from the raw survey files.
##      The raw files are not public, so this step is only run by the authors.
##   2. All analyses (02-05) start from one analysis dataset: the full dataset
##      (authors only) or the synthetic dataset (public; results are meaningless).
## 
## Results are written to output/. The results based on the full survey data, as
## reported in the deliverables, are included in results/.
################################################################################

# ---- Settings ------------------------------------------------------------------
# analysis_file <- file.path("data", "analysis_data.rds")          # full data (authors only)
analysis_file <- file.path("data", "analysis_data_synthetic.rds")  # synthetic data (public)

# switch running the individual R files on/off
run_cleaning     <- FALSE                      # stage 1; needs the raw data in data/raw/; only run by authors
datasets         <- c("DE", "NO", "NL", "ALL")   # datasets analysed by 02-04
run_targeting    <- TRUE                       # 03_targeting.R (heterogeneity, machine learning)
run_forest_seeds <- FALSE                     # 04_forest_seeds.R (slow) robustness of results to the seed
run_app_data     <- FALSE                      # 05_app_data.R (regional data for the map app); 
                                               # only included for documentation only, results are provided.


# ---- Packages ------------------------------------------------------------------
required_packages <- c(
  "haven", "dplyr", "tidyr", "forcats", "janitor", "ggplot2", "ggrepel", "scales",
  "gtsummary", "estimatr", "broom", "modelsummary", "flextable", "officer", "car",
  "ordinal", "marginaleffects", "sf", "giscoR", "plotly", "htmlwidgets",
  "grf", "policytree", "DiagrammeR", "jsonlite", "openxlsx"
)
missing <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop("Please install the missing packages first:\n  install.packages(c(\"",
       paste(missing, collapse = "\", \""), "\"))", call. = FALSE)
}

dir.create("output", showWarnings = FALSE)
t_start <- Sys.time()


# ---- Stage 1: analysis dataset from the raw data (authors only) ------------------
if (run_cleaning) source("01_cleaning.R", encoding = "UTF-8")


# ---- Stage 2: analyses -------------------------------------------------------------
source(file.path("R", "load_data.R"), encoding = "UTF-8")

# 02 and 03 run once per dataset (country subsets or pooled); 03 and 04 use objects of 02
for (label in datasets) {
  message("\n========== Dataset: ", label, " ==========")
  source("02_analysis.R", encoding = "UTF-8") # descriptive results and average treatment effects of information experiment of T5.3/T5.4
  if (run_targeting)    source("03_targeting.R",    encoding = "UTF-8") # heterogeneity and targeting analyses of T5.4
  if (run_forest_seeds) source("04_forest_seeds.R", encoding = "UTF-8") # robustness of results in 03_targeting to random seed
}

if (run_app_data) source("05_app_data.R", encoding = "UTF-8") # generates results for app T5.6


# ---- Software versions -------------------------------------------------------------
writeLines(capture.output(sessionInfo()), file.path("output", "sessionInfo.txt"))
message("\nDone (", analysis_file, "). Runtime: ",
        round(difftime(Sys.time(), t_start, units = "mins"), 1), " min. Results are in output/.")
