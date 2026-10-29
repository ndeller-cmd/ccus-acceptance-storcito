################################################################################
## R/load_data.R
## Stage 2 starts here: loads the analysis dataset and creates
##   df_all                 pooled data (all countries)
##   df_de, df_no, df_nl    the rows of one country each
##   forest_codes           numeric SPSS codes (R/forest_features.R)
##   spss_labels            question texts (fallback for short variable names)
## Called by 00_run_all.R, and by 02_analysis.R / 05_app_data.R when they are run
## on their own. Which file is loaded is set by `analysis_file` (default: the
## synthetic dataset; the full survey data are only available to the authors).
################################################################################

library(dplyr)
if (!exists("analysis_file")) analysis_file <- file.path("data", "analysis_data_synthetic.rds") #switch this 
# to "analysis_data.rds" for using the full dataset (not public)
if (!file.exists(analysis_file)) {
  stop("Analysis dataset not found: ", analysis_file,
       "\nThe full survey data (data/analysis_data.rds) are not public; use ",
       "data/analysis_data_synthetic.rds to run the code (see README.md).", call. = FALSE)
}

analysis_data  <- readRDS(analysis_file)
df_all         <- analysis_data$df_all
forest_codes   <- analysis_data$forest_codes
spss_labels    <- analysis_data$spss_labels
question_order <- analysis_data$question_order

# rows of one country, with the factor levels of the original country dataset
# (so that tables, charts and models are identical to a separate country file)
country_subset <- function(cn) {
  d  <- df_all[df_all$country == cn, ]
  lv <- analysis_data$factor_levels[[cn]]
  for (v in names(lv)) d[[v]] <- factor(as.character(d[[v]]), levels = lv[[v]])
  for (v in analysis_data$character_cols[[cn]]) d[[v]] <- as.character(d[[v]])
  d
}
for (cn in levels(df_all$country)) assign(paste0("df_", tolower(cn)), country_subset(cn))

cat("Loaded", analysis_file, "-", nrow(df_all), "respondents (",
    paste(levels(df_all$country), table(df_all$country), sep = ": ", collapse = ", "), ")\n")
