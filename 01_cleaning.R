################################################################################
## 01_cleaning.R
## Stage 1: creates two analysis datasets from the raw survey files.
##   - reads the SPSS files of DE, NO and NL (data/raw/, not public)
##   - keeps completed interviews and drops the soft-launch interviews
##   - recodes outcomes, treatment and moderators, harmonizes the countries
##   - joins the countries into one dataset and saves it as data/analysis_data.rds
##   - creates a synthetic dataset with the same structure (data/analysis_data_synthetic.rds),
##     so that the code can be run without access to the survey data. This is the dataset made public
##   - creates a codebook with the definition of the variables 
## All analyses (02-05) start from data/analysis_data.rds (or the synthetic dataset).
## This script documents how those files were created; 
## it needs the raw data and is run once by the authors.
################################################################################

library(haven)
library(dplyr)

# ---- 1. Country-specific settings --------------------------------------------
# path:          location of the SPSS file (folder data/raw/)
# soft_launch_n: number of earliest observations (soft launch) to drop
#                -> check this number separately for each country!
# income_var:    name of the country-specific income variable
settings <- list(
  DE = list(path = file.path("data", "raw", "survey_DE.sav"), # German survey data
            soft_launch_n = 54, income_var = "Q5_DE"),
  NO = list(path = file.path("data", "raw", "survey_NO.sav"), # Norwegian survey data
            soft_launch_n = 36, income_var = "Q5_NO"),
  NL = list(path = file.path("data", "raw", "survey_NL.sav"),  # Dutch survey data
            soft_launch_n = 56, income_var = "Q5_NL")
)

raw <- lapply(settings, function(s) {
  if (!file.exists(s$path)) stop("Raw data not found: ", s$path,
                                 "\nThe raw survey data are not public; to run the analyses, use ",
                                 "data/analysis_data_synthetic.rds (see README.md).", call. = FALSE)
  read_sav(s$path)
})


# ---- 2. Check that the coding is identical across countries -------------------
# The cleaning below relies on code positions:
#   "Don't know" = 8 in Q18/Q26, "Don't know/Undecided" = 4 in Q27,
#   1 = strongly support / 7 = strongly oppose in Q26, 1 = control in assign_S3,
#   "Higher education" in the labels of Q7.
# Compare the output for all three countries before trusting the results.
show_codes <- function(x) {
  if (is.labelled(x)) print_labels(x) else print(table(x, useNA = "ifany"))
}

for (v in c("Q18", "Q26A", "Q27r1", "Q19r1", "Q7", "assign_S3")) {
  for (cn in names(raw)) {
    cat("\n=====", v, "-", cn, "=====\n")
    show_codes(raw[[cn]][[v]])
  }
}


# ---- 3. Cleaning function (identical for every country) -----------------------

# support levels: only valid answers 1-7 are kept ("Don't know" (8) and any other
# code -> NA), reversed so that 7 = strong support
recode_support <- function(x) {
  x <- as.numeric(x)
  ifelse(x %in% 1:7, 8 - x, NA)
}

# moderators: categories without "Don't know" and "Prefer not to say"
to_mod <- function(x) {
  x <- as_factor(x)
  x[grepl("don.t know|prefer not", x, ignore.case = TRUE)] <- NA # set don't know to missing for analysis
  droplevels(x)
}
clean_country <- function(df, country, soft_launch_n, income_var) {

  # keep completed interviews (status 3) and drop soft-launch observations
  df <- subset(df, status == 3)
  df <- df[order(df$date), ]
  if (soft_launch_n > 0) df <- df[-(1:soft_launch_n), ]

  df$country <- country

  # create harmonized income variable (income categories are country-specific)
  df$income <- if (income_var %in% names(df)) df[[income_var]] else NA

  # create variables for support levels (Q18, Q26) based on recoding above
  df$support_gen     <- recode_support(df$Q18)
  df$support_biomass <- recode_support(df$Q26A)
  df$support_wte     <- recode_support(df$Q26B)
  df$support_util    <- recode_support(df$Q26C)
  df$support_pipe    <- recode_support(df$Q26D)
  df$support_stor    <- recode_support(df$Q26E)

  # create binary variables for petition signing (Q27); "Don't know/Undecided" (4) -> NA
  # + quality check for inconsistent answers: support but sign against, or oppose but sign for
  apps       <- c("biomass", "wte", "util", "pipe", "stor")
  petition_q <- c("Q27r1", "Q27r2", "Q27r3", "Q27r4", "Q27r5")
  support_q  <- c("Q26A", "Q26B", "Q26C", "Q26D", "Q26E")

  for (i in seq_along(apps)) {
    p <- as.numeric(df[[petition_q[i]]])
    s <- as.numeric(df[[support_q[i]]])
    df[[paste0("petition_for_", apps[i])]]     <- ifelse(p == 4, NA, p == 1)
    df[[paste0("petition_against_", apps[i])]] <- ifelse(p == 4, NA, p == 2)
    df[[paste0("petition_none_", apps[i])]]    <- ifelse(p == 4, NA, p == 3)
    # robustness: keep undecided respondents (undecided = 1 / = not signing for)
    df[[paste0("petition_undecided_", apps[i])]]      <- p == 4
    df[[paste0("petition_for_incl_undec_", apps[i])]] <- p == 1
    df[[paste0("contra_", apps[i])]] <- (s %in% 1:2 & p %in% 2) | (s %in% 6:7 & p %in% 1)
  }
  df$n_contradictions <- rowSums(df[paste0("contra_", apps)])

  # treatment groups: 1 = control, 2-6 = information treatments
  df$treat <- relevel(factor(as.numeric(df$assign_S3)), ref = "1")

  # moderators: new variables without Don't know answers for analysis. 
  for (m in c("Q11", "Q7", "Q14", "Q34")) df[[paste0(m, "_f")]] <- to_mod(df[[m]])
  
  # political orientation: parties grouped into blocs per country
  # (populist-right parties = "Right"; the mainstream centre-right is included in "Centre")
  
  party_blocs <- list(
    DE = list(Left   = c("SPD", "Bündnis 90/Die Grünen", "Die Linke"),
              Centre = c("CDU/CSU"),
              Right  = c( "AfD")),
    NO = list(Left   = c("Arbeiderpartiet","Sosialistisk Venstreparti"),
              Centre = c("Senterpartiet", "Høyre"),
              Right  = c("Fremskrittspartiet")),
    NL = list(Left   = c("GroenLinks-PvdA"),
              Centre = c("D66", "VVD", "CDA"),
              Right  = c("PVV"))
  )
  # assign no party preference indication to no_party variable
  no_party <- "I am not eligible to vote|Jeg er ikke stemmeberettiget|Ik heb geen stemrecht|Prefer not to say|Foretrekker å ikke si|Geen antwoord"
  
  # create political orientation variable party_bloc
  party_bloc <- function(x, blocs) {
    lab <- as.character(as_factor(x))
    out <- rep("Other", length(lab))
    for (b in rev(names(blocs))) {
      pattern <- paste0("\\b(", paste(blocs[[b]], collapse = "|"), ")")
      out[grepl(pattern, lab)] <- b
    }
    out[is.na(lab) | grepl(no_party, lab, ignore.case = TRUE)] <- NA
    factor(out, levels = c("Left", "Centre", "Right", "Other"))
  }
  # parties: keep the country in the label, so parties of different countries
  # stay separate categories in the pooled data (e.g. "NO: Høyre")
  party_lab <- as.character(as_factor(df$Q32))
  df$Q32_f  <- factor(ifelse(is.na(party_lab), NA, paste0(country, ": ", party_lab)))
  
  # parties grouped into Left / Centre / Right / Other ("Other" = other parties;
  # not eligible / prefer not to say -> NA)
  df$party_bloc <- party_bloc(df$Q32, party_blocs[[country]])
  df$party_bloc <- droplevels(df$party_bloc)
  
  df$Q7_f <- factor(ifelse(is.na(df$Q7_f), NA,
                           ifelse(grepl("Higher education", df$Q7_f),
                                  "University degree", "No university degree")),
                    levels = c("No university degree", "University degree"))

  # numeric answer codes, kept before the conversion to factors below
  # (as.numeric() on a factor gives the level position, not the SPSS code)
  for (v in c(paste0("Q27r", 1:5), paste0("Q19r", 1:6))) {
    df[[paste0(v, "_num")]] <- as.numeric(df[[v]])
  }

  # SPSS labels -> factors (labels instead of codes)
  as_factor(df)
}


# ---- 4. Clean each country ---------------------------------------------------
df_de <- clean_country(raw$DE, "DE", settings$DE$soft_launch_n, settings$DE$income_var)
df_no <- clean_country(raw$NO, "NO", settings$NO$soft_launch_n, settings$NO$income_var)
df_nl <- clean_country(raw$NL, "NL", settings$NL$soft_launch_n, settings$NL$income_var)


# ---- 5. Pooled dataset -------------------------------------------------------
# variables with different types across countries would break bind_rows(): check for variables with different types
janitor::compare_df_cols(df_de, df_no, df_nl, return = "mismatch")

# translate gender labels into English (the SPSS files use the German labels in all countries)
fix_gender <- function(d) {
  gender_map <- c("Männlich" = "Male", "Weiblich" = "Female", "Ich identifiziere mich als" = "Other")
  g <- as.character(d$Gender_new)
  g[g %in% names(gender_map)] <- gender_map[g[g %in% names(gender_map)]]
  d$Gender_new <- factor(g, levels = intersect(c("Male", "Female", "Other"), unique(g)))
  d
}
df_de <- fix_gender(df_de)
df_no <- fix_gender(df_no)
df_nl <- fix_gender(df_nl)

# create pooled dataset from individual, cleaned datasets
df_all <- bind_rows(df_de, df_no, df_nl)
df_all$country <- factor(df_all$country, levels = c("DE", "NO", "NL"))
df_all$treat   <- relevel(factor(df_all$treat), ref = "1")


# ---- 6. Checks ---------------------------------------------------------------
table(df_all$country)                  # respondents per country
table(df_all$country, df_all$treat)    # treatment groups per country
sum(duplicated(df_all$uuid))           # should be 0


# ---- 7. Additional information kept for the analysis ---------------------------

# numeric SPSS codes of ordered scales, used by the alternative coding of the
# characteristics for the forests (R/forest_features.R). 
num_codes <- function(x, valid) {
  x <- as.numeric(zap_labels(x))
  ifelse(x %in% valid, x, NA_real_)
}
rev_codes <- function(x, valid) max(valid) + min(valid) - num_codes(x, valid)
num_text  <- function(r, v) {
  if (!v %in% names(r)) return(NA_real_)
  suppressWarnings(as.numeric(as.character(r[[v]])))
}
# create table with alternative coding of variables as numbers instead of texts, 
# so forests can treat them as ordered. For robustness checks in 04_fores_seeds.R
forest_codes <- bind_rows(lapply(names(settings), function(cn) {
  r   <- raw[[cn]]
  inc <- settings[[cn]]$income_var
  tibble(
    uuid         = as.character(r$uuid),
    age_years    = as.numeric(r$age),
    Q2_num       = num_codes(r$Q2, 1:5),      # 6 = don't know
    Q6_num       = as.numeric(zap_labels(r$Q6)),
    Q7_num       = num_codes(r$Q7, 1:4),      # 99 = prefer not to say
    Q11_num      = num_codes(r$Q11, 1:3),
    Q14_num      = rev_codes(r$Q14, 1:5),
    Q34_num      = rev_codes(r$Q34, 1:5),
    Q37_num      = num_codes(r$Q37, 0:10),
    Q38_num      = num_codes(r$Q38, 0:10),
    Q39_num      = rev_codes(r$Q39, 1:5),
    tries_Q12    = num_text(r, "Q12Q13_tryCountr1"),
    tries_Q13    = num_text(r, "Q12Q13_tryCountr2"),
    income_class = if (inc %in% names(r)) num_codes(r[[inc]], 1:4) else NA_real_   # 5 = prefer not to say
  )
})) %>% filter(uuid %in% as.character(df_all$uuid))

# question texts (SPSS variable labels) and the order of the questions in the survey
spss_labels    <- unlist(lapply(raw$DE, function(x) { l <- attr(x, "label"); if (is.null(l)) NA_character_ else l }))
question_order <- names(raw$DE)

# sample flow, table showing how the sample was reduced during cleaning: raw -> completed -> after soft launch
sample_flow <- bind_rows(lapply(names(settings), function(cn) {
  r <- raw[[cn]]
  tibble(country = cn, raw = nrow(r), completed = sum(r$status == 3, na.rm = TRUE),
         soft_launch_dropped = settings[[cn]]$soft_launch_n,
         analysed = sum(df_all$country == cn))
}))
print(sample_flow)


# ---- 8. Save the analysis dataset ------------------------------------------------
# One pooled dataset. The country analyses use the rows of one country; to keep
# them identical to the separate country datasets above, the factor levels of each
# country are stored as well (R/load_data.R restores them).
df_list <- list(DE = df_de, NO = df_no, NL = df_nl)
analysis_data <- list(
  df_all         = df_all,
  factor_levels  = lapply(df_list, function(d) lapply(Filter(is.factor, d), levels)),
  character_cols = lapply(df_list, function(d) names(Filter(is.character, d))),
  forest_codes   = forest_codes,
  spss_labels    = spss_labels,
  question_order = question_order,
  sample_flow    = sample_flow,
  created        = Sys.time()
)
dir.create("data", showWarnings = FALSE)
saveRDS(analysis_data, file.path("data", "analysis_data.rds"))
cat("Saved data/analysis_data.rds:", nrow(df_all), "respondents\n")


# ---- 9. Synthetic dataset (for users without access to the survey data) ----------
# Creates data/analysis_data_synthetic.rds with exactly the same structure as the
# analysis dataset (variables, categories, countries, respondents per country), so
# that 02-05 run on it (set analysis_file in 00_run_all.R).
# Within each country, every variable is resampled independently from its own
# distribution. Each column therefore has realistic values and missing-value patterns,
# but the rows are random combinations: no synthetic respondent corresponds to a real
# one, and relationships between variables (e.g. treatment effects) are not preserved.
# Results based on the synthetic data are therefore meaningless; the data only show
# that the code runs and what it produces.
# Additional cautionary measure: identifiers, time stamps, survey metadata and free text are
# removed, respondent ids are replaced by random numbers, and postcodes are cut to
# four digits before resampling.
# Also creates a codebook to see the questions and 

make_synthetic <- TRUE

if (make_synthetic) {
  set.seed(2026)
  syn <- df_all
  
  # ---- 9a. Keep only the variables used by the analysis scripts (whitelist) --------
  # Identifiers, time stamps, survey metadata, free text and variables not used in
  # the analysis are not included.
  keep_patterns <- c(
    # design, outcomes and derived variables
    "^country$", "^treat$", "^uuid$", "^support_", "^petition_", "^contra_", "^n_contradictions$",
    # background characteristics and regions (regions for the maps and 05_app_data.R)
    "^age_group$", "^Gender_new$", "^income$", "^Q5_(DE|NO|NL)$", "^party_bloc$",
    "^region$", "^landsdel2024$", "^zipcode$",
    "^Q2$", "^Q6$", "^Q7$", "^Q8$", "^Q9$", "^Q10$", "^Q11$",
    "^Q12$", "^Q13$", "^Q12Q13_tryCountr[12]$",
    "^Q7_f$", "^Q11_f$", "^Q14_f$", "^Q34_f$", "^Q32$", "^Q32_f$",
    # questions after the treatment used in 02, 03 and 05
    "^Q14$", "^Q15$", "^Q16r([0-9]+)$", "^Q17r([0-9]+)$", "^Q18$", "^Q19r[0-9]+(_num)?$",
    "^Q20r[0-9]+$", "^Q21$", "^Q22$", "^Q23r[0-9]+$", "^Q24r[12]$", "^noanswerQ24_r99$",
    "^Q26[A-E]$", "^Q27r[0-9](_num)?$", "^Q31NEW$", "^Q33$", "^Q34$", "^Q36$",
    "^Q37$", "^Q38$", "^Q39$")
  keep <- names(syn)[grepl(paste(keep_patterns, collapse = "|"), names(syn))]
  drop <- setdiff(names(syn), keep)
  syn  <- syn[, keep]
  
  # postcodes shortened to four digits
  if ("zipcode" %in% names(syn))
    syn$zipcode <- substr(gsub("\\s", "", as.character(syn$zipcode)), 1, 4)
  cat("Synthetic data:", ncol(syn), "variables kept,", length(drop), "dropped\n")
  
  # rare values of personal characteristics (fewer than 5 respondents per country) -> missing
  personal_vars <- intersect(c("Gender_new", "region", "Q7", "Q9", "Q10", "Q32"), names(syn))
  for (v in personal_vars) {
    x <- as.character(syn[[v]])
    n <- ave(rep(1, length(x)), paste(syn$country, x), FUN = length)
    rare <- !is.na(x) & n < 5
    if (any(rare)) {
      syn[[v]][rare] <- NA
      if (is.factor(syn[[v]])) syn[[v]] <- droplevels(syn[[v]])
      cat("  rare values of", v, "set to missing:", sum(rare), "\n")
    }
  }
  
  # ---- Codebook (variables of the synthetic dataset, categories from the survey data) --
  codebook <- data.frame(
    variable   = names(syn),
    question   = unname(sapply(names(syn), function(v)
      if (v %in% names(spss_labels) && !is.na(spss_labels[[v]])) spss_labels[[v]] else "")),
    type       = unname(sapply(syn, function(x) class(x)[1])),
    categories = unname(sapply(syn, function(x)
      if (is.factor(x)) paste(levels(droplevels(x)), collapse = "; ")
      else if (is.logical(x)) "TRUE; FALSE"
      else if (is.numeric(x)) paste0("range ", paste(range(x, na.rm = TRUE), collapse = " to "))
      else "text")),
    missing    = unname(sapply(syn, function(x) sum(is.na(x))))
  )
  codebook$question[codebook$variable == "Gender_new"] <- "Gender"
  codebook$question[codebook$variable == "uuid"] <- "Respondent id (random number in the synthetic dataset)"
  
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "codebook")
  openxlsx::writeData(wb, "codebook", codebook,
                      headerStyle = openxlsx::createStyle(textDecoration = "bold", fgFill = "#E3E1EC",
                                                          border = "bottom"))
  openxlsx::setColWidths(wb, "codebook", cols = 1:5, widths = c(25, 60, 12, 60, 10))
  openxlsx::addStyle(wb, "codebook", openxlsx::createStyle(wrapText = TRUE, valign = "top"),
                     rows = 2:(nrow(codebook) + 1), cols = 1:5, gridExpand = TRUE)
  openxlsx::freezePane(wb, "codebook", firstRow = TRUE)
  openxlsx::addFilter(wb, "codebook", rows = 1, cols = 1:5)
  openxlsx::saveWorkbook(wb, file.path("data", "codebook.xlsx"), overwrite = TRUE)
  cat("Saved data/codebook.xlsx\n")
  
  # ---- 9b. Resample every variable independently within each country -----------------
  rows_by_country <- split(seq_len(nrow(syn)), syn$country)
  row_order       <- unlist(rows_by_country, use.names = FALSE)
  resample_within <- function(x) {
    i <- unlist(lapply(rows_by_country, function(ix) ix[sample.int(length(ix), length(ix), replace = TRUE)]),
                use.names = FALSE)
    y <- x[i]
    mostattributes(y) <- attributes(x)
    y
  }
  out <- syn[row_order, , drop = FALSE]
  for (v in setdiff(names(syn), "country")) out[[v]] <- resample_within(syn[[v]])
  
  # ---- 9c. New respondent ids ---------------------------------------------------------
  orig_ids <- as.character(syn$uuid[row_order])     # ids of the rows in country order
  out$uuid <- sprintf("S%05d", seq_len(nrow(out)))
  rownames(out) <- NULL
  
  # numeric codes for the alternative forest coding: resampled the same way
  fc <- forest_codes[match(orig_ids, forest_codes$uuid), ]
  fc_syn <- tibble(uuid = out$uuid)
  for (v in setdiff(names(fc), "uuid")) fc_syn[[v]] <- resample_within(fc[[v]][order(row_order)])
  
  # ---- 9d. Save in the same structure as the analysis dataset ---------------------------
  analysis_data_synthetic <- list(
    df_all         = out,
    factor_levels  = lapply(analysis_data$factor_levels, function(l) l[intersect(names(l), names(out))]),
    character_cols = lapply(analysis_data$character_cols, intersect, names(out)),
    forest_codes   = fc_syn,
    spss_labels    = spss_labels,
    question_order = question_order,
    sample_flow    = sample_flow,
    created        = Sys.time(),
    note           = paste("Synthetic data: variables resampled independently within country.",
                           "Relationships between variables are not preserved; results are meaningless.")
  )
  saveRDS(analysis_data_synthetic, file.path("data", "analysis_data_synthetic.rds"))
  cat("Saved data/analysis_data_synthetic.rds:", nrow(out), "synthetic respondents\n")
}