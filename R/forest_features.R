################################################################################
## R/forest_features.R
## Alternative preparation of the characteristics for the forests.
##
## 03_targeting.R (section F0) turns every SPSS variable into 0/1 dummies, because
## 01_cleaning.R converts all labelled variables to factors (as_factor). Trees split
## at thresholds ("risk <= 5"), so ordered answers work better as numbers: only the
## order matters, not the distance between the steps. Here:
##   - ordered scales       -> numeric code; "don't know" / "prefer not to say" -> NA
##                             (grf sends missing values to the better side of a split)
##   - Q14, Q34, Q39        -> reversed, so that higher = more knowledge / concern / trust
##   - age                  -> years instead of 4 age groups
##   - Q12/Q13 attempts     -> number of attempts instead of text
##   - income               -> class 1-4. The brackets are country-specific, so in the
##                             pooled data this is the RELATIVE income class within
##                             the country (03 leaves income out of the pooled data).
## Unordered variables (employment, sector, information sources, gender, party,
## country, region) stay dummies, built exactly as in 03.
##
## Used by 04_forest_seeds.R (features <- "recoded"). Needs forest_codes (R/load_data.R) and
## fd, cand, min_share (section F0 of 03_targeting.R); overwrites X_all and col_var.
################################################################################

# ---- Numeric versions of the ordered scales -----------------------------------
# created from the raw SPSS codes in 01_cleaning.R (section 7) and stored in the
# analysis dataset (loaded as forest_codes by R/load_data.R)
stopifnot(exists("forest_codes"))
codes <- forest_codes

var_labels <- c(var_labels,
                age_years    = "Age (years)",
                # only codes 1-4 occur, about equally often in every country: the SPSS labels
                # ("less than 2,000" ... "20,000 or more") do not seem to match the questionnaire
                Q2_num       = "Town size (code 1-4)",
                Q6_num       = "Household size",
                Q7_num       = "Education (1 = none ... 4 = university)",
                Q11_num      = "Area type (1 = rural ... 3 = urban)",
                Q14_num      = "Prior knowledge of CCUS (1 = none ... 5 = very well)",
                Q34_num      = "Climate concern (1 = low ... 5 = high)",
                Q37_num      = "Risk-taking (0-10)",
                Q38_num      = "Patience (0-10)",
                Q39_num      = "Generalised trust (1 = low ... 5 = high)",
                tries_Q12    = "Comprehension attempts (1st question)",
                tries_Q13    = "Comprehension attempts (2nd question)",
                income_class = "Income class within country (1-4)"
)

# variables of 03 that are replaced by the numeric versions above
replaced <- c("Q2", "Q6", "Q7", "Q11", "Q14", "Q34", "Q37", "Q38", "Q39",
              "Q12Q13_tryCountr1", "Q12Q13_tryCountr2", "age_group", "income",
              grep("^Q5_", cand, value = TRUE))
cand_cat <- setdiff(cand, replaced)


# ---- Numeric block (same rows and order as fd) --------------------------------

fx <- tibble(uuid = as.character(fd$uuid)) %>% left_join(codes, by = "uuid")
stopifnot(nrow(fx) == nrow(fd), !anyDuplicated(codes$uuid))

num_vars <- setdiff(names(codes), "uuid")
num_vars <- num_vars[sapply(num_vars, function(v) {        # same rules as in 03
  x <- fx[[v]]
  mean(is.na(x)) < 0.5 && length(unique(na.omit(x))) >= 2
})]
X_num <- as.matrix(fx[num_vars])
colnames(X_num) <- nice_name(num_vars)


# ---- Dummy block for the unordered variables (as in 03) ----------------------

mf    <- model.frame(~ ., data = fd[cand_cat], na.action = na.pass)
X_cat <- model.matrix(~ . - 1, mf)
col_var_cat <- gsub("`", "", attr(terms(mf), "term.labels"))[attr(X_cat, "assign")]

share  <- colMeans(X_cat, na.rm = TRUE)
binary <- apply(X_cat, 2, function(z) all(z %in% c(0, 1, NA)))
keep   <- !binary | (share >= min_share & share <= 1 - min_share)
X_cat  <- X_cat[, keep, drop = FALSE]
col_var_cat <- col_var_cat[keep]
col_lev <- substring(colnames(X_cat), nchar(col_var_cat) + 1)
colnames(X_cat) <- ifelse(col_lev == "", nice_name(col_var_cat),
                          paste0(nice_name(col_var_cat), ": ", col_lev))


# ---- Combine -----------------------------------------------------------------

X_all   <- cbind(X_num, X_cat)
colnames(X_all) <- make.unique(colnames(X_all))
col_var <- c(num_vars, col_var_cat)

cat("Recoded characteristics:", length(num_vars), "numeric +", ncol(X_cat), "dummy columns =",
    ncol(X_all), "columns (03: dummies only)\n")
cat("Numeric:", paste(nice_name(num_vars), collapse = "; "), "\n")
