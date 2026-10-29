################################################################################
## 02_analysis.R
## Descriptive analysis and treatment effects of the information treatments.
##
## Data: starts from the analysis dataset loaded by R/load_data.R (full or synthetic,
## set by analysis_file). 00_run_all.R runs this script once per dataset:
## "DE", "NO", "NL" (one country) and "ALL" (pooled data, with country fixed effects).
## When running it on its own, set label before running it (see section 0).
##
## Content:
## 0. Setup
##    - choice of the dataset (label) and general settings
##    - names for tables and plots
##    - outcome indices: support index and combined preference index
##    - checks of missing answers in the indices, income quartiles (income_q)
##    - helpers: output folder and file names, significance stars and
##      export of regression tables to Word
## 1. Sample characteristics (summary statistics)
## 2. Maps of respondents and support by federal state (Germany only)
## 3. Support for CCUS in general and by application
## 4. Willingness to sign a petition
## 5. Quality check: contradictory support and petition answers
## 6. Factors, beliefs and attitudes: important factors, expected consequences,
##    information sources, prior knowledge, budget allocation, perceived opinion of
##    fellow citizens, support by political orientation and by climate concern
## 7. Treatment effects of the information treatment (survey experiment)
##    7a. manipulation check (expected consequences) and treatment effects on support,
##        petition signing and the indices
##    7b. Treatment effects: OLS with robust standard errors
##    7c. correction for multiple testing (Benjamini-Hochberg)
##    7d. robustness: ordered logit, logit (average marginal effects), models with
##        covariates
## 8. Cross-country comparison (pooled data only): sample, support, petitions,
##    perceived vs. actual support, expected consequences, important factors,
##    budget allocation, treatment effects by country
## 9. Associations of climate concern and political orientation with support
##
## Output: tables (.docx, .csv) and figures (.png) in output/, file names starting
## with the dataset label (e.g. DE_support_overview.png). The results based on the
## full survey data are in results/.
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(gtsummary)
library(estimatr)
library(modelsummary)
library(sf)
library(giscoR)


################################################################################
## 0. Setup
################################################################################


# ---- Choose the dataset ------------------------------------------------------
# Set automatically by 00_run_all.R. When running this script on its own, set
# label before running it, e.g. label <- "DE" in the console:
#   "DE", "NO", "NL" = one country; "ALL" = pooled data (with country fixed effects).
# Which data file is used (full or synthetic) is set by analysis_file in 
# 00_run_all.R or in R/load_data.R when 02_analysis is run on its own
if (!exists("label")) label <- "ALL"   # "DE", "NO", "NL" or "ALL"
if (!exists("df_all")) source(file.path("R", "load_data.R"), encoding = "UTF-8")
df <- switch(label, DE = df_de, NO = df_no, NL = df_nl, ALL = df_all)
is_pooled <- label == "ALL"

# other settings within the dataset for later use in analysis: 

apps <- c("biomass", "wte", "util", "pipe", "stor") # sets the short names of the CCUS applications

# for the pooled data: include country fixed effects in all treatment regressions
rhs_fe <- if (is_pooled) " + country" else ""


single_support_charts <- FALSE # if TRUE: also creates six single support charts 
# in section 3. The combined chart is always made.


# ---- Set names used in all tables and plots ---------------------------------------

# treatment groups (randomly assigned; all respondents first read a general introduction to CCUS):
#   1 = control: no additional text
#   2 = jobs: creation of new local jobs through a CCUS project
#   3 = local economic growth: contribution of CCUS projects to local economic growth
#   4 = preservation of jobs and industries: CCUS can preserve local jobs and industries
#   5 = energy security: CCUS can improve local energy supply security
#   6 = climate change: role of CCUS in climate change mitigation and threats of climate change

treat_labels <- c("1" = "Control",
                  "2" = "Treatment Jobs",
                  "3" = "Treatment Local Economic Growth",
                  "4" = "Treatment Preservation of Jobs and Industries",
                  "5" = "Treatment Energy Security",
                  "6" = "Treatment Climate Change")
df$treat_label <- factor(treat_labels[as.character(df$treat)], levels = treat_labels)

# short descriptions of the variables
source_labels <- c("Newspaper", "Library/books", "Search engine", "Generative AI",
                   "News channels", "Local politicians", "Neighbours/friends/family")

var_labels <- c(
  # background characteristics
  age_group  = "Age group",
  Gender_new = "Gender",
  income     = "Household income",
  region     = "Federal state",
  country    = "Country",
  party_bloc = "Political orientation",
  Q6 = "Household size", Q9 = "Area of work", Q10 = "Area of work", 
  Q2  = "Town size",
  Q5_DE = "Household income", Q5_NO = "Household income", Q5_NL = "Household income",
  Q7  = "Education",               Q7_f  = "Education",
  Q8  = "Employment status",       
  Q11 = "Area type",               Q11_f = "Area type",
  Q12Q13_tryCountr1 = "Comprehension attempts (1st question)",
  Q12Q13_tryCountr2 = "Comprehension attempts (2nd question)",
  Q14 = "Prior knowledge of CCUS", Q14_f = "Prior knowledge of CCUS",
  Q15 = "Discussed CCUS with others",
  # information sources
  setNames(paste("Uses:", source_labels),   paste0("Q16r", 1:7)),
  Q16r97 = "Uses sources: none of these",          
  Q16r99 = "Uses sources: don't know",             
  setNames(paste("Trusts:", source_labels), paste0("Q17r", 1:7)),
  Q17r97 = "Trust in sources: none of these",       
  Q17r99 = "Trust in sources: don't know",           
  # attitudes and outcomes
  Q18 = "General support for CCUS",
  Q21 = "Public funds for CCUS",
  Q22 = "Importance of CCUS for climate protection",
  Q31NEW = "Climate activism",
  Q32 = "Party preference",        Q32_f = "Party preference",
  Q33 = "Climate change is human-made",
  Q34 = "Climate concern",         Q34_f = "Climate concern",
  Q36 = "Consequences for economy and jobs",
  Q37 = "Risk preferences", Q38 = "Patience", Q39 = "Trust in others",   
  Q19r6 = "CCUS reduces emissions (likelihood)",
  noanswerQ24_r99 = "No estimate of others' opinion"
)


# short name of one or more variables (falls back to the SPSS label, then the code)
nice_name <- function(v) {
  sapply(v, function(x) {
    if (!is.na(var_labels[x])) return(unname(var_labels[x]))
    l <- if (exists("spss_labels") && x %in% names(spss_labels) && !is.na(spss_labels[[x]])) spss_labels[[x]] else NULL
    if (!is.null(l)) return(ifelse(nchar(l) > 40, paste0(substr(l, 1, 37), "..."), l))
    x
  }, USE.NAMES = FALSE)
}

# named list of short names, for gtsummary tables
lab_list <- function(vars) setNames(as.list(nice_name(vars)), vars)

# readable regression terms, e.g. "treat2:Q7_fUniversity degree"
#   -> "Treatment Jobs × Education: University degree"
rename_part <- function(p) {
  if (p == "(Intercept)") return("Constant")
  if (p == "any_treat") return("Any treatment")
  if (grepl("^treat\\d$", p)) return(unname(treat_labels[sub("treat", "", p)]))
  vars <- names(var_labels)[!is.na(var_labels)]
  hit  <- vars[startsWith(p, vars)]
  if (length(hit) == 0) return(p)
  v   <- hit[which.max(nchar(hit))]              # longest match (Q7_f before Q7)
  lev <- substring(p, nchar(v) + 1)
  if (lev == "") unname(var_labels[v]) else paste0(var_labels[v], ": ", lev)
}
rename_terms <- function(x) {
  sapply(x, function(t) paste(sapply(strsplit(t, ":", fixed = TRUE)[[1]], rename_part),
                              collapse = " × "), USE.NAMES = FALSE)
}


# ---- Outcome indices ---------------------------------------------------------

# Create outcome indices from the number of individual outcomes

# 1. Support index: mean of the six support items (1-7), when at least 4 valid
# answers exist for this individual
support_items <- c("support_gen", "support_biomass", "support_wte",
                   "support_util", "support_pipe", "support_stor")

df$support_index <- ifelse(rowSums(!is.na(df[support_items])) >= 4,
                           rowMeans(df[support_items], na.rm = TRUE), NA)

# 2. Preference index: three equally weighted dimensions
# (general support, application-specific support, application-specific petition signing)

min_apps <- 3   # minimum valid answers out of the 5 applications, per dimension

app_support  <- paste0("support_", apps)
app_petition <- paste0("petition_net_", apps)

for (a in apps) {
  df[[paste0("petition_net_", a)]] <- as.numeric(df[[paste0("petition_for_", a)]]) -
    as.numeric(df[[paste0("petition_against_", a)]])
}

# scores per dimension (unweighted mean over the 5 applications)
score_general  <- df$support_gen
score_support  <- ifelse(rowSums(!is.na(df[app_support]))  >= min_apps,
                         rowMeans(df[app_support],  na.rm = TRUE), NA)
score_petition <- ifelse(rowSums(!is.na(df[app_petition])) >= min_apps,
                         rowMeans(df[app_petition], na.rm = TRUE), NA)

# standardize each score with the mean and SD of the control group
# (Kling, Liebman & Katz 2007), so effects are in control-group SDs
is_control <- df$treat == "1"
z <- function(x) (x - mean(x[is_control], na.rm = TRUE)) / sd(x[is_control], na.rm = TRUE)

# preference index: mean of the three standardized scores (all three required)
df$combined_index <- rowMeans(cbind(z(score_general), z(score_support), z(score_petition)))


# checks / drop rates for the indices
# count how many respondents were excluded because of the min valid answer restriction
# if they are many, consider imputing responses so incomplete opinions are not dropped
n_total <- nrow(df)

data.frame(
  step = c("All respondents",
           "Support index valid (>= 4 of 6 support items)",
           "General support valid",
           paste0("Application support valid (>= ", min_apps, " of 5)"),
           paste0("Petition score valid (>= ", min_apps, " of 5)"),
           "Combined index valid (all three scores)"),
  n = c(n_total,
        sum(!is.na(df$support_index)),
        sum(!is.na(score_general)),
        sum(!is.na(score_support)),
        sum(!is.na(score_petition)),
        sum(!is.na(df$combined_index)))
) %>%
  mutate(dropped = n_total - n,
         pct_valid = round(n / n_total * 100, 1))

# share of dropped responses because of min valid answer per treatment:
round(prop.table(table(df$treat_label, is.na(df$combined_index)), 1) * 100, 1)
# results for the full survey data --> between 25% and 30%, similar for control and treatment groups, so no bias
# because of dropping incomplete responses

# check whether there are less "Don't know"s in treatment groups
# i.e. treatment helps respondents form an opinion
df$any_treat <- as.numeric(df$treat != "1")
df$any_dont_know <- as.numeric(is.na(df$combined_index))
summary(lm_robust(as.formula(paste("any_dont_know ~ any_treat", rhs_fe)), data = df))
# results for the full survey data --> it doesn't, coefficient is positive and very small

# income quartile with "No answer" as its own category (works for pooled and single countries)

inc_num <- if (is_pooled) coalesce(as.numeric(df$Q5_DE), as.numeric(df$Q5_NO), as.numeric(df$Q5_NL)) else
  as.numeric(df$income)
df$income_q <- factor(ifelse(inc_num %in% 1:4, paste0(c("1st", "2nd", "3rd", "4th")[inc_num], " quartile"),
                             ifelse(!is.na(inc_num), "No answer", NA)),
                      levels = c(paste0(c("1st", "2nd", "3rd", "4th"), " quartile"), "No answer"))

var_labels["income_q"]    <- "Household income"

# ---- Helpers to create and save output ----------------------------------------------

# create folder /output where output is saved when this code is run. results created
# from the full dataset can be found in /results.
dir.create("output", showWarnings = FALSE)

# overwrite output:
# remove this dataset's old output files (incl. the shared CSVs in output/share),
# so the folders only contain current results
old_files <- c(list.files("output", pattern = paste0("^", label, "_"), full.names = TRUE),
               list.files(file.path("output", "share"), pattern = paste0("^", label, "_"),
                          full.names = TRUE))
unlink(old_files, recursive = TRUE)

# file path in the output folder with the dataset label (e.g. output/DE_name.docx)
out_file <- function(name) file.path("output", paste0(label, "_", name))

# show a plot and save it with the dataset label in the file name (e.g. NO_support_gen.png)
save_plot <- function(p, name, width = 10, height = 6) {
  print(p)
  ggsave(file.path("output", paste0(label, "_", name, ".png")), p,
         width = width, height = height, dpi = 300)
}

# helper to create percentage tables
pct_table <- function(var) {
  round(prop.table(table(df[[var]], useNA = "ifany")) * 100, 1)
}

# for distributions of one categorical variable (share of respondents)
# helper to create bar charts for showing the share of respondents
plot_dist <- function(var, title = NULL) {
  df %>%
    filter(!is.na(.data[[var]])) %>%
    count(answer = .data[[var]]) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ggplot(aes(x = answer, y = pct)) +
    geom_col(fill = "#584F80", width = 0.7) +
    geom_text(aes(label = paste0(round(pct, 1), " %")),
              vjust = -0.5, size = 8, colour = "#0B0B0B") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    scale_x_discrete(labels = scales::label_wrap(10)) +
    labs(x = NULL, y = "Share of respondents (%)", title = title) +
    theme_minimal(base_size = 25) +
    theme(plot.title = element_text(colour = "#584F80"),
          panel.grid.major.x = element_blank())
}

# "select all that apply" items: TRUE if selected ("NO TO ..." = not selected)
is_selected <- function(x) ifelse(is.na(x), NA, !grepl("^NO TO", as.character(x), ignore.case = TRUE))

# share of respondents who selected each option of a multiple-choice question
# helper for horizontal bar charts for the "select all that apply" questions
plot_selected <- function(vars, labels, xlab, title = NULL, no_pattern = NULL) {
  df %>%
    select(all_of(vars)) %>%
    # each option separately: "NO TO ..." -> 1 (not selected), everything else -> 2 (selected)
    mutate(across(everything(), function(x) {
      if (is.null(no_pattern)) return(as.numeric(x))
      ifelse(is.na(x), NA, 1 + !grepl(no_pattern, as.character(x), ignore.case = TRUE))
    })) %>%
    pivot_longer(everything(), names_to = "option", values_to = "value") %>%
    mutate(selected = value == max(value, na.rm = TRUE)) %>%
    group_by(option) %>%
    summarise(pct = mean(selected, na.rm = TRUE) * 100) %>%
    mutate(option = recode(option, !!!labels)) %>%
    ggplot(aes(x = pct, y = reorder(option, pct))) +
    geom_col(fill = "#584F80", width = 0.7) +
    geom_text(aes(label = paste0(round(pct), "%")),
              hjust = -0.1, size = 6, colour = "#0B0B0B") +
    scale_x_continuous(expand = expansion(mult = c(0, 0.25))) +
    labs(x = xlab, y = NULL, title = title) +
    theme_minimal(base_size = 18) +
    theme(plot.title = element_text(colour = "#584F80", face = "bold"),
          panel.grid.major.y = element_blank(),
          axis.text.y = element_text(size = 20, colour = "#0B0B0B"))
}

# helper to export a regression table to Word in a clean economics-paper style
#   title:   table title, e.g. "Table 3. OLS regression: treatment effects on support for CCUS"
#   dep_var: header above the model columns, e.g. "Dependent variable: support (1-7)"
#   notes:   notes below the table (stars and standard errors are added automatically)
#   significance stars, used in all regression tables (change here if needed)
star_levels <- c("*" = 0.1, "**" = 0.05, "***" = 0.01)
star_note   <- paste0(paste(names(star_levels), "p <", star_levels, collapse = "; "), ".")

export_word <- function(models, path, landscape = FALSE, coef_omit = "^[Cc]ountry",
                        gof_map = c("nobs", "r.squared", "adj.r.squared"),
                        title = NULL, dep_var = NULL, notes = NULL, ...) {
  ft <- modelsummary(models,
                     stars = star_levels,
                     gof_map = gof_map, coef_rename = rename_terms, coef_omit = coef_omit,
                     title = title, output = "flextable", ...)
  if (!is.null(dep_var)) {
    ft <- flextable::add_header_row(ft, values = c("", dep_var),
                                    colwidths = c(1, flextable::ncol_keys(ft) - 1))
  }
  ft <- flextable::add_footer_lines(ft, c(
    paste(star_note, "Robust standard errors (HC2) in parentheses."),
    if (!is.null(notes)) paste("Notes:", paste(notes, collapse = " "))))
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::fontsize(ft, size = 9, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::align(ft, j = 2:flextable::ncol_keys(ft), align = "center", part = "all")
  ft <- flextable::align(ft, align = "left", part = "footer")
  ft <- flextable::padding(ft, padding.top = 1, padding.bottom = 1, part = "all")
  ft <- flextable::autofit(ft)
  orient <- if (landscape) "landscape" else "portrait"
  flextable::save_as_docx(ft, path = path,
                          pr_section = officer::prop_section(page_size = officer::page_size(orient = orient)))
}
################################################################################
## 1. Sample characteristics: Summary statistics
################################################################################
if (is_pooled) {
  # income: income groups are roughly based on income quartiles from each country.
  #  the variable combines the country-specific income questions (Q5_DE, Q5_NO, Q5_NL)
  #  only for this table: 1-4 = 1st-4th quartile, any other answer = "No answer"
  inc_num <- coalesce(as.numeric(df$Q5_DE), as.numeric(df$Q5_NO), as.numeric(df$Q5_NL))
  quartiles <- c("1st income quartile", "2nd income quartile",
                 "3rd income quartile", "4th income quartile")
  
  sample_vars <- c("age_group", "Gender_new", "Q2", "income_q", "Q7")
  print(df %>%
          mutate(income_q = factor(ifelse(inc_num %in% 1:4, quartiles[inc_num],
                                          ifelse(!is.na(inc_num), "No answer", NA)),
                                   levels = c(quartiles, "No answer"))) %>%
          select(all_of(sample_vars)) %>%
          tbl_summary(label = c(lab_list(c("age_group", "Gender_new", "Q2", "Q7")),
                                list(income_q = "Household income")),
                      missing_text = "Missing"))
} else {
  sample_vars <- c("age_group", "Gender_new", "Q2", "income", "Q7")
  print(df %>%
          select(all_of(sample_vars)) %>%
          tbl_summary(label = lab_list(sample_vars), missing_text = "Missing"))
}

################################################################################
## 2. Map of respondents & average level of support by region (DE only)
################################################################################
# --- Maps only for DE (federal states, NUTS 1). Not applicable to NO and NL.----

map_cfg <- list(DE = list(level = 1, var = "region"))

# ---- Map 1: shows distribution of respondents over the federal states ------

if (label %in% names(map_cfg) && map_cfg[[label]]$var %in% names(df)) {   
  region_var <- map_cfg[[label]]$var
  map_shape  <- gisco_get_nuts(country = label, nuts_level = map_cfg[[label]]$level,
                               resolution = "10")

  counts <- df %>%
    count(region = .data[[region_var]], name = "n") %>%
    filter(!is.na(region)) %>%
    mutate(pct = n / sum(n) * 100, region = as.character(region))

  cat("Regions without a match on the map:\n")
  print(setdiff(counts$region, map_shape$NUTS_NAME))

  map_data <- map_shape %>% left_join(counts, by = c("NUTS_NAME" = "region"))

  p <- ggplot(map_data) +
    geom_sf(aes(fill = pct), colour = "white") +
    geom_sf_text(aes(label = ifelse(is.na(pct), "", paste0(round(pct, 1), " %"))),
                 size = 4.5,
                 colour = ifelse(!is.na(map_data$pct) &
                                   map_data$pct > max(map_data$pct, na.rm = TRUE) / 2,
                                 "#FFFFFF", "#0B0B0B")) +
    scale_fill_gradient(low = "#E6E4EE", high = "#584F80",
                        name = "Share of respondents (%)") +
    theme_void() +
    labs(title = "Respondents by region") +
    theme(plot.title = element_text(colour = "#584F80"))
  save_plot(p, "map", width = 7, height = 8)

  # ---- Map 2: shows average support index by federal state -----------
  
  col_low <- "#C0703A"; col_mid <- "#F4F3F8"; col_high <- "#584F80"

  idx_vals <- df %>%
    mutate(region = as.character(.data[[region_var]])) %>%
    group_by(region) %>%
    summarise(mean = mean(support_index, na.rm = TRUE),
              n = sum(!is.na(support_index)), .groups = "drop") %>%
    mutate(mean = ifelse(n < 20, NA, mean))            # too few respondents -> grey

  idx_map <- map_shape %>% left_join(idx_vals, by = c("NUTS_NAME" = "region"))
  idx_mid <- mean(df$support_index, na.rm = TRUE)     # national mean
  idx_lim <- max(abs(idx_map$mean - idx_mid), na.rm = TRUE)

  p <- ggplot(idx_map) +
    geom_sf(aes(fill = mean), colour = "white", linewidth = 0.4) +
    geom_sf_text(aes(label = ifelse(is.na(mean), "", sprintf("%.2f", mean))),
                 size = 4.5,
                 colour = ifelse(!is.na(idx_map$mean) & abs(idx_map$mean - idx_mid) > 0.6 * idx_lim,
                                 "white", "#0B0B0B")) +
    scale_fill_gradient2(low = col_low, mid = col_mid, high = col_high,
                         midpoint = idx_mid, limits = idx_mid + c(-idx_lim, idx_lim),
                         na.value = "#D9D9D9", name = "Support index (1-7)") +
    labs(title = "Support index by federal state",
         caption = paste0("Mean of six support items (1 = strongly oppose, 7 = strongly support).\n",
                          "Colour: above (lilac) or below (orange) the national mean of ",
                          sprintf("%.2f", idx_mid), ". Grey: fewer than 20 respondents.")) +
    theme_void(base_size = 16) +
    theme(plot.title = element_text(face = "bold", margin = margin(b = 10)),
          plot.caption = element_text(hjust = 0, colour = "#555555"),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"))
  save_plot(p, "map_DE_support_index", width = 8, height = 10)
}


################################################################################
## 3. Level of support for CCUS in general and per application
################################################################################

# ---- combined chart: all six support questions in one stacked chart ----------
# shows level of support for CCUS in general, for Biomass with CCUS, Waste-to-Energy 
# with CCUS, utilization of CO2 in greenhouses, pipelines for CO2 transport, CO2 storage
# built from the cleaned support variables (7 = strongly support, "Don't know" = NA);
# "Don't know" = answered the question (Q18 / Q26) but no support value
support_q <- c(support_gen = "Q18", support_biomass = "Q26A", support_wte = "Q26B",
               support_util = "Q26C", support_pipe = "Q26D", support_stor = "Q26E")
support_answers <- c("Strongly support", "Support", "Somewhat support",
                     "Neither support nor oppose", "Somewhat oppose", "Oppose",
                     "Strongly oppose", "Don't know")
support_cols <- c("#584F80", "#7A759B", "#B5B2CB", "#E3E1EC",
                  "#E6B38A", "#D08A4E", "#C0703A", "#B5B5B5")
support_text <- c("white", "white", "#0B0B0B", "#0B0B0B",
                  "#0B0B0B", "#0B0B0B", "white", "#0B0B0B")

p <- bind_rows(lapply(names(support_q), function(s)
  tibble(item   = support_q[[s]],
         answer = ifelse(!is.na(df[[s]]), 8 - df[[s]],            # 1 = strongly support ... 7 = strongly oppose
                         ifelse(!is.na(df[[support_q[[s]]]]), 8, NA))))) %>%   # 8 = don't know
  filter(answer %in% 1:8) %>%
  mutate(answer = factor(answer, levels = 1:8, labels = support_answers)) %>%
  count(item, answer) %>%
  group_by(item) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = pct, y = item, fill = answer)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.4,
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(pct >= 5, paste0(round(pct), "%"), ""), colour = answer),
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 4.5) +
  scale_fill_manual(values = setNames(support_cols, support_answers)) +
  scale_colour_manual(values = setNames(support_text, support_answers), guide = "none") +
  scale_y_discrete(limits = rev(c("Q18", "Q26A", "Q26B", "Q26C", "Q26D", "Q26E")),
                   labels = c(expression(CO[2]~"storage"),
                              expression(CO[2]~"pipelines"),
                              expression(CO[2]~"utilisation (greenhouses)"),
                              "Waste-to-energy with CCUS",
                              "Biomass energy with CCUS",
                              "CCUS in general")) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(x = "Share of respondents (%)", y = NULL, fill = NULL,
       title = "Support for CCUS in general and by application") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(colour = "#584F80", face = "bold"),
        panel.grid.major.y = element_blank(),
        legend.position = "top",
        axis.text.y = element_text(size = 17, colour = "#0B0B0B"))
save_plot(p, "support_overview", width = 15, height = 7)

# ---- optional: one chart per question (switch at the top of the script) ------
if (single_support_charts) {
  save_plot(plot_dist("Q18",  "Level of support"), "support_gen")
  save_plot(plot_dist("Q26A", "Level of support for Biomass Energy with CCUS"), "support_biomass")
  save_plot(plot_dist("Q26B", "Level of support for Waste to Energy with CCUS"), "support_wte")
  save_plot(plot_dist("Q26C", expression("Level of support for Utilization of CO"[2]*" in Greenhouses (CCU)")), "support_util")
  save_plot(plot_dist("Q26D", expression("Level of support for Pipelines for CO"[2]*" Transport")), "support_pipe")
  save_plot(plot_dist("Q26E", expression("Level of support for CO"[2]*" Storage (CCS)")), "support_stor")
}

# mean support without "Don't know" (1 = strongly oppose ... 7 = strongly support)
support_outcomes <- c("support_gen", "support_biomass", "support_wte",
                      "support_util", "support_pipe", "support_stor")
round(sapply(df[support_outcomes], mean, na.rm = TRUE), 2)


################################################################################
## 4. Willingness to sign a petition
################################################################################

# ---- combined chart: five petition questions in one stacked chart ----------
# shows shares of respondents willing to sign a petition for, against, neither 
# or undecided for all five CCUS application 
p <- df %>%
  select(Q27r1 = Q27r1_num, Q27r2 = Q27r2_num, Q27r3 = Q27r3_num,
         Q27r4 = Q27r4_num, Q27r5 = Q27r5_num) %>%     # original SPSS codes (see 01)
  pivot_longer(everything(), names_to = "application", values_to = "answer") %>%
  filter(answer %in% 1:4) %>%
  mutate(answer = factor(answer, levels = 1:4,
                         labels = c("Sign for", "Sign against", "Would not sign",
                                    "Don't know/Undecided"))) %>%
  count(application, answer) %>%
  group_by(application) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = pct, y = application, fill = answer)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.5,
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(pct >= 4, paste0(round(pct), " %"), ""),
                colour = answer),
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 7) +
  scale_fill_manual(values = c("Sign for" = "#584F80",
                               "Sign against" = "#0B0B0B",
                               "Would not sign" = "#D4D1E3",
                               "Don't know/Undecided" = "#B5B5B5")) +
  scale_colour_manual(values = c("Sign for" = "white",
                                 "Sign against" = "white",
                                 "Would not sign" = "#0B0B0B",
                                 "Don't know/Undecided" = "#0B0B0B"),
                      guide = "none") +
  scale_y_discrete(limits = rev(c("Q27r1", "Q27r2", "Q27r3", "Q27r4", "Q27r5")),
                   labels = c(expression(CO[2]~"storage"),
                              expression(CO[2]~"pipeline"),
                              expression(CO[2]~"utilisation"),
                              "Waste-to-energy plant with CCUS",
                              "Biomass plant with CCUS")) +
  labs(x = "Share of respondents (%)", y = NULL, fill = NULL,
       title = "Petition signing by application") +
  theme_minimal(base_size = 20) +
  theme(plot.title = element_text(colour = "#584F80", face = "bold"),
        panel.grid.major.y = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 20),
        axis.text.y = element_text(size = 20, colour = "#0B0B0B"))

save_plot(p, "petition", width = 14, height = 7)


################################################################################
## 5. Quality check: contradictory support and petition answers
################################################################################

# report contradictory answers of respondents that would sign a petition against 
# an application but are supportive of the same application;
# or would sign a petition for an application but are opposed to the same

table(df$n_contradictions)                       # respondents with 0, 1, 2, ... contradictions
colMeans(df[paste0("contra_", apps)]) * 100      # share of contradictions per application (%)


################################################################################
## 6. Factors, beliefs and attitudes
################################################################################

# most important factors for the CCUS opinion (Q20)
factor_labels <- c(Q20r1 = "Risks of transport and storage",
                   Q20r2 = "Potential impact on economy and jobs",
                   Q20r3 = "Reduction of emissions",
                   Q20r4 = "Required energy usage",
                   Q20r5 = "Costs of CCUS",
                   Q20r6 = "Continued use of fossil fuels",
                   Q20r7 = "Trust in institutions responsible for CCUS",
                   Q20r98 = "Other factors")
save_plot(plot_selected(
  vars   = names(factor_labels),
  labels = factor_labels,
  xlab   = "Share of respondents who selected the factor (%)",
  title  = "Important factors",
  no_pattern = "^NO TO"), "factors_Q20", width = 12)

# percentage tables
for (v in c("Q22", "Q34", "Q33", "Q32_f", "Q19r6", "Q21", "Q36", "Q15", "Q31NEW")) {
  cat("\n=====", nice_name(v), "=====\n")
  print(pct_table(v))
}

# Q23: ranking of emission reduction strategies (budget allocation)
budget_labels <- c(Q23r1 = "Renewable energies", Q23r2 = "Electrification",
                   Q23r3 = "Energy efficiency",  Q23r4 = "Natural carbon sinks",
                   Q23r5 = "Renewable fuels",    Q23r6 = "CCUS")
p <- df %>%
  select(all_of(names(budget_labels))) %>%
  pivot_longer(everything(), names_to = "option", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  group_by(option) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value)))) %>%
  mutate(option = recode(option, !!!budget_labels)) %>%
  ggplot(aes(x = mean, y = reorder(option, mean))) +
  geom_col(fill = "#584F80", width = 0.7) +
  geom_errorbar(aes(xmin = mean - 1.96 * se, xmax = mean + 1.96 * se),
                width = 0.2, colour = "#0B0B0B") +
  geom_text(aes(x = mean + 1.96 * se, label = paste0(round(mean, 1), " %")),
            hjust = -0.2, size = 6, colour = "#0B0B0B") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(x = "Average share of budget allocated (%)", y = NULL,
       title = "Budget allocation") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(colour = "#584F80", face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 20, colour = "#0B0B0B"))
save_plot(p, "budget_Q23")

# Q24: estimated opinion of fellow citizens with respect to CCUS 
# (estimated share of fellow citizens support or against CCUS)
p <- df %>%
  select(Q24r1, Q24r2) %>%
  pivot_longer(everything(), names_to = "option", values_to = "value") %>%
  mutate(value = as.numeric(value)) %>%
  group_by(option) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            se = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value)))) %>%
  mutate(option = recode(option,
                         Q24r1 = "...supportive of CCUS",
                         Q24r2 = "...against CCUS")) %>%
  ggplot(aes(x = mean, y = reorder(option, mean))) +
  geom_col(fill = "#584F80", width = 0.4) +
  geom_errorbar(aes(xmin = mean - 1.96 * se, xmax = mean + 1.96 * se),
                width = 0.1, colour = "#0B0B0B") +
  geom_text(aes(x = mean + 1.96 * se, label = paste0(round(mean, 1), " %")),
            hjust = -0.2, size = 6, colour = "#0B0B0B") +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Estimated share of citizens... (%)", y = NULL,
       title = "Estimated opinion on CCUS preferences of fellow citizens") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(colour = "#584F80", face = "bold"),
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 20, colour = "#0B0B0B"),
        aspect.ratio = 0.3)
save_plot(p, "estimates_Q24", height = 3.5)

cat("\n=====", nice_name("noanswerQ24_r99"), "=====\n")
print(pct_table("noanswerQ24_r99"))   # share who gave no estimate

# Q14: prior knowledge of CCUS
p <- df %>%
  filter(!is.na(Q14)) %>%
  count(Q14) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = Q14, y = pct)) +
  geom_col(fill = "#584F80", width = 0.5) +
  geom_text(aes(label = paste0(round(pct, 1), " %")),
            vjust = -0.5, size = 7.5, colour = "#0B0B0B") +
  scale_x_discrete(labels = scales::label_wrap(12)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "Share of respondents (%)") +
  theme_minimal(base_size = 21) +
  theme(panel.grid.major.x = element_blank(),
        aspect.ratio = 0.5,
        axis.text = element_text(size = 19, colour = "#0B0B0B"))
save_plot(p, "knowledge_Q14")

# Q16 / Q17: used and trusted information sources in one chart
p <- bind_rows(
  tibble(source = source_labels, type = "Used",
         pct = sapply(paste0("Q16r", 1:7), function(v) mean(is_selected(df[[v]]), na.rm = TRUE) * 100)),
  tibble(source = source_labels, type = "Trusted",
         pct = sapply(paste0("Q17r", 1:7), function(v) mean(is_selected(df[[v]]), na.rm = TRUE) * 100))
) %>%
  mutate(type = factor(type, levels = c("Used", "Trusted")),
         source = reorder(source, pct * (type == "Used"), FUN = sum)) %>%
  ggplot(aes(x = pct, y = source, fill = type)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_text(aes(label = paste0(round(pct), "%")),
            position = position_dodge(width = 0.8), hjust = -0.15, size = 5, colour = "#0B0B0B") +
  scale_fill_manual(values = c(Used = "#584F80", Trusted = "#D08A4E")) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(x = "Share of respondents (%)", y = NULL, fill = NULL,
       title = "Used and trusted information sources") +
  theme_minimal(base_size = 18) +
  theme(plot.title = element_text(colour = "#584F80", face = "bold"),
        panel.grid.major.y = element_blank(),
        legend.position = "top",
        axis.text.y = element_text(size = 17, colour = "#0B0B0B"))
save_plot(p, "sources_used_trusted", width = 12, height = 8)

# Q19: estimation of respondents how likely certain consequences of CCUS are
benefit_labels <- c(Q19r1 = "Creation of new jobs",
                    Q19r2 = "More economic activity",
                    Q19r3 = "Preservation of industries and jobs",
                    Q19r4 = "More secure energy system",
                    Q19r5 = "Negative impacts on the environment",
                    Q19r6 = "Reduction of emissions")
p <- df %>%
  select(Q19r1 = Q19r1_num, Q19r2 = Q19r2_num, Q19r3 = Q19r3_num,
         Q19r4 = Q19r4_num, Q19r5 = Q19r5_num, Q19r6 = Q19r6_num) %>%   # original SPSS codes (see 01)
  pivot_longer(everything(), names_to = "item", values_to = "answer") %>%
  mutate(answer = factor(answer, levels = 1:6,
                         labels = c("Very likely", "Likely",
                                    "Neither likely nor unlikely",
                                    "Unlikely", "Very unlikely", "Don't know"))) %>%
  filter(!is.na(answer)) %>%
  count(item, answer) %>%
  group_by(item) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = pct, y = item, fill = answer)) +
  geom_col(width = 0.6, colour = "white", linewidth = 0.5,
           position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(pct >= 4, paste0(round(pct), "%"), ""),
                colour = answer),
            position = position_stack(vjust = 0.5, reverse = TRUE), size = 6) +
  scale_fill_manual(values = c("Very likely" = "#584F80",
                               "Likely" = "#9A96B6",
                               "Neither likely nor unlikely" = "#E3E1EC",
                               "Unlikely" = "#E6B38A",
                               "Very unlikely" = "#C0703A",
                               "Don't know" = "#B5B5B5")) +
  scale_colour_manual(values = c("Very likely" = "white",
                                 "Likely" = "#0B0B0B",
                                 "Neither likely nor unlikely" = "#0B0B0B",
                                 "Unlikely" = "#0B0B0B",
                                 "Very unlikely" = "white",
                                 "Don't know" = "#0B0B0B"),
                      guide = "none") +
  scale_y_discrete(limits = rev(names(benefit_labels)), labels = benefit_labels) +
  labs(x = "Share of respondents (%)", y = NULL, fill = NULL) +
  guides(fill = guide_legend(nrow = 2)) +
  theme_minimal(base_size = 20) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "top",
        axis.text = element_text(size = 18, colour = "#0B0B0B"))
save_plot(p, "benefits_Q19", width = 14)

# ---- support by political orientation ---------------------------------------
# single country: individual parties; pooled data: Left / Centre / Right blocs
# "Other party / no party stated" kept as its own category; groups with n < 30 not shown

df$party_group <- dplyr::case_when(
  !is.na(df$party_bloc) & df$party_bloc == "Other" ~ "Other party",
  !is.na(df$party_bloc)                             ~ as.character(df$party_bloc),
  !is.na(df$Q32_f)                                  ~ "No party stated",
  TRUE                                              ~ NA_character_
)
df$party_group <- factor(df$party_group,
                         levels = c("Left", "Centre", "Right", "Other party", "No party stated"))
var_labels["party_group"] <- "Political orientation"

p <- df %>%
  mutate(group = party_group) %>%
  filter(!is.na(group), !is.na(support_index)) %>%
  group_by(group) %>%
  summarise(mean = mean(support_index), se = sd(support_index) / sqrt(n()), n = n(),
            .groups = "drop") %>%
  filter(n >= 30) %>%
  ggplot(aes(x = mean, y = reorder(group, mean))) +
  geom_vline(xintercept = mean(df$support_index, na.rm = TRUE), linetype = "dotted", colour = "#8C8C8C") +
  geom_pointrange(aes(xmin = mean - 1.96 * se, xmax = mean + 1.96 * se), colour = "#584F80", size = 0.8) +
  geom_text(aes(label = paste0("n = ", n)), vjust = -1.1, size = 4, colour = "#555555") +
  labs(x = "Support index (1 = strongly oppose, 7 = strongly support)", y = NULL,
       title = if (is_pooled) "Support for CCUS by political orientation" else "Support for CCUS by party preference",
       caption = paste0("Points: mean; lines: 95% confidence intervals. Dotted line: overall mean.\n",
                        "Groups with fewer than 30 respondents not shown.")) +
  theme_minimal(base_size = 17) +
  theme(plot.title = element_text(colour = "#584F80", face = "bold"),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, colour = "#555555"),
        plot.caption.position = "plot",
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 15, colour = "#0B0B0B"))
save_plot(p, "support_by_party", width = 11, height = 6)

# ---- support by climate concern ---------------------------------------------
# answer categories in their natural order (from strongly agree to strongly disagree);
# "Don't know" excluded; groups with n < 30 not shown

p <- df %>%
  filter(!is.na(Q34_f), !is.na(support_index)) %>%
  group_by(group = Q34_f) %>%
  summarise(mean = mean(support_index), se = sd(support_index) / sqrt(n()), n = n(),
            .groups = "drop") %>%
  filter(n >= 30) %>%
  mutate(group = factor(group, levels = rev(levels(droplevels(df$Q34_f))))) %>%
  ggplot(aes(x = mean, y = group)) +
  geom_vline(xintercept = mean(df$support_index, na.rm = TRUE), linetype = "dotted", colour = "#8C8C8C") +
  geom_pointrange(aes(xmin = mean - 1.96 * se, xmax = mean + 1.96 * se), colour = "#584F80", size = 0.8) +
  geom_text(aes(label = paste0("n = ", n)), vjust = -1.1, size = 4, colour = "#555555") +
  labs(x = "Support index (1 = strongly oppose, 7 = strongly support)", y = NULL,
       title = "Support for CCUS by climate concern",
       caption = paste0("Climate concern: agreement that climate change is an important problem.\n",
                        "Points: mean; lines: 95% confidence intervals. Dotted line: overall mean.\n",
                        "\"Don't know\" excluded; groups with fewer than 30 respondents not shown.")) +
  theme_minimal(base_size = 17) +
  theme(plot.title = element_text(colour = "#584F80", face = "bold"),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, colour = "#555555"),
        plot.caption.position = "plot",
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 15, colour = "#0B0B0B"))
save_plot(p, "support_by_climate_concern", width = 11, height = 6)

################################################################################
## 7. Treatment effects of the information treatments
################################################################################

# define notes shared by all treatment-effect tables
note_ref    <- "Reference category: control group (no information)."
note_fe     <- if (is_pooled) "All models include country fixed effects (not shown)." else NULL
note_sample <- paste0("Sample: ", if (is_pooled) "pooled data (DE, NO, NL)" else label,
                      "; respondents with a valid answer on the respective outcome.")
note_treat  <- paste("Treatments: short information texts on jobs, local economic growth,",
                     "preservation of jobs and industries, energy security, or climate change.")

# ---- 7a. Manipulation check: did the treatments change expected consequences? ----

# outcome: perceived likelihood of each consequence (Q19), measured after the treatment
# treatment should increase perceived likelihood of the respective co-benefits if treatment 
# texts on co-benefits worked/was believed
# scale: 1 = very likely ... 5 = very unlikely; "Don't know" excluded
# -> negative coefficient: consequence seen as MORE likely

consequence_items <- c(Q19r1_num = "Job creation",
                       Q19r2_num = "Economic activity",
                       Q19r3_num = "Preservation of industries and jobs",
                       Q19r4_num = "Energy security",
                       Q19r5_num = "Environmental harm",
                       Q19r6_num = "Emission reduction")

models_beliefs <- lapply(names(consequence_items), function(v)
  lm_robust(as.formula(paste0("ifelse(", v, " %in% 1:5, ", v, ", NA) ~ treat", rhs_fe)), data = df))
names(models_beliefs) <- unname(consequence_items)

belief_rows  <- if (is_pooled) data.frame(term = "Country fixed effects",
                                          a = "Yes", b = "Yes", c = "Yes",
                                          d = "Yes", e = "Yes", f = "Yes") else NULL


belief_notes <- c("Measured after the treatment; \"Don't know\" excluded.",
                  "Negative coefficients: the consequence is seen as more likely than in the control group.",
                  note_treat, note_ref, note_sample)

modelsummary(models_beliefs,
             stars = star_levels, gof_map = c("nobs", "r.squared"),
             coef_rename = rename_terms, coef_omit = "^[Cc]ountry",
             add_rows = belief_rows, notes = belief_notes)
export_word(models_beliefs, out_file("treatment_effects_expected_consequences.docx"), landscape = TRUE,
            title = "OLS regression: treatment effects on expected consequences of CCUS",
            dep_var = "Dependent variable: perceived likelihood (1 = very likely, 5 = very unlikely)",
            add_rows = belief_rows, notes = belief_notes)

# ---- 7b. Treatment effects ------------------------------------------------------
# Each outcome is regressed on the treatment groups with a linear model:
# Ordinary Least Square (OLS) with robust standard errors.
# The control group is the reference.
# The pooled data include country fixed effects, so the effects are estimated within countries.
# Outcomes: 
#   - support for CCUS in general and for the five applications (1-7 scale)
#   - willingness to sign a petition for / against each application (linear
#     probability models; undecided respondents excluded)
#   - support index and combined preference index, for each treatment and for
#     any treatment vs. control to reduce the number of tests undertaken.
# two checks for the petition models are included : whether the treatments change the share of
# undecided respondents, and "sign for" with undecided respondents kept
# The tables are exported to Word (output/<label>_treatment_effects_*.docx).

# Helper to fit one regression per outcome in a list
run_models <- function(outcomes) {
  m <- lapply(outcomes, function(y)
    lm_robust(as.formula(paste(y, "~ treat", rhs_fe)), data = df))
  names(m) <- outcomes
  m
}

table(df$treat_label, useNA = "ifany")

# --- OLS models: outcome = support for CCUS in general and for the five applications ---
# support (1 = strongly oppose ... 7 = strongly support)
models_support <- run_models(support_outcomes)
names(models_support) <- c("General", "Biomass", "Waste-to-energy",
                           "CO2 utilisation", "CO2 pipelines", "CO2 storage")

modelsummary(models_support,
             stars = star_levels, gof_map = c("nobs", "r.squared"),
             coef_rename = rename_terms, coef_omit = "^[Cc]ountry")
export_word(models_support, out_file("treatment_effects_support.docx"), landscape = TRUE,
            title = "OLS regression: treatment effects on support for CCUS",
            dep_var = "Dependent variable: support (1 = strongly oppose, 7 = strongly support)",
            notes = c("Support for CCUS in general and for five CCUS applications;",
                      "\"Don't know\" answers are excluded.", note_treat, note_ref, note_fe, note_sample))

# --- Linear probability models: outcome = petition signing FOR each of the five applications ---
models_pet_for <- run_models(paste0("petition_for_", apps))
names(models_pet_for) <- c("Biomass", "Waste-to-energy", "CO2 utilisation", "CO2 pipelines", "CO2 storage")

modelsummary(models_pet_for,
             stars = star_levels, gof_map = c("nobs", "r.squared"),
             coef_rename = rename_terms, coef_omit = "^[Cc]ountry")
export_word(models_pet_for, out_file("treatment_effects_petition_for.docx"), landscape = TRUE,
            title = "Linear probability models: treatment effects on willingness to sign a petition in favour",
            dep_var = "Dependent variable: would sign a petition in favour (0/1)",
            notes = c("Coefficients are changes in the probability of signing in favour (in percentage points / 100).",
                      "Respondents who are undecided are excluded.", note_treat, note_ref, note_fe, note_sample))

# --- Linear probability models: outcome = petition signing AGAINST each of the five applications ---
models_pet_against <- run_models(paste0("petition_against_", apps))
names(models_pet_against) <- c("Biomass", "Waste-to-energy", "CO2 utilisation", "CO2 pipelines", "CO2 storage")

modelsummary(models_pet_against,
             stars = star_levels, gof_map = c("nobs", "r.squared"),
             coef_rename = rename_terms, coef_omit = "^[Cc]ountry")
export_word(models_pet_against, out_file("treatment_effects_petition_against.docx"), landscape = TRUE,
            title = "Linear probability models: treatment effects on willingness to sign a petition against",
            dep_var = "Dependent variable: would sign a petition against (0/1)",
            notes = c("Coefficients are changes in the probability of signing against (in percentage points / 100).",
                      "Respondents who are undecided are excluded.", note_treat, note_ref, note_fe, note_sample))
# undecided respondents are NA in the models above (only respondents who decided).

# If the treatment changes who decides, these estimates are biased, so two checks:
# (a) does the treatment change the share of undecided respondents?
# estimated via linear probability models: outcome = being undecided about petition signing
models_pet_undecided <- run_models(paste0("petition_undecided_", apps))
names(models_pet_undecided) <- c("Biomass", "Waste-to-energy", "CO2 utilisation", "CO2 pipelines", "CO2 storage")

modelsummary(models_pet_undecided,
             stars = star_levels, gof_map = c("nobs", "r.squared"),
             coef_rename = rename_terms, coef_omit = "^[Cc]ountry")
export_word(models_pet_undecided, out_file("treatment_effects_petition_undecided.docx"), landscape = TRUE,
            title = "Linear probability models: treatment effects on being undecided about a petition",
            dep_var = "Dependent variable: undecided whether to sign a petition (0/1)",
            notes = c("Robustness check: tests whether the treatments change the share of undecided respondents,",
                      "who are excluded from the petition models.", note_treat, note_ref, note_fe, note_sample))

# (b) linear probability models on "sign for" with undecided respondents kept:
# undecided respondents are considered here and counted as not signing in favor
models_pet_for_incl <- run_models(paste0("petition_for_incl_undec_", apps))
names(models_pet_for_incl) <- c("Biomass", "Waste-to-energy", "CO2 utilisation", "CO2 pipelines", "CO2 storage")

modelsummary(models_pet_for_incl,
             stars = star_levels, gof_map = c("nobs", "r.squared"),
             coef_rename = rename_terms, coef_omit = "^[Cc]ountry")
export_word(models_pet_for_incl, out_file("treatment_effects_petition_for_incl_undecided.docx"), landscape = TRUE,
            title = "Linear probability models: willingness to sign in favour, undecided respondents included",
            dep_var = "Dependent variable: would sign a petition in favour (0/1)",
            notes = c("Robustness check: undecided respondents are kept and coded as not signing in favour.",
                      note_treat, note_ref, note_fe, note_sample))

# --- OLS models: outcome indices (support index and combined index) ------

# OLS models for both indices for each treatment arm and the pooled treatment (any treatment vs. control)
# any treatment is a binary indicator of being in one of the treatment groups (having received any treatment)
# treatment are binary indicators for each one of the treatment groups. 
models_index <- list(
  "Support index"  = lm_robust(as.formula(paste("support_index ~ treat", rhs_fe)), data = df),
  "Support index"  = lm_robust(as.formula(paste("support_index ~ any_treat", rhs_fe)), data = df),
  "Combined index" = lm_robust(as.formula(paste("combined_index ~ treat", rhs_fe)), data = df),
  "Combined index" = lm_robust(as.formula(paste("combined_index ~ any_treat", rhs_fe)), data = df)
)

index_rows  <- if (is_pooled) data.frame(term = "Country fixed effects",
                                         a = "Yes", b = "Yes", c = "Yes", d = "Yes") else NULL
index_notes <- c("Support index: mean of six support items (1 = strongly oppose, 7 = strongly support).",
                 paste("Combined index: mean of three standardised scores (general support, application-specific",
                       "support, application-specific petition signing), standardised with the control-group mean and SD;",
                       "respondents with valid answers on all three dimensions."),
                 "Reference category: Control.")

modelsummary(models_index,
             stars = star_levels,
             gof_map = c("nobs", "r.squared"),
             coef_rename = rename_terms,
             coef_omit = "^[Cc]ountry",
             add_rows = index_rows,
             notes = index_notes)
export_word(models_index, out_file("treatment_effects_indices.docx"),
            title = "OLS regression: treatment effects on the support and composite preference indices",
            dep_var = "Dependent variable",
            add_rows = index_rows, notes = c(index_notes, note_treat, note_sample))

# ---- 7c. Correction for multiple testing (Benjamini-Hochberg) -----------------
# Each table above tests 5 treatment arms on several outcomes. 
# With many tests (multiple testing), some effects may appear significant by chance alone. 
# The Benjamini-Hochberg method adjust the p-values for this reason within each family of
# related outcomes. 
# p-values are corrected within each family of outcomes (support, petition for, petition against, ...).
# models with any_treatment are single tests and not adjusted. 

bh_family <- function(models, family) {
  bind_rows(lapply(names(models), function(nm)
    broom::tidy(models[[nm]]) %>%
      filter(grepl("^treat\\d$", term)) %>%
      mutate(outcome = nm))) %>%
    mutate(family = family,
           treatment = unname(treat_labels[sub("treat", "", term)]),
           p_adj_BH = p.adjust(p.value, method = "BH")) %>%
    select(family, outcome, treatment, estimate, std.error, p.value, p_adj_BH)
}

main_effects_bh <- bind_rows(
  bh_family(models_support,       "Support"),
  bh_family(models_pet_for,       "Petition: sign for"),
  bh_family(models_pet_against,   "Petition: sign against"),
  bh_family(models_pet_undecided, "Petition: undecided"),
  bh_family(models_pet_for_incl,  "Petition: sign for (undecided kept)"),
  bh_family(models_index[c(1, 3)], "Indices")
)
print(main_effects_bh %>% mutate(across(where(is.numeric), ~ round(.x, 3))))
write.csv2(main_effects_bh, file.path("output", paste0(label, "_treatment_effects_BH.csv")),
           row.names = FALSE)
main_effects_bh %>%
  filter(p.value < 0.1) %>%
  arrange(p_adj_BH) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# ---- 7d. Robustness: nonlinear models ------------------------------------------

# The main models (7b) are linear regressions, which are easy to interpret, but
# treat the 1-7 support scale as if the distances between answers were equal; and
# can predict probabilities below 0 or above 1 for the petition outcomes. The
# checks below test whether the results depend on these choices.

# install.packages(c("ordinal", "marginaleffects"))   # only once

# (a) ordered logit for the six support items (1-7)
# treats the support scale as ordered categories without assuming equal distances between them.
models_support_ologit <- lapply(support_outcomes, function(y)
  ordinal::clm(as.formula(paste0("factor(", y, ", ordered = TRUE) ~ treat", rhs_fe)), data = df))
names(models_support_ologit) <- names(models_support)

modelsummary(models_support_ologit, stars = star_levels,
             coef_rename = rename_terms, coef_omit = "^[Cc]ountry|\\|",   # hide country and cut-points
             gof_map = "nobs",
             notes = "Ordered logit coefficients (log-odds); positive = higher support than control.")
export_word(models_support_ologit, out_file("robustness_support_ordered_logit.docx"),
            landscape = TRUE, coef_omit = "^[Cc]ountry|\\|", gof_map = "nobs",
            notes = "Ordered logit coefficients (log-odds); positive = higher support than control.")

# (b) logit for petition signing, reported as average marginal effects (percentage points)
#  the change in the probability of signing, in percentage points, as in the linear probability models 
# of 7b, but from a model that keeps predicted probabilities between 0 and 1.
logit_ame <- function(y) {
  m <- glm(as.formula(paste(y, "~ treat", rhs_fe)), data = df, family = binomial)
  marginaleffects::avg_slopes(m, variables = "treat")
}
ame_pet_for     <- lapply(paste0("petition_for_", apps), logit_ame)
ame_pet_against <- lapply(paste0("petition_against_", apps), logit_ame)
names(ame_pet_for) <- names(ame_pet_against) <- names(models_pet_for)

modelsummary(ame_pet_for, stars = star_levels, gof_map = "nobs", shape = term + contrast ~ model,
             notes = "Logit, average marginal effects: change in the probability of signing for.")
modelsummary(ame_pet_against, stars = star_levels, gof_map = "nobs", shape = term + contrast ~ model,
             notes = "Logit, average marginal effects: change in the probability of signing against.")

# (c) OLS: estimating treatment effects with control variables, outcome = outcome indices
# pre-treatment covariates and covariates not affected by treatment 
# prior knowledge of CCUS, climate concern were measured after treatment but are balance-checked 
# this should make the estimates more precise, but same expected values

# control variables used: age group, gender, education, income group, area type, prior knowledge of CCUS 
# and climate concern
covariates <- "+ age_group + Gender_new + Q7_f + income_q + Q11_f + Q14_f + Q34_f"

models_index_cov <- list(
  "Support index"  = lm_robust(as.formula(paste("support_index ~ treat", covariates, rhs_fe)), data = df),
  "Support index"  = lm_robust(as.formula(paste("support_index ~ any_treat", covariates, rhs_fe)), data = df),
  "Combined index" = lm_robust(as.formula(paste("combined_index ~ treat", covariates, rhs_fe)), data = df),
  "Combined index" = lm_robust(as.formula(paste("combined_index ~ any_treat", covariates, rhs_fe)), data = df)
)
# 
cov_omit <- paste0("^[Cc]ountry|^age_group|^Age group|^Gender|^Q7_f|^Education|^income_q|",
                   "^Household income|^Q11_f|^Area type|^Q14_f|^Prior knowledge|^Q34_f|^Climate concern")
cov_notes <- c(index_notes,
               paste("Controls: age group, gender, education, household income quartile, area type,",
                     "prior knowledge of CCUS and climate concern (coefficients not shown)."))

modelsummary(models_index_cov, stars = star_levels, gof_map = c("nobs", "r.squared"),
             coef_rename = rename_terms, coef_omit = cov_omit, notes = cov_notes)
export_word(models_index_cov, out_file("robustness_indices_covariates.docx"),
            coef_omit = cov_omit, notes = cov_notes)

# multiple-testing correction for the covariate models (same method as in 7b)
cov_effects_bh <- bh_family(models_index_cov[c(1, 3)], "Indices (with covariates)")
print(cov_effects_bh %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

# models with BH-adjusted p-values for the treatment arms (treat2 ... treat6), corrected
# jointly across all models in the list; stars in the table then follow the adjusted p-values
bh_adjust_models <- function(models) {
  tid  <- lapply(models, broom::tidy)
  is_t <- lapply(tid, function(t) grepl("^treat\\d$", t$term))
  padj <- p.adjust(unlist(Map(function(t, s) t$p.value[s], tid, is_t)), method = "BH")
  k <- 0
  out <- lapply(seq_along(models), function(i) {
    t <- tid[[i]]; s <- is_t[[i]]
    t$p.value[s] <- padj[k + seq_len(sum(s))]
    k <<- k + sum(s)
    structure(list(tidy = t, glance = broom::glance(models[[i]])), class = "modelsummary_list")
  })
  names(out) <- names(models)
  out
}

adj <- bh_adjust_models(models_index_cov[c(1, 3)])          # the two models with treatment arms
models_index_cov_bh <- list("Support index"  = adj[[1]],
                            "Support index"  = models_index_cov[[2]],   # any treatment: single test
                            "Combined index" = adj[[2]],
                            "Combined index" = models_index_cov[[4]])

bh_note <- paste("Stars for the treatment arms are based on p-values adjusted for multiple testing",
                 "(Benjamini-Hochberg) within this table; \"Any treatment\" is a single test and not adjusted.")

modelsummary(models_index_cov_bh, stars = star_levels, gof_map = c("nobs", "r.squared"),
             coef_rename = rename_terms, coef_omit = cov_omit, notes = c(cov_notes, bh_note))
export_word(models_index_cov_bh, out_file("robustness_indices_covariates_BH.docx"),
            coef_omit = cov_omit, notes = c(cov_notes, bh_note))

################################################################################
## 8. Cross-country comparison (pooled data only)
################################################################################
# Charts for the comparison chapter of the report: one summary per topic, with the
# three countries side by side. The full distributions per country come from
# running sections 1-7 with label = "DE", "NO" and "NL".
# Whether treatment effects differ significantly between countries: 03_targeting.R,
# section B2 (joint test for "Country").

if (is_pooled) {

  country_cols <- c(DE = "#584F80", NO = "#D08A4E", NL = "#0B0B0B")
  cty_levels   <- c("DE", "NO", "NL")

  # shared look of the comparison charts (dots per country)
  theme_cty <- function() {
    theme_minimal(base_size = 17) +
      theme(plot.title = element_text(colour = "#584F80", face = "bold"),
            plot.title.position = "plot",
            plot.caption = element_text(hjust = 0, colour = "#555555"),
            plot.caption.position = "plot",
            panel.grid.major.y = element_blank(),
            legend.position = "top",
            axis.text.y = element_text(size = 15, colour = "#0B0B0B"))
  }

  # mean / share with 95% confidence interval per country
  ci_summary <- function(d) d %>%
    summarise(mean = mean(value, na.rm = TRUE),
              se   = sd(value, na.rm = TRUE) / sqrt(sum(!is.na(value))),
              .groups = "drop") %>%
    mutate(lo = mean - 1.96 * se, hi = mean + 1.96 * se)


  # ---- 8.0 Sample and attitudes by country (Viewer and console) ----------------

  print(df %>%
          select(country, age_group, Gender_new, Q2, Q7) %>%
          tbl_summary(by = country,
                      label = lab_list(c("age_group", "Gender_new", "Q2", "Q7")),
                      missing_text = "Missing"))

  for (v in c("Q22", "Q34", "Q33", "party_bloc", "Q19r6",  "Q19r5","Q21", "Q36", "Q14", "Q15", "Q31NEW", "noanswerQ24_r99")) {
    cat("\n=====", nice_name(v), "by country =====\n")
    print(round(prop.table(table(df$country, df[[v]], useNA = "ifany"), 1) * 100, 1))
  }


  # ---- 8.1 Mean support by country ---------------------------------------------

  p <- df %>%
    select(country, all_of(c(support_outcomes, "support_index"))) %>%
    pivot_longer(-country, names_to = "outcome", values_to = "value") %>%
    group_by(country, outcome) %>%
    ci_summary() %>%
    mutate(country = factor(country, levels = cty_levels)) %>%
    ggplot(aes(x = mean, y = outcome, colour = country)) +
    geom_pointrange(aes(xmin = lo, xmax = hi),
                    position = position_dodge(width = 0.5), size = 0.8) +
    scale_colour_manual(values = country_cols) +
    scale_x_continuous(limits = c(1, 7), breaks = 1:7) +
    scale_y_discrete(limits = rev(c("support_index", support_outcomes)),
                     labels = c(expression(CO[2]~"storage"),
                                expression(CO[2]~"pipelines"),
                                expression(CO[2]~"utilisation"),
                                "Waste-to-energy with CCUS",
                                "Biomass energy with CCUS",
                                "CCUS in general",
                                expression(bold("Support index")))) +
    labs(x = "Mean support (1 = strongly oppose, 7 = strongly support)",
         y = NULL, colour = NULL,
         title = "Support for CCUS by country",
         caption = "Points: mean; lines: 95% confidence intervals. \"Don't know\" excluded.") +
    theme_cty()
  save_plot(p, "cty_support", width = 12, height = 7)


  # ---- 8.2 Net petition support by country ---------------------------------------
  # share signing for minus share signing against (all answers incl. undecided = 100%)
  
  app_labels <- c(Q27r1 = "Biomass plant with CCUS", Q27r2 = "Waste-to-energy plant with CCUS",
                  Q27r3 = "CO2 utilisation", Q27r4 = "CO2 pipeline", Q27r5 = "CO2 storage")
  p <- df %>%
    select(country, Q27r1 = Q27r1_num, Q27r2 = Q27r2_num, Q27r3 = Q27r3_num,
           Q27r4 = Q27r4_num, Q27r5 = Q27r5_num) %>%
    pivot_longer(-country, names_to = "item", values_to = "answer") %>%
    filter(answer %in% 1:4) %>%
    mutate(value = case_when(answer == 1 ~ 100,    # sign for
                             answer == 2 ~ -100,   # sign against
                             TRUE ~ 0)) %>%        # would not sign / undecided
    group_by(country, item) %>%
    ci_summary() %>%
    mutate(country = factor(country, levels = cty_levels)) %>%
    ggplot(aes(x = mean, y = item, colour = country)) +
    geom_vline(xintercept = 0, linetype = "dashed", colour = "#8C8C8C") +
    geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.5), size = 0.8) +
    scale_colour_manual(values = country_cols) +
    scale_y_discrete(limits = rev(names(app_labels)), labels = app_labels) +
    labs(x = "Net petition support", y = NULL, colour = NULL,
         title = "Net petition support by country",
         caption = paste0("Net petition support: share of respondents who would sign a petition for the project ",
                          "minus share who would sign against it.\n",
                          "Positive values: more support than opposition. ",
                          "Points: estimate; lines: 95% confidence intervals. ", 
                          "Shares are based on all respondents.")) +
    theme_cty()
  save_plot(p, "cty_petition", width = 12, height = 6)
  
  

  # ---- 8.3 Perceived vs. actual support by country -----------------------------
  # perceived: estimated share of fellow citizens supporting CCUS (Q24r1)
  # actual:    share of respondents with at least "somewhat support" for CCUS in general
  #            (support_gen >= 5 on the 1-7 scale; "Don't know" excluded)

  p <- bind_rows(
    df %>% transmute(country, type = "Actual share of supporters",
                     value = ifelse(is.na(support_gen), NA, (support_gen >= 5) * 100)),
    df %>% transmute(country, type = "Perceived share of supporters",
                     value = as.numeric(Q24r1))
  ) %>%
    group_by(country, type) %>%
    ci_summary() %>%
    mutate(country = factor(country, levels = rev(cty_levels))) %>%
    ggplot(aes(x = mean, y = country, colour = type)) +
    geom_line(aes(group = country), colour = "#B5B5B5", linewidth = 1.2) +
    geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.9) +
    geom_text(aes(label = paste0(round(mean), "%")), vjust = -1.2, size = 5, show.legend = FALSE) +
    scale_colour_manual(values = c("Actual share of supporters" = "#584F80",
                                   "Perceived share of supporters" = "#D08A4E")) +
    scale_x_continuous(limits = c(0, 100)) +
    labs(x = "Share of the population supporting CCUS (%)", y = NULL, colour = NULL,
         title = "Perceived vs. actual support for CCUS",
         caption = paste0("Actual: share of respondents who at least somewhat support CCUS in general.\n",
                          "Perceived: respondents' estimate of the share of fellow citizens who support CCUS ",
                          "(respondents who gave an estimate).")) +
    theme_cty()
  save_plot(p, "cty_perceived_vs_actual", width = 12, height = 5)


  # ---- 8.4 Expected consequences of CCUS by country -----------------------------------
  # share answering "likely" or "very likely" (all answers incl. "Don't know" = 100%)

  p <- df %>%
    select(country, Q19r1 = Q19r1_num, Q19r2 = Q19r2_num, Q19r3 = Q19r3_num,
           Q19r4 = Q19r4_num, Q19r5 = Q19r5_num, Q19r6 = Q19r6_num) %>%
    pivot_longer(-country, names_to = "item", values_to = "answer") %>%
    filter(answer %in% 1:6) %>%
    mutate(value = (answer %in% 1:2) * 100) %>%
    group_by(country, item) %>%
    ci_summary() %>%
    mutate(country = factor(country, levels = cty_levels)) %>%
    ggplot(aes(x = mean, y = item, colour = country)) +
    geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.5), size = 0.8) +
    scale_colour_manual(values = country_cols) +
    scale_y_discrete(limits = rev(names(benefit_labels)), labels = benefit_labels) +
    scale_x_continuous(limits = c(0, 100)) +
    labs(x = "Share answering \"likely\" or \"very likely\" (%)", y = NULL, colour = NULL,
         title = "Expected consequences of CCUS by country",
         caption = "Points: share; lines: 95% confidence intervals. All answers incl. \"Don't know\" = 100%.") +
    theme_cty()
  save_plot(p, "cty_benefits", width = 12, height = 7)


  # ---- 8.5 Important factors by country ----------------------------------------

  p <- df %>%
    select(country, all_of(names(factor_labels))) %>%
    mutate(across(-country, ~ as.numeric(is_selected(.x)) * 100)) %>%
    pivot_longer(-country, names_to = "item", values_to = "value") %>%
    group_by(country, item) %>%
    ci_summary() %>%
    mutate(country = factor(country, levels = cty_levels)) %>%
    ggplot(aes(x = mean, y = item, colour = country)) +
    geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.5), size = 0.8) +
    scale_colour_manual(values = country_cols) +
    scale_y_discrete(limits = rev(names(factor_labels)), labels = factor_labels) +
    scale_x_continuous(limits = c(0, 100)) +
    labs(x = "Share of respondents who selected the factor (%)", y = NULL, colour = NULL,
         title = "Important factors for the opinion on CCUS by country",
         caption = "Points: share; lines: 95% confidence intervals.") +
    theme_cty()
  save_plot(p, "cty_factors", width = 12, height = 7)


  # ---- 8.6 Budget allocation to emission strategies by country ----------------------------------------

  p <- df %>%
    select(country, all_of(names(budget_labels))) %>%
    mutate(across(-country, as.numeric)) %>%
    pivot_longer(-country, names_to = "item", values_to = "value") %>%
    group_by(country, item) %>%
    ci_summary() %>%
    mutate(country = factor(country, levels = cty_levels)) %>%
    ggplot(aes(x = mean, y = item, colour = country)) +
    geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.5), size = 0.8) +
    scale_colour_manual(values = country_cols) +
    scale_y_discrete(limits = rev(names(budget_labels)), labels = budget_labels) +
    labs(x = "Average share of the budget allocated (%)", y = NULL, colour = NULL,
         title = "Budget allocation to emission reduction strategies by country",
         caption = "Points: mean share; lines: 95% confidence intervals.") +
    theme_cty()
  save_plot(p, "cty_budget", width = 12, height = 7)


  # ---- 8.7 Treatment effects by country (support index) ------------------------
  # OLS regressions: outcome = support index
  # one column per country, each treatment vs. control

  models_cty <- lapply(cty_levels, function(cn)
    lm_robust(support_index ~ treat, data = subset(df, country == cn)))
  names(models_cty) <- cty_levels

  cty_notes <- c("Outcome: support index (1 = strongly oppose, 7 = strongly support).",
                 "Each column estimated separately for one country.",
                 "Reference category: Control.")

  print(modelsummary(models_cty,
                     stars = star_levels, gof_map = c("nobs", "r.squared","adj.r.squared"),
                     coef_rename = rename_terms, notes = cty_notes))
  export_word(models_cty, out_file("treatment_effects_by_country.docx"),
              title = "OLS regression: treatment effects on the support index by country",
              dep_var = "Dependent variable: support index (1 = strongly oppose, 7 = strongly support)",
              notes = cty_notes)
}



################################################################################
## 9. Correlation climate concern & political orientation with CCUS support
################################################################################

# ---- OLS regressions: outcome = support index. -------------
# independent variables of the model: level of climate concern and political orientation
# political orientation incl. "Other party / no party stated", so nobody is dropped;

# controls pre-treatment or tested to be independent of treatment
# control variables used: age group, gender, education, income group, area type, prior knowledge of CCUS, 
# and treatment group
controls <- "+ age_group + Gender_new + Q7_f + income_q + Q11_f + Q14_f + treat"

# 4 OLS regressions: 
# Model (1) includes climate concern only; model (2) political orientation only;
# Model (3) both climate concern and political orientation; model (4) additionally includes control variables.

models_drivers <- list(
  "(1)" = lm_robust(as.formula(paste("support_index ~ Q34_f", rhs_fe)), data = df),
  "(2)" = lm_robust(as.formula(paste("support_index ~ party_group", rhs_fe)), data = df),
  "(3)" = lm_robust(as.formula(paste("support_index ~ Q34_f + party_group", rhs_fe)), data = df),
  "(4)" = lm_robust(as.formula(paste("support_index ~ Q34_f + party_group", controls, rhs_fe)), data = df)
)

driver_rows  <- data.frame(term = c("Controls", "Country fixed effects"),
                           a = c("No", ifelse(is_pooled, "Yes", "No")),
                           b = c("No", ifelse(is_pooled, "Yes", "No")),
                           c = c("No", ifelse(is_pooled, "Yes", "No")),
                           d = c("Yes", ifelse(is_pooled, "Yes", "No")))

driver_omit <- paste0("^[Cc]ountry|^age_group|^Age group|^Gender|^Q7_f|^Education|^income_q|",
                      "^Household income|^Q11_f|^Area type|^Q14_f|^Prior knowledge|^treat|^Treatment")

driver_notes <- c(
  "Model (1) includes climate concern only; model (2) political orientation only;",
  "model (3) both climate concern and political orientation; model (4) additionally includes control variables.",
  "The support index is the mean of six support items (CCUS in general and five applications),",
  "at least four valid answers required; \"don't know\" answers are excluded.",
  "Climate concern: agreement that climate change is an important problem (reference: strongly agree).",
  "Political orientation: party blocs based on party preference (reference: left);",
  "\"right\" refers to populist-right parties, the mainstream centre-right is included in \"centre\";",
  "\"other party\" and \"no party stated\" are included as separate categories.",
  "Controls in model (4), coefficients not shown: age group, gender, education, household income quartile,",
  "area type, prior knowledge of CCUS and treatment group.",
  if (is_pooled) "Pooled data (DE, NO, NL) with country fixed effects (not shown)." else paste0("Sample: ", label, "."),
  "The coefficients describe associations, not causal effects.")

modelsummary(models_drivers,
             stars = star_levels, gof_map = c("nobs", "r.squared"),
             coef_rename = rename_terms, coef_omit = driver_omit,
             add_rows = driver_rows, notes = driver_notes)

export_word(models_drivers, out_file("support_drivers.docx"),
            title = "Climate concern, political orientation and support for CCUS (OLS regression)",
            dep_var = "Dependent variable: support index (1 = strongly oppose, 7 = strongly support)",
            coef_omit = driver_omit, add_rows = driver_rows, notes = driver_notes)