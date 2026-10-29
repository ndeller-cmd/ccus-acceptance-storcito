################################################################################
## 05_app_data.R
## Aggregated data per region for the map app, plus rule-based suggestions
## for communication (one profile and suggestion per region).
## Regions: DE federal states, NO landsdeler, NL provinces (from postcodes).
##
## Only aggregated values (shares and means per region) are exported. Small regions
## are combined with others.
## Uses df_de, df_no, df_nl from the analysis dataset (R/load_data.R) and the Dutch
## postcode-to-province table in data/lookup/ (included; tools/make_nl_lookup.R
## documents how it was built).
##
## Output (folder output/app; the results based on the full survey data are in results/app):
##   app_data_<country>.xlsx  sheet "overview" (readable) and "app_data" (for the app)
##   app_data_<country>.json  one row per region (+ one row for the country)
##   app_data_all.json        all countries together
##   app_messages.xlsx        the message texts (edit them below)
################################################################################

library(dplyr)
library(tidyr)
library(jsonlite)
library(openxlsx)

if (!exists("df_de")) source(file.path("R", "load_data.R"), encoding = "UTF-8")


# ---- Settings ----------------------------------------------------------------

flag_n    <- 30   # regions with fewer respondents are flagged in the overview
min_n_rec <- 30   # regions with fewer respondents get no suggestions

# regions per country:
#   var   = variable with the region (NA = country level only)
#   merge = small regions merged into a neighbour (regions with n<30)
region_cfg <- list(
  DE = list(var = "region", merge = c(),
            groups = c("Berlin"                 = "Brandenburg, Berlin, Mecklenburg-Vorpommern",
                       "Brandenburg"            = "Brandenburg, Berlin, Mecklenburg-Vorpommern",
                       "Mecklenburg-Vorpommern" = "Brandenburg, Berlin, Mecklenburg-Vorpommern",
                       "Hamburg"                = "Schleswig-Holstein and Hamburg",
                       "Schleswig-Holstein"     = "Schleswig-Holstein and Hamburg",
                       "Sachsen-Anhalt"         = "Sachsen and Sachsen-Anhalt",
                       "Sachsen"                = "Sachsen and Sachsen-Anhalt")),
  NO = list(var = "landsdel2024", merge = c()),
  NL = list(var = "nl_region", merge = c(),
            groups = c("Flevoland"  = "Gelderland and Flevoland",
                       "Gelderland" = "Gelderland and Flevoland"))
)

# Netherlands: province from the 4-digit postcode (lookup built from CBS tables)
nl_postcode_var <- "zipcode"                     # <- name of the postcode variable in df_nl
nl_lookup_file <- file.path("data", "lookup", "pc4_province_nl.csv")


add_nl_region <- function(d) {
  if (!nl_postcode_var %in% names(d) || !file.exists(nl_lookup_file)) {
    message("NL: postcode variable or lookup file not found -> country level only")
    d$nl_region <- NA_character_
    return(d)
  }
  lookup <- read.csv(nl_lookup_file, colClasses = "character")
  digits <- gsub("\\D", "", as.character(d[[nl_postcode_var]]))           # keep digits only
  digits <- ifelse(nchar(digits) == 5 & substr(digits, 1, 1) == "0",      # "06941" -> "6941"
                   substr(digits, 2, 5), digits)
  pc4    <- ifelse(nchar(digits) >= 4 & substr(digits, 1, 1) != "0",      # valid PC4 only
                   substr(digits, 1, 4), NA)
  d$nl_region <- lookup$province[match(pc4, lookup$pc4)]
  cat("NL: postcodes matched to a province:", sum(!is.na(d$nl_region)), "of", nrow(d), "\n")
  d
}

apps      <- c("biomass", "wte", "util", "pipe", "stor")      # order = Q27r1 ... Q27r5
app_names <- c(biomass = "Biomass with CCUS", wte = "Waste-to-energy with CCUS",
               util = "CO2 utilisation", pipe = "CO2 pipelines", stor = "CO2 storage")

src_keys  <- c("newspaper", "library_books", "search_engine", "generative_ai",
               "news_channels", "local_politicians", "friends_family")       # Q16r1-7 / Q17r1-7
src_names <- c("newspapers", "libraries / books", "search engines", "generative AI",
               "news channels", "local politicians", "neighbours, friends and family")

factor_keys <- c(Q20r1 = "risks_transport_storage", Q20r2 = "economy_jobs",
                 Q20r3 = "emission_reduction",      Q20r4 = "energy_use",
                 Q20r5 = "costs",                   Q20r6 = "fossil_fuels",
                 Q20r7 = "trust_institutions")
factor_names <- c(risks_transport_storage = "Risks of transport and storage",
                  economy_jobs = "Impact on economy and jobs",
                  emission_reduction = "Reduction of emissions",
                  energy_use = "Required energy use",
                  costs = "Costs of CCUS",
                  fossil_fuels = "Continued use of fossil fuels",
                  trust_institutions = "Trust in responsible institutions")


# ---- Message texts ----------------------------------------------

# one message per important factor
factor_msg <- c(
  risks_transport_storage = "Address safety concerns: provide information on monitoring, liability and actual risks of CO2 transport and storage.",
  economy_jobs            = "Address local economic effects (jobs, value added, tax revenue)",
  emission_reduction      = "Focus on the climate benefit: how much CO2 is avoided and how this is verified.",
  energy_use              = "Be transparent about the energy needed for capture and where it comes from.",
  costs                   = "Explain cost trajectories and who pays for them.",
  fossil_fuels            = "Present CCUS as a complement for hard-to-avoid emissions, not as a reason to delay the phase-out of fossil fuels.",
  trust_institutions      = "Make responsibilities clear and involve independent oversight and local actors.")

# messages triggered by the regional situation (thresholds in the rules below)
flag_msg <- c(
  contested      = "Contested here: {apps}. Engage early and offer real participation before decisions are made.",
  low_knowledge  = "Most residents know little about CCUS: start with transparent, neutral information.",
  undecided      = "Many residents are undecided: dialogue formats (information events, site visits) may be promising.",
  climate_frame  = "Climate concern is widespread: show how CCUS contributes to climate goals.")


# ---- Helpers -----------------------------------------------------------------

pct   <- function(x) round(mean(x, na.rm = TRUE) * 100, 1)
avg   <- function(x) round(mean(as.numeric(x), na.rm = TRUE), 2)
sel   <- function(x) ifelse(is.na(x), NA, !grepl("^NO TO", as.character(x), ignore.case = TRUE))  # selected?
agree <- function(x) {                       # "strongly agree" / "somewhat agree" / "agree"
  l <- tolower(as.character(x))
  ifelse(is.na(x) | grepl("know", l), NA, grepl("agree", l) & !grepl("disagree|neither", l))
}

# all indicators for one group of respondents
region_summary <- function(s) {
  out <- list(n = nrow(s))

  # support (1 = strongly oppose ... 7 = strongly support; "Don't know" excluded)
  out$mean_support_index <- avg(s$support_index)
  for (a in c("gen", apps)) {
    v <- s[[paste0("support_", a)]]
    out[[paste0("mean_support_", a)]] <- avg(v)
    out[[paste0("pct_support_", a)]]  <- pct(v >= 5)
    out[[paste0("pct_oppose_", a)]]   <- pct(v <= 3)
  }

  # petitions (shares of all respondents, incl. undecided)
  for (i in seq_along(apps)) {
    p <- s[[paste0("Q27r", i, "_num")]]
    p[!p %in% 1:4] <- NA
    out[[paste0("pct_petition_for_", apps[i])]]       <- pct(p == 1)
    out[[paste0("pct_petition_against_", apps[i])]]   <- pct(p == 2)
    out[[paste0("pct_petition_undecided_", apps[i])]] <- pct(p == 4)
  }

  # information sources used / trusted
  for (i in seq_along(src_keys)) {
    out[[paste0("pct_uses_", src_keys[i])]]   <- pct(sel(s[[paste0("Q16r", i)]]))
    out[[paste0("pct_trusts_", src_keys[i])]] <- pct(sel(s[[paste0("Q17r", i)]]))
  }

  # important factors for the opinion on CCUS
  for (f in names(factor_keys)) out[[paste0("pct_factor_", factor_keys[[f]])]] <- pct(sel(s[[f]]))

  # climate concern and prior knowledge
  out$pct_climate_concern <- pct(agree(s$Q34_f))
  out$pct_low_knowledge   <- pct(grepl("little|no knowledge", tolower(as.character(s$Q14))) &
                                   !is.na(s$Q14))
  as_tibble(out)
}

# rule-based suggestions for one region (one row of the table)

recommend <- function(r) {
  if (isTRUE(r$n < min_n_rec)) return(list(top_factors = NA, top_channels = NA, suggestions =
                                             paste0("Too few respondents (n = ", r$n, ") for reliable suggestions")))
  # applications with at least 20% opposition or 15% signing a petition against
  contested_apps <- unname(app_names[apps][sapply(apps, function(a)
    isTRUE(r[[paste0("pct_oppose_", a)]] >= 20) ||
      isTRUE(r[[paste0("pct_petition_against_", a)]] >= 15))])
  f  <- unlist(r[paste0("pct_factor_", factor_keys)]);  names(f)  <- factor_keys
  tr <- unlist(r[paste0("pct_trusts_", src_keys)]);     names(tr) <- src_names
  top_f  <- names(sort(f,  decreasing = TRUE))[1:3]
  top_tr <- names(sort(tr, decreasing = TRUE))[1:2]

  msgs <- c(
    unname(factor_msg[top_f]),
    if (length(contested_apps) > 0)
      sub("{apps}", paste(contested_apps, collapse = ", "), flag_msg[["contested"]], fixed = TRUE),
    if (isTRUE(r$pct_low_knowledge >= 60)) flag_msg[["low_knowledge"]],
    if (isTRUE(mean(unlist(r[paste0("pct_petition_undecided_", apps)])) >= 25)) flag_msg[["undecided"]],
    if (isTRUE(r$pct_climate_concern >= 70)) flag_msg[["climate_frame"]]
  )
  list(top_factors     = paste(factor_names[top_f], collapse = "; "),
       top_channels    = paste(top_tr, collapse = "; "),
       suggestions = paste(msgs, collapse = " | "))
}


# ---- Build the tables ----------------------------------------------------------

support_items <- c("support_gen", paste0("support_", apps))

build_country <- function(d, cn) {
  cfg <- region_cfg[[cn]]
  if (cn == "NL") d <- add_nl_region(d)

  # support index (same definition as in 02_analysis.R)
  d$support_index <- ifelse(rowSums(!is.na(d[support_items])) >= 4,
                            rowMeans(d[support_items], na.rm = TRUE), NA)

  # region, with small regions merged into a neighbour
  if (!is.na(cfg$var) && cfg$var %in% names(d)) {
    r <- as.character(d[[cfg$var]])
    if (!is.null(cfg$groups)) r <- ifelse(r %in% names(cfg$groups), unname(cfg$groups[r]), r)
    for (s in names(cfg$merge)) r[r %in% c(s, cfg$merge[[s]])] <- paste(cfg$merge[[s]], "+", s)
    d$region_app <- r
  } else {
    if (!is.na(cfg$var)) message(cn, ": variable '", cfg$var, "' not found -> country level only")
    d$region_app <- NA_character_
  }

  regional <- if (all(is.na(d$region_app))) NULL else
    d %>% filter(!is.na(region_app)) %>%
      group_by(region = region_app) %>% group_modify(~ region_summary(.x)) %>% ungroup()
  national <- region_summary(d) %>% mutate(region = "All regions", .before = 1)

  tab <- bind_rows(regional, national) %>%
    mutate(country = cn, .before = 1,
           small_sample = n < flag_n)
  
  # overall assessment of the support index (1-7 scale, 4 = neutral)
  tab <- tab %>%
    mutate(support_level = case_when(
      is.na(mean_support_index)   ~ NA_character_,
      mean_support_index >= 5.5   ~ "Broad support",
      mean_support_index >= 4.5   ~ "Moderate support",
      mean_support_index >= 4     ~ "Divided views",
      TRUE                        ~ "Predominantly opposed"),
      support_colour = recode(support_level,
                              "Broad support" = "green", "Moderate support" = "yellow",
                              "Divided views" = "red", "Predominantly opposed" = "red"),
      support_message = recode(support_level,
                               "Broad support"         = "Most residents support CCUS; opposition is limited.",
                               "Moderate support"      = "Residents are moderately supportive; many have mixed or undecided views.",
                               "Divided views"         = "Support and scepticism are close to balanced; acceptance cannot be taken for granted.",
                               "Predominantly opposed" = "More residents oppose than support CCUS; acceptance is a major challenge."))
  # comparison with the national average (support index)
  national_mean <- tab$mean_support_index[tab$region == "All regions"]
  tab <- tab %>%
    mutate(diff_to_national = round(mean_support_index - national_mean, 2),
           comparison = case_when(
             region == "All regions" | is.na(diff_to_national) ~ NA_character_,
             diff_to_national >=  0.15 ~ "Above national average",
             diff_to_national <= -0.15 ~ "Below national average",
             TRUE                      ~ "Around national average"),
           comparison_message = ifelse(is.na(comparison), NA,
                                       paste0(comparison, " (", sprintf("%+.2f", diff_to_national),
                                              " points on the support index; national average: ",
                                              sprintf("%.2f", national_mean), ").")))

  rec <- bind_rows(lapply(seq_len(nrow(tab)), function(i) as_tibble(recommend(tab[i, ]))))
  bind_cols(tab, rec)
}

app_data <- bind_rows(
  build_country(df_de, "DE"),
  build_country(df_no, "NO"),
  build_country(df_nl, "NL")
)

# overview: respondents per region and the recommendations
print(app_data %>% select(country, region, n, small_sample, top_factors, top_channels), n = Inf)


# ---- Export --------------------------------------------------------------------
# install.packages("openxlsx")   # only once
#   app_data_<country>.xlsx  sheet "overview": one row per indicator, one column per region
#                            sheet "app_data": one row per region (format for the app)
#   app_data_<country>.json  the same data for the app
#   app_messages.xlsx        the message texts

app_dir <- file.path("output", "app")
dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)

# readable indicator names for the overview sheet
nice_indicator <- function(x) {
  x <- sub("^pct_",  "% ", x)
  x <- sub("^mean_", "Mean ", x)
  for (a in names(app_names)) x <- sub(paste0("_", a, "$"), paste0(": ", app_names[[a]]), x)
  x <- sub("_gen$", ": CCUS in general", x)
  gsub("_", " ", x)
}

write_country_xlsx <- function(d, file) {
  wb <- openxlsx::createWorkbook()
  
  # overview: indicators as rows, regions as columns
  # regions with fewer than flag_n respondents are marked in the header and highlighted
  ov <- d %>% select(-any_of(c("country", "small_sample", "suppressed")))
  small   <- d$n < flag_n
  regions <- ifelse(small, paste0(ov$region, " (n < ", flag_n, ")"), ov$region)
  ov <- as.data.frame(t(ov[, -1]), stringsAsFactors = FALSE)
  names(ov) <- regions
  ov <- cbind(Indicator = nice_indicator(rownames(ov)), ov)
  openxlsx::addWorksheet(wb, "overview")
  openxlsx::writeData(wb, "overview", ov, headerStyle = openxlsx::createStyle(textDecoration = "bold"))
  openxlsx::freezePane(wb, "overview", firstRow = TRUE, firstCol = TRUE)
  openxlsx::setColWidths(wb, "overview", cols = 1, widths = 45)
  openxlsx::setColWidths(wb, "overview", cols = 2:ncol(ov), widths = 22)
  openxlsx::addStyle(wb, "overview", openxlsx::createStyle(wrapText = TRUE, valign = "top"),
                     rows = 1:(nrow(ov) + 1), cols = 2:ncol(ov), gridExpand = TRUE)
  
  # app format: one row per region
  openxlsx::addWorksheet(wb, "app_data")
  openxlsx::writeData(wb, "app_data", d, headerStyle = openxlsx::createStyle(textDecoration = "bold"))
  openxlsx::freezePane(wb, "app_data", firstRow = TRUE)
  
  openxlsx::activeSheet(wb) <- "overview"
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

for (cn in unique(app_data$country)) {
  d <- filter(app_data, country == cn)
  write_country_xlsx(d, file.path(app_dir, paste0("app_data_", cn, ".xlsx")))
  write_json(d, file.path(app_dir, paste0("app_data_", cn, ".json")), pretty = TRUE, na = "null")
}
write_json(app_data, file.path(app_dir, "app_data_all.json"), pretty = TRUE, na = "null")

openxlsx::write.xlsx(bind_rows(tibble(type = "factor", key = names(factor_msg), message = unname(factor_msg)),
                               tibble(type = "flag",   key = names(flag_msg),   message = unname(flag_msg))),
                     file.path(app_dir, "app_messages.xlsx"), overwrite = TRUE)

