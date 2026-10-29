################################################################################
## 03_targeting.R
## Heterogeneity and targeting potential analyses. 
## This R file investigates whether support for CCUS and the effects of the 
## information treatments differ between groups of respondents, 
## and whether it would be worthwhile to target the information at specific groups
## Part 1: Pre-defined subgroups (OLS regression analysis)
##   A.  Where is support high or low?            (levels by subgroup, all outcomes)
##   M.  Map of support levels                    (Germany only)
##   B.  For whom does which treatment work?      (effects by subgroup and arm)
##   B2. Do the effects differ between groups?    (interaction tests, balance checks)
##   C.  Intervention potential                   (low support + positive effect)
##   D.  Targeting table                          (summary per subgroup)
##   E.  Interactive potential chart              (support index)
## Part 2: Forests (machine-learning methods: data-driven)
##   F0. settings and data
##   F1. Random forest: which characteristics predict the level of support?
##   F2. Causal forest: do treatment effects differ between subgroups?
##       average treatment effects, predicted effect per respondent, 
##       calibration test, sample split used in F3-F5
##   F3. Effects by quartile of the predicted effect, held-out calibration test, 
##       distribution of predicted effects
##   F4. Is targeting worthwhile? RATE, TOC, AUTOC
##   F5. Policy tree: targeting rule.
##       Sensitive characteristics can be excluded from the rule. 
##   F6. Summary table of machine-learning results. 
##
## Run 02_analysis.R first (for the same dataset). This script uses df, label,
## is_pooled, rhs_fe, save_plot(), export_word(), treat_labels, var_labels,
## nice_name() and rename_terms() from 02_analysis.R, so it runs for whichever
## dataset is selected there ("DE", "NO", "NL" or "ALL").
##
## Output: figures and tables in output/ (file names starting with the dataset
## label, e.g. ALL_targeting_B2_heterogeneity.png, ALL_forest_summary.csv). The
## results based on the full survey data are in results/.
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(estimatr)
library(broom)
library(modelsummary)
# install.packages("ggrepel")   # only once (for the labels in plot C)

stopifnot(exists("df"), exists("label"), exists("save_plot"),
          exists("treat_labels"), exists("nice_name"), exists("rename_terms"))


# ---- Settings ----------------------------------------------------------------

# Define minimum group sizes for results 
min_n     <- 30   # smallest subgroup shown (smaller groups give unreliable estimates)
min_n_arm <- 10   # smallest number of respondents per treatment arm AND in the control
                  # group within a subgroup; effects below this are not estimated (grey)

# Define settings for the figures and tables
outcomes <- c("support_gen", "support_biomass", "support_wte",
              "support_util", "support_pipe", "support_stor", "support_index")

# labels with a proper subscript for CO2 (plotmath)
outcome_plotmath <- c(support_gen     = "'CCUS in general'",
                      support_biomass = "'Biomass + CCUS'",
                      support_wte     = "'Waste-to-energy + CCUS'",
                      support_util    = "CO[2]~'utilisation'",
                      support_pipe    = "CO[2]~'pipelines'",
                      support_stor    = "CO[2]~'storage'",
                      support_index   = "bold('Support index')")

# two-line versions for the column headings of the heatmap
outcome_header <- c(support_gen     = "atop('CCUS', 'in general')",
                    support_biomass = "atop('Biomass', '+ CCUS')",
                    support_wte     = "atop('Waste-to-', 'energy + CCUS')",
                    support_util    = "atop(CO[2], 'utilisation')",
                    support_pipe    = "atop(CO[2], 'pipelines')",
                    support_stor    = "atop(CO[2], 'storage')",
                    support_index   = "atop(bold('Support'), bold('index'))")

# treatment names (from treat_labels in 02_analysis.R)
treat_names <- c(setNames(unname(treat_labels[-1]), paste0("treat", 2:6)),
                 any_treat = "Any treatment")

# Select subgroups for regression analyses with pre-defined subgroups: 
# political orientation in all datasets, country only in the pooled data
# prior knowledge (Q14) and climate concern (Q34) were measured after the treatment:
# the balance checks are in section B2
mod_vars <- c("age_group", "Gender_new", "Q7_f", "Q11_f", "Q14_f", "Q34_f", "party_bloc")
if (is_pooled) mod_vars <- c(mod_vars, "country")
mod_list <- setNames(nice_name(mod_vars), mod_vars)

# Storcito colour scheme: orange = below average / negative, lilac = above average / positive
col_low  <- "#C0703A"
col_mid  <- "#F4F3F8"
col_high <- "#584F80"


# ---- Make sure the index and the pooled treatment exist ----------------------

if (!"support_index" %in% names(df)) {
  items <- setdiff(outcomes, "support_index")
  df$support_index <- ifelse(rowSums(!is.na(df[items])) >= 4,
                             rowMeans(df[items], na.rm = TRUE), NA)
}
df$any_treat <- as.numeric(df$treat != "1")


# ---- Long data: one block of rows per subgroup variable ----------------------

long <- bind_rows(lapply(names(mod_list), function(m)
  df %>%
    filter(!is.na(.data[[m]])) %>%
    transmute(moderator = mod_list[[m]],
              group     = as.character(.data[[m]]),
              treat, any_treat,
              country   = as.character(country),
              across(all_of(outcomes)))))

# keep the original order of the categories (top to bottom)
group_levels <- unique(unlist(lapply(names(mod_list), function(m)
  levels(droplevels(as.factor(df[[m]]))))))
long$group     <- factor(long$group, levels = rev(group_levels))
long$moderator <- factor(long$moderator, levels = unname(mod_list))

# shared look of the heatmaps
theme_heat <- function() {
  theme_minimal(base_size = 15) +
    theme(panel.grid = element_blank(),
          strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 1),
          axis.text.x.top = element_text(colour = "#0B0B0B", face = "bold"),
          axis.text.y = element_text(colour = "#0B0B0B"),
          panel.spacing.y = unit(0.6, "lines"),
          plot.title = element_text(face = "bold", margin = margin(b = 15)),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0, colour = "#555555"),
          plot.caption.position = "plot",
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"))
}

# colour scale capped at the 95th percentile, so single extreme values
# (usually from small groups) don't make everything else look pale
cap_at <- function(x) as.numeric(quantile(abs(x), 0.95, na.rm = TRUE))


################################################################################
## Part 1: Regression analyses
################################################################################
##
################################################################################
## A. Where is support high or low? (all respondents, all outcomes)
################################################################################

# Heatmap visualizing among which subgroups which level of support for CCUS exists
# (Outcome Heterogeneity)

overall_mean <- sapply(df[outcomes], mean, na.rm = TRUE)

levels_tab <- long %>%
  pivot_longer(all_of(outcomes), names_to = "outcome", values_to = "value") %>%
  group_by(moderator, group, outcome) %>%
  summarise(mean = mean(value, na.rm = TRUE),
            n = sum(!is.na(value)), .groups = "drop") %>%
  filter(n >= min_n) %>%
  mutate(diff = mean - overall_mean[outcome],
         outcome = factor(outcome, levels = outcomes))

lim_a <- cap_at(levels_tab$diff)

p <- ggplot(levels_tab, aes(x = outcome, y = group, fill = diff)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", mean),
                colour = abs(diff) > 0.6 * lim_a), size = 3.6, show.legend = FALSE) +
  scale_colour_manual(values = c(`FALSE` = "#0B0B0B", `TRUE` = "white"), guide = "none") +
  scale_fill_gradient2(low = col_low, mid = col_mid, high = col_high,
                       midpoint = 0, limits = c(-lim_a, lim_a), oob = scales::squish,
                       name = "Difference to average (points)") +
  scale_x_discrete(labels = parse(text = outcome_header[outcomes]), position = "top") +
  facet_grid(moderator ~ ., scales = "free_y", space = "free_y", switch = "y") +
  labs(x = NULL, y = NULL,
       title = "Support by subgroup",
       caption = paste0("Cells: mean support (1 = strongly oppose, 7 = strongly support).\n",
                        "Colour: difference to the overall mean. Groups with n < ", min_n, " not shown.")) +
  theme_heat()
save_plot(p, "targeting_A_support_levels", width = 14,
          height = 3 + 0.35 * n_distinct(paste(levels_tab$moderator, levels_tab$group)))

# Are the differences between groups larger than chance? (joint F-test, BH-corrected)
group_tests <- expand.grid(moderator = levels(long$moderator), outcome = outcomes,
                           stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(p_value = {
    d <- long[long$moderator == moderator & !is.na(long[[outcome]]), ]
    d <- d[d$group %in% names(which(table(d$group) >= min_n)), ]
    f <- lm_robust(as.formula(paste(outcome, "~ group")), data = d)$fstatistic
    pf(f[1], f[2], f[3], lower.tail = FALSE)
  }) %>%
  ungroup() %>%
  mutate(p_adj_BH = round(p.adjust(p_value, method = "BH"), 3))

# p-values (corrected) for "the groups differ": rows = subgroup variable, columns = outcome
print(group_tests %>%
        select(moderator, outcome, p_adj_BH) %>%
        pivot_wider(names_from = outcome, values_from = p_adj_BH))


################################################################################
## M. Map of support levels (Germany only)
################################################################################
# ---- Level of support by federal state ----------
# Outcome Heterogeneity on a German map; subgroups are the different states

if (label == "DE" && "region" %in% names(df)) {  
  map_shape <- giscoR::gisco_get_nuts(country = "DE", nuts_level = 1, resolution = "10")

  map_vals <- df %>%
    mutate(region = as.character(region)) %>%
    pivot_longer(all_of(outcomes), names_to = "outcome", values_to = "value") %>%
    group_by(region, outcome) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              n = sum(!is.na(value)), .groups = "drop") %>%
    mutate(mean = ifelse(n < 20, NA, mean),            # too few respondents -> grey
           diff = mean - overall_mean[outcome])

  map_data <- map_shape %>%
    inner_join(map_vals, by = c("NUTS_NAME" = "region")) %>%
    mutate(outcome = factor(outcome, levels = outcomes,
                            labels = outcome_plotmath[outcomes]))

  lim_m <- max(abs(map_data$diff), na.rm = TRUE)

  p <- ggplot(map_data) +
    geom_sf(aes(fill = diff), colour = "white", linewidth = 0.3) +
    geom_sf_text(aes(label = ifelse(is.na(mean), "", sprintf("%.1f", mean))),
                 size = 2.8, colour = "#0B0B0B") +
    scale_fill_gradient2(low = col_low, mid = col_mid, high = col_high,
                         midpoint = 0, limits = c(-lim_m, lim_m), na.value = "#D9D9D9",
                         name = "Difference to\nnational mean") +
    facet_wrap(~ outcome, ncol = 4, labeller = label_parsed) +
    labs(caption = "Numbers: mean support (1 = strongly oppose, 7 = strongly support). Grey: fewer than 20 respondents.") +
    theme_void(base_size = 14) +
    theme(strip.text = element_text(face = "bold", margin = margin(b = 4)),
          legend.position = "bottom",
          plot.caption = element_text(hjust = 0, colour = "#555555"))
  save_plot(p, "targeting_map_DE", width = 14, height = 9)



  # ---- Treatment effect by region (North / East / South / West) ----------
  # Federal states are too small for treatment effects (about 10 control
  # respondents in many states), so states are grouped into four regions.

  macro <- c("Schleswig-Holstein" = "North", "Hamburg" = "North",
             "Niedersachsen" = "North", "Bremen" = "North",
             "Berlin" = "East", "Brandenburg" = "East", "Mecklenburg-Vorpommern" = "East",
             "Sachsen" = "East", "Sachsen-Anhalt" = "East", "Thüringen" = "East",
             "Nordrhein-Westfalen" = "West", "Hessen" = "West",
             "Rheinland-Pfalz" = "West", "Saarland" = "West",
             "Bayern" = "South", "Baden-Württemberg" = "South")

  df_macro <- df %>%
    mutate(macro = unname(macro[as.character(region)])) %>%
    filter(!is.na(macro), !is.na(support_index))

  te_macro <- df_macro %>%
    group_by(macro) %>%
    group_modify(~ tidy(lm_robust(support_index ~ any_treat, data = .x)) %>%
                   filter(term == "any_treat") %>%
                   mutate(n = nrow(.x),
                          baseline = mean(.x$support_index[.x$any_treat == 0]))) %>%
    ungroup()
  print(te_macro %>% select(macro, n, baseline, estimate, conf.low, conf.high, p.value))

  # do the effects differ between the regions? (joint test of the interactions)
  m_macro <- lm_robust(support_index ~ any_treat * macro, data = df_macro)
  test_macro <- car::linearHypothesis(m_macro, grep("^any_treat:", names(coef(m_macro)), value = TRUE))
  p_macro <- test_macro[2, ncol(test_macro)]

  macro_shape <- map_shape %>%
    mutate(macro = unname(macro[NUTS_NAME])) %>%
    group_by(macro) %>%
    summarise(.groups = "drop") %>%                  # merges the states of each region
    left_join(te_macro, by = "macro")

  te_lim <- max(abs(macro_shape$estimate), na.rm = TRUE)

  p <- ggplot() +
    geom_sf(data = macro_shape, aes(fill = estimate), colour = "white", linewidth = 1.2) +
    geom_sf(data = map_shape, fill = NA, colour = "white", linewidth = 0.2) +   # state borders
    geom_sf_text(data = macro_shape,
                 aes(label = paste0(macro, "\n", sprintf("%+.2f", estimate),
                                    "\n[", sprintf("%.2f", conf.low), ", ",
                                    sprintf("%.2f", conf.high), "]\nn = ", n)),
                 size = 4, colour = "#0B0B0B") +
    scale_fill_gradient2(low = col_low, mid = col_mid, high = col_high,
                         midpoint = 0, limits = c(-te_lim, te_lim),
                         name = "Effect of any treatment (points)") +
    labs(title = "Treatment effect on the support index by region",
         caption = paste0("Effect of any treatment vs. control within each region, with 95% confidence interval.\n",
                          "Test whether the effects differ between regions: p = ", sprintf("%.2f", p_macro), ".")) +
    theme_void(base_size = 16) +
    theme(plot.title = element_text(face = "bold", margin = margin(b = 10)),
          plot.caption = element_text(hjust = 0, colour = "#555555"),
          legend.position = "bottom",
          legend.key.width = unit(2, "cm"))
  save_plot(p, "map_DE_treatment_effect", width = 8, height = 10)
}

# joint test per subgroup variable: does the support index differ between the groups?
test_vars <- mod_list   # same subgroup variables as the heatmap (country only in the pooled data)

support_tests <- bind_rows(lapply(names(test_vars), function(v) {
  fe <- if (is_pooled && v != "country") " + country" else ""
  m  <- lm_robust(as.formula(paste("support_index ~", v, fe)), data = df)
  terms <- grep(paste0("^", v), names(coef(m)), value = TRUE)
  w  <- car::linearHypothesis(m, paste(terms, "= 0"), test = "F")
  tibble(variable = test_vars[[v]], F = round(w$F[2], 2), p = w$`Pr(>F)`[2])
})) %>%
  mutate(p_adj_BH = p.adjust(p, method = "BH"),
         across(c(p, p_adj_BH), ~ signif(.x, 2)))
print(support_tests)


################################################################################
## B. For whom does which treatment work? (effects by subgroup and arm)
################################################################################

# Heatmap of average treatment effects
# treatment effects within one subgroup: each arm vs. control + pooled treatment
fit_arms <- function(d, y) {
  fe <- if (is_pooled && n_distinct(d$country) > 1) " + country" else ""
  arms   <- tidy(lm_robust(as.formula(paste(y, "~ treat", fe)), data = d))
  pooled <- tidy(lm_robust(as.formula(paste(y, "~ any_treat", fe)), data = d))
  # respondents with an answer per arm ("1" = control) -> too small arms are not estimated
  n_by_arm <- table(factor(d$treat[!is.na(d[[y]])], levels = c("1", 2:6)))
  n_cmp <- c(setNames(as.numeric(n_by_arm[2:6]), paste0("treat", 2:6)),
             any_treat = sum(n_by_arm[2:6]))
  bind_rows(arms, pooled) %>%
    filter(term %in% names(treat_names)) %>%
    mutate(n_arm     = unname(n_cmp[term]),
           too_small = n_arm < min_n_arm | n_by_arm[["1"]] < min_n_arm,
           across(c(estimate, std.error, statistic, p.value, conf.low, conf.high),
                  ~ ifelse(too_small, NA, .x))) %>%
    select(-too_small) %>%
    mutate(n         = sum(!is.na(d[[y]])),
           n_control = sum(!is.na(d[[y]]) & d$treat == "1"),
           baseline  = mean(d[[y]][d$treat == "1"], na.rm = TRUE),   # support without treatment
           # support of all respondents in the group: used for targeting (plots C, E and
           # table D). The control mean is also part of the effect estimate, so groups with
           # a low control mean by chance would automatically show larger effects; the mean
           # of the whole group does not share this noise with the effect.
           group_mean = mean(d[[y]], na.rm = TRUE))
}

cate_all <- bind_rows(lapply(outcomes, function(y)
  long %>%
    filter(!is.na(.data[[y]])) %>%
    group_by(moderator, group) %>%
    filter(n() >= min_n) %>%
    group_modify(~ fit_arms(.x, y)) %>%
    ungroup() %>%
    mutate(outcome = y))) %>%
  mutate(treatment = factor(treat_names[term], levels = treat_names)) %>%
  group_by(outcome) %>%
  mutate(p_adj_BH = p.adjust(p.value, method = "BH")) %>%   # correction within each outcome
  ungroup() %>%
  mutate(sig = case_when(p_adj_BH < 0.05 ~ "**",
                         p.value  < 0.05 ~ "*",
                         TRUE ~ ""))

plot_effects_heatmap <- function(y) {
  d <- cate_all[cate_all$outcome == y, ]
  lim <- cap_at(d$estimate)
  ggplot(d, aes(x = treatment, y = group, fill = estimate)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = ifelse(is.na(estimate), "",
                                 paste0(sprintf("%.2f", estimate), sig)),
                  colour = !is.na(estimate) & abs(estimate) > 0.6 * lim),
              size = 3.6, show.legend = FALSE) +
    scale_colour_manual(values = c(`FALSE` = "#0B0B0B", `TRUE` = "white"), guide = "none") +
    scale_fill_gradient2(low = col_low, mid = col_mid, high = col_high,
                         midpoint = 0, limits = c(-lim, lim), na.value = "#D9D9D9",
                         oob = scales::squish, name = "Treatment effect (points)") +
    scale_x_discrete(position = "top", labels = scales::label_wrap(14)) +
    facet_grid(moderator ~ ., scales = "free_y", space = "free_y", switch = "y") +
    labs(x = NULL, y = NULL,
         title = parse(text = outcome_plotmath[y])[[1]],
         caption = paste0("Cells: effect vs. control within the group (points on the 1-7 scale).\n",
                          "* p < 0.05 before correction, ** p < 0.05 after Benjamini-Hochberg correction.\n",
                          "Grey: not estimable. Colours capped at the 95th percentile.")) +
    theme_heat()
}

for (y in outcomes) {
  n_rows <- n_distinct(paste(cate_all$moderator, cate_all$group)[cate_all$outcome == y])
  save_plot(plot_effects_heatmap(y), paste0("targeting_B_effects_", y),
            width = 13, height = 3.5 + 0.35 * n_rows)
}


################################################################################
## B2. Do the treatment effects differ between groups? (interaction tests)
################################################################################
# Heatmap B shows the effect within each group. Whether the effect in one group is
# significantly different from another group is tested here with interaction
# regressions (treatment x subgroup variable), outcome: support index.
# Country: the joint test answers "do the treatment effects differ between countries?"

het_outcome <- "support_index"

# ---- Balance checks ------------------------------------------------------------
# Prior knowledge (Q14) and climate concern (Q34) were asked AFTER the treatment.
# They can only be used as moderators if the treatment did not change them:
# p > 0.05 in both tests -> no sign that the treatment affected the moderator.
for (m in c("Q14_f", "Q34_f")) {
  cat("\n===== Balance check:", nice_name(m), "=====\n")
  print(summary(lm_robust(as.formula(paste0("as.numeric(", m, ") ~ any_treat", rhs_fe)), data = df)))
  print(chisq.test(table(df$any_treat, df[[m]])))
}

# ---- Interaction regressions (OLS) ------------------------------------------------
# country is already in rhs_fe for the pooled data, so it is not added twice
fe_for <- function(z) if (z == "country") "" else rhs_fe

# respondents per category and treatment group (cells < ~10 give unreliable estimates)
for (m in mod_vars) {
  cat("\n=====", mod_list[[m]], "=====\n")
  print(table(df$treat_label, df[[m]], useNA = "ifany"))
}

# groups that are too small for an interaction test: if a treatment arm or the
# control group has fewer than min_n_arm respondents in a group, that group is left
# out of the tests (as in heatmap B). Otherwise the interaction term fits one or two
# people exactly, the robust variance is close to zero and the test gives p = 0.
df_het <- df
# political orientation: "Other" (mix of parties not assigned to a bloc) is left out 
# because it does not provide a valid comparison 
df_het$party_bloc[df_het$party_bloc == "Other"] <- NA
df_het$party_bloc <- droplevels(df_het$party_bloc)
excluded_groups <- setNames(rep("", length(mod_vars)), mod_vars)
for (m in mod_vars) {
  cells <- table(df$treat, df[[m]])
  small <- colnames(cells)[apply(cells, 2, min) < min_n_arm]
  if (length(small) > 0) {
    df_het[[m]][as.character(df_het[[m]]) %in% small] <- NA
    if (is.factor(df_het[[m]])) df_het[[m]] <- droplevels(df_het[[m]])
    excluded_groups[[m]] <- paste(small, collapse = "; ")
    cat("Interaction tests,", mod_list[[m]], "- left out (fewer than", min_n_arm,
        "respondents in an arm):", excluded_groups[[m]], "\n")
  }
}
# variables with at least two remaining groups
het_vars <- mod_vars[sapply(mod_vars, function(m) length(unique(na.omit(df_het[[m]]))) >= 2)]

# reference category for the interaction tests = the largest group of each characteristic,
# so the comparisons in the figure are as precise as possible (the joint tests do not change)
for (m in setdiff(het_vars, "country")) {
  if (is.factor(df_het[[m]])) df_het[[m]] <- forcats::fct_infreq(df_het[[m]])
}

# each treatment arm x subgroup variable
models_het <- lapply(het_vars, function(z)
  lm_robust(as.formula(paste(het_outcome, "~ treat *", z, fe_for(z))), data = df_het))
names(models_het) <- mod_list[het_vars]

modelsummary(models_het,
             stars = star_levels, gof_map = c("nobs", "r.squared"),
             coef_rename = rename_terms, coef_omit = "^country[A-Z]|^Country: ")

# any treatment x subgroup variable (more power: all treated vs. control)
models_het_pooled <- lapply(het_vars, function(z)
  lm_robust(as.formula(paste(het_outcome, "~ any_treat *", z, fe_for(z))), data = df_het))
names(models_het_pooled) <- mod_list[het_vars]

modelsummary(models_het_pooled,
             stars = star_levels, gof_map = c("nobs", "r.squared"),
             coef_rename = rename_terms, coef_omit = "^country[A-Z]|^Country: ")
export_word(models_het_pooled, file.path("output", paste0(label, "_heterogeneity_interactions.docx")),
            landscape = TRUE)

# ---- Joint tests: does the effect differ at all across the groups of a variable? --
# (robust Wald test that all interaction terms of one subgroup variable are zero)
joint_test <- function(model, pattern) {
  b     <- coef(model)
  terms <- names(b)[grepl(pattern, names(b)) & !is.na(b)]
  terms <- intersect(terms, rownames(vcov(model)))
  if (length(terms) == 0) return(NA_real_)
  w <- as.numeric(t(b[terms]) %*% solve(vcov(model)[terms, terms]) %*% b[terms])
  pchisq(w, df = length(terms), lower.tail = FALSE)
}

het_joint <- tibble(
  subgroup_variable = unname(mod_list[het_vars]),
  groups_left_out   = unname(excluded_groups[het_vars]),
  p_any_treatment   = sapply(models_het_pooled, joint_test, pattern = "^any_treat:"),
  p_all_arms        = sapply(models_het,        joint_test, pattern = "^treat\\d:")
) %>%
  mutate(p_any_treatment_BH = p.adjust(p_any_treatment, method = "BH"),
         p_all_arms_BH      = p.adjust(p_all_arms,      method = "BH"),
         across(where(is.numeric), ~ round(.x, 3)))
print(het_joint)

# ---- Single interaction terms, corrected for multiple testing (Benjamini-Hochberg) ---

# any treatment x subgroup
p_int_pooled <- bind_rows(lapply(names(models_het_pooled), function(nm)
  tidy(models_het_pooled[[nm]]) %>%
    filter(grepl("^any_treat:", term)) %>%
    mutate(subgroup_variable = nm))) %>%
  mutate(p_adj_BH = p.adjust(p.value, method = "BH"),
         term = rename_terms(term)) %>%
  select(subgroup_variable, term, estimate, std.error, p.value, p_adj_BH)
print(p_int_pooled)

# single treatment arms x subgroup
p_int_arms <- bind_rows(lapply(names(models_het), function(nm)
  tidy(models_het[[nm]]) %>%
    filter(grepl("^treat\\d:", term)) %>%
    mutate(subgroup_variable = nm))) %>%
  mutate(p_adj_BH = p.adjust(p.value, method = "BH"),
         term = rename_terms(term)) %>%
  select(subgroup_variable, term, estimate, std.error, p.value, p_adj_BH)
print(p_int_arms)

write.csv(het_joint,    file.path("output", paste0(label, "_heterogeneity_joint_tests.csv")),   row.names = FALSE)
write.csv(p_int_pooled, file.path("output", paste0(label, "_heterogeneity_any_treatment.csv")), row.names = FALSE)
write.csv(p_int_arms,   file.path("output", paste0(label, "_heterogeneity_arms.csv")),          row.names = FALSE)

# ---- Figure: differences in the treatment effect between groups ------------------
het_plot_data <- bind_rows(lapply(names(models_het_pooled), function(nm) {
  z   <- het_vars[mod_list[het_vars] == nm]
  ref <- levels(droplevels(as.factor(df_het[[z]])))[1]                # reference group
  ord <- intersect(levels(as.factor(df[[z]])), unique(as.character(na.omit(df_het[[z]]))))  # natural order
  est <- tidy(models_het_pooled[[nm]]) %>%
    filter(grepl("^any_treat:", term)) %>%
    transmute(group = sub(paste0("^any_treat:", z), "", term), estimate, conf.low, conf.high)
  bind_rows(tibble(group = ref, estimate = 0, conf.low = NA_real_, conf.high = NA_real_), est) %>%
    mutate(reference = group == ref,
           variable  = nm,
           group     = factor(group, levels = rev(ord)))
}))

het_strip <- het_joint %>%
  mutate(lab = paste0(subgroup_variable, "\n(joint test: ",
                      ifelse(p_any_treatment_BH < 0.001, "p < 0.001",
                             sprintf("p = %.2f", p_any_treatment_BH)), ")"))
het_strip <- setNames(het_strip$lab, het_strip$subgroup_variable)

p <- ggplot(het_plot_data, aes(x = estimate, y = group)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#8C8C8C") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, colour = col_high, na.rm = TRUE) +
  geom_point(aes(shape = reference), size = 2.8, colour = col_high) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1), guide = "none") +
  facet_grid(variable ~ ., scales = "free_y", space = "free_y", switch = "y",
             labeller = labeller(variable = het_strip)) +
  labs(x = "Difference in the treatment effect compared with the reference group (points)", y = NULL,
       title = "Do treatment effects differ between groups?",
       caption = paste0("Interaction of any treatment with each characteristic; outcome: support index (1-7).\n",
                        "Open circle: reference group (the largest group; difference = 0). Lines: 95% confidence intervals.\n",
                        "Joint test: do effects differ between the groups of this characteristic? ",
                        "p-values adjusted for multiple testing (Benjamini-Hochberg).\n",
                        "Groups with fewer than ", min_n_arm, " respondents in a treatment or control group are left out.")) +
  theme_minimal(base_size = 13) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, hjust = 1, face = "bold"),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold"), plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, colour = "#555555"), plot.caption.position = "plot")
save_plot(p, "targeting_B2_heterogeneity", width = 11, height = 9)


################################################################################
## C. Intervention potential: low support and positive treatment effect
################################################################################

# plots charts combining the support level of each subgroup with its treatment effect. 
# potential for targeting could be with the groups where support is low and treatment
# effect is positive
plot_potential <- function(y) {
  d <- cate_all[cate_all$outcome == y & cate_all$term == "any_treat", ]
  base_all <- mean(df[[y]], na.rm = TRUE)

  ggplot(d, aes(x = group_mean, y = estimate)) +
    annotate("rect", xmin = -Inf, xmax = base_all, ymin = 0, ymax = Inf,
             fill = col_high, alpha = 0.08) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "#8C8C8C") +
    geom_vline(xintercept = base_all, linetype = "dotted", colour = "#8C8C8C") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0,
                  colour = col_high, alpha = 0.35) +
    geom_point(aes(size = n), colour = col_high) +
    ggrepel::geom_text_repel(aes(label = group), size = 3.4, colour = "#0B0B0B",
                             min.segment.length = 0, seed = 1) +
    facet_wrap(~ moderator, ncol = 4) +
    coord_cartesian(ylim = c(-1, 1)) +          # very wide intervals are cut off
    scale_size(range = c(1.5, 5), guide = "none") +
    labs(x = "Average support in the group (1 = strongly oppose, 7 = strongly support)",
         y = "Effect of any treatment (points)",
         title = parse(text = outcome_plotmath[y])[[1]],
         caption = paste0("Shaded: below-average support and positive effect (highest targeting potential).\n",
                          "Support: mean of all respondents in the group (not only the control group, whose noise is also in the effect).\n",
                          "Dotted line: average support of all respondents. Dashed line: no effect.\n",
                          "Points sized by number of respondents; lines: 95% confidence intervals (cut off beyond -1 and 1).")) +
    theme_minimal(base_size = 14) +
    theme(strip.text = element_text(face = "bold", hjust = 0),
          panel.spacing = unit(1, "lines"),
          plot.title = element_text(face = "bold"),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0, colour = "#555555"),
          plot.caption.position = "plot")
}

for (y in outcomes) {
  save_plot(plot_potential(y), paste0("targeting_C_potential_", y), width = 15, height = 9)
}


################################################################################
## D. Targeting table (one row per subgroup, lowest support first)
################################################################################

# table indicating subgroups with support level and effect of any treatment
fmt <- function(est, lo, hi, sig) sprintf("%.2f [%.2f, %.2f]%s", est, lo, hi, sig)

best_arm <- cate_all %>%
  filter(term != "any_treat", !is.na(estimate)) %>%
  group_by(outcome, moderator, group) %>%
  slice_max(estimate, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(outcome, moderator, group,
            best_treatment = as.character(treatment),
            best_effect    = fmt(estimate, conf.low, conf.high, sig))

targeting_table <- cate_all %>%
  filter(term == "any_treat") %>%
  transmute(outcome, moderator, group, n,
            support_group   = round(group_mean, 2),
            support_control = round(baseline, 2),
            effect_any_treatment = fmt(estimate, conf.low, conf.high, sig)) %>%
  left_join(best_arm, by = c("outcome", "moderator", "group")) %>%
  arrange(outcome, support_group)       # sorted by the support of the whole group (see fit_arms)

# opens in RStudio's data viewer; change the outcome to see the others
View(targeting_table %>% filter(outcome == "support_index"),
     title = paste("Targeting", label))


################################################################################
## E. Interactive intervention-potential chart (main outcome only)
################################################################################
# Interactive only where it helps: in the potential chart, many subgroups overlap,
# so the details are shown on hover instead of as labels. Click a legend entry
# to hide or show a subgroup variable.
# install.packages(c("plotly", "htmlwidgets"))   # only once

library(plotly)

interactive_outcomes <- c("support_index")   # add more outcomes here if needed

outcome_html <- c(support_gen = "CCUS in general", support_biomass = "Biomass + CCUS",
                  support_wte = "Waste-to-energy + CCUS", support_util = "CO<sub>2</sub> utilisation",
                  support_pipe = "CO<sub>2</sub> pipelines", support_stor = "CO<sub>2</sub> storage",
                  support_index = "Support index")

interactive_potential <- function(y) {
  d <- cate_all %>%
    filter(outcome == y, term == "any_treat") %>%
    mutate(tooltip = paste0("<b>", group, "</b> (", moderator, ")",
                            "<br>Respondents: ", n,
                            "<br>Support in the group: ", round(group_mean, 2),
                            " (control group: ", round(baseline, 2), ")",
                            "<br>Effect of any treatment: ", round(estimate, 2),
                            " [", round(conf.low, 2), ", ", round(conf.high, 2), "]",
                            "<br>p-value: ", round(p.value, 3),
                            " (corrected: ", round(p_adj_BH, 3), ")"))
  base_all <- mean(df[[y]], na.rm = TRUE)

  g <- ggplot(d, aes(x = group_mean, y = estimate, colour = moderator, text = tooltip)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "#8C8C8C") +
    geom_vline(xintercept = base_all, linetype = "dotted", colour = "#8C8C8C") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0, alpha = 0.5) +
    geom_point(aes(size = n), alpha = 0.9) +
    scale_colour_manual(values = c("#584F80", "#D08A4E", "#0B0B0B", "#8FA888",
                                   "#E6C27A", "#7A4A2A", "#9A96B6", "#8C8C8C")) +
    scale_size(range = c(2, 7), guide = "none") +
    labs(x = "Average support in the group (1 = strongly oppose, 7 = strongly support)",
         y = "Effect of any treatment (points)", colour = NULL) +
    theme_minimal(base_size = 13)

  ggplotly(g, tooltip = "text") %>%
    layout(title  = list(text = paste("Targeting potential:", outcome_html[y]),
                         x = 0.02, xanchor = "left", y = 0.98, yanchor = "top"),
           margin = list(t = 70, b = 90),
           legend = list(orientation = "h", y = -0.25))
}

for (y in interactive_outcomes) {
  p <- interactive_potential(y)
  print(p)
  htmlwidgets::saveWidget(p, file.path(getwd(), "output",
                                       paste0(label, "_interactive_potential_", y, ".html")),
                          selfcontained = TRUE)
}


# ---- Export aggregated targeting results for sharing ------------------------
share_dir <- file.path("output", "share")
dir.create(share_dir, showWarnings = FALSE)

write.csv(levels_tab,      file.path(share_dir, paste0(label, "_support_by_subgroup.csv")), row.names = FALSE)
write.csv(group_tests,     file.path(share_dir, paste0(label, "_subgroup_difference_tests.csv")), row.names = FALSE)
write.csv(cate_all,        file.path(share_dir, paste0(label, "_treatment_effects_by_subgroup.csv")), row.names = FALSE)
write.csv(targeting_table, file.path(share_dir, paste0(label, "_targeting_table.csv")), row.names = FALSE)
write.csv(het_joint,       file.path(share_dir, paste0(label, "_heterogeneity_joint_tests.csv")), row.names = FALSE)

if (label == "DE" && "region" %in% names(df)) {
  write.csv(filter(map_vals, n >= 20), file.path(share_dir, "DE_support_by_state.csv"), row.names = FALSE)
  write.csv(te_macro,                  file.path(share_dir, "DE_treatment_effect_by_region.csv"), row.names = FALSE)
}


################################################################################
## Part 2: Machine learning 
################################################################################

# Sections A-D look at subgroups chosen in advance. Here, the forests get all
# pre-treatment/treatment-unaffected characteristics and decide themselves which ones matter.
# All forest analyses use the pooled treatment (any treatment vs. control).
# Runs for every dataset (DE, NO, NL, ALL)

# install.packages(c("grf", "policytree", "DiagrammeR"))   # only once
library(grf)
library(policytree)

# ---- F0. Settings ------------------------------------------------------------

forest_outcome <- "support_index"   # outcome analysed by the forests

# Characteristics: Q1 ... Q13 were asked BEFORE the treatment (the treatment texts
# follow Q12/Q13) and are all candidates, incl. the comprehension attempts.
first_post_q <- 14

# Questions asked AFTER the treatment that the treatment cannot plausibly change:
#   Q14 heard of CCUS before (refers to the time before the survey),
#   Q16 / Q17 used and trusted information sources (long-term habits and trust)
#   Q37 - Q39 general traits (trust, patience, ...), not related to the treatment topic
#   (Q40, trust in government, is excluded: the treatment texts could affect it)
post_but_stable <- c("^Q14$", "^Q16r", "^Q17r", "^Q37$", "^Q38$", "^Q39$")

# Switch: climate concern (Q34) could have been changed by the treatment texts ->
# regression and Chi-squared test of independence suggest it was not (section B2)
# but can be excluded with FALSE
include_climate <- TRUE
if (include_climate) post_but_stable <- c(post_but_stable, "^Q34$")

# characteristics without a Q-number
extra_vars <- c("age_group", "Gender_new", "income", "region", "party_bloc")
if (is_pooled) extra_vars <- c(setdiff(extra_vars, c("income", "region")), "country")

# Pre-selection of the characteristics for the causal forests (pilot forest, keep
# columns with above-average importance):
#   "split" = selection within each half of the sample split: the forest trained on
#             one half only uses columns chosen with that half, so the held-out half
#             has not influenced the model it is used to test (default, valid tests)
#   "pilot" = selection with ALL respondents, incl. the half used later for the
#             held-out tests. This leaks information from the test half into the
#             model and overstates the evidence for heterogeneity (kept for comparison)
#   "none"  = no selection, all columns
forest_selection <- "split"

min_share  <- 0.03   # drop categories held by fewer than 3% of respondents
treat_cost <- 0      # cost of treating one person, in outcome units (0 = treatment is free)


# ---- F0. Data ----------------------------------------------------------------

# candidate characteristics: Q1 ... Q13 (raw versions, "Don't know" kept as a
# category) + the stable post-treatment questions + the extra variables
q_num <- suppressWarnings(as.numeric(sub("^Q(\\d+).*$", "\\1", names(df))))
cand  <- c(names(df)[!is.na(q_num) & q_num < first_post_q & !grepl("_f$", names(df))],
           grep(paste(post_but_stable, collapse = "|"), names(df), value = TRUE),
           intersect(extra_vars, names(df)))
cand  <- cand[!grepl("_f$", cand)]
cand  <- unique(cand)
if ("income" %in% cand) cand <- setdiff(cand, grep("^Q5_", cand, value = TRUE))   # same as income

# drop unusable columns: mostly missing, constant, or free text / IDs
cand <- cand[sapply(cand, function(v) {
  x <- df[[v]]
  n_lev <- length(unique(na.omit(x)))
  mean(is.na(x)) < 0.5 && n_lev >= 2 && (is.numeric(x) || n_lev <= 30)
})]
cat("Candidate characteristics (", length(cand), "):\n",
    paste(nice_name(cand), collapse = ", "), "\n")

fd <- df %>% filter(!is.na(.data[[forest_outcome]]), !is.na(any_treat))

# categories -> 0/1 columns; missing values are kept (grf handles them)
mf    <- model.frame(~ ., data = fd[cand], na.action = na.pass)
X_all <- model.matrix(~ . - 1, mf)
col_var <- gsub("`", "", attr(terms(mf), "term.labels"))[attr(X_all, "assign")]

# drop very rare (or almost universal) categories
share  <- colMeans(X_all, na.rm = TRUE)
binary <- apply(X_all, 2, function(z) all(z %in% c(0, 1, NA)))
keep   <- !binary | (share >= min_share & share <= 1 - min_share)
X_all  <- X_all[, keep, drop = FALSE]
col_var <- col_var[keep]

# readable column names, e.g. "Prior knowledge of CCUS: Some knowledge"
col_lev <- substring(colnames(X_all), nchar(col_var) + 1)
colnames(X_all) <- make.unique(ifelse(col_lev == "", nice_name(col_var),
                                      paste0(nice_name(col_var), ": ", col_lev)))

Y     <- fd[[forest_outcome]]
W     <- fd$any_treat
W_hat <- rep(mean(W), length(W))          # known randomisation probability

cat("Respondents:", length(Y), "| characteristic columns:", ncol(X_all), "\n")

# importance per characteristic (summed over its categories), top 15
plot_importance <- function(forest, vars, title, file) {
  imp <- tibble(var = vars, importance = variable_importance(forest)[, 1]) %>%
    group_by(var) %>%
    summarise(importance = sum(importance), .groups = "drop") %>%
    slice_max(importance, n = 15) %>%
    mutate(label = nice_name(var))
  p <- ggplot(imp, aes(x = importance, y = reorder(label, importance))) +
    geom_col(fill = col_high, width = 0.7) +
    labs(x = "Variable importance (share of splits, weighted by depth)", y = NULL,
         title = title) +
    theme_minimal(base_size = 14) +
    theme(panel.grid.major.y = element_blank(),
          plot.title = element_text(face = "bold"), plot.title.position = "plot")
  save_plot(p, file, width = 12, height = 7)
  imp
}


################################################################################
## F1. Random forest: which characteristics predict the level of support
################################################################################

set.seed(123)
rf    <- regression_forest(X_all, Y, num.trees = 4000)
Y_hat <- predict(rf)$predictions                       # out-of-bag predictions
rf_r2 <- 1 - mean((Y - Y_hat)^2) / var(Y)
cat("Random forest: share of variance explained (out-of-bag):", round(rf_r2, 3), "\n")

imp_rf <- plot_importance(rf, col_var,
                          paste0("What predicts support? (random forest, R² = ", round(rf_r2, 2), ")"),
                          "forest_F1_importance_support")


################################################################################
## F2. Causal forest: does the treatment effect differ between people? (CATE)
################################################################################
# Full-sample forest: average effect, predicted effects per person (histogram) and
# which characteristics it splits on (descriptive). The tests of heterogeneity use
# the sample split below. With forest_selection = "pilot", the forest only uses
# characteristics with above-average importance in a pilot forest (Athey & Wager
# 2019); with "split" or "none", the full-sample forest uses all characteristics
# (with "split", the selection happens within the halves of the sample split).

if (forest_selection == "pilot") {
  set.seed(123)
  cf_pilot <- causal_forest(X_all, Y, W, W.hat = W_hat, Y.hat = Y_hat, num.trees = 2000)
  vi       <- variable_importance(cf_pilot)[, 1]
  selected <- vi >= mean(vi)
} else {
  selected <- rep(TRUE, ncol(X_all))
}
X          <- X_all[, selected, drop = FALSE]
col_var_cf <- col_var[selected]
cat("Columns used by the full-sample causal forest:", ncol(X), "of", ncol(X_all), "\n")

set.seed(123)
cf <- causal_forest(X, Y, W, W.hat = W_hat, Y.hat = Y_hat, num.trees = 4000)

ate   <- average_treatment_effect(cf)
calib <- test_calibration(cf)
print(ate)
print(calib)      # differential.forest.prediction: p < 0.05 -> real heterogeneity

imp_cf <- plot_importance(cf, col_var_cf, "Which characteristics did the causal forest split on?",
                          "forest_F2_importance_effect")


# ---- Sample split (used in F3, F4 and F5) ------------------------------------
# One forest per half. Ranking people and estimating the effects in the ranked
# groups with the same data is not valid inference, so people are always ranked
# by the forest trained on the OTHER half.

set.seed(123)
train <- sample(c(TRUE, FALSE), length(Y), replace = TRUE)

# columns for the forest of each half: with "split", chosen by a pilot forest that
# only sees that half; otherwise the same columns as the full-sample forest
pick_columns <- function(idx) {
  p  <- causal_forest(X_all[idx, , drop = FALSE], Y[idx], W[idx],
                      W.hat = W_hat[idx], Y.hat = Y_hat[idx], num.trees = 2000)
  vi <- variable_importance(p)[, 1]
  vi >= mean(vi)
}
if (forest_selection == "split") {
  sel_tr <- pick_columns(train)
  sel_ev <- pick_columns(!train)
} else {
  sel_tr <- sel_ev <- selected
}
X_tr <- X_all[, sel_tr, drop = FALSE]     # columns of the forest trained on `train`
X_ev <- X_all[, sel_ev, drop = FALSE]     # columns of the forest trained on `!train`
cat("Columns per half (selection: ", forest_selection, "): ", sum(sel_tr), " and ", sum(sel_ev),
    " of ", ncol(X_all), "\n", sep = "")

fit_cf <- function(idx, Xs) causal_forest(Xs[idx, , drop = FALSE], Y[idx], W[idx],
                                          W.hat = W_hat[idx], Y.hat = Y_hat[idx], num.trees = 2000)
cf_train <- fit_cf(train, X_tr)
cf_eval  <- fit_cf(!train, X_ev)

################################################################################
## F3. Effects by predicted-effect quartile, held-out calibration test
################################################################################

tau <- predict(cf, estimate.variance = TRUE)          # out-of-bag CATE per person
fd$cate <- tau$predictions                            # (only for the histogram)

# cross-fitted ranking: each half is ranked by the forest of the other half,
# quartiles are formed within each half
fd$cate_xfit <- NA_real_
fd$cate_xfit[!train] <- predict(cf_train, X_tr[!train, , drop = FALSE])$predictions
fd$cate_xfit[train]  <- predict(cf_eval,  X_ev[train,  , drop = FALSE])$predictions
q_xfit <- integer(length(Y))
q_xfit[train]  <- ntile(fd$cate_xfit[train], 4)
q_xfit[!train] <- ntile(fd$cate_xfit[!train], 4)
fd$cate_group <- factor(q_xfit, levels = 1:4,
                        labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)"))

# doubly robust scores from each half's own forest (these did not do the ranking)
dr <- numeric(length(Y))
dr[train]  <- get_scores(cf_train)
dr[!train] <- get_scores(cf_eval)

# held-out calibration test (best linear predictor, BLP): regress the doubly robust
# scores on the cross-fitted predicted effect, centred within each half. Slope 1 =
# the predicted differences are right on average, 0 = no heterogeneity. Unlike
# test_calibration() in F2, the predictions come from the forest of the other half.
# One-sided p-value (H1: slope > 0), as in test_calibration().
blp_d <- data.frame(dr = dr, half = train, tc = fd$cate_xfit - ave(fd$cate_xfit, train))
blp   <- lm_robust(dr ~ half + tc, data = blp_d)
blp_b <- unname(coef(blp)["tc"])
blp_p <- pt(unname(blp$statistic["tc"]), blp$df["tc"], lower.tail = FALSE)
cat("Held-out calibration (BLP): slope =", round(blp_b, 2), ", one-sided p =", round(blp_p, 3), "\n")

# distribution of the predicted effects
p <- ggplot(fd, aes(x = cate)) +
  geom_histogram(bins = 40, fill = col_high, colour = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#8C8C8C") +
  geom_vline(xintercept = ate[["estimate"]], linetype = "dotted", colour = col_low) +
  labs(x = "Predicted treatment effect per respondent (CATE)", y = "Respondents",
       title = "Distribution of predicted treatment effects",
       caption = "Dashed line: no effect. Dotted line: average treatment effect.") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"), plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, colour = "#555555"))
save_plot(p, "forest_F3_cate_distribution", width = 10, height = 6)

# actual (doubly robust) effect within each quartile of predicted effects
gates <- bind_rows(lapply(levels(fd$cate_group), function(g) {
  s <- dr[fd$cate_group == g]
  tibble(group = g, n = length(s),
         estimate = mean(s), std.err = sd(s) / sqrt(length(s)))
})) %>%
  mutate(conf.low = estimate - 1.96 * std.err, conf.high = estimate + 1.96 * std.err,
         group = factor(group, levels = levels(fd$cate_group)))

# test: is the effect in the top quartile larger than in the bottom quartile?
top <- fd$cate_group == "Q4 (highest)"; bot <- fd$cate_group == "Q1 (lowest)"
diff_tb <- mean(dr[top]) - mean(dr[bot])
se_tb   <- sqrt(var(dr[top]) / sum(top) + var(dr[bot]) / sum(bot))
p_tb    <- 2 * pnorm(-abs(diff_tb / se_tb))

p <- ggplot(gates, aes(x = estimate, y = group)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#8C8C8C") +
  geom_vline(xintercept = ate[["estimate"]], linetype = "dotted", colour = col_low) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high), colour = col_high, size = 0.8) +
  labs(x = "Treatment effect (points)", y = "Quartile of predicted effect",
       title = "Treatment effect by quartile of the predicted effect",
       caption = paste0("Quartiles from a forest trained on the other half of the sample (cross-fitted);\n",
                        "effects re-estimated within each quartile (doubly robust), with 95% confidence intervals.\n",
                        "Characteristics selected: ", forest_selection, ". ",
                        "Difference highest - lowest quartile: ", sprintf("%.2f", diff_tb),
                        " (p = ", sprintf("%.2f", p_tb), "). Dotted line: average effect.")) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold"), plot.title.position = "plot",
        plot.caption = element_text(hjust = 0, colour = "#555555"))
save_plot(p, "forest_F3_effects_by_quartile", width = 10, height = 6)

# who is in the highest vs. lowest quartile? (largest differences in characteristics)
# Descriptive only: the forests built the quartiles from exactly these characteristics,
# so the quartiles differ in them by construction and p-values would be meaningless.
# Only interpret this if F3/F4 show real heterogeneity.
profile <- bind_rows(lapply(seq_len(ncol(X_all)), function(j) {
  x <- X_all[, j]
  tibble(characteristic = colnames(X_all)[j],
         mean_highest = mean(x[top], na.rm = TRUE),
         mean_lowest  = mean(x[bot], na.rm = TRUE),
         difference   = mean_highest - mean_lowest)
})) %>%
  arrange(desc(abs(difference))) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

View(head(profile, 15), title = paste("Who responds most vs least (descriptive) -", label))


################################################################################
## F4. Is targeting worthwhile? (RATE / TOC, on held-out data)
################################################################################
# Forest trained on one half (cf_train, see sample split above) ranks the other
# half by predicted effect; a separate forest on that half (cf_eval) tests whether
# high-ranked people really respond more.

priority <- predict(cf_train, X_tr[!train, , drop = FALSE])$predictions
rate     <- rank_average_treatment_effect(cf_eval, priority)
rate_p   <- 2 * pnorm(-abs(rate$estimate / rate$std.err))
print(rate)

png(file.path("output", paste0(label, "_forest_F4_TOC.png")), width = 1800, height = 1200, res = 200)
plot(rate, main = "Targeting operator characteristic (TOC)",
     sub = paste0("AUTOC = ", round(rate$estimate, 3), ", p = ", round(rate_p, 2)))
dev.off()
plot(rate, main = "Targeting operator characteristic (TOC)",
     sub = paste0("AUTOC = ", round(rate$estimate, 3), ", p = ", round(rate_p, 2)))


################################################################################
## F5. Policy tree: whom to treat (any treatment vs. control)
################################################################################
# Learns a simple rule (depth 2) on one half and evaluates it on the other half.

# Policy trees need complete data. Setting missing values to 0 would put them into
# the reference category (e.g. no party bloc -> "Left"), so every characteristic
# with missing values gets its own column "<characteristic>: missing"; then
# dummies are set to 0 and numeric columns to the median.
# columns of the forest trained on `train` (the tree is learned on that half)
X_pol   <- X_tr
col_pol <- col_var[sel_tr]

# If implementation of a targeting rule is considered:
# select characteristics that may not be used as a basis for targeting. 
# implementing a targeting rule should only be based on characteristics that are
# not sensitive. Ethical restrictions of targeting for real-life use should be switched on. 
# characteristics may be adjusted to exclude or include others. 
# select here characteristics the policy tree may not use as a basis for targeting

restrict_targeting <- FALSE # turn on restricted targeting here
no_target <- c("^party_bloc$", "^Gender_new$", "^Q12Q13_tryCount")
if (restrict_targeting) {
  excl    <- grepl(paste(no_target, collapse = "|"), col_pol)
  X_pol   <- X_pol[, !excl, drop = FALSE]
  col_pol <- col_pol[!excl]
}
X_pt <- X_pol
na_cols <- colSums(is.na(X_pol)) > 0
for (v in unique(col_pol[na_cols])) {
  miss <- as.numeric(rowSums(is.na(X_pol[, col_pol == v, drop = FALSE])) > 0)
  X_pt <- cbind(X_pt, miss)
  colnames(X_pt)[ncol(X_pt)] <- paste0(nice_name(v), ": missing")
}
for (j in which(na_cols)) {
  fill <- if (all(X_pol[, j] %in% c(0, 1, NA))) 0 else median(X_pol[, j], na.rm = TRUE)
  X_pt[is.na(X_pt[, j]), j] <- fill
}

dr_scores <- function(forest, cost) { g <- double_robust_scores(forest); g[, -1] <- g[, -1] - cost; g }

eval_policy <- function(tree, X_test, gamma_test) {
  a  <- predict(tree, X_test)
  v_rule <- gamma_test[cbind(seq_along(a), a)]
  v_all  <- gamma_test[, ncol(gamma_test)]          # treat everyone (last action)
  tibble(value_rule      = mean(v_rule),
         value_treat_all = mean(v_all),
         value_treat_none = mean(gamma_test[, 1]),
         gain_vs_treat_all = mean(v_rule - v_all),
         se_gain          = sd(v_rule - v_all) / sqrt(length(a)),
         share_treated    = mean(a != 1))
}

tree_bin <- policy_tree(X_pt[train, ], dr_scores(cf_train, treat_cost), depth = 2, min.node.size = 50)
policy_bin <- eval_policy(tree_bin, X_pt[!train, ], dr_scores(cf_eval, treat_cost)) %>%
  mutate(p_gain = 2 * pnorm(-abs(gain_vs_treat_all / se_gain)))
print(tree_bin)        # action 1 = Control, action 2 = Any treatment
print(policy_bin)
tryCatch(print(plot(tree_bin, leaf.labels = c("Control", "Any treatment"))),
         error = function(e) message("Install DiagrammeR to draw the tree."))

################################################################################
## F6. Summary table
################################################################################

forest_summary <- tibble(
  dataset = label,
  outcome = forest_outcome,
  respondents = length(Y),
  characteristics_columns = ncol(X_all),
  selection = forest_selection,
  selected_columns = (sum(sel_tr) + sum(sel_ev)) / 2,
  rf_variance_explained = round(rf_r2, 3),
  ate_estimate = round(ate[["estimate"]], 3),
  ate_se = round(ate[["std.err"]], 3),
  calibration_coef = round(calib["differential.forest.prediction", "Estimate"], 2),
  calibration_p = round(calib["differential.forest.prediction", "Pr(>t)"], 3),
  top_minus_bottom_quartile = round(diff_tb, 3),
  top_minus_bottom_p = round(p_tb, 3),
  blp_heldout_coef = round(blp_b, 3),
  blp_heldout_p = round(blp_p, 3),
  autoc = round(rate$estimate, 3),
  autoc_p = round(rate_p, 3),
  policy_gain_vs_treat_all = round(policy_bin$gain_vs_treat_all, 3),
  policy_gain_p = round(policy_bin$p_gain, 3)
)
print(t(forest_summary))

write.csv(forest_summary, file.path("output", paste0(label, "_forest_summary.csv")), row.names = FALSE)
write.csv(gates,          file.path("output", paste0(label, "_forest_effects_by_quartile.csv")), row.names = FALSE)
write.csv(profile,        file.path("output", paste0(label, "_forest_profile_top_vs_bottom.csv")), row.names = FALSE)
