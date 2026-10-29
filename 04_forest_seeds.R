################################################################################
## 04_forest_seeds.R
## Robustness of the machine-learning results (03_targeting.R, section F) to the
## random seed. 03_targeting.R uses one seed (123); here F1-F5 are repeated for
## many seeds. The seed changes the forests AND the sample split.
##
## Reported per dataset:
##   - distribution of every summary statistic of F6 across the seeds
##   - share of seeds with p < 0.05 for the four tests
##   - stability of the individual predicted effects (rank correlation of the
##     CATEs between seeds): is "who responds most" the same person every time?
##
## Run 02_analysis.R and 03_targeting.R first (dataset chosen in 02, like 03).
## Runtime: about 4 min per dataset for 50 seeds (pooled data: 20-35 min, mostly the
## policy tree).
################################################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(grf)
library(policytree)
library(estimatr)

stopifnot(exists("df"), exists("label"), exists("nice_name"))


# ---- Settings ----------------------------------------------------------------

if (!exists("n_seeds"))  n_seeds  <- 50           # set before sourcing to change it
# characteristics: "original" = as in 03_targeting.R (dummies only),
#                  "recoded"  = ordered scales as numbers (R/forest_features.R)
if (!exists("features")) features <- "original"
# variable selection (as forest_selection in 03_targeting.R, section F0):
#   "split" = pilot selection separately within each half of the sample split: the
#             forest trained on one half only uses columns chosen with that half
#             (default, as in 03)
#   "pilot" = pilot selection with ALL respondents, so the held-out half has already
#             influenced which characteristics the forest may use (overstates the
#             evidence for heterogeneity; kept for comparison)
#   "none"  = no selection, all columns (importance favours variables with many values)
if (!exists("selection")) selection <- if (exists("forest_selection")) forest_selection else "split"
# outcome: "index"  = support index 1-7 as in 03_targeting.R,
#          "binary" = support index >= 5 (rather supportive), robustness check without
#                     a ceiling: people close to 7 cannot increase their support much
if (!exists("outcome_type")) outcome_type <- "index"
stopifnot(features %in% c("original", "recoded"), selection %in% c("pilot", "none", "split"),
          outcome_type %in% c("index", "binary"))
is_default <- features == "original" && outcome_type == "index" &&
  selection == (if (exists("forest_selection")) forest_selection else "split")
tag <- paste0(label,                                                   # file names
              if (features == "recoded") "_recoded" else "",
              if (selection == "none") "_noselect" else "",
              if (selection == "split") "_split" else "",
              if (outcome_type == "binary") "_binary" else "",
              if (exists("tag_extra")) tag_extra else "")   # e.g. "_20seeds"
variant <- paste0("characteristics: ", features, ", selection: ", selection,
                  ", outcome: ", if (outcome_type == "binary") "support index >= 5" else "support index")
# seed 123 first: it must reproduce the numbers of 03_targeting.R (check below)
seeds   <- c(123, setdiff(seq_len(n_seeds + 1), 123))[1:n_seeds]

seed_dir <- file.path("output", "seeds")
dir.create(seed_dir, showWarnings = FALSE, recursive = TRUE)

col_low  <- "#C0703A"   # same colours as 03_targeting.R
col_high <- "#584F80"


# ---- Data: identical to 03_targeting.R, section F0 ----------------------------
# The F0 block is read from 03_targeting.R so that the outcome, the treatment and
# the characteristics stay exactly the same as in the main analysis.

if (!exists("file_03")) file_03 <- "03_targeting.R"
src  <- readLines(file_03, encoding = "UTF-8")
from <- grep("^# ---- F0\\. Settings", src)
to   <- grep("^## F1\\.", src) - 1
df$any_treat <- as.numeric(df$treat != "1")
eval(parse(text = src[from:to], encoding = "UTF-8"))
if (features == "recoded") source(file.path("R", "forest_features.R"), local = TRUE, encoding = "UTF-8")
if (outcome_type == "binary") Y <- as.numeric(Y >= 5)
cat(tag, "|", variant, "| respondents:", length(Y), "| columns:", ncol(X_all), "\n")


# ---- One run of F1-F5 for a given seed ---------------------------------------
# Same steps and settings as 03_targeting.R; set.seed(s) replaces set.seed(123).

dr_scores <- function(forest, cost) { g <- double_robust_scores(forest); g[, -1] <- g[, -1] - cost; g }

run_seed <- function(s) {
  # F1. random forest for the outcome level (also gives Y_hat)
  set.seed(s)
  rf    <- regression_forest(X_all, Y, num.trees = 4000)
  Y_hat <- predict(rf)$predictions
  rf_r2 <- 1 - mean((Y - Y_hat)^2) / var(Y)
  
  # F2. pilot forest -> variable selection -> final causal forest
  if (selection == "pilot") {
    set.seed(s)
    cf_pilot <- causal_forest(X_all, Y, W, W.hat = W_hat, Y.hat = Y_hat, num.trees = 2000)
    vi       <- variable_importance(cf_pilot)[, 1]
    selected <- vi >= mean(vi)
  } else {
    # "none", and "split" (selection happens within the halves below; the full-
    # sample forest is then only used for the ATE and the CATE stability)
    selected <- rep(TRUE, ncol(X_all))
  }
  X        <- X_all[, selected, drop = FALSE]
  col_var_cf <- col_var[selected]
  
  set.seed(s)
  cf    <- causal_forest(X, Y, W, W.hat = W_hat, Y.hat = Y_hat, num.trees = 4000)
  ate   <- average_treatment_effect(cf)
  calib <- test_calibration(cf)
  # with "split" and "none", the full-sample forest uses all columns, so its
  # calibration test is not affected by the selection (as in 03)
  
  # sample split
  set.seed(s)
  train <- sample(c(TRUE, FALSE), length(Y), replace = TRUE)
  if (selection == "split") {
    # pilot forest on one half only -> columns for the forest trained on that half
    pick <- function(idx) {
      p  <- causal_forest(X_all[idx, , drop = FALSE], Y[idx], W[idx],
                          W.hat = W_hat[idx], Y.hat = Y_hat[idx], num.trees = 2000)
      vi <- variable_importance(p)[, 1]
      vi >= mean(vi)
    }
    sel_tr <- pick(train)
    sel_ev <- pick(!train)
  } else {
    sel_tr <- sel_ev <- selected
  }
  X_tr <- X_all[, sel_tr, drop = FALSE]     # columns of the forest trained on `train`
  X_ev <- X_all[, sel_ev, drop = FALSE]     # columns of the forest trained on `!train`
  fit_cf <- function(idx, Xs) causal_forest(Xs[idx, , drop = FALSE], Y[idx], W[idx],
                                            W.hat = W_hat[idx], Y.hat = Y_hat[idx], num.trees = 2000)
  cf_train <- fit_cf(train, X_tr)
  cf_eval  <- fit_cf(!train, X_ev)
  
  # F3. effects by quartile of the cross-fitted predicted effect
  cate      <- predict(cf)$predictions
  cate_xfit <- numeric(length(Y))
  cate_xfit[!train] <- predict(cf_train, X_tr[!train, , drop = FALSE])$predictions
  cate_xfit[train]  <- predict(cf_eval,  X_ev[train,  , drop = FALSE])$predictions
  q <- integer(length(Y))
  q[train]  <- ntile(cate_xfit[train], 4)
  q[!train] <- ntile(cate_xfit[!train], 4)
  dr <- numeric(length(Y))
  dr[train]  <- get_scores(cf_train)
  dr[!train] <- get_scores(cf_eval)
  top <- q == 4; bot <- q == 1
  diff_tb <- mean(dr[top]) - mean(dr[bot])
  se_tb   <- sqrt(var(dr[top]) / sum(top) + var(dr[bot]) / sum(bot))
  
  # held-out calibration (BLP): regress the doubly robust scores on the cross-
  # fitted predicted effect, centred within each half. Slope 1 = the predicted
  # differences are right on average, 0 = no heterogeneity. Unlike test_calibration
  # in F2, the predictions come from the forest trained on the other half.
  # One-sided p-value (H1: slope > 0), as in test_calibration.
  blp_d  <- data.frame(dr = dr, half = train, tc = cate_xfit - ave(cate_xfit, train))
  blp    <- lm_robust(dr ~ half + tc, data = blp_d)
  blp_b  <- unname(coef(blp)["tc"])
  blp_p  <- pt(unname(blp$statistic["tc"]), blp$df["tc"], lower.tail = FALSE)
  
  # F4. RATE / AUTOC on held-out data
  priority <- predict(cf_train, X_tr[!train, , drop = FALSE])$predictions
  rate     <- rank_average_treatment_effect(cf_eval, priority)
  
  # F5. policy tree on the columns of the forest trained on `train`
  #     (missing values handled as in 03_targeting.R)
  X          <- X_tr
  col_var_cf <- col_var[sel_tr]
  X_pt <- X
  na_cols <- colSums(is.na(X)) > 0
  for (v in unique(col_var_cf[na_cols])) {
    X_pt <- cbind(X_pt, as.numeric(rowSums(is.na(X[, col_var_cf == v, drop = FALSE])) > 0))
  }
  for (j in which(na_cols)) {
    fill <- if (all(X[, j] %in% c(0, 1, NA))) 0 else median(X[, j], na.rm = TRUE)
    X_pt[is.na(X_pt[, j]), j] <- fill
  }
  tree  <- policy_tree(X_pt[train, ], dr_scores(cf_train, treat_cost), depth = 2, min.node.size = 50)
  gamma <- dr_scores(cf_eval, treat_cost)
  a     <- predict(tree, X_pt[!train, ])
  gain  <- gamma[cbind(seq_along(a), a)] - gamma[, ncol(gamma)]
  
  list(
    stats = tibble(
      seed = s,
      selected_columns          = (sum(sel_tr) + sum(sel_ev)) / 2,   # = ncol(X) unless "split"
      rf_variance_explained     = rf_r2,
      ate_estimate              = ate[["estimate"]],
      ate_se                    = ate[["std.err"]],
      calibration_coef          = calib["differential.forest.prediction", "Estimate"],
      calibration_p             = calib["differential.forest.prediction", "Pr(>t)"],
      top_minus_bottom_quartile = diff_tb,
      top_minus_bottom_p        = 2 * pnorm(-abs(diff_tb / se_tb)),
      blp_heldout_coef          = blp_b,
      blp_heldout_p             = blp_p,
      autoc                     = rate$estimate,
      autoc_p                   = 2 * pnorm(-abs(rate$estimate / rate$std.err)),
      policy_gain_vs_treat_all  = mean(gain),
      policy_gain_p             = 2 * pnorm(-abs(mean(gain) / (sd(gain) / sqrt(length(a))))),
      policy_share_treated      = mean(a != 1)
    ),
    cate = cate
  )
}


# ---- Run all seeds -----------------------------------------------------------

t0  <- Sys.time()
res <- vector("list", length(seeds))
for (i in seq_along(seeds)) {
  res[[i]] <- run_seed(seeds[i])
  cat(label, "- seed", seeds[i], "done (", i, "/", length(seeds), ")\n")
}
cat("Runtime:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")

per_seed <- bind_rows(lapply(res, `[[`, "stats"))
cate_mat <- sapply(res, `[[`, "cate")


# ---- Check: seed 123 reproduces 03_targeting.R -------------------------------

orig_file <- file.path("output", paste0(label, "_forest_summary.csv"))
if (is_default && file.exists(orig_file)) {
  orig <- read.csv(orig_file)
  cols <- intersect(names(orig), names(per_seed))
  cols <- cols[sapply(orig[cols], is.numeric)]
  # 03_targeting.R rounds the calibration coefficient to 2 digits, all else to 3
  digits <- ifelse(cols == "calibration_coef", 2, 3)
  dev  <- abs(mapply(round, unlist(per_seed[per_seed$seed == 123, cols]), digits) - unlist(orig[1, cols]))
  cat("Seed 123 vs. 03_targeting.R: largest difference =", max(dev),
      if (max(dev) < 1e-9) "-> identical\n" else
        paste("-> NOT identical, check:", paste(cols[dev >= 1e-9], collapse = ", "), "\n"))
}


# ---- Summary across seeds ----------------------------------------------------

stat_cols <- setdiff(names(per_seed), "seed")
stat_cols <- stat_cols[colSums(!is.na(per_seed[stat_cols])) > 0]   # calibration is NA for "split"
summary_seeds <- per_seed %>%
  pivot_longer(all_of(stat_cols), names_to = "statistic") %>%
  group_by(statistic) %>%
  summarise(seed_123 = value[seed == 123],
            mean     = mean(value),
            median   = median(value),
            p10      = quantile(value, 0.10),
            p90      = quantile(value, 0.90),
            min      = min(value),
            max      = max(value),
            share_p_below_05 = if (grepl("_p$", first(statistic))) mean(value < 0.05) else NA_real_,
            .groups = "drop") %>%
  mutate(statistic = factor(statistic, levels = stat_cols)) %>%
  arrange(statistic) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# stability of the individual predicted effects between seeds
rho <- cor(cate_mat, method = "spearman")
rho <- rho[upper.tri(rho)]
cate_stability <- tibble(
  dataset = label,
  features = features,
  selection = selection,
  outcome = outcome_type,
  seeds = length(seeds),
  median_rank_correlation = round(median(rho), 3),
  min_rank_correlation    = round(min(rho), 3),
  # do the same people land in the top quartile? share of the top quartile of
  # seed 123 that is also in the top quartile of the seed-averaged CATE
  top_quartile_overlap_seed123_vs_mean = round(mean(
    ntile(cate_mat[, 1], 4) == 4 & ntile(rowMeans(cate_mat), 4) == 4) /
      mean(ntile(cate_mat[, 1], 4) == 4), 3)
)

print(as.data.frame(summary_seeds))
print(as.data.frame(cate_stability))

write.csv(per_seed,       file.path(seed_dir, paste0(tag, "_forest_seeds_per_seed.csv")), row.names = FALSE)
write.csv(summary_seeds,  file.path(seed_dir, paste0(tag, "_forest_seeds_summary.csv")),  row.names = FALSE)
write.csv(cate_stability, file.path(seed_dir, paste0(tag, "_forest_seeds_cate_stability.csv")), row.names = FALSE)


# ---- Plot: p-values of the five tests across seeds ---------------------------

p_labels <- c(calibration_p      = "Calibration test\n(heterogeneity?)",
              blp_heldout_p      = "Held-out calibration\n(BLP)",
              top_minus_bottom_p = "Top vs. bottom\nquartile",
              autoc_p            = "RATE / AUTOC\n(targeting worthwhile?)",
              policy_gain_p      = "Policy tree vs.\ntreat everyone")

p_long <- per_seed %>%
  select(seed, all_of(names(p_labels))) %>%
  pivot_longer(-seed, names_to = "test", values_to = "p") %>%
  filter(!is.na(p)) %>%
  mutate(test = factor(p_labels[test], levels = p_labels),
         is_123 = seed == 123)

share_sig <- p_long %>% group_by(test) %>%
  summarise(lab = sprintf("p < 0.05 in %d of %d seeds", sum(p < 0.05), n()), .groups = "drop")

p <- ggplot(p_long, aes(x = p, y = test)) +
  geom_vline(xintercept = 0.05, linetype = "dashed", colour = "#8C8C8C") +
  geom_point(data = filter(p_long, !is_123), colour = col_high, alpha = 0.55, size = 2.6,
             position = position_jitter(height = 0.12, width = 0, seed = 1)) +
  geom_point(data = filter(p_long, is_123), colour = col_low, size = 4, shape = 18) +
  geom_text(data = share_sig, aes(x = 1, y = test, label = lab), hjust = 1, vjust = -1.6,
            size = 3.6, colour = "#555555") +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.05, 0.25, 0.5, 0.75, 1)) +
  labs(x = "p-value", y = NULL,
       title = paste0("Forest results across ", length(seeds), " seeds - ", label,
                      if (is_default) "" else " (variant)"),
       subtitle = variant,
       caption = paste0("One dot per seed (forests and sample split change with the seed). ",
                        "Orange diamond: seed 123 as in 03_targeting.R.\n",
                        "Dashed line: p = 0.05. Any treatment vs. control.")) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"), plot.title.position = "plot",
        plot.subtitle = element_text(colour = "#555555"),
        plot.caption = element_text(hjust = 0, colour = "#555555"))
ggsave(file.path(seed_dir, paste0(tag, "_forest_seeds_pvalues.png")), p,
       width = 11, height = 6, dpi = 200, bg = "white")
print(p)
