# Bayesian hierarchical multinomial logistic regression for the institutional projects
# (LifeStraw, Amazi Meza). Run from the repository root after R/prep.R.
suppressMessages({library(dplyr); library(brms); library(readr)})
d <- readRDS("prepped.rds")
inst <- d$inst
cat("N obs:", nrow(inst), "| schools:", n_distinct(inst$school_id), "\n")
m <- brm(risk ~ period + (1 | school_id), data = inst, family = categorical(),
         chains = 4, cores = 4, iter = 2000, seed = 20260529, refresh = 0, silent = 2)
print(summary(m))
fx <- fixef(m)
cats <- c("IntermediateRisk", "HighRisk", "VeryHighRiskUnsafe")
tab <- data.frame(category = cats,
  intercept = fx[paste0("mu", cats, "_Intercept"), "Estimate"],
  log_odds = fx[paste0("mu", cats, "_periodMonitoring"), "Estimate"],
  OR = exp(fx[paste0("mu", cats, "_periodMonitoring"), "Estimate"]),
  lo = exp(fx[paste0("mu", cats, "_periodMonitoring"), "Q2.5"]),
  hi = exp(fx[paste0("mu", cats, "_periodMonitoring"), "Q97.5"]))
dir.create("results", showWarnings = FALSE)
write_csv(tab |> mutate(across(-category, ~ round(.x, 3))), "results/institutional_model.csv")
print(tab, digits = 3)
cat("max Rhat:", max(rhat(m), na.rm = TRUE), "\n")
cat("n by period x category:\n"); print(table(inst$period, inst$risk))
nd <- data.frame(period = factor(c("Baseline", "Monitoring"), levels = c("Baseline", "Monitoring")))
pp <- fitted(m, newdata = nd, re_formula = NA, summary = TRUE)
cat("Predicted probabilities (population level):\n"); print(round(pp[, "Estimate", ], 3))
saveRDS(m, "model.rds")
