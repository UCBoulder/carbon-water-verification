# Sensitivity analyses for the institutional model (revision, PLOS Water PWAT-D-26-00197)
#  S1  special-purpose campaigns (cleaning, re-testing of flagged filters) added to monitoring
#  S2  country as a fixed effect
#  S3  period x country interaction (monitoring odds ratio within each country)
suppressMessages({library(dplyr); library(readr); library(brms)})
d <- readRDS("prepped.rds")
fitm <- function(f, dat) brm(f, data = dat, family = categorical(), chains = 4, cores = 4, iter = 2000,
                             seed = 20260529, refresh = 0, silent = 2)
or_rows <- function(m, dat, tag) {
  fx <- fixef(m); r <- grep("_periodMonitoring$", rownames(fx), value = TRUE)
  data.frame(analysis = tag, n = nrow(dat), schools = n_distinct(dat$school_id),
             category = sub("_periodMonitoring", "", sub("^mu", "", r)),
             OR = exp(fx[r, "Estimate"]), lo = exp(fx[r, "Q2.5"]), hi = exp(fx[r, "Q97.5"]),
             rhat_max = max(rhat(m), na.rm = TRUE))
}
res <- bind_rows(
  or_rows(fitm(risk ~ period + (1 | school_id), d$inst), d$inst, "Main"),
  or_rows(fitm(risk ~ period + (1 | school_id), d$inst_spec), d$inst_spec, "S1 special campaigns included"),
  or_rows(fitm(risk ~ period + country + (1 | school_id), d$inst), d$inst, "S2 country fixed effect"))
m3 <- fitm(risk ~ period * country + (1 | school_id), d$inst)
dr <- as_draws_df(m3)
for (k in c("IntermediateRisk", "HighRisk", "VeryHighRiskUnsafe")) {
  b <- dr[[paste0("b_mu", k, "_periodMonitoring")]]
  ia <- dr[[paste0("b_mu", k, "_periodMonitoring:countryRwanda")]]
  for (cc in c("Kenya", "Rwanda")) {
    x <- if (cc == "Kenya") b else b + ia
    res <- bind_rows(res, data.frame(analysis = paste0("S3 interaction (", cc, ")"), n = sum(d$inst$country == cc),
      schools = n_distinct(d$inst$school_id[d$inst$country == cc]), category = k,
      OR = exp(median(x)), lo = exp(quantile(x, .025)), hi = exp(quantile(x, .975)), rhat_max = max(rhat(m3), na.rm = TRUE)))
  }
}
dir.create("results", showWarnings = FALSE)
res <- res |> mutate(across(c(OR, lo, hi, rhat_max), ~ signif(.x, 3)))
write_csv(res, "results/institutional_sensitivity.csv"); print(res, row.names = FALSE)
