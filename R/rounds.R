# Round-by-round summary (Table 2 of the revised paper) and same-season comparisons.
# Run from the repository root after R/prep.R.
suppressMessages({library(readr); library(dplyr); library(tidyr)})
rd <- function(f) suppressWarnings(read_csv(file.path("data", f), show_col_types = FALSE, progress = FALSE))
relab <- function(x) case_when(
  x %in% c("Intermediate Risk/Possibly Safe", "Intermediate Risk/Probably Safe") ~ "Intermediate Risk",
  x %in% c("High Risk/Probably Unsafe", "High Risk/Possibly Unsafe") ~ "High Risk",
  x == "Unsafe" ~ "Very High Risk/Unsafe", TRUE ~ x)
LEV4 <- c("Low Risk/Safe", "Intermediate Risk", "High Risk", "Very High Risk/Unsafe")
summ <- function(df, project, round) df |> mutate(risk = relab(ecoli_risk)) |> filter(risk %in% LEV4) |>
  summarise(n = n(), detect_pct = 100 * mean(risk != "Low Risk/Safe"), vhigh_pct = 100 * mean(risk == "Very High Risk/Unsafe")) |>
  mutate(project = project, round = round, .before = 1)
pa <- function(x, project, round) tibble(project = project, round = round, n = length(x),
  detect_pct = 100 * mean(x == "Present"), vhigh_pct = NA_real_)

ls <- rd("LifeStraw_WQ.csv"); am <- rd("AmaziMeza_WQ.csv"); as <- rd("Asili_WQ.csv")
hel <- rd("Helvetas_WQ.csv"); bur <- rd("Burundi_WQ.csv"); mm <- rd("MWA_monitor.csv")
mb <- rd("MWA_baseline.csv"); d1 <- rd("DRIP_first_monitor.csv")
wb <- rd("WaterMission_baseline.csv"); wm <- rd("WaterMission_monitor.csv")

dep <- function(df, d) filter(df, deployment == d)
rounds <- bind_rows(
  summ(filter(ls, deployment == "July 2024 Baseline", sample_type == "Water point"), "LifeStraw", "Jul 2024 baseline"),
  summ(filter(ls, deployment == "January 2025 Baseline", sample_type == "Water point"), "LifeStraw", "Jan 2025 baseline"),
  summ(filter(ls, deployment == "Migori Baseline Surveys July 2025", sample_type == "Water point"), "LifeStraw", "Jul 2025 baseline (Migori)"),
  summ(filter(ls, deployment == "September 2024 Monitoring", sample_type == "Filter"), "LifeStraw", "Sep 2024 monitoring"),
  summ(filter(ls, deployment == "Jan Monitoring 2026", sample_type == "Filter"), "LifeStraw", "Jan 2026 monitoring"),
  summ(filter(ls, deployment == "May 2026 Monitoring", sample_type == "Filter"), "LifeStraw", "May 2026 monitoring"),
  summ(filter(ls, sample_type == "Water point", grepl("Monitoring", deployment)), "LifeStraw", "Water-point re-tests during monitoring (excluded)"),
  summ(dep(am, "2023 Baseline"), "Amazi Meza", "2023 baseline"),
  summ(dep(am, "Baseline Scale up"), "Amazi Meza", "Scale-up baseline"),
  summ(dep(am, "2025 Valver monitoring campaign"), "Amazi Meza", "2025 monitoring"),
  summ(dep(am, "January 2026 Monitoring Campaign"), "Amazi Meza", "Jan-Mar 2026 monitoring"),
  summ(dep(am, "July 2025 Cleaning Campaign"), "Amazi Meza", "Jul 2025 cleaning campaign (special)"),
  summ(dep(am, "2026 SWT cleaning campaign"), "Amazi Meza", "2026 cleaning campaign (special)"),
  summ(dep(am, "Contaminated Filters"), "Amazi Meza", "Re-test of flagged filters (special)"),
  summ(dep(as, "August/September 2024 Baseline"), "Asili", "Aug-Sep 2024 baseline"),
  summ(dep(as, "January 2025 - Monitoring"), "Asili", "Jan 2025 monitoring"),
  summ(dep(as, "Internal water testing September 2025"), "Asili", "Sep 2025 internal"),
  summ(filter(as, deployment == "Internal water testing September 2025", grepl("household", sample_location)), "Asili", "  Sep 2025 internal, households"),
  summ(filter(as, deployment == "Internal water testing September 2025", !grepl("household", sample_location)), "Asili", "  Sep 2025 internal, kiosks/water points"),
  summ(dep(as, "Training August 2025"), "Asili", "Aug 2025 enumerator training (excluded)"),
  pa(mb$ecoli_pa, "DRIP-FUNDI", "2024 baseline (P/A)"),
  pa(d1$ecoli_pa, "DRIP-FUNDI", "First monitoring, untraceable (excluded, P/A)"),
  summ(mm, "DRIP-FUNDI", "Mar 2026 monitoring"),
  summ(dep(hel, "April 2025_ Baseline"), "Ranovola", "Apr 2025 baseline"),
  summ(dep(hel, "Post_ Baseline_test_2025"), "Ranovola", "2025 post-baseline test (excluded)"),
  summ(dep(hel, "Internal campaign October 2025"), "Ranovola", "Oct 2025 internal"),
  summ(dep(hel, "March 2026 Internal Monitoring"), "Ranovola", "Mar 2026 internal"),
  summ(dep(hel, "Monitoring August 2026"), "Ranovola", "Aug 2026 monitoring"),
  summ(dep(bur, "Baseline March 2026"), "Amazi Water", "Mar 2026 baseline"),
  summ(dep(bur, "Additional sites September 2026"), "Amazi Water", "Sep 2026 monitoring"),
  summ(filter(bur, deployment == "Additional sites September 2026", sample_location == "Household"), "Amazi Water", "  Sep 2026, households"),
  summ(filter(bur, deployment == "Additional sites September 2026", sample_location == "Tap stand"), "Amazi Water", "  Sep 2026, tap stands"),
  pa(wb$ecoli_pa, "Water Mission", "2024 source assessment (P/A)"),
  pa(ifelse(wm$ecoli_pa == "Not Present", "Absent", "Present"), "Water Mission", "2026 monitoring (P/A)")
) |> mutate(across(c(detect_pct, vhigh_pct), ~ round(.x, 1)))
dir.create("results", showWarnings = FALSE)
write_csv(rounds, "results/rounds.csv")
print(as.data.frame(rounds))

# Amazi Water: systems sampled at baseline and at monitoring
sys_b <- unique(bur$system_code[bur$deployment == "Baseline March 2026"])
sys_m <- unique(bur$system_code[bur$deployment == "Additional sites September 2026"])
cat("\nAmazi Water systems: baseline", length(sys_b), "| monitoring", length(sys_m), "| in both", length(intersect(sys_b, sys_m)), "\n")
