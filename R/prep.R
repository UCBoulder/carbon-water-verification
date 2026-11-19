# ============================================================
# Prepare analysis datasets from the de-identified files in data/
# Run from the repository root:  Rscript R/prep.R   -> prepped.rds
# ============================================================
suppressMessages({library(readr); library(dplyr); library(tidyr); library(stringr)})
rd <- function(f) suppressWarnings(read_csv(file.path("data", f), show_col_types = FALSE, progress = FALSE))

# mWater reports six risk descriptors; these are sub-labels of the four WHO categories
# used throughout (Table 3 of the paper).
relab <- function(x) case_when(
  x %in% c("Intermediate Risk/Possibly Safe", "Intermediate Risk/Probably Safe") ~ "Intermediate Risk",
  x %in% c("High Risk/Probably Unsafe", "High Risk/Possibly Unsafe") ~ "High Risk",
  x == "Unsafe" ~ "Very High Risk/Unsafe",
  TRUE ~ x)
LEV4 <- c("Low Risk/Safe", "Intermediate Risk", "High Risk", "Very High Risk/Unsafe")

# ---- round partitioning ----
LS_BASE <- c("July 2024 Baseline", "January 2025 Baseline", "Migori Baseline Surveys July 2025")
LS_MON  <- c("September 2024 Monitoring", "Jan Monitoring 2026", "May 2026 Monitoring")
AM_BASE <- "2023 Baseline"                       # institutional model baseline
AM_BASE_ALL <- c("2023 Baseline", "Baseline Scale up")   # general baseline characterization
AM_MON  <- c("January 2026 Monitoring Campaign", "2025 Valver monitoring campaign")
AM_SPEC <- c("July 2025 Cleaning Campaign", "2026 SWT cleaning campaign", "Contaminated Filters")
AS_BASE <- "August/September 2024 Baseline"
AS_MON  <- c("January 2025 - Monitoring", "Internal water testing September 2025")
HEL_ROUNDS <- c("April 2025_ Baseline", "Internal campaign October 2025",
                "March 2026 Internal Monitoring", "Monitoring August 2026")
HEL_EXCL <- "Post_ Baseline_test_2025"          # excluded at the data owner's direction

ls <- rd("LifeStraw_WQ.csv") |> mutate(risk = relab(ecoli_risk))
am <- rd("AmaziMeza_WQ.csv") |> mutate(risk = relab(ecoli_risk))
as <- rd("Asili_WQ.csv")     |> mutate(risk = relab(ecoli_risk))
hel <- rd("Helvetas_WQ.csv") |> mutate(risk = relab(ecoli_risk))
bur <- rd("Burundi_WQ.csv")  |> mutate(risk = relab(ecoli_risk))
mwa_b <- rd("MWA_baseline.csv")
mwa_m <- rd("MWA_monitor.csv") |> mutate(risk = relab(ecoli_risk))
wm_b <- rd("WaterMission_baseline.csv")
wm_m <- rd("WaterMission_monitor.csv")

# ============================================================
# INSTITUTIONAL (LifeStraw + Amazi Meza): cohort of schools with both periods
# ============================================================
inst_build <- function(am_mon = AM_MON) {
  ls_b <- ls |> filter(deployment %in% LS_BASE, sample_type == "Water point") |>
    transmute(school_id = school_code, risk, period = "Baseline", country = "Kenya", project = "LifeStraw (Kenya)")
  ls_m <- ls |> filter(deployment %in% LS_MON, sample_type == "Filter") |>
    transmute(school_id = school_code, risk, period = "Monitoring", country = "Kenya", project = "LifeStraw (Kenya)")
  lc <- intersect(ls_b$school_id, ls_m$school_id)
  am_b <- am |> filter(deployment == AM_BASE) |>
    transmute(school_id = school_code, risk, period = "Baseline", country = "Rwanda", project = "Amazi Meza (Rwanda)")
  am_r <- am |> filter(deployment %in% AM_MON) |> pull(school_code)
  ac <- intersect(am_b$school_id, am_r)        # cohort defined by the routine rounds
  am_m <- am |> filter(deployment %in% am_mon) |>
    transmute(school_id = school_code, risk, period = "Monitoring", country = "Rwanda", project = "Amazi Meza (Rwanda)")
  bind_rows(filter(ls_b, school_id %in% lc), filter(ls_m, school_id %in% lc),
            filter(am_b, school_id %in% ac), filter(am_m, school_id %in% ac)) |>
    filter(risk %in% LEV4) |>
    mutate(risk = factor(risk, levels = LEV4), period = factor(period, levels = c("Baseline", "Monitoring")))
}
inst <- inst_build()
inst_spec <- inst_build(c(AM_MON, AM_SPEC))

# ============================================================
# COMMUNITY PROJECTS
# ============================================================
asili <- as |> filter(deployment %in% c(AS_BASE, AS_MON), risk %in% LEV4) |>
  mutate(round = factor(recode(deployment, !!AS_BASE := "Baseline\nAug-Sep 2024",
                               "January 2025 - Monitoring" = "Monitoring\nJan 2025",
                               "Internal water testing September 2025" = "Internal round\nSep 2025"),
                        levels = c("Baseline\nAug-Sep 2024", "Monitoring\nJan 2025", "Internal round\nSep 2025")),
         where = case_when(deployment == AS_BASE ~ "Household",
                           str_detect(sample_location, "household") ~ "Household",
                           TRUE ~ "Kiosk/water point"),
         risk = factor(risk, levels = LEV4))

helvetas <- hel |> filter(deployment %in% HEL_ROUNDS, risk %in% LEV4) |>
  mutate(round = factor(deployment, levels = HEL_ROUNDS,
                        labels = c("Baseline\nApr 2025", "Internal\nOct 2025", "Internal\nMar 2026", "Verification\nAug 2026")),
         risk = factor(risk, levels = LEV4))

burundi <- bur |> filter(risk %in% LEV4) |> mutate(risk = factor(risk, levels = LEV4))

drip <- tibble(
  round = factor(c("Baseline\n2024 (presence/absence)", "Monitoring\nMar 2026 (MPN)"),
                 levels = c("Baseline\n2024 (presence/absence)", "Monitoring\nMar 2026 (MPN)")),
  n = c(nrow(mwa_b), nrow(mwa_m)),
  pct_detect = c(mean(mwa_b$ecoli_pa == "Present") * 100, mean(mwa_m$risk != "Low Risk/Safe") * 100))

# ============================================================
# GENERAL BASELINE (all baseline records, all seven projects)
# ============================================================
GU <- "E. coli present (presence/absence test)"
base_all <- bind_rows(
  am |> filter(deployment %in% AM_BASE_ALL) |> transmute(risk, project = "Amazi Meza (Rwanda)"),
  ls |> filter(deployment %in% LS_BASE) |> transmute(risk, project = "LifeStraw (Kenya)"),
  as |> filter(deployment == AS_BASE) |> transmute(risk, project = "Asili (DRC)"),
  hel |> filter(deployment == "April 2025_ Baseline") |> transmute(risk, project = "Ranovola (Madagascar)"),
  bur |> filter(deployment == "Baseline March 2026") |> transmute(risk, project = "Amazi Water (Burundi)"),
  wm_b |> transmute(risk = ifelse(ecoli_pa == "Present", GU, "Low Risk/Safe"), project = "Water Mission (Tanzania)"),
  mwa_b |> transmute(risk = ifelse(ecoli_pa == "Present", GU, "Low Risk/Safe"), project = "DRIP-FUNDI (Kenya)")) |>
  filter(!is.na(risk))

saveRDS(list(inst = inst, inst_spec = inst_spec, asili = asili, helvetas = helvetas, burundi = burundi,
             drip = drip, base_all = base_all, wm_m = wm_m, GU = GU, LEV4 = LEV4), "prepped.rds")

# ============================================================
# REPORT
# ============================================================
cat("Institutional model: n =", nrow(inst), "| schools =", n_distinct(inst$school_id), "\n")
print(inst |> count(project, period) |> pivot_wider(names_from = period, values_from = n))
print(inst |> distinct(project, school_id) |> count(project, name = "schools"))
cat("With special campaigns: n =", nrow(inst_spec), "\n")
cat("\nGeneral baseline: n =", nrow(base_all),
    "| any E. coli =", round(mean(base_all$risk != "Low Risk/Safe") * 100, 1), "%\n")
g <- base_all |> filter(!project %in% c("Water Mission (Tanzania)", "DRIP-FUNDI (Kenya)"))
cat("Graded (MPN) baseline: n =", nrow(g), "\n"); print(round(prop.table(table(factor(g$risk, levels = LEV4))) * 100, 1))
cat("\nResults with an E. coli result (excluding the 16 Ranovola records excluded by the data owner;",
    "DRIP_first_monitor.csv is not read):",
    sum(!is.na(ls$ecoli_risk)) + sum(!is.na(am$ecoli_risk)) + sum(!is.na(as$ecoli_risk)) +
    sum(!is.na(hel$ecoli_risk[hel$deployment != HEL_EXCL])) + sum(!is.na(bur$ecoli_risk)) +
    sum(!is.na(mwa_m$ecoli_risk)) + nrow(mwa_b) + nrow(wm_b) + nrow(wm_m), "\n")
cat("\nDRIP-FUNDI\n"); print(drip)
