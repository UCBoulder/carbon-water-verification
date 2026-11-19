# Figures 2-7 (Fig 1, the map, is R/render_map.R). Run from the repository root after
# R/prep.R and R/model.R. Writes PDF (for the LaTeX build) and 300 dpi TIFF (for submission).
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(scales); library(brms)})
d <- readRDS("prepped.rds")
LEV4 <- d$LEV4; GU <- d$GU
dir.create("figures", showWarnings = FALSE)
save_fig <- function(p, name, w = 7.5, h = 4.5) {
  ggsave(file.path("figures", paste0(name, ".pdf")), p, width = w, height = h)
  ggsave(file.path("figures", paste0(name, ".tif")), p, width = w, height = h, dpi = 300,
         compression = "lzw", bg = "white")
}
risk_cols <- c("Low Risk/Safe" = "#1B9E77", "Intermediate Risk" = "#E6AB02",
               "High Risk" = "#D95F02", "Very High Risk/Unsafe" = "#A6001A")
risk_cols5 <- c(risk_cols, setNames("#7570B3", GU))
th <- theme_minimal(base_size = 10) + theme(panel.grid.major.x = element_blank(), legend.position = "right")
nlab <- function(df, ...) df |> count(...) |> mutate(lab = paste0("n = ", comma(n)))

# ---- Fig 2: baseline risk by project ----
b <- d$base_all |> mutate(risk = factor(risk, levels = c(LEV4, GU)))
bn <- nlab(b, project)
p2 <- ggplot(b, aes(project, fill = risk)) + geom_bar(position = "fill", width = 0.7) +
  geom_text(data = bn, aes(project, 1.04, label = lab), inherit.aes = FALSE, size = 2.8, colour = "grey30") +
  scale_fill_manual(values = risk_cols5, drop = FALSE) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, .25), limits = c(0, 1.07)) + th +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(x = NULL, y = "Share of baseline samples", fill = expression(italic("E. coli") ~ "risk category"))
save_fig(p2, "Fig2", h = 4.8)

# ---- Fig 3: institutional projects, baseline (school water sources) vs monitoring (installed filters) ----
inst <- d$inst |> mutate(period = recode(period, Baseline = "Baseline\n(school water sources)",
                                          Monitoring = "Monitoring\n(installed filters)"))
inn <- nlab(inst, project, period)
p3 <- ggplot(inst, aes(period, fill = risk)) + geom_bar(position = "fill", width = 0.65) +
  geom_text(data = inn, aes(period, 1.05, label = lab), inherit.aes = FALSE, size = 2.8, colour = "grey30") +
  facet_wrap(~project) + scale_fill_manual(values = risk_cols) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, .25), limits = c(0, 1.08)) + th +
  labs(x = NULL, y = "Share of samples", fill = expression(italic("E. coli") ~ "risk category"))
save_fig(p3, "Fig3")

# ---- Fig 4: predicted probabilities from the institutional model ----
m <- readRDS("model.rds")
nd <- data.frame(period = factor(c("Baseline", "Monitoring"), levels = c("Baseline", "Monitoring")))
pp <- fitted(m, newdata = nd, re_formula = NA, summary = TRUE)
clean_cat <- function(x) gsub("\\)$", "", gsub("^P\\(Y = ", "", x))
pdf_df <- bind_rows(lapply(1:2, function(i) bind_rows(lapply(dimnames(pp)[[3]], function(ct) data.frame(
  period = nd$period[i], risk = clean_cat(ct), est = pp[i, "Estimate", ct], lo = pp[i, "Q2.5", ct], hi = pp[i, "Q97.5", ct]))))) |>
  mutate(risk = factor(risk, levels = LEV4))
stopifnot(!any(is.na(pdf_df$risk)))
dodge <- position_dodge(width = 0.7)
p4 <- ggplot(pdf_df, aes(risk, est, fill = period)) + geom_col(position = dodge, width = 0.65) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = dodge, width = 0.18, linewidth = 0.4, colour = "grey30") +
  geom_text(aes(label = sprintf("%.2f", est), y = hi), position = dodge, vjust = -0.6, size = 2.8, colour = "grey25") +
  scale_fill_manual(values = c(Baseline = "#D95F02", Monitoring = "#1B9E77")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2), expand = expansion(mult = c(0, 0.08))) +
  th + theme(legend.position = "top", legend.title = element_blank()) +
  labs(x = expression(italic("E. coli") ~ "risk category"), y = "Predicted probability")
save_fig(p4, "Fig4")

# ---- Fig 5: Asili, all three rounds, split by where the sample was drawn ----
as <- d$asili
asn <- nlab(as, round, where)
p5 <- ggplot(as, aes(where, fill = risk)) + geom_bar(position = "fill", width = 0.65) +
  geom_text(data = asn, aes(where, 1.05, label = lab), inherit.aes = FALSE, size = 2.8, colour = "grey30") +
  facet_grid(~round, scales = "free_x", space = "free_x") + scale_fill_manual(values = risk_cols, drop = FALSE) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, .25), limits = c(0, 1.08)) + th +
  labs(x = NULL, y = "Share of samples", fill = expression(italic("E. coli") ~ "risk category"))
save_fig(p5, "Fig5")

# ---- Fig 6: DRIP-FUNDI, baseline and monitoring, any detectable E. coli ----
dt <- d$drip |> mutate(Detected = pct_detect / 100, `Not detected` = 1 - pct_detect / 100) |>
  pivot_longer(c(Detected, `Not detected`), names_to = "status", values_to = "share") |>
  mutate(status = factor(status, levels = c("Not detected", "Detected")))
p6 <- ggplot(dt, aes(round, share, fill = status)) + geom_col(width = 0.55) +
  geom_text(data = d$drip, aes(round, 1.05, label = paste0("n = ", n)), inherit.aes = FALSE, size = 2.8, colour = "grey30") +
  scale_fill_manual(values = c("Not detected" = "#1B9E77", "Detected" = "#D95F02")) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, .25), limits = c(0, 1.08)) + th +
  labs(x = NULL, y = "Share of samples", fill = expression(italic("E. coli")))
save_fig(p6, "Fig6", w = 5.5)

# ---- Fig 7: Ranovola, four rounds ----
hv <- d$helvetas
hvn <- nlab(hv, round)
p7 <- ggplot(hv, aes(round, fill = risk)) + geom_bar(position = "fill", width = 0.65) +
  geom_text(data = hvn, aes(round, 1.05, label = lab), inherit.aes = FALSE, size = 2.8, colour = "grey30") +
  scale_fill_manual(values = risk_cols, drop = FALSE) +
  scale_y_continuous(labels = percent, breaks = seq(0, 1, .25), limits = c(0, 1.08)) + th +
  labs(x = NULL, y = "Share of household samples", fill = expression(italic("E. coli") ~ "risk category"))
save_fig(p7, "Fig7")
cat("FIGS DONE\n")
