# script2025.R
# Part 1: replicates "Threshold Dose Response of Aluminum Adjuvants Seen in
#         Population Data" (doc/preprints-185592-presentation.pdf), Tables 1-7,
#         on data/joined_data2025.csv (birth years 2011-2017 by default).
# Part 2: models the state-level glyphosate exposure index against the same
#         outcomes, with blood disorders as the negative control.
# Part 3: aluminum dose-count model; its residuals vs glyphosate.
# Part 4: same model with micrograms of aluminum per window (low/high product bounds).
#
# Usage:  Rscript script2025.R [joined.csv] [results_dir] > results/script2025_output.txt
#   e.g.  Rscript script2025.R data/joined_data2025_nis_reconstructed.csv results_nis_reconstructed
# Needs only base R + mgcv (ships with R). Tables are also written to results/.

suppressMessages(library(mgcv))
options(width = 200)

args <- commandArgs(trailingOnly = TRUE)
IN_CSV  <- if (length(args) >= 1) args[1] else "data/joined_data2025.csv"
RES_DIR <- if (length(args) >= 2) args[2] else "results"
a_all <- read.csv(IN_CSV)
dir.create(RES_DIR, showWarnings = FALSE)
cat("Input:", IN_CSV, "\n")

PAPER_YEARS <- 2011:2017   # birth years used in the paper
GLYPH_YEARS <- sort(unique(a_all$year[!is.na(a_all$glyph_popw_high)]))  # birth years with USGS glyphosate estimates

outcomes <- c(autism = "asd", allergies = "allergies", adhd = "adhd",
              asthma = "asthma", blood = "blood", epilepsy = "epilepsy",
              obesity = "obesity", tourettes = "tourettes")
NEG_CONTROL <- "blood"
prev  <- paste0(outcomes, "_prev")
count <- paste0(outcomes, "_count")
names(prev) <- names(count) <- names(outcomes)

# ============================================================== helpers
stars <- function(p) ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))
fmt_r <- function(r, p) ifelse(is.na(r), "NA", paste0(sub("^(-?)0\\.", "\\1.", sprintf("%.2f", r)), stars(p)))

# Pearson correlation table (rows x cols), formatted like the paper's tables
cor_table <- function(d, rows, cols, rnames = rows, cnames = cols, title = "", file = NULL) {
  r <- p <- matrix(NA, length(rows), length(cols), dimnames = list(rnames, cnames))
  for (i in seq_along(rows)) for (j in seq_along(cols)) {
    if (rows[i] == cols[j]) next
    ok <- complete.cases(d[[rows[i]]], d[[cols[j]]])
    if (sum(ok) < 4) next
    ct <- cor.test(d[[rows[i]]][ok], d[[cols[j]]][ok])
    r[i, j] <- ct$estimate; p[i, j] <- ct$p.value
  }
  out <- matrix(fmt_r(r, p), nrow(r), dimnames = dimnames(r))
  out[is.na(r) & outer(rows, cols, "==")] <- "-"
  cat("\n", title, "\n", sep = "")
  print(noquote(out))
  cat("Note. Pearson r. * p<.05, ** p<.01, *** p<.001\n")
  if (!is.null(file)) write.csv(data.frame(var = rnames, out, check.names = FALSE),
                                file.path(RES_DIR, file), row.names = FALSE)
  invisible(list(r = r, p = p))
}

pooled_prev <- function(d, o) sum(d[[prev[o]]] * d[[count[o]]], na.rm = TRUE) / sum(d[[count[o]]][!is.na(d[[prev[o]]])])

# Cluster-robust (CR1) covariance for a weighted lm, clustered by state
vcov_cl <- function(fit, cl) {
  keep <- !is.na(coef(fit))
  X <- model.matrix(fit)[, keep, drop = FALSE]
  w <- weights(fit); if (is.null(w)) w <- rep(1, nrow(X))
  e <- resid(fit)
  bread <- solve(crossprod(X * sqrt(w)))
  U <- rowsum(X * (w * e), cl)
  G <- nrow(U); n <- nrow(X); k <- ncol(X)
  (G / (G - 1)) * ((n - 1) / (n - k)) * bread %*% crossprod(U) %*% bread
}

# Weighted regression of one outcome on `xvar` (+ optional covariates).
# Weights = NSCH sample size, as in the paper. Returns the xvar coefficient
# with naive and state-clustered inference.
wls <- function(d, o, xvar, covars = NULL) {
  pv <- paste0(o, "_prev"); cn <- paste0(o, "_count")
  vars <- c(pv, cn, xvar, all.vars(as.formula(paste("~", paste(c("1", covars), collapse = "+")))), "region")
  d <- d[complete.cases(d[, unique(vars)]), ]
  f <- as.formula(paste(pv, "~", paste(c(xvar, covars), collapse = " + ")))
  fit <- lm(f, d, weights = d[[cn]])
  s <- summary(fit)$coefficients
  V <- vcov_cl(fit, d$region)
  se_cl <- sqrt(V[xvar, xvar]); G <- length(unique(d$region))
  b <- coef(fit)[xvar]
  data.frame(outcome = names(outcomes)[outcomes == o], n = nrow(d), beta = b,
             t = s[xvar, "t value"], p = s[xvar, "Pr(>|t|)"],
             se_cl = se_cl, p_cl = 2 * pt(-abs(b / se_cl), G - 1),
             r2 = summary(fit)$r.squared,
             # effect of +1 SD of exposure, as % of mean prevalence
             pct_per_sd = 100 * b * sd(d[[xvar]]) / weighted.mean(d[[pv]], d[[cn]]),
             row.names = NULL)
}

print_df <- function(x, title, file = NULL, digits = 4) {
  cat("\n", title, "\n", sep = "")
  num <- sapply(x, is.numeric)
  y <- x; y[num] <- lapply(x[num], function(v) signif(v, digits))
  print(y, row.names = FALSE)
  if (!is.null(file)) write.csv(x, file.path(RES_DIR, file), row.names = FALSE)
}

# ============================================================== vaccine windows
# Same construction as script2022.R: likelihood that a dose of each
# aluminum-adjuvanted vaccine was received within each age window.
add_windows <- function(a) {
  a$DTaP.3  <- a$DTaP.1Dose.3
  a$DTaP.5  <- a$DTaP.2Doses.5
  a$DTaP.7  <- a$DTaP.3Doses.7
  a$DTaP.13 <- a$DTaP.3Doses.13 - a$DTaP.3Doses.7
  a$DTaP.19 <- a$DTaP.3Doses.19 - a$DTaP.3Doses.13
  a$DTaP.24 <- (a$DTaP.3Doses.24 - a$DTaP.3Doses.19) + (a$DTaP.4Doses.24 - a$DTaP.4Doses.19)
  a$DTaP.35 <- (a$DTaP.3Doses.35 - a$DTaP.3Doses.24) + (a$DTaP.4Doses.35 - a$DTaP.4Doses.24)

  a$HepB.3  <- a$HepB.1Dose.3
  a$HepB.5  <- a$HepB.2Doses.5
  a$HepB.7  <- a$HepB.2Doses.7 - a$HepB.2Doses.5
  a$HepB.13 <- (a$HepB.2Doses.13 - a$HepB.2Doses.7) + (a$HepB.3Doses.13 - a$HepB.3Doses.7)
  a$HepB.19 <- a$HepB.3Doses.19 - a$HepB.3Doses.13
  a$HepB.24 <- a$HepB.3Doses.24 - a$HepB.3Doses.19
  a$HepB.35 <- a$HepB.3Doses.35 - a$HepB.3Doses.24

  a$Hib.3  <- a$Hib.1Dose.3
  a$Hib.5  <- a$Hib.2Doses.5
  a$Hib.7  <- a$Hib.2Doses.7 - a$Hib.2Doses.5
  a$Hib.13 <- (a$Hib.2Doses.13 - a$Hib.2Doses.7) + (a$Hib.3Doses.13 - a$Hib.3Doses.7)
  a$Hib.19 <- a$Hib.3Doses.19 - a$Hib.3Doses.13
  a$Hib.24 <- a$Hib.3Doses.24 - a$Hib.3Doses.19
  a$Hib.35 <- a$Hib.3Doses.35 - a$Hib.3Doses.24

  a$PCV.3  <- a$PCV.1Dose.3
  a$PCV.5  <- a$PCV.2Doses.5
  a$PCV.7  <- a$PCV.3Doses.7
  a$PCV.13 <- a$PCV.3Doses.13 - a$PCV.3Doses.7
  a$PCV.19 <- a$PCV.3Doses.19 - a$PCV.3Doses.13
  a$PCV.24 <- (a$PCV.3Doses.24 - a$PCV.3Doses.19) + (a$PCV.4Doses.24 - a$PCV.4Doses.19)
  a$PCV.35 <- (a$PCV.3Doses.35 - a$PCV.3Doses.24) + (a$PCV.4Doses.35 - a$PCV.4Doses.24)

  a$Polio.3  <- a$Polio.1Dose.3
  a$Polio.5  <- a$Polio.2Doses.5
  a$Polio.7  <- a$Polio.2Doses.7 - a$Polio.2Doses.5
  a$Polio.13 <- a$Polio.2Doses.13 - a$Polio.2Doses.7
  a$Polio.19 <- a$Polio.3Doses.19
  a$Polio.24 <- a$Polio.3Doses.24 - a$Polio.3Doses.19
  a$Polio.35 <- a$Polio.3Doses.35 - a$Polio.3Doses.24

  for (m in c(3, 5, 7, 13, 19, 24, 35))
    a[[paste0("All.", m)]] <- rowSums(a[, paste0(c("DTaP", "HepB", "Hib", "PCV", "Polio"), ".", m)])
  a
}
a_all <- add_windows(a_all)

vaccines <- c("DTaP", "HepB", "Hib", "PCV", "Polio")
windows  <- c("0m-2m" = 3, "2m-4m" = 5, "4m-6m" = 7, "6m-12m" = 13,
              "12m-18m" = 19, "18m-24m" = 24, "24m-36m" = 35)

# ============================================================== PART 1: paper
a <- a_all[a_all$year %in% PAPER_YEARS, ]
cat("==================== PART 1: replication of the paper ====================\n")
cat(sprintf("Birth years %d-%d, %d state-year rows\n", min(a$year), max(a$year), nrow(a)))

t1 <- data.frame(disease = names(outcomes),
                 prevalence_pct = sapply(names(outcomes), function(o) 100 * pooled_prev(a, o)),
                 n = sapply(names(outcomes), function(o) sum(a[[count[o]]])))
print_df(t1, "Table 1. Prevalence of disease as measured in NSCH.", "paper_table1_prevalence.csv")

cor_table(a, prev, prev, names(outcomes), names(outcomes),
          "Table 2. Associations between disease prevalence.", "paper_table2_disease_cor.csv")

t3 <- do.call(rbind, lapply(outcomes, function(o) wls(a, o, "All.13")))
print_df(t3[, c("outcome", "beta", "t", "p", "r2", "p_cl")],
         "Table 3. Prevalence ~ summed aluminum-vaccine likelihood at 6-12 months (All.13), weighted by NSCH n.\n(p_cl = p-value with state-clustered SEs, not in the paper)",
         "paper_table3_regression_All13.csv")

for (tb in list(list(4, 13, "6-12"), list(5, 7, "4-6"), list(6, 19, "12-18"))) {
  cor_table(a, prev, paste0(vaccines, ".", tb[[2]]), names(outcomes), vaccines,
            sprintf("Table %d. Vaccine likelihood from %s months vs chronic disease.", tb[[1]], tb[[3]]),
            sprintf("paper_table%d_vaccines_%s.csv", tb[[1]], tb[[2]]))
}
cat(sprintf("\ncor(Hib.13, HepB.13) = %.2f\n", cor(a$Hib.13, a$HepB.13, use = "complete")))

cor_table(a, prev, paste0("All.", windows), names(outcomes), names(windows),
          "Table 7. Aggregated vaccine likelihood vs chronic disease for each time bucket.",
          "paper_table7_windows.csv")

fit_asd3 <- lm(asd_prev3 ~ All.13, a, weights = a$asd_count)
cat("\nAutism diagnosed before age 3 ~ All.13 (from script2022.R):\n")
print(summary(fit_asd3)$coefficients)

# Extended cohorts: later birth years are surveyed younger, so "ever diagnosed"
# prevalence falls with birth year. Birth-year fixed effects absorb that.
ae <- a_all[complete.cases(a_all$All.13), ]
t3e <- do.call(rbind, lapply(outcomes, function(o) wls(ae, o, "All.13", "factor(year)")))
print_df(t3e[, c("outcome", "n", "beta", "p", "p_cl", "pct_per_sd")],
         sprintf("Table 3 extended: birth years %d-%d, prevalence ~ All.13 + birth-year fixed effects", min(ae$year), max(ae$year)),
         "paper_table3_extended_yearFE.csv")

# ============================================================== PART 2: glyphosate
cat("\n\n==================== PART 2: glyphosate ====================\n")
g <- a_all[a_all$year %in% GLYPH_YEARS & !is.na(a_all$glyph_popw_high), ]
# Exposure is very right-skewed (DC = 0, Iowa ~ 76 kg/km2); use log(x + 0.1).
lg <- function(x) log(x + 0.1)
g$glyph      <- lg(g$glyph_popw_high)      # primary: population-weighted kg/km2, EPest-high
g$glyph_low  <- lg(g$glyph_popw_low)
g$glyph_birw <- lg(g$glyph_birthw_high)
g$glyph_area <- lg(g$glyph_kg_km2_high)
g$glyph_cap  <- log(g$glyph_kg_per_capita_high + 0.001)
cat(sprintf("Birth years %d-%d, %d states (AK/HI not covered by USGS), %d rows\n",
            min(g$year), max(g$year), length(unique(g$region)), nrow(g)))
cat(sprintf("Share of log-exposure variance between states: %.1f%%\n",
            100 * summary(lm(glyph ~ factor(region), g))$r.squared))

expos <- c(popw_high = "glyph", popw_low = "glyph_low", birthw_high = "glyph_birw",
           area_high = "glyph_area", per_capita = "glyph_cap")
cor_table(g, prev, expos, names(outcomes), names(expos),
          "Table G1. Correlation of disease prevalence with log glyphosate exposure (several index definitions).",
          "glyph_table1_cor.csv")

cor_table(g, c("glyph", paste0("All.", windows)), c("glyph", paste0("All.", windows)),
          c("glyphosate", names(windows)), c("glyphosate", names(windows)),
          "Table G2. Correlation of glyphosate exposure with aluminum-vaccine windows (confounding check).")

# Regression models; primary exposure = log population-weighted kg/km2 (high)
models <- list(
  "M1 crude"                          = NULL,
  "M2 + birth year FE"                = "factor(year)",
  "M3 + birth year FE + All.13"       = c("factor(year)", "All.13"),
  "M4 + birth year FE + age at survey"= c("factor(year)", "sc_age_mean"),
  "M5 state FE + birth year FE"       = c("factor(year)", "factor(region)")
)
gm <- do.call(rbind, lapply(names(models), function(m)
  cbind(model = m, do.call(rbind, lapply(outcomes, function(o) wls(g, o, "glyph", models[[m]]))))))
gm$sig <- stars(gm$p_cl)
print_df(gm[, c("model", "outcome", "n", "beta", "p", "se_cl", "p_cl", "sig", "pct_per_sd")],
         paste("Table G3. Weighted regression of prevalence on log glyphosate exposure.",
               "beta = change in prevalence per 1 log-unit; p = naive; p_cl/sig = state-clustered;",
               "pct_per_sd = effect of +1 SD exposure as % of mean prevalence.",
               "M5 uses only within-state year-to-year changes (little variation; low power).", sep = "\n"),
         "glyph_table3_models.csv")

# Quasibinomial GLM (counts rather than proportions) as a robustness check for M2
qb <- do.call(rbind, lapply(names(outcomes), function(o) {
  d <- g[!is.na(g[[prev[o]]]), ]
  yes <- round(d[[prev[o]]] * d[[count[o]]])
  fit <- glm(cbind(yes, d[[count[o]]] - yes) ~ glyph + factor(year), quasibinomial, data = d)
  s <- summary(fit)$coefficients["glyph", ]
  data.frame(outcome = o, odds_ratio_per_log = exp(s[1]), t = s[3], p = s[4], sig = stars(s[4]))
}))
print_df(qb, "Table G4. Quasibinomial GLM: prevalence ~ log glyphosate + birth-year FE.", "glyph_table4_quasibinomial.csv")

# Dose response by exposure quintile (birth-year FE, reference = lowest quintile)
g$glyph_q <- cut(g$glyph, quantile(g$glyph, 0:5 / 5), include.lowest = TRUE, labels = paste0("Q", 1:5))
qt <- do.call(rbind, lapply(names(outcomes), function(o) {
  d <- g[!is.na(g[[prev[o]]]), ]
  fit <- lm(as.formula(paste(prev[o], "~ glyph_q + factor(year)")), d, weights = d[[count[o]]])
  V <- vcov_cl(fit, d$region)
  qn <- paste0("glyph_qQ", 2:5)
  b <- coef(fit)[qn]
  se <- sqrt(diag(V)[qn])
  p <- 2 * pt(-abs(b / se), length(unique(d$region)) - 1)
  base <- weighted.mean(d[[prev[o]]][d$glyph_q == "Q1"], d[[count[o]]][d$glyph_q == "Q1"])
  data.frame(outcome = o, Q1_prev = base,
             t(setNames(paste0(sprintf("%+.4f", b), stars(p)), paste0("Q", 2:5, "_vs_Q1"))))
}))
cat("\nQuintile cut points (kg/km2, population-weighted, high):",
    paste(round(exp(quantile(g$glyph, 0:5 / 5)) - 0.1, 2), collapse = " | "), "\n")
print_df(qt, "Table G5. Prevalence difference by glyphosate quintile vs Q1 (birth-year FE, state-clustered stars).",
         "glyph_table5_quintiles.csv")

# Nonlinearity: GAM smooth of exposure with birth-year FE
gam_t <- do.call(rbind, lapply(names(outcomes), function(o) {
  d <- g[!is.na(g[[prev[o]]]), ]
  fit <- gam(as.formula(paste(prev[o], "~ s(glyph, k = 5) + factor(year)")), data = d,
             weights = d[[count[o]]] / mean(d[[count[o]]]), method = "REML")
  st <- summary(fit)$s.table
  data.frame(outcome = o, edf = st[1, "edf"], F = st[1, "F"], p = st[1, "p-value"], sig = stars(st[1, "p-value"]))
}))
print_df(gam_t, "Table G6. GAM smooth term s(log glyphosate) + birth-year FE (edf > 1 suggests nonlinearity; naive SEs).",
         "glyph_table6_gam.csv")
pdf(file.path(RES_DIR, "glyph_gam_smooths.pdf"), width = 10, height = 5)
par(mfrow = c(2, 4), mar = c(4, 4, 2, 1))
for (o in names(outcomes)) {
  d <- g[!is.na(g[[prev[o]]]), ]
  fit <- gam(as.formula(paste(prev[o], "~ s(glyph, k = 5) + factor(year)")), data = d,
             weights = d[[count[o]]] / mean(d[[count[o]]]), method = "REML")
  plot(fit, select = 1, shade = TRUE, main = o, xlab = "log(glyphosate kg/km2 + 0.1)", ylab = "partial effect")
}
invisible(dev.off())

# Sensitivity of M2 to exposure definition / data vintage
sens <- list(
  "popw_high (primary)"        = list(g, "glyph"),
  "popw_low"                   = list(g, "glyph_low"),
  "births-weighted high"       = list(g, "glyph_birw"),
  "state kg/km2 high"          = list(g, "glyph_area"),
  "kg per capita high"         = list(g, "glyph_cap"),
  "final USGS est. only / paper years (2011-2017)" = list(g[g$glyph_prelim == 0, ], "glyph")
)
st <- do.call(rbind, lapply(names(sens), function(s) do.call(rbind, lapply(outcomes, function(o) {
  r <- wls(sens[[s]][[1]], o, sens[[s]][[2]], "factor(year)")
  data.frame(sensitivity = s, outcome = r$outcome, beta = r$beta, p_cl = r$p_cl, sig = stars(r$p_cl))
}))))
cell <- function(s, o) { r <- st[st$sensitivity == s & st$outcome == o, ]; paste0(sprintf("%+.4f", r$beta), r$sig) }
sw <- data.frame(sensitivity = names(sens), sapply(names(outcomes), function(o) sapply(names(sens), cell, o = o)), row.names = NULL)
print_df(sw, "Table G7. Sensitivity of M2 (birth-year FE) glyphosate coefficient; stars use state-clustered p.",
         "glyph_table7_sensitivity.csv")

# Negative-control summary
cat("\n==================== Negative control (blood disorders) ====================\n")
nc <- gm[gm$outcome == NEG_CONTROL, c("model", "beta", "p_cl", "pct_per_sd")]
print(nc, row.names = FALSE)
inf <- gm[gm$outcome != NEG_CONTROL & gm$model == "M2 + birth year FE", ]
cat(sprintf("M2: %d of %d inflammatory outcomes significant (clustered p<.05); blood p_cl = %.3f\n",
            sum(inf$p_cl < .05), nrow(inf), gm$p_cl[gm$outcome == NEG_CONTROL & gm$model == "M2 + birth year FE"]))
cat("A glyphosate association with blood disorders comparable to the inflammatory outcomes\n",
    "would point to confounding (state-level differences unrelated to inflammation).\n", sep = "")

# ============================================================== PART 3: aluminum model residuals vs glyphosate
# Step 1: for each outcome, model prevalence from the aluminum-vaccine exposure
#         windows (all seven, so the paper's sign-reversal pattern is captured)
#         plus birth-year FE (age at survey / secular trend).
# Step 2: compare what the aluminum model leaves unexplained against glyphosate.
#   (a) residual ~ glyphosate                          (the direct comparison)
#   (b) joint model: aluminum + year FE + glyphosate   (Frisch-Waugh: same as
#       regressing the residual on glyphosate residualized on the same terms;
#       corrects (a) for the small correlation between glyphosate and the windows)
# All fits use the same sample: birth years 2011-2019, rows with a glyphosate value.
cat("\n\n==================== PART 3: aluminum exposure model, residuals vs glyphosate ====================\n")

wald_cl <- function(fit, cl, terms) {
  V <- vcov_cl(fit, cl); b <- coef(fit)[terms]
  q <- length(terms); G <- length(unique(cl))
  Fs <- as.numeric(t(b) %*% solve(V[terms, terms]) %*% b) / q
  c(F = Fs, p = pf(Fs, q, G - 1, lower.tail = FALSE))
}
wssr <- function(fit) sum(weights(fit) * resid(fit)^2)

alum <- paste0("All.", windows)
g3 <- g[complete.cases(g[, alum]), ]
g3$glyph_q <- cut(g3$glyph, quantile(g3$glyph, 0:5 / 5), include.lowest = TRUE, labels = paste0("Q", 1:5))
cat(sprintf("Sample: birth years %d-%d, %d states, %d rows\n",
            min(g3$year), max(g3$year), length(unique(g3$region)), nrow(g3)))

alum_coef <- list(); alum_fit <- list(); resid_tab <- list(); quint_tab <- list(); res_out <- g3[, c("year", "region", "glyph")]
for (o in names(outcomes)) {
  pv <- prev[[o]]; cn <- count[[o]]
  d <- g3[!is.na(g3[[pv]]), ]
  w <- d[[cn]]
  f0 <- lm(as.formula(paste(pv, "~ factor(year)")), d, weights = w)
  fA <- lm(as.formula(paste(pv, "~", paste(alum, collapse = " + "), "+ factor(year)")), d, weights = w)
  fJ <- update(fA, . ~ . + glyph)
  wA <- wald_cl(fA, d$region, alum)

  # aluminum model summary
  V <- vcov_cl(fA, d$region); b <- coef(fA)[alum]; p <- 2 * pt(-abs(b / sqrt(diag(V)[alum])), length(unique(d$region)) - 1)
  alum_coef[[o]] <- data.frame(outcome = o, t(setNames(paste0(sprintf("%+.3f", b), stars(p)), names(windows))), check.names = FALSE)
  alum_fit[[o]] <- data.frame(outcome = o, n = nrow(d), r2_full = summary(fA)$r.squared,
                              partial_r2_alum = 1 - wssr(fA) / wssr(f0),
                              wald_F = wA[["F"]], wald_p_cl = wA[["p"]], sig = stars(wA[["p"]]))

  # residual comparison
  d$res <- resid(fA)
  res_out[match(paste(d$year, d$region), paste(res_out$year, res_out$region)), paste0(o, "_alum_resid")] <- d$res
  fR <- lm(res ~ glyph, d, weights = w)
  VR <- vcov_cl(fR, d$region)
  ct <- cor.test(d$res, d$glyph)
  bJ <- coef(fJ)["glyph"]; seJ <- sqrt(vcov_cl(fJ, d$region)["glyph", "glyph"])
  pJ <- 2 * pt(-abs(bJ / seJ), length(unique(d$region)) - 1)
  gm_r <- gam(res ~ s(glyph, k = 5), data = d, weights = w / mean(w), method = "REML")
  mp <- weighted.mean(d[[pv]], w)
  resid_tab[[o]] <- data.frame(
    outcome = o,
    r_resid_glyph = ct$estimate, p_r = ct$p.value,
    beta_resid = coef(fR)["glyph"], p_resid_cl = 2 * pt(-abs(coef(fR)["glyph"] / sqrt(VR["glyph", "glyph"])), length(unique(d$region)) - 1),
    beta_joint = bJ, p_joint_cl = pJ, sig_joint = stars(pJ),
    pct_per_sd_joint = 100 * bJ * sd(d$glyph) / mp,
    partial_r2_glyph = 1 - wssr(fJ) / wssr(fA),
    gam_edf = summary(gm_r)$s.table[1, "edf"], gam_p_naive = summary(gm_r)$s.table[1, "p-value"],
    row.names = NULL)

  # mean residual by glyphosate quintile, as % of mean prevalence (state-clustered test vs Q1)
  fq <- lm(res ~ glyph_q, d, weights = w); Vq <- vcov_cl(fq, d$region)
  qn <- paste0("glyph_qQ", 2:5)
  pq <- 2 * pt(-abs(coef(fq)[qn] / sqrt(diag(Vq)[qn])), length(unique(d$region)) - 1)
  mq <- tapply(seq_len(nrow(d)), d$glyph_q, function(i) weighted.mean(d$res[i], w[i]))
  quint_tab[[o]] <- data.frame(outcome = o, t(setNames(
    paste0(sprintf("%+.1f%%", 100 * mq / mp), c("", stars(pq))), paste0("Q", 1:5))))
}

print_df(do.call(rbind, alum_fit),
         paste("Table A1. Aluminum exposure model: prevalence ~ 7 aluminum-vaccine windows + birth-year FE (weighted by NSCH n).",
               "partial_r2_alum = share of variance beyond birth year explained by the windows; wald = joint test of the 7 windows (state-clustered).", sep = "\n"),
         "alum_table1_fit.csv")
ac <- do.call(rbind, alum_coef)
cat("\nTable A2. Aluminum model coefficients by age window (change in prevalence per unit summed vaccine likelihood; state-clustered stars).\n")
print(ac, row.names = FALSE)
write.csv(ac, file.path(RES_DIR, "alum_table2_coefficients.csv"), row.names = FALSE)

rt <- do.call(rbind, resid_tab)
print_df(rt,
         paste("Table A3. Aluminum-model residuals vs log glyphosate exposure.",
               "r/beta_resid: residual regressed directly on glyphosate.",
               "beta_joint: glyphosate coefficient added to the aluminum model (Frisch-Waugh adjusted); pct_per_sd_joint = +1 SD glyphosate as % of mean prevalence.",
               "partial_r2_glyph = share of the aluminum model's residual variance explained by glyphosate. GAM p-values are not clustered.", sep = "\n"),
         "alum_table3_resid_vs_glyph.csv")

qt3 <- do.call(rbind, quint_tab)
cat("\nTable A4. Mean aluminum-model residual by glyphosate quintile, as % of mean prevalence (stars: Q vs Q1, state-clustered).\n")
cat("Quintile cut points (kg/km2):", paste(round(exp(quantile(g3$glyph, 0:5 / 5)) - 0.1, 2), collapse = " | "), "\n")
print(qt3, row.names = FALSE)
write.csv(qt3, file.path(RES_DIR, "alum_table4_resid_by_glyph_quintile.csv"), row.names = FALSE)

write.csv(res_out, file.path(RES_DIR, "alum_model_residuals.csv"), row.names = FALSE)

pdf(file.path(RES_DIR, "alum_resid_vs_glyph.pdf"), width = 12, height = 6)
par(mfrow = c(2, 4), mar = c(4, 4, 2, 1))
for (o in names(outcomes)) {
  rc <- paste0(o, "_alum_resid"); d <- res_out[!is.na(res_out[[rc]]), ]
  w <- g3[[count[[o]]]][match(paste(d$year, d$region), paste(g3$year, g3$region))]
  plot(d$glyph, d[[rc]], cex = sqrt(w / mean(w)) * 0.6, col = adjustcolor("grey30", .45), pch = 16,
       main = paste0(o, if (o == NEG_CONTROL) " (neg. control)" else ""),
       xlab = "log(glyphosate kg/km2 + 0.1)", ylab = "aluminum-model residual")
  abline(h = 0, lty = 3)
  abline(lm(d[[rc]] ~ d$glyph, weights = w), col = "firebrick", lwd = 2)
  lo <- loess(y ~ x, data.frame(x = d$glyph, y = d[[rc]]), weights = w, span = .75)
  xs <- seq(min(d$glyph), max(d$glyph), length.out = 100)
  lines(xs, predict(lo, data.frame(x = xs)), col = "steelblue", lwd = 2)
}
invisible(dev.off())

cat("\n==================== Negative control (blood) after aluminum model ====================\n")
print(rt[rt$outcome == NEG_CONTROL, c("r_resid_glyph", "p_resid_cl", "beta_joint", "p_joint_cl", "pct_per_sd_joint")], row.names = FALSE)
cat(sprintf("Inflammatory outcomes with residual-glyphosate association (joint model, clustered p<.05): %s\n",
            paste(rt$outcome[rt$outcome != NEG_CONTROL & rt$p_joint_cl < .05], collapse = ", ")))

# ============================================================== PART 4: aluminum micrograms model
# Same idea as the paper's windows, but each window is the mean micrograms of
# vaccine aluminum received in it (scripts/reconstruct_aluminum.py), using the
# product identified by each dose's NIS type code. Where the code does not
# identify the product (unbranded DTaP, pre-2009 Hib), two versions are fitted:
# LOW (lightest possible product) and HIGH (heaviest). Coefficients are per
# 100 ug. Weights = NSCH n, birth-year FE, state-clustered inference.
if (!all(paste0("al_low_", windows) %in% names(a_all))) {
  cat("\n\nPART 4 skipped: no al_* columns (run scripts/reconstruct_aluminum.py, then rebuild the joined CSV)\n")
} else {
cat("\n\n==================== PART 4: aluminum micrograms model (low / high product bounds) ====================\n")
bounds <- c("low", "high")
ug <- function(b) paste0("ug_", b, "_", windows)
u <- a_all[complete.cases(a_all[, c(paste0("al_low_", windows), paste0("al_high_", windows), alum)]), ]
for (b in bounds) for (m in windows) u[[paste0("ug_", b, "_", m)]] <- u[[paste0("al_", b, "_", m)]] / 100
cat(sprintf("Sample: birth years %d-%d, %d states, %d rows. Units: 100 ug aluminum.\n",
            min(u$year), max(u$year), length(unique(u$region)), nrow(u)))

# U1. Descriptives: how much aluminum per window, how much the bounds differ,
#     and how much of the ug variation is NOT explained by dose counts.
u1 <- do.call(rbind, lapply(seq_along(windows), function(j) {
  m <- windows[j]; lo <- u[[paste0("al_low_", m)]]; hi <- u[[paste0("al_high_", m)]]; cnt <- u[[paste0("All.", m)]]
  data.frame(window = names(windows)[j],
             mean_ug_low = mean(lo), mean_ug_high = mean(hi),
             sd_ug_low = sd(lo), sd_ug_high = sd(hi),
             r_low_high = cor(lo, hi),
             r_ug_low_vs_count = cor(lo, cnt), r_ug_high_vs_count = cor(hi, cnt),
             between_state_share_high = summary(lm(hi ~ factor(region) + factor(year), u))$r.squared -
                                        summary(lm(hi ~ factor(year), u))$r.squared)
}))
print_df(u1, paste("Table U1. Mean vaccine aluminum (ug) per window, low vs high bound.",
                   "r_ug_*_vs_count = correlation with the paper's dose-count window (All.m); below 1 = product-mix variation.",
                   "between_state_share_high = extra share of variance explained by state beyond birth year.", sep = "\n"),
         "ug_table1_descriptives.csv", digits = 3)

# U2. Paper headline window (6-12 months): prevalence ~ ug_13 + year FE,
#     in the paper's years and in all years; plus ug_13 adjusted for the dose count.
u2 <- list()
for (b in bounds) {
  x <- paste0("ug_", b, "_13")
  for (yy in list(list("paper 2011-2017", PAPER_YEARS), list("all years", unique(u$year)))) {
    d <- u[u$year %in% yy[[2]], ]
    for (o in outcomes) {
      r1 <- wls(d, o, x, "factor(year)")
      r2 <- wls(d, o, x, c("All.13", "factor(year)"))
      u2[[length(u2) + 1]] <- data.frame(bound = b, years = yy[[1]], outcome = r1$outcome,
        beta_per100ug = r1$beta, p_cl = r1$p_cl, sig = stars(r1$p_cl), pct_per_sd = r1$pct_per_sd,
        beta_adj_count = r2$beta, p_cl_adj_count = r2$p_cl, sig_adj = stars(r2$p_cl))
    }
  }
}
u2 <- do.call(rbind, u2)
print_df(u2, paste("Table U2. Prevalence ~ ug aluminum received at 6-12 months (per 100 ug) + birth-year FE.",
                   "*_adj_count: same, also adjusting for the paper's dose-count window All.13 (isolates product-mix / brand variation).", sep = "\n"),
         "ug_table2_window13.csv")

# U3. Full ug model: all 7 windows + year FE (the analogue of Part 3's model)
# U4. Do ug windows add anything beyond the 7 dose-count windows? (Wald test on the ug terms)
u3c <- list(); u3f <- list()
for (b in bounds) for (o in names(outcomes)) {
  pv <- prev[[o]]; cn <- count[[o]]
  d <- u[!is.na(u[[pv]]), ]; w <- d[[cn]]
  f0 <- lm(as.formula(paste(pv, "~ factor(year)")), d, weights = w)
  fU <- lm(as.formula(paste(pv, "~", paste(ug(b), collapse = " + "), "+ factor(year)")), d, weights = w)
  fC <- lm(as.formula(paste(pv, "~", paste(alum, collapse = " + "), "+ factor(year)")), d, weights = w)
  fB <- lm(as.formula(paste(pv, "~", paste(c(alum, ug(b)), collapse = " + "), "+ factor(year)")), d, weights = w)
  wU <- wald_cl(fU, d$region, ug(b)); wB <- wald_cl(fB, d$region, ug(b))
  V <- vcov_cl(fU, d$region); bb <- coef(fU)[ug(b)]
  p <- 2 * pt(-abs(bb / sqrt(diag(V)[ug(b)])), length(unique(d$region)) - 1)
  u3c[[length(u3c) + 1]] <- data.frame(bound = b, outcome = o,
    t(setNames(paste0(sprintf("%+.5f", bb), stars(p)), names(windows))), check.names = FALSE)
  u3f[[length(u3f) + 1]] <- data.frame(bound = b, outcome = o, n = nrow(d),
    partial_r2_ug = 1 - wssr(fU) / wssr(f0), partial_r2_count = 1 - wssr(fC) / wssr(f0),
    wald_p_ug = wU[["p"]], sig_ug = stars(wU[["p"]]),
    wald_p_ug_beyond_count = wB[["p"]], sig_beyond = stars(wB[["p"]]))
}
print_df(do.call(rbind, u3f),
         paste("Table U3. Aluminum ug model: prevalence ~ 7 ug windows + birth-year FE.",
               "partial_r2_* = share of variance beyond birth year explained by the ug windows vs the paper's 7 dose-count windows.",
               "wald_p_ug = joint test of the 7 ug windows; wald_p_ug_beyond_count = same test with the 7 count windows also in the model (U4).", sep = "\n"),
         "ug_table3_fit.csv")
u3c <- do.call(rbind, u3c)
cat("\nTable U3b. ug model coefficients by window (change in prevalence per 100 ug; state-clustered stars).\n")
print(u3c, row.names = FALSE)
write.csv(u3c, file.path(RES_DIR, "ug_table3b_coefficients.csv"), row.names = FALSE)

# U5. ug-model residuals vs glyphosate (joint model, as in Table A3)
ug_g <- u[u$year %in% GLYPH_YEARS & !is.na(u$glyph_popw_high), ]
ug_g$glyph <- lg(ug_g$glyph_popw_high)
u5 <- list()
for (b in bounds) for (o in names(outcomes)) {
  pv <- prev[[o]]; cn <- count[[o]]
  d <- ug_g[!is.na(ug_g[[pv]]), ]
  fJ <- lm(as.formula(paste(pv, "~", paste(ug(b), collapse = " + "), "+ factor(year) + glyph")), d, weights = d[[cn]])
  bJ <- coef(fJ)["glyph"]; pJ <- 2 * pt(-abs(bJ / sqrt(vcov_cl(fJ, d$region)["glyph", "glyph"])), length(unique(d$region)) - 1)
  u5[[length(u5) + 1]] <- data.frame(bound = b, outcome = o, n = nrow(d), beta_glyph = bJ, p_cl = pJ, sig = stars(pJ),
                                     pct_per_sd = 100 * bJ * sd(d$glyph) / weighted.mean(d[[pv]], d[[cn]]))
}
print_df(do.call(rbind, u5), sprintf("Table U5. Glyphosate added to the ug model (birth years %d-%d).", min(ug_g$year), max(ug_g$year)),
         "ug_table5_glyph_joint.csv")

cat("\n==================== Negative control (blood), ug model ====================\n")
nc <- do.call(rbind, u3f); nc <- nc[nc$outcome == NEG_CONTROL, ]
print(nc[, c("bound", "wald_p_ug", "wald_p_ug_beyond_count")], row.names = FALSE)
print(u2[u2$outcome == NEG_CONTROL, c("bound", "years", "beta_per100ug", "p_cl", "beta_adj_count", "p_cl_adj_count")], row.names = FALSE)
}
