# Table 2: post-breeding strategy profiles.
# n per strategy; key behavioural variables used for clustering (mean +/- SD);
# variables not used for clustering (trip duration, mass gain, isotopes);
# isotopic niche size (SEA_B). Tests come from the PB report's saved results.
# Works from the project root or the code/ folder (run NESE_PB_ForagingStrategies.Rmd first).
# Outputs: results/tables/Table2_PB_strategy_profiles.csv and .docx (Word)

library(dplyr)

ROOT <- if (dir.exists("data")) "." else if (dir.exists("../data")) ".." else
  stop("Cannot find the data/ folder: set the working directory to the project root or code/")
p <- function(...) file.path(ROOT, ...)
RES <- p("results", "PB_strategies")

STRATS <- c("Subarctic", "North Central Pacific", "California Current")

meta <- read.csv(p("data", "IsotopesDatasetForLuis_2023_4_4.csv")) %>%
  filter(!is.na(TOPPID)) %>%
  transmute(TOPPID = as.character(TOPPID), TripDuration_days, MassGainRate_kgPerDay_Dcrocker) %>%
  distinct(TOPPID, .keep_all = TRUE)
beh <- readRDS(p("data", "f.PCAdata.1.rds")) %>%
  mutate(TOPPID = as.character(TOPPID)) %>% dplyr::select(-ProcedureYear)

dat <- read.csv(file.path(RES, "A_strategy_assignments.csv")) %>%
  mutate(TOPPID = as.character(TOPPID)) %>%
  left_join(beh, by = "TOPPID") %>% left_join(meta, by = "TOPPID") %>%
  mutate(strategy = factor(strategy, STRATS),
         WestLon = 360 - MinLon_degrees,                  # 0-360 \u00b0E to \u00b0W
         across(c(MesoEcoProportionSubArcticPac, MesoEcoProportionNorthCPac,
                  MesoEcoProportionCAC, MesoEcoProportionShelf, Pct_BenthicDives), ~ .x * 100),
         Mean_Duration_min = Mean_Duration_sec / 60)
stopifnot(nrow(dat) == 93, !anyNA(dat$strategy))

# variable, label, decimals, section
VARS <- tibble::tribble(
  ~var,                            ~label,                                  ~dp, ~section,
  "MaxLat_degrees",                "Maximum latitude (\u00b0N)",            1,   "Behaviour (used for clustering)",
  "WestLon",                       "Westernmost longitude (\u00b0W)",       1,   "Behaviour (used for clustering)",
  "MedianDistToContShelf_km",      "Distance to continental shelf (km)",    0,   "Behaviour (used for clustering)",
  "MesoEcoProportionSubArcticPac", "Time in Subarctic Pacific (%)",         0,   "Behaviour (used for clustering)",
  "MesoEcoProportionNorthCPac",    "Time in North Central Pacific (%)",     0,   "Behaviour (used for clustering)",
  "MesoEcoProportionCAC",          "Time in California Current (%)",        0,   "Behaviour (used for clustering)",
  "MesoEcoProportionShelf",        "Time on continental shelf (%)",         0,   "Behaviour (used for clustering)",
  "Mean_Depth_m",                  "Mean dive depth (m)",                   0,   "Behaviour (used for clustering)",
  "Mean_Duration_min",             "Mean dive duration (min)",              1,   "Behaviour (used for clustering)",
  "Pct_BenthicDives",              "Benthic dives (%)",                     0,   "Behaviour (used for clustering)",
  "Pct_ForagingDives",             "Foraging dives (%)",                    0,   "Behaviour (used for clustering)",
  "TripDuration_days",             "Trip duration (d)",                     0,   "Not used for clustering",
  "MassGainRate_kgPerDay_Dcrocker","Mass gain rate (kg d\u207b\u00b9)",     2,   "Not used for clustering",
  "d13C",                          "\u03b4\u00b9\u00b3C (\u2030)",          2,   "Not used for clustering",
  "d15N",                          "\u03b4\u00b9\u2075N (\u2030)",          2,   "Not used for clustering")

msd <- function(x, dp) {
  x <- x[!is.na(x)]
  s <- paste(formatC(mean(x), format = "f", digits = dp), "\u00b1", formatC(sd(x), format = "f", digits = dp))
  gsub("(^|\\s)-", "\\1\u2212", s)                          # typographic minus
}
fmtp <- function(pv) if (pv < 0.001) "P < 0.001" else sprintf("P = %.3f", pv)

# Tests for the variables not used for clustering
ext <- read.csv(file.path(RES, "A_k2_vs_k3_external_validation.csv"), row.names = 1)
iso <- read.csv(file.path(RES, "B_isotope_tests.csv")) %>% filter(covariates == "ProcedureYear")
TEST <- c(
  TripDuration_days = paste0(fmtp(ext["TripDuration_days", "p_perm"]), "\u1d43"),
  MassGainRate_kgPerDay_Dcrocker = paste0(fmtp(ext["MassGainRate_kgPerDay_Dcrocker", "p_perm"]), "\u1d43"),
  d13C = sprintf("F = %.1f, %s\u1d47", iso$F[iso$response == "d13C"], fmtp(iso$p_BH[iso$response == "d13C"])),
  d15N = sprintf("F = %.1f, %s\u1d47", iso$F[iso$response == "d15N"], fmtp(iso$p_BH[iso$response == "d15N"])))

rows <- lapply(seq_len(nrow(VARS)), function(i) {
  v <- VARS$var[i]; dp <- VARS$dp[i]
  out <- c(Section = VARS$section[i], Variable = VARS$label[i],
           sapply(STRATS, function(s) msd(dat[[v]][dat$strategy == s], dp)),
           All = msd(dat[[v]], dp), Test = unname(ifelse(v %in% names(TEST), TEST[v], "")))
  out
})
tab <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)

# Isotopic niche size: SIBER SEA_B (median and 95% credible interval); nicheROVER if SIBER was not run
sea_file <- if (file.exists(file.path(RES, "B_SIBER_niche_size.csv"))) "B_SIBER_niche_size.csv" else "B_niche_size.csv"
sea <- read.csv(file.path(RES, sea_file))
sea_txt <- setNames(sprintf("%.2f (%.2f\u2013%.2f)", sea$SEAb_median, sea$SEAb_lwr, sea$SEAb_upr), sea$strategy)
tab <- rbind(tab, c(Section = "Not used for clustering", Variable = "SEA_B (\u2030\u00b2, 95 % CrI)",
                    sea_txt[STRATS], All = "", Test = ""))

ns <- table(dat$strategy)
names(tab)[3:6] <- c(sprintf("%s (n = %d)", STRATS, as.integer(ns[STRATS])), sprintf("All (n = %d)", nrow(dat)))

dir.create(p("results", "tables"), showWarnings = FALSE)
write.csv(tab, p("results", "tables", "Table2_PB_strategy_profiles.csv"), row.names = FALSE)
print(tab, right = FALSE)

# ---- Word table ----------------------------------------------------------
if (!requireNamespace("flextable", quietly = TRUE) || !requireNamespace("officer", quietly = TRUE))
  stop("Install the packages for the Word table: install.packages(c(\"flextable\", \"officer\"))")
library(flextable)

# Insert a label row before each section
sec <- tab$Section
wt <- do.call(rbind, lapply(unique(sec), function(s) {
  lab <- tab[1, ]; lab[] <- ""; lab$Variable <- s
  rbind(lab, tab[sec == s, ])
}))
sec_rows <- which(wt$Variable %in% unique(sec))
wt$Section <- NULL

ft <- flextable(wt) %>%
  set_header_labels(Variable = "") %>%
  theme_booktabs() %>%
  bold(i = sec_rows, part = "body") %>%
  italic(i = sec_rows, part = "body") %>%
  hline(i = sec_rows[-1] - 1, border = officer::fp_border(width = 0.5), part = "body") %>%
  bold(part = "header") %>%
  align(j = 2:6, align = "center", part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  padding(padding.top = 1, padding.bottom = 1, part = "all") %>%
  set_caption("Table 2. Profiles of the post-breeding foraging strategies (mean \u00b1 SD).") %>%
  add_footer_lines(c(
    "\u1d43 California Current females vs the remaining females of the two-cluster Subarctic group (permutation test).",
    "\u1d47 Differences among strategies adjusted for year (Freedman\u2013Lane permutation test; Benjamini\u2013Hochberg-adjusted P).",
    sprintf("SEA_B: Bayesian standard ellipse area (%s), median and 95 %% credible interval.",
            if (sea_file == "B_SIBER_niche_size.csv") "SIBER" else "nicheROVER"))) %>%
  fontsize(size = 8, part = "footer") %>%
  autofit() %>% fix_border_issues()

save_as_docx(ft, path = p("results", "tables", "Table2_PB_strategy_profiles.docx"),
             pr_section = officer::prop_section(page_size = officer::page_size(orient = "landscape")))
cat("Word table written to", p("results", "tables", "Table2_PB_strategy_profiles.docx"), "\n")
ft
