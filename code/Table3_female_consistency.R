# Table 3: individual consistency of adult females.
# Strategy fidelity, repeatability of behaviour and isotopes, spatial fidelity and
# within-female isotope differences, read from results/female_consistency/
# (run NESE_FemaleConsistency.Rmd first).
# Works from the project root or the code/ folder.
# Outputs: results/tables/Table3_female_consistency.csv and .docx (Word)

library(dplyr)

ROOT <- if (dir.exists("results")) "." else if (dir.exists("../results")) ".." else
  stop("Cannot find the results/ folder: set the working directory to the project root or code/")
p <- function(...) file.path(ROOT, ...)
RES <- p("results", "female_consistency")

fmtp <- function(x) ifelse(is.na(x), "", ifelse(x < 0.001, "< 0.001", sprintf("%.3f", x)))
f2 <- function(x) sprintf("%.2f", x)
PAIR <- c("PB-PB" = "Post-breeding \u2013 post-breeding", "PM-PM" = "Post-moult \u2013 post-moult",
          "PB-PM" = "Post-breeding \u2013 post-moult", "All pairs" = "All pairs")

# 1. Strategy fidelity
sf <- read.csv(file.path(RES, "1_strategy_fidelity.csv")) %>%
  mutate(o = match(pair_type, names(PAIR))) %>% arrange(o)
t1 <- data.frame(Section = "Strategy fidelity (same strategy on both trips)",
                 Measure = PAIR[sf$pair_type],
                 n = paste(sf$n_pairs, "pairs"),
                 Observed = sprintf("%d of %d", sf$n_same, sf$n_pairs),
                 Comparison = sprintf("%.1f expected by chance", sf$expected),
                 P = fmtp(sf$P))

# 2. Repeatability (trip type fixed; female and year random)
rb <- read.csv(file.path(RES, "2_behaviour_repeatability.csv"))
ri <- read.csv(file.path(RES, "4_isotope_repeatability.csv")) %>%
  filter(grepl("\\(c\\)", variable)) %>%
  mutate(variable = ifelse(grepl("^d13C", variable), "\u03b4\u00b9\u00b3C", "\u03b4\u00b9\u2075N"))
B_ORDER <- c("Maximum latitude", "Dim 1 (latitude / offshore axis)", "Distance to shelf (log)",
             "California Current use (logit)", "Dim 2 (diving-effort axis)")
B_LAB <- c("Maximum latitude", "Latitude / offshore axis (PC1)", "Distance to shelf (log)",
           "California Current use (logit)", "Diving-effort axis (PC2)")
rb <- rb[match(B_ORDER, rb$variable), ]
rep_rows <- function(d, lab, unit) data.frame(
  Section = "Repeatability",
  Measure = lab,
  n = sprintf("%d %s, %d females (%d repeat)", d$n_obs, unit, d$n_females, d$n_repeat_females),
  Observed = sprintf("R = %s (%s\u2013%s)", f2(d$R), f2(d$lwr), f2(d$upr)),
  Comparison = ifelse(is.na(d$R_year), "", sprintf("year: R = %s", f2(d$R_year))),
  P = fmtp(d$P_LRT))
t2 <- rbind(rep_rows(rb, B_LAB, "trips"), rep_rows(ri, ri$variable, "samples"))

# 3. Spatial fidelity (Bhattacharyya's affinity between two trips of the same female)
sp <- read.csv(file.path(RES, "3_spatial_fidelity.csv")) %>%
  filter(pair_type != "All pairs") %>%
  mutate(o = match(pair_type, names(PAIR))) %>% arrange(o)
t3 <- data.frame(Section = "Spatial fidelity (overlap between a female's trips, BA)",
                 Measure = PAIR[sp$pair_type],
                 n = paste(sp$n_pairs, "pairs"),
                 Observed = paste("BA =", f2(sp$BA_same_female)),
                 Comparison = ifelse(is.na(sp$BA_null_strategy),
                                     sprintf("random females: %s", f2(sp$BA_null_population)),
                                     sprintf("same-strategy females: %s; random females: %s",
                                             f2(sp$BA_null_strategy), f2(sp$BA_null_population))),
                 P = ifelse(is.na(sp$P_strategy), fmtp(sp$P_population),
                            paste0(fmtp(sp$P_strategy), "\u1d43")))

# 4. Isotope differences between a female's own samples
idf <- read.csv(file.path(RES, "4_isotope_pair_differences.csv"))
t4 <- data.frame(Section = "Isotope differences between a female's samples",
                 Measure = ifelse(idf$isotope == "d13C", "\u03b4\u00b9\u00b3C", "\u03b4\u00b9\u2075N"),
                 n = "",
                 Observed = sprintf("|\u0394| = %s \u2030", f2(idf$mean_abs_diff_same_female)),
                 Comparison = sprintf("different females, same years: %s \u2030", f2(idf$mean_abs_diff_year_matched_null)),
                 P = fmtp(idf$P))

tab <- rbind(t1, t2, t3, t4)
rownames(tab) <- NULL
dir.create(p("results", "tables"), showWarnings = FALSE)
write.csv(tab, p("results", "tables", "Table3_female_consistency.csv"), row.names = FALSE)
print(tab, right = FALSE)

# ---- Word table ----------------------------------------------------------
if (!requireNamespace("flextable", quietly = TRUE) || !requireNamespace("officer", quietly = TRUE))
  stop("Install the packages for the Word table: install.packages(c(\"flextable\", \"officer\"))")
library(flextable)

sec <- tab$Section
wt <- do.call(rbind, lapply(unique(sec), function(s) {
  lab <- tab[1, ]; lab[] <- ""; lab$Measure <- s
  rbind(lab, tab[sec == s, ])
}))
sec_rows <- which(wt$Measure %in% unique(sec))
wt$Section <- NULL

ft <- flextable(wt) %>%
  set_header_labels(Measure = "", n = "n", Observed = "Observed",
                    Comparison = "Comparison", P = "P") %>%
  theme_booktabs() %>%
  merge_h_range(i = sec_rows, j1 = 1, j2 = ncol(wt), part = "body") %>%
  bold(i = sec_rows, part = "body") %>% italic(i = sec_rows, part = "body") %>%
  hline(i = sec_rows[-1] - 1, border = officer::fp_border(width = 0.5), part = "body") %>%
  bold(part = "header") %>%
  align(j = 2:ncol(wt), align = "center", part = "all") %>%
  align(i = sec_rows, j = 1, align = "left", part = "body") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  padding(padding.top = 1, padding.bottom = 1, part = "all") %>%
  set_caption("Table 3. Individual consistency of adult female northern elephant seals.") %>%
  add_footer_lines(c(
    "Strategy fidelity: strategy labels permuted among trips of the same type (9,999 permutations).",
    "Repeatability: adjusted R from mixed models with trip type as a fixed effect and female and year as random effects (rptR); 95 % CI by parametric bootstrap; P from likelihood-ratio tests. \u201cyear\u201d is the proportion of variance explained by year.",
    "BA, Bhattacharyya's affinity between utilisation distributions; P from 9,999 randomisations replacing one trip with a trip of the same type by a random female, or \u1d43 by a female of the same strategy.",
    "Isotope differences: mean absolute difference between a female's samples vs between different females sampled in the same years and trip types (9,999 randomisations).")) %>%
  fontsize(size = 8, part = "footer") %>%
  autofit() %>% fix_border_issues()

save_as_docx(ft, path = p("results", "tables", "Table3_female_consistency.docx"),
             pr_section = officer::prop_section(page_size = officer::page_size(orient = "landscape")))
cat("Word table written to", p("results", "tables", "Table3_female_consistency.docx"), "\n")
ft
