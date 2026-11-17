# Table 1 (alternative): mean ± SD of the main variables by sex, trip type and year.
# Uses the same trips as the analyses: females = trips in the PB/PM strategy
# assignments (93 PB, 86 PM); males = arrival isotope samples (48).
# Works from the project root or the code/ folder.
# Outputs: results/tables/Table1_by_year_sex.csv and Table1_by_year_sex.docx (Word)

library(dplyr)

# Project root: the folder that contains data/ (works whether the working
# directory is the project root, e.g. an RStudio project, or code/)
ROOT <- if (dir.exists("data")) "." else if (dir.exists("../data")) ".." else
  stop("Cannot find the data/ folder: set the working directory to the project root or code/")
p <- function(...) file.path(ROOT, ...)

meta <- read.csv(p("data", "IsotopesDatasetForLuis_2023_4_4.csv"))
beh  <- readRDS(p("data", "f.PCAdata.1.rds")) %>% mutate(TOPPID = as.character(TOPPID))
m1 <- meta %>% filter(!is.na(TOPPID)) %>%
  transmute(TOPPID = as.character(TOPPID), TripDuration_days,
            MassGainRate_kgPerDay_Dcrocker, StandardLength_cm) %>%
  distinct(TOPPID, .keep_all = TRUE)

fem <- bind_rows(
  read.csv(p("results", "PB_strategies", "A_strategy_assignments.csv")) %>% mutate(Trip = "Post-breeding"),
  read.csv(p("results", "PM_strategies", "A_strategy_assignments.csv")) %>% mutate(Trip = "Post-moult")) %>%
  transmute(TOPPID = as.character(TOPPID), Trip, Year = ProcedureYear) %>%
  left_join(beh %>% dplyr::select(-ProcedureYear, -Trip, -Sex), by = "TOPPID") %>%
  left_join(m1, by = "TOPPID") %>%
  mutate(Sex = "Female", Mean_Duration_min = Mean_Duration_sec / 60)

mal <- meta %>%
  filter(Sex == "Male", TimePeriod == "Arrival", !is.na(d13C), !is.na(d15N),
         Trip %in% c("PostBreeding", "PostMolt")) %>%
  transmute(Sex = "Male", Trip = ifelse(Trip == "PostBreeding", "Post-breeding", "Post-moult"),
            Year = ProcedureYear, StandardLength_cm, d13C, d15N)

VARS <- c(TripDuration_days = 0, MassGainRate_kgPerDay_Dcrocker = 2, MaxLat_degrees = 1,
          MedianDistToContShelf_km = 0, Mean_Depth_m = 0, Mean_Duration_min = 1,
          StandardLength_cm = 0, d13C = 2, d15N = 2)
msd <- function(x, dp) {
  x <- x[!is.na(x)]
  if (!length(x)) return("—")
  if (length(x) == 1) return(formatC(x, format = "f", digits = dp))
  paste(formatC(mean(x), format = "f", digits = dp), "±", formatC(sd(x), format = "f", digits = dp))
}
summ <- function(d) {
  out <- data.frame(n = nrow(d))
  for (v in names(VARS)) out[[v]] <- if (v %in% names(d)) msd(d[[v]], VARS[[v]]) else "—"
  out
}
tab <- bind_rows(fem, mal) %>%
  group_by(Sex, Trip) %>%
  group_modify(~ bind_rows(
    .x %>% group_by(Year) %>% group_modify(~ summ(.x)) %>% ungroup() %>% mutate(Year = as.character(Year)),
    summ(.x) %>% mutate(Year = "All"))) %>%
  ungroup() %>% relocate(Year, .after = Trip)

dir.create(p("results", "tables"), showWarnings = FALSE)
write.csv(tab, p("results", "tables", "Table1_by_year_sex.csv"), row.names = FALSE)
tab

# ---- Word table ----------------------------------------------------------
# Writes results/tables/Table1_by_year_sex.docx: a formatted table (landscape,
# 9 pt Times New Roman, merged Sex/Trip cells, bold "All years" rows) that can
# be copied straight into the manuscript.
if (!requireNamespace("flextable", quietly = TRUE) || !requireNamespace("officer", quietly = TRUE))
  stop("Install the packages for the Word table: install.packages(c(\"flextable\", \"officer\"))")
library(flextable)

HEAD <- c(Sex = "Sex", Trip = "Trip", Year = "Year", n = "n",
          TripDuration_days = "Trip duration (d)",
          MassGainRate_kgPerDay_Dcrocker = "Mass gain (kg d⁻¹)",
          MaxLat_degrees = "Max. latitude (°N)",
          MedianDistToContShelf_km = "Distance to shelf (km)",
          Mean_Depth_m = "Dive depth (m)", Mean_Duration_min = "Dive duration (min)",
          StandardLength_cm = "Standard length (cm)",
          d13C = "δ¹³C (‰)", d15N = "δ¹⁵N (‰)")

wt <- tab %>%
  mutate(Year = ifelse(Year == "All", "All years", Year),
         across(c(d13C, d15N), ~ sub("^-", "−", .x)),   # typographic minus
         across(everything(), as.character))
all_rows <- which(wt$Year == "All years")

ft <- flextable(wt, col_keys = names(HEAD)) %>%
  set_header_labels(values = as.list(HEAD)) %>%
  theme_booktabs() %>%
  merge_v(j = c("Sex", "Trip")) %>%
  valign(j = c("Sex", "Trip"), valign = "top", part = "body") %>%
  bold(i = all_rows, part = "body") %>%
  hline(i = all_rows, border = officer::fp_border(width = 0.75), part = "body") %>%
  bold(part = "header") %>%
  align(j = 3:length(HEAD), align = "center", part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  padding(padding.top = 1, padding.bottom = 1, part = "all") %>%
  set_caption("Table 1. Mean ± SD of the main variables by sex, trip type and year.") %>%
  add_footer_lines(paste0(
    "n = tracked trips (females) or isotope samples (males); behavioural variables are available for females only. ",
    sprintf("Male standard length was available for %d of %d samples. ",
            sum(!is.na(mal$StandardLength_cm)), nrow(mal)),
    "Years with n ≤ 3 should be read with caution.")) %>%
  fontsize(size = 8, part = "footer") %>%
  autofit() %>%
  fix_border_issues()

save_as_docx(ft, path = p("results", "tables", "Table1_by_year_sex.docx"),
             pr_section = officer::prop_section(page_size = officer::page_size(orient = "landscape")))
cat("Word table written to", p("results", "tables", "Table1_by_year_sex.docx"), "\n")
ft
