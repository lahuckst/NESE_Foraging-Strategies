# Table 1 (alternative): mean ± SD of the main variables by sex, trip type and year.
# Uses the same trips as the analyses: females = trips in the PB/PM strategy
# assignments (93 PB, 86 PM); males = arrival isotope samples (48).
# Works from the project root or the code/ folder. Output: results/tables/Table1_by_year_sex.csv

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
