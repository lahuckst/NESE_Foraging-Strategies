# Isotopic niches of females and males, split by trip type (post-breeding females by strategy;
# post-moult females as one group, because post-moult foraging is a continuum).
#   (a) d13C-d15N biplot with standard ellipses (SEAc), one panel per trip type;
#       colour = strategy / males, symbol = sex.
#   (b) Bayesian standard ellipse areas (SEA_B, posterior median and 95 % CrI).
# Groups: post-breeding females (Subarctic, North Central Pacific, California Current),
# post-moult females (one group), males (PB, PM).
# SEA_B uses SIBER when SIBER + rjags (JAGS) are installed, otherwise nicheROVER
# (same quantity: pi * sqrt(det(Sigma)) from the posterior covariance).
# Works from the project root or the code/ folder (run the PB and PM scripts first).
# Outputs: figures/isotopic_niches/ (PDFs) and results/isotopic_niches/SEA_B_by_group.csv

library(dplyr)
library(ggplot2)

ROOT <- if (dir.exists("data")) "." else if (dir.exists("../data")) ".." else
  stop("Cannot find the data/ folder: set the working directory to the project root or code/")
p <- function(...) file.path(ROOT, ...)
FIG <- p("figures", "isotopic_niches"); OUT <- p("results", "isotopic_niches")
dir.create(FIG, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# ---- Settings -------------------------------------------------------------
SEED <- 20261117
ONE_PER_INDIVIDUAL <- FALSE  # TRUE: one sample per individual within each group (earliest year);
                             # FALSE matches the trips used in the PB/PM niche analyses
MIN_N  <- 5                  # smallest group for which SEA_B is estimated
N_POST <- 1e4                # nicheROVER posterior draws
USE_SIBER <- requireNamespace("SIBER", quietly = TRUE) && requireNamespace("rjags", quietly = TRUE)
SIBER_PARMS  <- list(n.iter = 2e4, n.burnin = 1e3, n.thin = 10, n.chains = 2)
SIBER_PRIORS <- list(R = 1 * diag(2), k = 2, tau.mu = 1.0E-3)

STRAT_COLS <- c("Subarctic" = "#1b9e77", "North Central Pacific" = "#d95f02",
                "California Current" = "#7570b3", "Post-moult females" = "grey40",
                "Males" = "#d73027")
SEX_SHAPES <- c(Female = 16, Male = 17)          # filled circle, filled triangle
TRIP_LAB   <- c(PB = "Post-breeding", PM = "Post-moult")
LAB_C <- "\u03b4\u00b9\u00b3C (\u2030)"; LAB_N <- "\u03b4\u00b9\u2075N (\u2030)"

# ---- Data -----------------------------------------------------------------
meta <- read.csv(p("data", "IsotopesDatasetForLuis_2023_4_4.csv"))
iso_by_trip <- meta %>% filter(!is.na(TOPPID)) %>%
  transmute(TOPPID = as.character(TOPPID), d13C, d15N) %>% distinct(TOPPID, .keep_all = TRUE)

females <- bind_rows(
  read.csv(p("results", "PB_strategies", "A_strategy_assignments.csv")) %>% mutate(trip = "PB"),
  read.csv(p("results", "PM_strategies", "A_strategy_assignments.csv")) %>% mutate(trip = "PM")) %>%
  transmute(TOPPID = as.character(TOPPID), ID = as.character(MainID), year = ProcedureYear,
            trip, sex = "Female",
            # post-moult females are not split into strategies
            strategy = ifelse(trip == "PM", "Post-moult females", strategy)) %>%
  left_join(iso_by_trip, by = "TOPPID")

males <- meta %>%
  filter(Sex == "Male", TimePeriod == "Arrival", !is.na(d13C), !is.na(d15N),
         Trip %in% c("PostBreeding", "PostMolt")) %>%
  transmute(TOPPID = as.character(TOPPID), ID = as.character(MainID), year = ProcedureYear,
            trip = ifelse(Trip == "PostBreeding", "PB", "PM"), strategy = "Males", sex = "Male",
            d13C, d15N)

dat <- bind_rows(females, males) %>%
  filter(!is.na(d13C), !is.na(d15N)) %>%
  mutate(group = ifelse(sex == "Male", paste("Males", trip),
                        ifelse(trip == "PM", "Females PM", paste("Females", trip, strategy))))
if (ONE_PER_INDIVIDUAL)
  dat <- dat %>% arrange(year) %>% distinct(group, ID, .keep_all = TRUE)

GROUP_ORDER <- c(paste("Females PB", c("Subarctic", "North Central Pacific", "California Current")),
                 "Males PB",
                 "Females PM",
                 "Males PM")
dat <- dat %>%
  mutate(group = factor(group, intersect(GROUP_ORDER, unique(group))),
         strategy = factor(strategy, names(STRAT_COLS)),
         sex = factor(sex, names(SEX_SHAPES)),
         trip_lab = factor(TRIP_LAB[trip], TRIP_LAB))
print(dat %>% count(trip, group, sex))

# ---- Standard ellipses (SEAc: small-sample corrected, ~40 % of the data) ---
std_ellipse <- function(x, y, npts = 100) {
  n <- length(x); S <- cov(cbind(x, y)) * (n - 1) / (n - 2)
  e <- eigen(S); th <- seq(0, 2 * pi, length.out = npts)
  xy <- cbind(cos(th), sin(th)) %*% diag(sqrt(e$values)) %*% t(e$vectors)
  data.frame(d13C = mean(x) + xy[, 1], d15N = mean(y) + xy[, 2])
}
ell <- dat %>% group_by(group, trip_lab, strategy, sex) %>%
  filter(n() >= MIN_N) %>%
  group_modify(~ std_ellipse(.x$d13C, .x$d15N)) %>% ungroup()

# ---- SEA_B posteriors ----------------------------------------------------
big <- dat %>% group_by(group) %>% filter(n() >= MIN_N) %>% ungroup() %>% mutate(group = droplevels(group))
set.seed(SEED)
if (USE_SIBER) {
  gid <- as.integer(big$group)
  so  <- SIBER::createSiberObject(data.frame(iso1 = big$d13C, iso2 = big$d15N,
                                             group = gid, community = 1))
  post <- SIBER::siberMVN(so, SIBER_PARMS, SIBER_PRIORS)
  seab <- SIBER::siberEllipses(post)
  colnames(seab) <- levels(big$group)[as.integer(sub("^1\\.", "", names(post)))]
  METHOD <- "SIBER"
} else {
  par <- tapply(seq_len(nrow(big)), big$group, function(ii)
    nicheROVER::niw.post(nsamples = N_POST, X = as.matrix(big[ii, c("d13C", "d15N")])))
  seab <- sapply(par, function(sp) apply(sp$Sigma, 3, function(S) pi * sqrt(det(S))))
  METHOD <- "nicheROVER"
}
message("SEA_B estimated with ", METHOD)

key <- big %>% distinct(group, trip_lab, strategy, sex) %>% mutate(group = as.character(group))
sea_long <- data.frame(group = rep(colnames(seab), each = nrow(seab)), SEA_B = as.vector(seab)) %>%
  left_join(key, by = "group")
sea_sum <- sea_long %>% group_by(group, trip_lab, strategy, sex) %>%
  summarise(median = median(SEA_B), lwr = quantile(SEA_B, 0.025), upr = quantile(SEA_B, 0.975),
            .groups = "drop") %>%
  left_join(big %>% count(group = as.character(group)), by = "group") %>%
  mutate(method = METHOD,
         group = factor(group, intersect(GROUP_ORDER, group))) %>% arrange(group)
print(sea_sum, n = Inf)
write.csv(sea_sum, file.path(OUT, "SEA_B_by_group.csv"), row.names = FALSE)

# ---- Plots ---------------------------------------------------------------
th <- theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(), strip.background = element_rect(fill = "grey95"),
        legend.position = "bottom", legend.box = "vertical")

g_iso <- ggplot(dat, aes(d13C, d15N)) +
  geom_point(aes(colour = strategy, shape = sex), size = 2, alpha = 0.7) +
  geom_path(data = ell, aes(colour = strategy, linetype = sex, group = group), linewidth = 0.9) +
  facet_wrap(~ trip_lab) +
  scale_colour_manual(values = STRAT_COLS, name = NULL, drop = TRUE) +
  scale_shape_manual(values = SEX_SHAPES, name = NULL) +
  scale_linetype_manual(values = c(Female = "solid", Male = "22"), name = NULL) +
  labs(x = LAB_C, y = LAB_N, tag = "a") + th

xlab_of <- function(d) with(d, ifelse(sex == "Male", "Males", ifelse(trip_lab == "Post-moult", "Females",
                            sub("North Central Pacific", "N Central\nPacific",
                                sub("California Current", "California\nCurrent", as.character(strategy))))))
sea_sum$xlab <- xlab_of(sea_sum)
# Violins show the posterior draws; extreme tails (outside the central 99 %) are
# dropped for display only, so a long tail in a small group does not squash the axis
sea_plot <- sea_long %>% mutate(xlab = xlab_of(.)) %>%
  group_by(group) %>%
  filter(SEA_B >= quantile(SEA_B, 0.005), SEA_B <= quantile(SEA_B, 0.995)) %>% ungroup()
g_sea <- ggplot(sea_plot, aes(xlab, SEA_B)) +
  geom_violin(aes(fill = strategy, colour = strategy), alpha = 0.35, scale = "width",
              width = 0.8, linewidth = 0.4) +
  geom_point(data = sea_sum, aes(xlab, median, colour = strategy, shape = sex), size = 2.8) +
  facet_grid(~ trip_lab, scales = "free_x", space = "free_x") +
  scale_fill_manual(values = STRAT_COLS, guide = "none") +
  scale_colour_manual(values = STRAT_COLS, guide = "none") +
  scale_shape_manual(values = SEX_SHAPES, guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = "SEA_B (\u2030\u00b2)", tag = "b",
       caption = sprintf("Posterior distributions (central 99 %%) with medians (%s)", METHOD)) + th

ggsave(file.path(FIG, "isotope_biplot_by_sex.pdf"), g_iso, width = 9, height = 5, device = cairo_pdf)
ggsave(file.path(FIG, "SEA_B_by_sex.pdf"), g_sea, width = 7, height = 4, device = cairo_pdf)
if (requireNamespace("patchwork", quietly = TRUE)) {
  g_all <- patchwork::wrap_plots(g_iso, g_sea, ncol = 1, heights = c(1.4, 1))
  ggsave(file.path(FIG, "Fig_isotopic_niches_by_sex.pdf"), g_all, width = 9, height = 9, device = cairo_pdf)
  print(g_all)
} else {
  message("Install 'patchwork' to save both panels as one figure.")
  print(g_iso); print(g_sea)
}
