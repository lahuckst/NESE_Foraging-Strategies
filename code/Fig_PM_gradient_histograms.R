# Post-moult behavioural gradient: histograms of trip scores on the PCA axes,
# coloured by the two weak modes (same format and colours as
# figures/PM_strategies/C_gradient_distribution.pdf, which shows Dim 1).
# Recomputes the PCA exactly as NESE_PM_ForagingStrategies.Rmd does (same trips,
# same 19 variables, standardised PCA), from the data that script saves.
# Works from the project root or the code/ folder (run the PM script first).
# Outputs: figures/PM_strategies/C_gradient_distribution_Dim1.pdf, ..._Dim2.pdf

library(dplyr)
library(ggplot2)
library(FactoMineR)

ROOT <- if (dir.exists("results")) "." else if (dir.exists("../results")) ".." else
  stop("Cannot find the results/ folder: set the working directory to the project root or code/")
p <- function(...) file.path(ROOT, ...)
FIG <- p("figures", "PM_strategies")

DIMS_TO_PLOT <- c(1, 2)     # axes to plot
N_DIM <- 4                  # components retained in the PM analysis (parallel analysis)

# Same variables and palette as the PM script
SPACE <- c("MaxLat_degrees", "MinLon_degrees",
           "MedianDistToContShelf_km", "SdDistContShelf_km",
           "MesoEcoProportionShelf", "MesoEcoProportionCAC",
           "MesoEcoProportionSubArcticPac", "MesoEcoProportionNorthCPac")
DIVE  <- c("Mean_Depth_m", "Mean_Depth_sd", "Mean_Duration_sec", "Mean_Duration_sd",
           "Pct_TransitDives", "Pct_ForagingDives", "Pct_DriftDives", "Pct_BenthicDives",
           "PctTripDiving", "DeepDayForageDivesMedian_m", "DeepNightForageDivesMedian_m")
BEHAV <- c(SPACE, DIVE)
PAL <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666")

dat <- readRDS(p("results", "PM_strategies", "PM_strategies.rds"))   # 86 trips, with strategy
stopifnot(all(BEHAV %in% names(dat)), "strategy" %in% names(dat))
COLS <- setNames(PAL[seq_along(levels(dat$strategy))], levels(dat$strategy))

res.pca <- PCA(dat[BEHAV], ncp = N_DIM, graph = FALSE)
pct_var <- res.pca$eig[, "percentage of variance"]
scores  <- as.data.frame(res.pca$ind$coord); names(scores) <- paste0("Dim", seq_len(ncol(scores)))
gd <- cbind(dat["strategy"], scores)

# Check against the saved eigenvalues from the PM report
saved <- read.csv(p("results", "PM_strategies", "A_pca_dimensions.csv"))
stopifnot(isTRUE(all.equal(unname(res.pca$eig[1:N_DIM, 1]), saved$eigenvalue[1:N_DIM], tolerance = 1e-6)))

# What each axis represents (variable-axis correlations, |r| >= 0.5)
for (d in DIMS_TO_PLOT) {
  r <- res.pca$var$cor[, d]
  cat(sprintf("\nDim %d:\n", d)); print(round(sort(r[abs(r) >= 0.5]), 2))
}

for (d in DIMS_TO_PLOT) {
  ax <- paste0("Dim", d)
  g <- ggplot(gd, aes(.data[[ax]], fill = strategy)) + geom_histogram(bins = 25, colour = "white") +
    scale_fill_manual(values = COLS) + theme_bw() +
    labs(x = sprintf("Dim %d score (%.1f%% of variance)", d, pct_var[d]),
         y = "Number of trips", fill = "Mode")
  print(g)
  ggsave(file.path(FIG, sprintf("C_gradient_distribution_Dim%d.pdf", d)), g, width = 7, height = 4)

  # Hartigan's dip test of unimodality along this axis
  if (requireNamespace("diptest", quietly = TRUE)) print(diptest::dip.test(gd[[ax]])) else
    message("Install the 'diptest' package for a formal test of unimodality.")
}
