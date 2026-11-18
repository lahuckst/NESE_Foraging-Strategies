# Table: individual-level overlap of post-breeding utilisation distributions (UDs).
# Bhattacharyya's affinity (BA) between the UDs of every pair of post-breeding trips,
# summarised within and between strategies as median (25th-75th percentile).
# Reads results/spatial_strategies/PB_individual_overlap.csv (run NESE_SpatialStrategies.Rmd first).
# Works from the project root or the code/ folder.
# Outputs: results/tables/Table_PB_individual_overlap.csv and .docx (Word)

library(dplyr)

ROOT <- if (dir.exists("results")) "." else if (dir.exists("../results")) ".." else
  stop("Cannot find the results/ folder: set the working directory to the project root or code/")
p <- function(...) file.path(ROOT, ...)

STRATS <- c("Subarctic", "North Central Pacific", "California Current")
ind <- read.csv(p("results", "spatial_strategies", "PB_individual_overlap.csv"))

cell <- function(s1, s2) {
  key <- if (s1 == s2) paste0(s1, " (within)") else
    c(paste(s1, s2, sep = " - "), paste(s2, s1, sep = " - "))
  r <- ind[ind$pair %in% key, ]
  if (!nrow(r)) return("")
  sprintf("%.2f (%.2f\u2013%.2f)\nn = %d", r$median_BA, r$q25, r$q75, r$n_pairs)
}
# Lower triangle: diagonal = within strategy, below = between strategies
tab <- data.frame(Strategy = STRATS, stringsAsFactors = FALSE)
for (j in seq_along(STRATS))
  tab[[STRATS[j]]] <- sapply(seq_along(STRATS), function(i) if (i >= j) cell(STRATS[i], STRATS[j]) else "")

dir.create(p("results", "tables"), showWarnings = FALSE)
write.csv(tab, p("results", "tables", "Table_PB_individual_overlap.csv"), row.names = FALSE)
print(tab, right = FALSE)

# ---- Word table ----------------------------------------------------------
if (!requireNamespace("flextable", quietly = TRUE) || !requireNamespace("officer", quietly = TRUE))
  stop("Install the packages for the Word table: install.packages(c(\"flextable\", \"officer\"))")
library(flextable)

ft <- flextable(tab) %>%
  set_header_labels(Strategy = "") %>%
  theme_booktabs() %>%
  bold(part = "header") %>% bold(j = 1, part = "body") %>%
  align(j = 2:ncol(tab), align = "center", part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 9, part = "all") %>%
  set_caption("Table SX. Overlap between the utilisation distributions of individual post-breeding trips, within and between strategies.") %>%
  add_footer_lines(c(
    "Values are the median Bhattacharyya's affinity (BA; 0 = no shared space, 1 = identical UDs) with the 25th\u201375th percentiles, over all pairs of trips (n = number of pairs).",
    "Diagonal: pairs of trips within the same strategy; below the diagonal: pairs of trips from different strategies. Pairs include two trips by the same female where she was tracked twice.")) %>%
  fontsize(size = 8, part = "footer") %>%
  width(j = 1, width = 1.6) %>% width(j = 2:ncol(tab), width = 1.6) %>%
  fix_border_issues()

save_as_docx(ft, path = p("results", "tables", "Table_PB_individual_overlap.docx"))
cat("Word table written to", p("results", "tables", "Table_PB_individual_overlap.docx"), "\n")
ft
