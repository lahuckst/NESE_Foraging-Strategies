library(ggforce)
library(tidyverse)
library(sf)
library(RColorBrewer)

NESE_IsotopesAll <- read_csv("../results/NESE_IsotopesAll.csv")

PB <- NESE_IsotopesAll[which(NESE_IsotopesAll$Trip=='PostBreeding'),]
PM <- NESE_IsotopesAll[which(NESE_IsotopesAll$Trip=='PostMolt'),]

# Calculate the overlap for both sexes during PB using Minimum COnvex polygons


################################################################################
########################### POST- BREEDING #####################################
################################################################################

# Define the colour palette following the pattern from the paper
cluster_cols <- c("Males_PBC1" = "#aa6fac",
                  "Males_PBC2" = "#4eb15e",
                  "Males_PBC3" = "#168ece",
                  "Females_PMC1" = "#7570b3",
                  "Females_PMC2" = "#1b9e77",
                  "Females_PMC3" = "#d95f02")

# Plots the data for both isotopes for males and females, including the MCPs
MPC.PB <- ggplot(PB, aes(d13C, d15N, 
               colour = factor(Cluster),
               shape = factor(Sex))) +
  geom_point(size = 3) +
  geom_mark_hull(aes(fill = factor(Cluster)),
                 alpha = 0.2,
                 colour = NA,
                 concavity = Inf)+
  scale_colour_manual(values = cluster_cols) +
  scale_fill_manual(values = cluster_cols) +
  theme_classic()

# Save the figure with the MCPs
ggsave(filename = "../figures/NESE_MCP_PB.pdf",
       plot = MPC.PB,
       device = cairo_pdf)


# Gets the cartesian coordinates in the isotopic bi-space
pts <- st_as_sf(PB,
                coords = c("d13C", "d15N"),
                remove = FALSE)

# Calculated tge MCPs for each cluster/sex
mcps <-
  pts %>%
  group_by(Cluster) %>%
  summarise() %>%
  st_convex_hull()

#Calculate the overlap between MCPs for the clusters in parts per mil ^2

clusters <- unique(mcps$Cluster)

overlap <- expand.grid(cluster1 = clusters,
                       cluster2 = clusters)

overlap$area_overlap <- NA

for(i in seq_len(nrow(overlap))){
  
  p1 <- mcps[mcps$Cluster == overlap$cluster1[i], ]
  p2 <- mcps[mcps$Cluster == overlap$cluster2[i], ]
  
  inter <- st_intersection(p1, p2)
  
  overlap$area_overlap[i] <-
    if(nrow(inter) == 0) 0 else st_area(inter)
}

overlap


#Calculate the overlap between MCPs for the clusters in %

overlap$percent_overlap_cluster1 <- NA
overlap$percent_overlap_cluster2 <- NA

for(i in seq_len(nrow(overlap))){
  
  p1 <- mcps[mcps$Cluster == overlap$cluster1[i], ]
  p2 <- mcps[mcps$Cluster == overlap$cluster2[i], ]
  
  inter <- st_intersection(p1, p2)
  
  if(nrow(inter)==0){
    
    overlap$percent_overlap_cluster1[i] <- 0
    overlap$percent_overlap_cluster2[i] <- 0
    
  } else{
    
    Aint <- as.numeric(st_area(inter))
    A1   <- as.numeric(st_area(p1))
    A2   <- as.numeric(st_area(p2))
    
    overlap$percent_overlap_cluster1[i] <- 100*Aint/A1
    overlap$percent_overlap_cluster2[i] <- 100*Aint/A2
    # Gets the overlaps = 100 to NAs
    if(round(overlap$percent_overlap_cluster1[i]) == 100){overlap$percent_overlap_cluster1[i] <- NA}
   if(round(overlap$percent_overlap_cluster2[i]) == 100){overlap$percent_overlap_cluster2[i] <- NA}
  }
}

# Plot the grid of overlap
overlap.PB <- ggplot(overlap,
       aes(cluster1, cluster2, fill = percent_overlap_cluster2)) +
  geom_tile() +
  scale_fill_distiller(
    palette = "RdBu",
    direction = -1
  ) +
  coord_equal() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

ggsave(filename = "../figures/NESE_IsoOverlap_PB.pdf",
       plot = overlap.PB,
       device = cairo_pdf)

################################################################################
########################### POST- MOULT ########################################
################################################################################

# Define the colour palette following the pattern from the paper
cluster_cols <- c("Males_PMC1" = "#aa6fac",
                  "Males_PMC2" = "#4eb15e",
                  "Males_PMC3" = "#168ece",
                  "Females_PMC1" = "#7571b2",
                  "Females_PMC2" = "#1c9f79",
                  "Females_PMC3" = "#d96116",
                  "Females_PMC4" = "#e52e89",
                  "Females_PMC5" = "#fdc608")

# Plots the data for both isotopes for males and females, including the MCPs
MPC.PM <- ggplot(PM, aes(d13C, d15N, 
               colour = factor(Cluster),
               shape = factor(Sex))) +
  geom_point(size = 3) +
  geom_mark_hull(aes(fill = factor(Cluster)),
                 alpha = 0.2,
                 colour = NA,
                 concavity = Inf)+
  scale_colour_manual(values = cluster_cols) +
  scale_fill_manual(values = cluster_cols) +
  theme_classic()

# Save the figure with the MCPs
ggsave(filename = "../figures/NESE_MCP_PM.pdf",
       plot = MPC.PM,
       device = cairo_pdf)

# Gets the cartesian coordinates in the isotopic bi-space
pts <- st_as_sf(PM,
                coords = c("d13C", "d15N"),
                remove = FALSE)

# Calculated tge MCPs for each cluster/sex
mcps <-
  pts %>%
  group_by(Cluster) %>%
  summarise() %>%
  st_convex_hull()

#Calculate the overlap between MCPs for the clusters in parts per mil ^2

clusters <- unique(mcps$Cluster)

overlap <- expand.grid(cluster1 = clusters,
                       cluster2 = clusters)

overlap$area_overlap <- NA

for(i in seq_len(nrow(overlap))){
  
  p1 <- mcps[mcps$Cluster == overlap$cluster1[i], ]
  p2 <- mcps[mcps$Cluster == overlap$cluster2[i], ]
  
  inter <- st_intersection(p1, p2)
  
  overlap$area_overlap[i] <-
    if(nrow(inter) == 0) 0 else st_area(inter)
}

overlap


#Calculate the overlap between MCPs for the clusters in %

overlap$percent_overlap_cluster1 <- NA
overlap$percent_overlap_cluster2 <- NA

for(i in seq_len(nrow(overlap))){
  
  p1 <- mcps[mcps$Cluster == overlap$cluster1[i], ]
  p2 <- mcps[mcps$Cluster == overlap$cluster2[i], ]
  
  inter <- st_intersection(p1, p2)
  
  if(nrow(inter)==0){
    
    overlap$percent_overlap_cluster1[i] <- 0
    overlap$percent_overlap_cluster2[i] <- 0
    
  } else{
    
    Aint <- as.numeric(st_area(inter))
    A1   <- as.numeric(st_area(p1))
    A2   <- as.numeric(st_area(p2))
    if(Aint == 0){
      overlap$percent_overlap_cluster1[i] <- 0
      overlap$percent_overlap_cluster2[i] <- 0
      
    } else{
    
    overlap$percent_overlap_cluster1[i] <- 100*Aint/A1
    overlap$percent_overlap_cluster2[i] <- 100*Aint/A2
    # Gets the overlaps = 100 to NAs
    if(round(overlap$percent_overlap_cluster1[i]) == 100){overlap$percent_overlap_cluster1[i] <- NA}
    if(round(overlap$percent_overlap_cluster2[i]) == 100){overlap$percent_overlap_cluster2[i] <- NA}
  }}
}


# Need to manually vhange the overlap for PMC5
overlap$percent_overlap_cluster1[37] <- NA
overlap$percent_overlap_cluster2[37] <- NA

# Plot the grid of overlap
overlap.PM <- ggplot(overlap,
                     aes(cluster1, cluster2, fill = percent_overlap_cluster2)) +
  geom_tile() +
  scale_fill_distiller(
    palette = "RdBu",
    direction = -1
  ) +
  coord_equal() +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

ggsave(filename = "../figures/NESE_IsoOverlap_PM.pdf",
       plot = overlap.PM,
       device = cairo_pdf)
