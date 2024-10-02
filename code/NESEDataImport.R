# Setting up the analysis

library(gclus)
library(SIBER)
library(ggplot2)
library(plyr)
library(dplyr)
library(factoextra)
library(ggfortify)
library(pls)
library(FactoMineR)
library(tibble)
library(nicheROVER)
library(mclust)
library(qwraps2)
library(ggstatsplot)
library(dispRity)
library(NbClust)

data.iso <- read.csv("data/IsotopesDatasetForLuis_2023_4_4.csv")
data.iso$fSex <- as.factor(data.iso$Sex)
data.iso$fYear <- as.factor(data.iso$ProcedureYear)
temp1 <- read.csv('data/EsealHgPOPs_Master.csv')
temp1 <- temp1[,c("MasterID",'MaxLat_degrees','MinLon_degrees','SdDistContShelf_km'),]
temp2 <- data.iso %>% right_join(temp1, by="MasterID")
data.iso <- temp2
rm(list=c('temp1','temp2'))


# Text to create summary talble for variables
our_summary1 <-
  list("d13C" = list("mean (sd)" = ~ qwraps2::mean_sd(d13C)),
       "d15N" = list("mean (sd)" = ~ qwraps2::mean_sd(d15N)),
       "MaxLat_degrees" = list("mean (sd)" = ~ qwraps2::mean_sd(MaxLat_degrees)),
       "MinLon_degrees" = list("mean (sd)" = ~ qwraps2::mean_sd(MinLon_degrees)),
       "MedianDistToContShelf_km" = list("mean (sd)" = ~ qwraps2::mean_sd(MedianDistToContShelf_km)),
       "SdDistContShelf_km" = list("mean (sd)" = ~ qwraps2::mean_sd(SdDistContShelf_km)),
       "MesoEcoProportionShelf" = list("mean (sd)" = ~ qwraps2::mean_sd(MesoEcoProportionShelf)),
       "MesoEcoProportionCAC" = list("mean (sd)" = ~ qwraps2::mean_sd(MesoEcoProportionCAC)),
       "MesoEcoProportionSubArcticPac" = list("mean (sd)" = ~ qwraps2::mean_sd(MesoEcoProportionSubArcticPac)),
       "MesoEcoProportionNorthCPac" = list("mean (sd)" = ~ qwraps2::mean_sd(MesoEcoProportionNorthCPac)),
       "Mean_Depth_m" = list("mean (sd)" = ~ qwraps2::mean_sd(Mean_Depth_m)),
       "Mean_Depth_sd" = list("mean (sd)" = ~ qwraps2::mean_sd(Mean_Depth_sd)),
       "Mean_Duration_sec" = list("mean (sd)" = ~ qwraps2::mean_sd(Mean_Duration_sec)),
       "Mean_Duration_sd" = list("mean (sd)" = ~ qwraps2::mean_sd(Mean_Duration_sd)),
       "Pct_TransitDives" = list("mean (sd)" = ~ qwraps2::mean_sd(Pct_TransitDives)),
       "Pct_ForagingDives" = list("mean (sd)" = ~ qwraps2::mean_sd(Pct_ForagingDives)),
       "Pct_DriftDives" = list("mean (sd)" = ~ qwraps2::mean_sd(Pct_DriftDives)),
       "Pct_BenthicDives" = list("mean (sd)" = ~ qwraps2::mean_sd(Pct_BenthicDives)),
       "PctTripDiving" = list("mean (sd)" = ~ qwraps2::mean_sd(PctTripDiving)),
       "DeepDayForageDivesMedian_m" = list("mean (sd)" = ~ qwraps2::mean_sd(DeepDayForageDivesMedian_m)),
       "DeepNightForageDivesMedian_m" = list("mean (sd)" = ~ qwraps2::mean_sd(DeepNightForageDivesMedian_m))
  )

saveRDS(data.iso, file = 'data/data_iso.rds')
