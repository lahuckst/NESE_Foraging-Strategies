# NESE PCA/HCPC cluster validation workflow
# Run from repository root; input: data/data_iso.rds
# This is a standalone replacement for the fixed-k sections of the original
# all-female and trip-specific HCPC scripts.
#
# Clustering uses movement/habitat/diving variables only. d13C/d15N and year
# are external checks, not clustering inputs. No partial varimax rotation is
# applied to the FactoMineR PCA object.
#
# Packages: dplyr, tidyr, purrr, readr, ggplot2, cluster
# For final analyses increase N_BOOT to 300-500 after testing.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr)
  library(ggplot2); library(cluster)
})
set.seed(20261023)

INPUT_FILE <- "data/data_iso.rds"
OUT <- "results/cluster_validation"
FIG <- file.path(OUT, "figures")
dir.create(FIG, recursive=TRUE, showWarnings=FALSE)

cluster_vars <- c(
  "MaxLat_degrees", "MinLon_degrees", "MedianDistToContShelf_km",
  "SdDistContShelf_km", "MesoEcoProportionShelf", "MesoEcoProportionCAC",
  "MesoEcoProportionSubArcticPac", "MesoEcoProportionNorthCPac",
  "Mean_Depth_m", "Mean_Depth_sd", "Mean_Duration_sec", "Mean_Duration_sd",
  "Pct_TransitDives", "Pct_ForagingDives", "Pct_DriftDives",
  "Pct_BenthicDives", "PctTripDiving", "DeepDayForageDivesMedian_m",
  "DeepNightForageDivesMedian_m"
)
external_vars <- c("d13C", "d15N", "ProcedureYear")
K_MIN <- 2
K_MAX <- 10
N_BOOT <- 500
BOOT_FRACTION <- 0.80
MIN_CLUSTER_N <- 8
VARIANCE_TARGET <- 0.75
MAX_PCS <- 8

ward <- function(x, k) {
  cutree(hclust(dist(x), method="ward.D2"), k=k)
}
mean_sil <- function(x, k) {
  mean(cluster::silhouette(as.integer(ward(x,k)), dist(x))[, "sil_width"])
}

# Resample 80% of individuals without replacement. For every pair, calculate
# how often it is assigned to the same cluster when both individuals occur in
# the resample. Summarise co-assignment for pairs belonging to the full-data
# clusters. This is label-invariant and measures partition reproducibility.
stability <- function(x, k, B=N_BOOT, fraction=BOOT_FRACTION) {
  n <- nrow(x)
  co <- seen <- matrix(0, n, n)
  for (b in seq_len(B)) {
    ix <- sort(sample(seq_len(n), max(k+2, floor(fraction*n))))
    lab <- ward(x[ix,,drop=FALSE], k)
    same <- outer(as.integer(lab), as.integer(lab), "==") * 1
    seen[ix,ix] <- seen[ix,ix] + 1
    co[ix,ix] <- co[ix,ix] + same
  }
  p <- co / pmax(seen, 1)
  diag(p) <- NA_real_
  full <- ward(x,k)
  vals <- unlist(lapply(seq_len(n), function(i) {
    mates <- which(full == full[i] & seq_len(n) != i)
    if (length(mates)) p[i,mates] else numeric()
  }))
  vals <- vals[is.finite(vals)]
  c(mean=if(length(vals)) mean(vals) else NA_real_,
    median=if(length(vals)) median(vals) else NA_real_,
    sd=if(length(vals)>1) sd(vals) else NA_real_)
}

kw <- function(x, group) {
  ok <- is.finite(x) & !is.na(group)
  x <- x[ok]; group <- droplevels(factor(group[ok]))
  if (nlevels(group)<2 || length(x)<5) return(c(H=NA,p=NA,epsilon2=NA))
  z <- kruskal.test(x ~ group)
  n <- length(x); k <- nlevels(group)
  c(H=unname(z$statistic), p=z$p.value,
    epsilon2=max(0,(unname(z$statistic)-k+1)/(n-k)))
}

analyse <- function(dat, label) {
  message("Analysing ", label)
  needed <- c("TOPPID","Trip","Sex",cluster_vars,external_vars)
  miss <- setdiff(needed,names(dat))
  if(length(miss)) stop("Missing columns: ",paste(miss,collapse=", "))

  dat <- dat %>% filter(Sex=="Female") %>%
    select(all_of(needed)) %>%
    filter(if_all(all_of(cluster_vars), ~ is.finite(as.numeric(.x)))) %>%
    filter(is.finite(as.numeric(d13C)), is.finite(as.numeric(d15N))) %>%
    distinct(TOPPID,.keep_all=TRUE)
  if(nrow(dat)<20) warning(label, ": small sample (n=",nrow(dat),").")

  X <- dat %>% select(all_of(cluster_vars)) %>% mutate(across(everything(),as.numeric))
  Xs <- scale(X)
  keep <- apply(Xs,2,function(z) all(is.finite(z)) && sd(z)>0)
  Xs <- Xs[,keep,drop=FALSE]
  pca <- prcomp(Xs, center=FALSE, scale.=FALSE)
  ve <- pca$sdev^2/sum(pca$sdev^2)
  cum <- cumsum(ve)
  npc <- min(which(cum>=VARIANCE_TARGET)[1],MAX_PCS,ncol(pca$x))
  npc <- max(2,npc)
  scores <- pca$x[,seq_len(npc),drop=FALSE]
  write_csv(tibble(PC=seq_along(ve),variance=ve,cumulative=cum),
            file.path(OUT,paste0(label,"_PCA_variance.csv")))

  kmax <- min(K_MAX,nrow(scores)-1)
  ks <- K_MIN:kmax
  sil <- map_dbl(ks, ~mean_sil(scores,.x))
  stab <- map_dfr(ks,function(k) {
    s <- stability(scores,k)
    tibble(k=k,stability_mean=s["mean"],stability_median=s["median"],
           stability_sd=s["sd"])
  })
  metrics <- tibble(k=ks,mean_silhouette=sil) %>% left_join(stab,by="k")
  write_csv(metrics,file.path(OUT,paste0(label,"_k_diagnostics.csv")))

  g1 <- ggplot(metrics,aes(k,mean_silhouette)) + geom_line()+geom_point(size=2)+
    scale_x_continuous(breaks=ks)+theme_bw()+
    labs(title=paste(label,"— cluster-number assessment"),
         x="Number of clusters (k)",y="Mean silhouette width")
  ggsave(file.path(FIG,paste0(label,"_silhouette.png")),g1,width=7,height=4.5,dpi=300)
  g2 <- ggplot(metrics,aes(k,stability_mean)) + geom_line()+geom_point(size=2)+
    geom_errorbar(aes(ymin=pmax(0,stability_mean-stability_sd),
                      ymax=pmin(1,stability_mean+stability_sd)),width=.15,na.rm=TRUE)+
    scale_x_continuous(breaks=ks)+coord_cartesian(ylim=c(0,1))+theme_bw()+
    labs(title=paste(label,"— resampling stability"),x="Number of clusters (k)",
         y="Mean within-cluster co-assignment probability")
  ggsave(file.path(FIG,paste0(label,"_stability.png")),g2,width=7,height=4.5,dpi=300)

  # Provisional choice: highest silhouette among solutions with >= MIN_CLUSTER_N
  # members per cluster. Inspect all metrics and profiles before finalising k.
  eligible <- ks[vapply(ks,function(k) min(table(ward(scores,k)))>=MIN_CLUSTER_N,logical(1))]
  chosen <- if(length(eligible)) eligible[which.max(metrics$mean_silhouette[match(eligible,ks)])] else ks[which.max(sil)]
  if(!length(eligible)) warning(label,": no k has all clusters >= ",MIN_CLUSTER_N,"; chosen k is provisional.")
  cl <- factor(paste0("Cluster ",ward(scores,chosen)))
  out <- dat %>% mutate(cluster=cl,analysis=label)
  write_csv(out,file.path(OUT,paste0(label,"_assignments.csv")))
  write_csv(as.data.frame(scores) %>% setNames(paste0("PC",seq_len(npc))) %>%
              mutate(TOPPID=dat$TOPPID,.before=1),
            file.path(OUT,paste0(label,"_PC_scores.csv")))
  write_csv(as.data.frame(table(cluster=cl)),file.path(OUT,paste0(label,"_cluster_sizes.csv")))

  # Silhouette widths for chosen k identify poorly assigned individuals.
  si <- silhouette(as.integer(cl),dist(scores))
  write_csv(tibble(TOPPID=dat$TOPPID,cluster=cl,silhouette=si[,"sil_width"]),
            file.path(OUT,paste0(label,"_chosenK_silhouettes.csv")))

  # External biological checks: isotopes and year were not used to create clusters.
  tests <- map_dfr(c("d13C","d15N"),function(v) {
    z <- kw(as.numeric(out[[v]]),out$cluster)
    tibble(variable=v,H=z["H"],p=z["p"],epsilon2=z["epsilon2"])
  })
  year_tab <- table(out$cluster,out$ProcedureYear)
  year_p <- tryCatch({
    chi <- suppressWarnings(chisq.test(year_tab))
    if(any(chi$expected<5)) fisher.test(year_tab,simulate.p.value=TRUE,B=10000)$p.value
    else chi$p.value
  },error=function(e) NA_real_)
  tests <- bind_rows(tests,tibble(variable="ProcedureYear",H=NA_real_,
                                  p=year_p,epsilon2=NA_real_)) %>%
    mutate(p_BH=p.adjust(p,method="BH"),analysis=label,k=chosen,n=nrow(out))
  write_csv(tests,file.path(OUT,paste0(label,"_external_validity.csv")))

  # Original-variable profiles for biological interpretation. Clustering-variable
  # separation is descriptive (not independent validation); isotope/year patterns
  # provide more independent corroboration.
  profvars <- c(cluster_vars,"d13C","d15N")
  prof <- out %>% select(cluster,all_of(profvars)) %>%
    pivot_longer(-cluster,names_to="variable",values_to="value") %>%
    group_by(variable) %>% mutate(z=as.numeric(scale(value))) %>%
    group_by(cluster,variable) %>%
    summarise(mean_z=mean(z,na.rm=TRUE),median=median(value,na.rm=TRUE),
              q25=quantile(value,.25,na.rm=TRUE),q75=quantile(value,.75,na.rm=TRUE),
              .groups="drop")
  write_csv(prof,file.path(OUT,paste0(label,"_cluster_profiles.csv")))
  gp <- ggplot(prof,aes(variable,cluster,fill=mean_z))+geom_tile()+
    scale_fill_gradient2()+theme_bw()+
    theme(axis.text.x=element_text(angle=60,hjust=1))+
    labs(title=paste(label,"— standardized cluster profiles"),x=NULL,y=NULL,fill="Mean z")
  ggsave(file.path(FIG,paste0(label,"_profiles.png")),gp,width=11,height=5.5,dpi=300)

  plotdat <- data.frame(PC1=scores[,1],PC2=scores[,2],cluster=cl)
  gpc <- ggplot(plotdat,aes(PC1,PC2,colour=cluster))+geom_point(alpha=.8,size=2)+
    theme_bw()+labs(title=paste(label,"— Ward clusters in PC space"),
      subtitle=paste0("k=",chosen,"; PCs=",npc,"; variance=",round(100*cum[npc],1),"%"))
  ggsave(file.path(FIG,paste0(label,"_PC_clusters.png")),gpc,width=7,height=5.5,dpi=300)

  summary <- tibble(analysis=label,n=nrow(out),n_variables=ncol(Xs),n_PCs=npc,
    cumulative_variance=cum[npc],provisional_k=chosen,
    minimum_cluster_n=min(table(cl)),
    mean_silhouette=metrics$mean_silhouette[match(chosen,metrics$k)],
    stability=metrics$stability_mean[match(chosen,metrics$k)])
  write_csv(summary,file.path(OUT,paste0(label,"_summary.csv")))
  message("Provisional k=",chosen,"; n=",nrow(out),"; PCs=",npc,
          "; variance explained=",round(100*cum[npc],1),"%")
  list(summary=summary,assignments=out,metrics=metrics)
}

if(!file.exists(INPUT_FILE)) stop("Cannot find ",INPUT_FILE,
  ". Run from repository root or edit INPUT_FILE.")
data_iso <- readRDS(INPUT_FILE)
# Retain original scripts' sign convention. Sign reversal does not change
# Euclidean geometry when consistently applied.
for(v in c("DeepDayForageDivesMedian_m","DeepNightForageDivesMedian_m"))
  if(v %in% names(data_iso)) data_iso[[v]] <- -data_iso[[v]]

all_females <- analyse(data_iso,"AllFemales")
trip_results <- list()
for(tr in sort(unique(as.character(data_iso$Trip[!is.na(data_iso$Trip)])))) {
  lab <- gsub("[^A-Za-z0-9]+","",tr)
  trip_results[[tr]] <- analyse(filter(data_iso,Trip==tr),lab)
}
write_csv(bind_rows(all_females$summary,map_dfr(trip_results,"summary")),
          file.path(OUT,"ALL_analysis_summary.csv"))
message("Done. Inspect k diagnostics, stability, cluster sizes, profiles and external validity.")
message("The selected k is provisional; do not choose it from one metric alone.")
