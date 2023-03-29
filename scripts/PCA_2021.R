# Purpose: Conduct PCA for 2021 soil and plant data to see what is driving variation.

library(tidyverse)
library(FactoMineR)
library(factoextra)

# Load data ---------------------------------------------------------------

dat.2021 <- read_csv("data/cleaned/Data-2021_clean.csv")


# Data wrangling ----------------------------------------------------------

# Narrow down variables
dat.pca <- dat.2021 |> 
  select(Cover, rich, shan, TN_log, TC_log, OM_perc, Richness.barc, Shannon.barc) |> 
  rename(TN = TN_log,
         TC = TC_log,
         OM = OM_perc,
         Richness = rich,
         Shannon = shan)

dat.pca.trt <- dat.2021 |> 
  filter(Treatment3 == "Treated") |> 
  select(Herbaceous, rich, shan, TN_log, TC_log, OM_perc, Richness.barc, Shannon.barc) |> 
  rename(TN = TN_log,
         TC = TC_log,
         OM = OM_perc,
         Richness = rich,
         Shannon = shan)

dat.pca.ctrl <- dat.2021 |> 
  filter(Treatment3 == "Control") |> 
  select(Herbaceous, rich, shan, TN_log, TC_log, OM_perc, Richness.barc, Shannon.barc) |> 
  rename(TN = TN_log,
         TC = TC_log,
         OM = OM_perc,
         Richness = rich,
         Shannon = shan)


# PCA by Treatment3 -------------------------------------------------------


# All
PCA(dat.pca, scale.unit = TRUE, graph = TRUE)
pca.all <- PCA(dat.pca, scale.unit = TRUE, graph = FALSE)

get_eigenvalue(pca.all)

var.all <- get_pca_var(pca.all)
head(var.all$coord) # coordinates
head(var.all$cos2)
head(var.all$contrib)

fviz_pca_var(pca.all,
             repel = TRUE)

ind.all <- get_pca_ind(pca.all)
fviz_pca_ind(pca.all)

fviz_pca_ind(pca.all,
             geom.ind = "point",
             col.ind = dat.2021$Treatment3,
             palette = c("red", "#1F78B4"),
             addEllipses = TRUE)

fviz_pca_ind(pca.all,
             geom.ind = "point",
             col.ind = dat.2021$Channel,
             palette = c("red", "#1F78B4", "red", "#1F78B4"),
             addEllipses = TRUE)


# Separate Control & Treatment
# Control
pca.ctrl3 <- PCA(dat.pca.ctrl, scale.unit = TRUE, graph = FALSE)
get_eigenvalue(pca.ctrl3)

fviz_pca_var(pca.ctrl3,
             repel = TRUE) +
  labs(title = "PCA for Control")

var.ctrl3 <- get_pca_var(pca.ctrl3) # variable contributions
var.ctrl3$contrib
fviz_contrib(pca.ctrl3, choice = "var", axes = 1, top = 10) +
  labs(title = "Var contribution PC1 Control") # contrib PC1
fviz_contrib(pca.ctrl3, choice = "var", axes = 2, top = 10) +
  labs(title = "Var contribution PC2 Control") # contrib PC2


# Treated
pca.trt3 <- PCA(dat.pca.trt, scale.unit = TRUE, graph = FALSE)
get_eigenvalue(pca.trt3)

fviz_pca_var(pca.trt3,
             repel = TRUE) +
  labs(title = "PCA for Treated")

var.trt3 <- get_pca_var(pca.trt3) # variable contributions
var.trt3$contrib
fviz_contrib(pca.trt3, choice = "var", axes = 1, top = 10) # contrib PC1
fviz_contrib(pca.trt3, choice = "var", axes = 2, top = 10) # contrib PC2

save.image("RData/PCA_2021.RData")
