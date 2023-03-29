# Purpose: Conduct PCA for 2021 soil and plant data to see what is driving variation.

library(tidyverse)
library(FactoMineR)
library(factoextra)

# Load data ---------------------------------------------------------------

dat.2021 <- read_csv("data/cleaned/Data-2021_clean.csv")


# Data wrangling ----------------------------------------------------------

# Narrow down variables
dat.pca <- dat.2021 |> 
  select(Cover, rich, shan, TN_log, TC_log, OM_perc, Richness.barc, Shannon.barc)


# PCA by Treatment3 -------------------------------------------------------


# All
PCA(dat.pca, scale.unit = TRUE, graph = TRUE)
pca.all <- PCA(dat.pca, scale.unit = TRUE, graph = FALSE)

get_eigenvalue(pca.all)

var.all <- get_pca_var(pca.all)
head(var.all$coord) # coordinates
head(var.all$cos2)
head(var.all$contrib)

fviz_pca_var(pca.all)

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


