# Purpose: Conduct PCA for 2021 soil and plant data to see what is driving variation.
# One version uses total cover, another uses herbaceous cover.
# Other variables: perennial richness & Shannon, TN_log, TC_log, OM, bac/arc richness & Shannon.
# Created: 2023-03-28
# Last updated: 2023-03-29

library(tidyverse)
library(FactoMineR)
library(factoextra)

# Load data ---------------------------------------------------------------

dat.2021 <- read_csv("data/cleaned/Data-2021_clean.csv")


# Data wrangling ----------------------------------------------------------

# Narrow down variables
# All, total cover
dat.total.all <- dat.2021 |> 
  select(Cover, rich, shan, TN_log, TC_log, OM_perc, Richness.barc, Shannon.barc) |> 
  rename(TN = TN_log,
         TC = TC_log,
         OM = OM_perc,
         Richness = rich,
         Shannon = shan)

# By Treated/Control, total cover
dat.total.trt <- dat.2021 |> 
  filter(Treatment3 == "Treated") |> 
  select(Cover, rich, shan, TN_log, TC_log, OM_perc, Richness.barc, Shannon.barc) |> 
  rename(TN = TN_log,
         TC = TC_log,
         OM = OM_perc,
         Richness = rich,
         Shannon = shan)

dat.total.ctrl <- dat.2021 |> 
  filter(Treatment3 == "Control") |> 
  select(Herbaceous, rich, shan, TN_log, TC_log, OM_perc, Richness.barc, Shannon.barc) |> 
  rename(TN = TN_log,
         TC = TC_log,
         OM = OM_perc,
         Richness = rich,
         Shannon = shan)

# All, herbaceous cover
dat.herb.all <- dat.2021 |> 
  select(Herbaceous, rich, shan, TN_log, TC_log, OM_perc, Richness.barc, Shannon.barc) |> 
  rename(TN = TN_log,
         TC = TC_log,
         OM = OM_perc,
         Richness = rich,
         Shannon = shan)

# By Treated/Control, herbaceous cover
dat.herb.trt <- dat.2021 |> 
  filter(Treatment3 == "Treated") |> 
  select(Herbaceous, rich, shan, TN_log, TC_log, OM_perc, Richness.barc, Shannon.barc) |> 
  rename(TN = TN_log,
         TC = TC_log,
         OM = OM_perc,
         Richness = rich,
         Shannon = shan)

dat.herb.ctrl <- dat.2021 |> 
  filter(Treatment3 == "Control") |> 
  select(Herbaceous, rich, shan, TN_log, TC_log, OM_perc, Richness.barc, Shannon.barc) |> 
  rename(TN = TN_log,
         TC = TC_log,
         OM = OM_perc,
         Richness = rich,
         Shannon = shan)



# PCA by Treatment3, total cover ------------------------------------------

# All
PCA(dat.total.all, scale.unit = TRUE, graph = TRUE)
pca.total.all <- PCA(dat.total.all, scale.unit = TRUE, graph = FALSE)

get_eigenvalue(pca.total.all)

var.total.all <- get_pca_var(pca.total.all)
head(var.total.all$coord) # coordinates
head(var.total.all$cos2)
head(var.total.all$contrib)

fviz_pca_var(pca.total.all,
             repel = TRUE)

ind.total.all <- get_pca_ind(pca.total.all)
fviz_pca_ind(pca.total.all)

fviz_pca_ind(pca.total.all,
             geom.ind = "point",
             col.ind = dat.2021$Treatment3,
             palette = c("red", "#1F78B4"),
             addEllipses = TRUE)

fviz_pca_ind(pca.total.all,
             geom.ind = "point",
             col.ind = dat.2021$Channel,
             palette = c("red", "#1F78B4", "red", "#1F78B4"),
             addEllipses = TRUE)


# Separate Control & Treatment
# Control
pca.total.ctrl3 <- PCA(dat.total.ctrl, scale.unit = TRUE, graph = FALSE)
get_eigenvalue(pca.total.ctrl3)

fviz_pca_var(pca.total.ctrl3,
             repel = TRUE) +
  labs(title = "PCA for Control")

var.total.ctrl3 <- get_pca_var(pca.total.ctrl3) # variable contributions
var.total.ctrl3$contrib
fviz_contrib(pca.total.ctrl3, choice = "var", axes = 1, top = 10) +
  labs(title = "Var contribution PC1 Control") # contrib PC1
fviz_contrib(pca.total.ctrl3, choice = "var", axes = 2, top = 10) +
  labs(title = "Var contribution PC2 Control") # contrib PC2


# Treated
pca.total.trt3 <- PCA(dat.total.trt, scale.unit = TRUE, graph = FALSE)
get_eigenvalue(pca.total.trt3)

fviz_pca_var(pca.total.trt3,
             repel = TRUE) +
  labs(title = "PCA for Treated")

var.total.trt3 <- get_pca_var(pca.total.trt3) # variable contributions
var.total.trt3$contrib
fviz_contrib(pca.total.trt3, choice = "var", axes = 1, top = 10) # contrib PC1
fviz_contrib(pca.total.trt3, choice = "var", axes = 2, top = 10) # contrib PC2


# PCA by Treatment3, herb cover -------------------------------------------

# All
PCA(dat.herb.all, scale.unit = TRUE, graph = TRUE)
pca.herb.all <- PCA(dat.herb.all, scale.unit = TRUE, graph = FALSE)

get_eigenvalue(pca.herb.all)

var.herb.all <- get_pca_var(pca.herb.all)
head(var.herb.all$coord) # coordinates
head(var.herb.all$cos2)
head(var.herb.all$contrib)

fviz_pca_var(pca.herb.all,
             repel = TRUE)

ind.herb.all <- get_pca_ind(pca.herb.all)
fviz_pca_ind(pca.herb.all)

fviz_pca_ind(pca.herb.all,
             geom.ind = "point",
             col.ind = dat.2021$Treatment3,
             palette = c("red", "#1F78B4"),
             addEllipses = TRUE)

fviz_pca_ind(pca.herb.all,
             geom.ind = "point",
             col.ind = dat.2021$Channel,
             palette = c("red", "#1F78B4", "red", "#1F78B4"),
             addEllipses = TRUE)


# Separate Control & Treatment
# Control
pca.herb.ctrl3 <- PCA(dat.herb.ctrl, scale.unit = TRUE, graph = FALSE)
get_eigenvalue(pca.herb.ctrl3)

fviz_pca_var(pca.herb.ctrl3,
             repel = TRUE) +
  labs(title = "PCA for Control")

var.herb.ctrl3 <- get_pca_var(pca.herb.ctrl3) # variable contributions
var.herb.ctrl3$contrib
fviz_contrib(pca.herb.ctrl3, choice = "var", axes = 1, top = 10) +
  labs(title = "Var contribution PC1 Control") # contrib PC1
fviz_contrib(pca.herb.ctrl3, choice = "var", axes = 2, top = 10) +
  labs(title = "Var contribution PC2 Control") # contrib PC2


# Treated
pca.herb.trt3 <- PCA(dat.herb.trt, scale.unit = TRUE, graph = FALSE)
get_eigenvalue(pca.herb.trt3)

fviz_pca_var(pca.herb.trt3,
             repel = TRUE) +
  labs(title = "PCA for Treated")

var.herb.trt3 <- get_pca_var(pca.herb.trt3) # variable contributions
var.herb.trt3$contrib
fviz_contrib(pca.herb.trt3, choice = "var", axes = 1, top = 10) # contrib PC1
fviz_contrib(pca.herb.trt3, choice = "var", axes = 2, top = 10) # contrib PC2



save.image("RData/PCA_2021.RData")

