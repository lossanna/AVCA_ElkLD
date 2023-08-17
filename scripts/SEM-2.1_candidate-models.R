# Purpose: Test candidate models for SEM analysis.
#   SEM 2.1 incorporates better assessment of model fit, according to procedure from Grace 2020 "WOE",
#     and there are meta-models drawn for each in the PowerPoint (Writing/SEM meta model.pptx).

# Findings:

# Created: 2023-08-17
# Updated: 2023-08-17


library(lavaan)
library(tidyverse)
library(semPlot)

# Load data ---------------------------------------------------------------

dat.2021 <- read.csv("data/cleaned/Data-2021_clean.csv") 


# Data wrangling ----------------------------------------------------------

# Add Control/Treated as binary variable and select only variables needed for SEM
sem.dat.unscaled <- dat.2021 |> 
  mutate(rocks = case_when(
    Treatment3 == "Control" ~ 0,
    Treatment3 == "Treated" ~ 1)) |>
  mutate(wide = case_when(
    str_detect(Channel, "21|19") ~ 1,
    str_detect(Channel, "13|12") ~ 0)) |> 
  select(Sample, rocks, wide, herb, herb.18, notree, notree.18, tree, perveg.richness, perveg.shannon,
         TN_log, CN_ratio, OM_log, OM_perc, barc.richness, fungi.richness,
         chemoheterotrophy_log, n.cycler_log, saprotroph) 

# Center and scale variables
sem.dat <- sem.dat.unscaled |> 
  mutate(rocks = as.character(rocks),
         Sample = as.character(Sample),
         wide = as.character(wide)) |> 
  mutate_if(is.numeric, scale) |> 
  mutate(rocks = as.numeric(rocks),
         wide = as.numeric(wide))



# 1 Soil mic & soil chem as latent; Veg18 & tree included -----------------

# Initial attempt
mod1.0 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ TN_log + OM_log
  
  # regressions
  notree ~ rocks + notree.18 + tree 
  soil_chem ~ rocks
  soil_microbe ~ rocks
  notree.18 ~ rocks + tree
  
  # covariance
  soil_chem ~~ soil_microbe
  soil_chem ~~ notree
  soil_microbe ~~ notree
'
fit1.0 <- sem(mod1.0, data = sem.dat) # lavaan WARNING: some estimated ov variances are negative
summary(fit1.0, fit.measures = TRUE)
#   Poor global fit: low chi-sq p-value, and negative variances
#   Figured out from SEM-2.0 that OM & TN were collinear 


# 2 Soil mic as latent endogenous, TN; Veg18 & tree included --------------

# Remove OM_log
mod2.0 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # structure
  notree ~ rocks + notree.18 + tree 
  TN_log ~ rocks
  soil_microbe ~ rocks
  
  # covariance
  TN_log ~~ soil_microbe
  TN_log ~~ notree
  soil_microbe ~~ notree
'
fit2.0 <- sem(mod2.0, data = sem.dat) 
summary(fit2.0, fit.measures = TRUE)
# Mod2.0 diagnostics:
# Global fit:
#   chi-sq p-value is good, but chi-sq statistic is pretty high
#   CFI is okay (>0.95)
#   RMSEA is okay (<0.1, lower CI is 0)
#   SRMR is okay (<0.1)