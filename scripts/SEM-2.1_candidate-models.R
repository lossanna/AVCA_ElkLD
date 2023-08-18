# Purpose: Test candidate models for SEM analysis.
#   SEM 2.1 incorporates better assessment of model fit, according to procedure from Grace 2020 "WOE",
#     and there are meta-models drawn for each in the PowerPoint (Writing/SEM meta model.pptx).

# Findings:
#   Remove OM to improve model fit
#   Removing tree cover does not improve model fit

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




# 2 Soil mic latent, TN; Veg18 & tree included ----------------------------

# Initial attempt
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
summary(fit2.0, fit.measures = TRUE, standardized = TRUE)
# Mod2.0 diagnostics:
# Global fit:
#   chi-sq p-value is good (>0.05)
#   CFI is okay (>0.95)
#   RMSEA is okay (<0.1, lower CI is 0)
#   SRMR is okay (<0.1)
modindices(fit2.0, sort = TRUE)
#  Adding covariance with TN_log: there could be an unmeasured factor that affects these together
#   (i.e. it seems scientifically plausible), but the pathways do not end up significant, and
#   it seems weird to randomly allow those things to covary when they are part of the larger soil microbiome,
#   so I am going to stick with this one

semPaths(fit2.0, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7,
         nCharNodes = 6, node.width = 1.3, layout = "tree2", reorder = FALSE)


# Add covariances based on MI (modification indices)
mod2.1 <- '
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
  chemoheterotrophy_log ~~ TN_log
  barc.richness ~~ n.cycler_log
  fungi.richness ~~ TN_log
'
fit2.1 <- sem(mod2.1, data = sem.dat) 
summary(fit2.1, fit.measures = TRUE, standardized = TRUE)
# Mod2.1 diagnostics:
#   Improved chi-sq stat and p-value
#   Not much change in AIC
#   Other global fit metrics remain okay
modindices(fit2.1, sort = TRUE, minimum.value = 3.5)
#   Covariance between notree.18 and tree doesn't make sense scientifically
#   TN & chemmohet are the only significant path of the ones added based on MI
#     are N-cyclers chemoheterotrophs? Why isn't there a correlation between TN & N-cyclers?


# Chemohet~~TN only
mod2.2 <- '
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
  chemoheterotrophy_log ~~ TN_log
'
fit2.2 <- sem(mod2.2, data = sem.dat) 
summary(fit2.2, fit.measures = TRUE, standardized = TRUE)
# This doesn't really seem that different from mod2.0 or 2.1, and the scientific justification isn't great
#   Will probably just go with  mod2.0




# 3 Soil mic latent, TN; Veg18 included -----------------------------------

# Remove tree
mod3.0 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # regressions
  notree ~ rocks + notree.18 
  notree.18 ~ rocks
  soil_microbe ~ rocks
  TN_log ~ rocks
  
  # covariance
  soil_microbe ~~ TN_log
  soil_microbe ~~ notree
  TN_log ~~ notree
'
fit3.0 <- sem(mod3.0, data = sem.dat)
summary(fit3.0, standardized = TRUE, fit.measures = TRUE)
# Mod3.0 diagnostics:
# Global fit:
#   chi-sq p-value is good (>0.05)
#   CFI is okay (>0.95)
#   RMSEA is okay (<0.1, lower CI is 0)
#   SRMR is okay (<0.1)
# But removing tree does not actually improve global fit (it actually gets worse)

semPaths(fit3.0, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 6,
         nCharNodes = 6, node.width = 1.3, layout = "tree2")

