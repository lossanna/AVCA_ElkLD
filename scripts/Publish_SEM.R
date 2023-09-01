# Purpose: Create code for SEM (figs are made in PPT - I don't want to talk about it,
#                                                       I tried my best in R)
# Am taking out the mod indices because I decided not to add/remove any paths based on significance.
# Model numbers correspond to number they were given in manuscript:
#   SEM-2.1 mod3 = manuscript mod1
#   SEM-2.1 mod2 = manuscript mod2
#   SEM-2.1 mod7 = manuscript mod3
#   SEM-2.1 mod8 = manuscript mod4

# Created: 2023-08-29
# Updated: 2023-08-30

library(lavaan)
library(tidyverse)

# Load data ---------------------------------------------------------------

dat.2021 <- read.csv("data/publish/Veg-soil-elev_2021.csv") 


# Data wrangling ----------------------------------------------------------

# Add Control/Treated as binary variable and select only variables needed for SEM
sem.dat.unscaled <- dat.2021 |> 
  mutate(rocks = case_when(
    Treatment == "Control" ~ 0,
    Treatment == "Treated" ~ 1)) |>
  select(Sample, rocks, notree, notree.18, herb, herb.18, tree, perveg.richness, perveg.shannon,
         TN_log, TC_log, OM_log, barc.richness, fungi.richness,
         chemoheterotrophy_log, n.cycler_log, saprotroph) 

# Center and scale continuous variables
sem.dat <- sem.dat.unscaled |> 
  mutate(rocks = as.character(rocks),
         Sample = as.character(Sample)) |> 
  mutate_if(is.numeric, scale) |> 
  mutate(rocks = as.numeric(rocks))


# Latent variables only ---------------------------------------------------

# Model latent variable separately - soil microbe
lvmod.soimic <- '
  # latent variable model
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
'
fit.soimic <- sem(lvmod.soimic, data = sem.dat)
summary(fit.soimic, fit.measures = TRUE, standardized = TRUE)
# Latent variable model diagnostics (soil microbiome):
#   chi-sq p-value: 0.743 (good, >0.05)
#   ratio of test statistic to df: 2.723/5 = 0.5446 (good, <2)
#   CFI: 1.000 (good, >0.95)
#   RMSEA: 0.000 (okay, <0.1)
#   RMSEA lower CI: 0.000 (good)
#   SRMR: 0.044 (good, <0.1)


# Model latent variable separately - soil chemistry
lvmod.soichem <- '
  # latent variable model
  soil_chem =~ TN_log + OM_log
'
fit.soichem <- sem(lvmod.soichem, data = sem.dat) # lavaan WARNING: Could not compute standard errors!
summary(fit.soichem)
#   something is wrong with soil chem as latent variable



# 1 Soil mic as latent, OM; Veg18 & tree included -------------------------

# Full model, initial attempt
mod1 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # structure
  notree ~ rocks + notree.18 + tree 
  OM_log ~ rocks
  soil_microbe ~ rocks
  notree.18 ~ rocks
  
  # covariance
  OM_log ~~ soil_microbe
  OM_log ~~ notree
  soil_microbe ~~ notree
'
fit1 <- sem(mod1, data = sem.dat) 
summary(fit1, fit.measures = TRUE, standardized = TRUE)
# Mod1 diagnostics:
# Global fit:
#   chi-sq statistic: 26.242
#   chi-sq stat to df ratio: 0.87 (good; <2)
#   chi-sq p-value: 0.663 (good, >0.05)
#   CFI: 1.000 (good, >0.95)
#   RMSEA: 0.000 (good, <0.1)
#   RMSEA lower CI: 0.000 (good)
#   SRMR: 0.076 (good, <0.1)
#   Akaike (AIC): 1345.448

modindices(fit1, sort = TRUE, minimum.value = 3.5)



# 2 Soil mic latent, TN; Veg18 & tree included ----------------------------

mod2 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # structure
  notree ~ rocks + notree.18 + tree 
  TN_log ~ rocks
  soil_microbe ~ rocks
  notree.18 ~ rocks
  
  # covariance
  TN_log ~~ soil_microbe
  TN_log ~~ notree
  soil_microbe ~~ notree
'
fit2 <- sem(mod2, data = sem.dat) 
summary(fit2, fit.measures = TRUE, standardized = TRUE)
# Mod2 diagnostics:
# Global fit:
#   chi-sq statistic: 31.847
#   chi-sq stat to df ratio: 1.06 (good; <2)
#   chi-sq p-value: 0.375 (good, >0.05)
#   CFI: 0.985 (good, >0.95)
#   RMSEA: 0.032 (good, <0.1)
#   RMSEA lower CI: 0.000 (good)
#   SRMR: 0.079 (good, <0.1)
#   AIC: 1310.762

modindices(fit2, sort = TRUE, minimum.value = 3.5)


# 3 Herb, OM, SoMic, herb18, tree -----------------------------------------

# Full model, initial attempt
mod3 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # structure
  herb ~ rocks + herb.18 + tree 
  OM_log ~ rocks
  soil_microbe ~ rocks
  herb.18 ~ rocks
  
  # covariance
  OM_log ~~ soil_microbe
  OM_log ~~ herb
  soil_microbe ~~ herb
'
fit3 <- sem(mod3, data = sem.dat) 
summary(fit3, fit.measures = TRUE, standardized = TRUE)
# Mod3 diagnostics:
# Global fit:
#   chi-sq statistic: 21.703
#   chi-sq stat to df ratio: 0.72 (good; <2)
#   chi-sq p-value: 0.865 (good, >0.05)
#   CFI: 1.000 (good, >0.95)
#   RMSEA: 0.000 (good, <0.1)
#   RMSEA lower CI: 0.000 (good)
#   SRMR: 0.069 (good, <0.1)
#   Akaike (AIC): 1351.278

modindices(fit3, sort = TRUE, minimum.value = 3.5)


# 4 Herb, OM, SoMic, herb18, tree -----------------------------------------

# Full model, initial attempt
mod4 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # structure
  herb ~ rocks + herb.18 + tree 
  TN_log ~ rocks
  soil_microbe ~ rocks
  herb.18 ~ rocks
  
  # covariance
  TN_log ~~ soil_microbe
  TN_log ~~ herb
  soil_microbe ~~ herb
'
fit4 <- sem(mod4, data = sem.dat) 
summary(fit4, fit.measures = TRUE, standardized = TRUE)
# Mod4 diagnostics:
# Global fit:
#   chi-sq statistic: 25.702
#   chi-sq stat to df ratio: 0.86 (good; <2)
#   chi-sq p-value: 0.690 (good, >0.05)
#   CFI: 1.000 (good, >0.95)
#   RMSEA: 0.000 (good, <0.1)
#   RMSEA lower CI: 0.000 (good)
#   SRMR: 0.066 (good, <0.1)
#   Akaike (AIC): 1320.884

modindices(fit4, sort = TRUE, minimum.value = 3.5)


save.image("RData/Publish_SEM.RData")
