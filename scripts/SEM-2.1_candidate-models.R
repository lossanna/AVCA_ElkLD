# Purpose: Test candidate models for SEM analysis.
#   SEM 2.1 incorporates better assessment of model fit, according to procedure from Grace 2020 "WOE",
#     and there are meta-models drawn for each in the PowerPoint (Writing/SEM metamodel and path diagrams.pptx).
#     SEM 2.1 also graphs the path diagrams in PPT because semPaths is not flexible enough.

# Findings:
#   Soil chem cannot be a latent variable because TN & OM are also too collinear.
#   Removing tree cover does not improve model fit (and removing non-significant paths is not good practice).
#   OM_log performs better than TN_log, but both are okay models (good model fit).
#   TC_log does not produce good model fit.

# Created: 2023-08-17
# Updated: 2023-08-24


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
  select(Sample, rocks, wide, notree, notree.18, herb, herb.18, tree, perveg.richness, perveg.shannon,
         TN_log, TC_log, CN_ratio, OM_log, barc.richness, fungi.richness,
         chemoheterotrophy_log, n.cycler_log, saprotroph) 

# Center and scale variables
sem.dat <- sem.dat.unscaled |> 
  mutate(rocks = as.character(rocks),
         Sample = as.character(Sample),
         wide = as.character(wide)) |> 
  mutate_if(is.numeric, scale) |> 
  mutate(rocks = as.numeric(rocks),
         wide = as.numeric(wide))


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
modindices(fit.soimic, sort = TRUE)
#   No paths need to be added based on MI


# Model latent variable separately - soil chemistry
lvmod.soichem <- '
  # latent variable model
  soil_chem =~ TN_log + OM_log
'
fit.soichem <- sem(lvmod.soichem, data = sem.dat) # lavaan WARNING: Could not compute standard errors!
summary(fit.soichem)
#   something is wrong with soil chem as latent variable


# Model latent variable separately - plants
lvmod.plants <- '
  # latent variable model
  plants =~ notree + perveg.richness + perveg.shannon
'
fit.plants <- sem(lvmod.plants, data = sem.dat)
summary(fit.plants, fit.measures = TRUE, standardized = TRUE)
# for some reason there are 0 degrees of freedom? idk what's going on
# this might be because there are not enough indicator variables



# 1 Soil mic & soil chem as latent; Veg18 & tree included -----------------

# Full model, initial attempt
#   (this initial attempt is kind of moot based on the soil chem latent variable turning out so badly)
mod1.0 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ TN_log + OM_log
  
  # regressions
  notree ~ rocks + notree.18 + tree 
  soil_chem ~ rocks
  soil_microbe ~ rocks
  notree.18 ~ rocks
  
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

# Model latent variable separately - soil microbe (this is the same as Section 1)
lvmod.soimic <- '
  # latent variable model
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
'
fit.soimic <- sem(lvmod.soimic, data = sem.dat)
summary(fit.soimic)
# Latent variable model diagnostics:
#   chi-sq p-value: 0.743 (good, >0.05)
#   ratio of test statistic to df: 2.723/5 = 0.5446 (good, <2)
modindices(fit.soimic, sort = TRUE)
#   No paths need to be added based on MI


# Full model, initial attempt
mod2.0 <- '
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
fit2.0 <- sem(mod2.0, data = sem.dat) 
summary(fit2.0, fit.measures = TRUE, standardized = TRUE)
# Mod2.0 diagnostics:
# Global fit:
#   chi-sq statistic: 31.847
#   chi-sq stat to df ratio: 1.06 (good; <2)
#   chi-sq p-value: 0.375 (good, >0.05)
#   CFI: 0.985 (good, >0.95)
#   RMSEA: 0.032 (good, <0.1)
#   RMSEA lower CI: 0.000 (good)
#   SRMR: 0.079 (good, <0.1)
#   AIC: 1310.762
modindices(fit2.0, sort = TRUE)
#  Adding covariance with TN_log: there could be an unmeasured factor that affects these together
#   (i.e. it seems scientifically plausible), but the pathways do not end up significant, and
#   it seems weird to randomly allow those things to covary when they are part of the larger soil microbiome,
#   so I am going to stick with this one

semPaths(fit2.0, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7,
         nCharNodes = 6, node.width = 1.3, layout = "tree2", reorder = FALSE)


# Add covariances based on MI (modification indices)
#   (am leaving this here just to have, but Kline 2016 says not to add paths based on MI; Grace 2020 says to do so)
mod2.1 <- '
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
  notree.18 ~ rocks
  
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




# 3 Soil mic as latent, OM; Veg18 & tree included -------------------------

# Full model, initial attempt
mod3.0 <- '
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
fit3.0 <- sem(mod3.0, data = sem.dat) 
summary(fit3.0, fit.measures = TRUE, standardized = TRUE)
# Mod3.0 diagnostics:
# Global fit:
#   chi-sq statistic: 26.242
#   chi-sq stat to df ratio: 0.87 (good; <2)
#   chi-sq p-value: 0.663 (good, >0.05)
#   CFI: 1.000 (good, >0.95)
#   RMSEA: 0.000 (good, <0.1)
#   RMSEA lower CI: 0.000 (good)
#   SRMR: 0.076 (good, <0.1)
#   Akaike (AIC): 1345.448
modindices(fit3.0, sort = TRUE)
#  Adding covariance saprotrophs~~OM_log: could be scientifically plausible to have more decomposers
#   with more OM, but I am just going to leave the path between OM & soil_microbe as enough

semPaths(fit3.0, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7,
         nCharNodes = 6, node.width = 1.3, layout = "tree2", reorder = FALSE)




# 4 Soil mic as latent, TC; Veg18 & tree included -------------------------

# Full model, initial attempt
mod4.0 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # structure
  notree ~ rocks + notree.18 + tree 
  TC_log ~ rocks
  soil_microbe ~ rocks
  notree.18 ~ rocks
  
  # covariance
  TC_log ~~ soil_microbe
  TC_log ~~ notree
  soil_microbe ~~ notree
'
fit4.0 <- sem(mod4.0, data = sem.dat) 
summary(fit4.0, fit.measures = TRUE, standardized = TRUE)
# Mod4.0 diagnostics:
# Global fit:
#   chi-sq statistic: 44.782
#   chi-sq stat to df ratio: 1.49 (good; <2)
#   chi-sq p-value: 0.040 (not good, <0.05)
#   CFI: 0.885 (not good, <0.95)
#   RMSEA: 0.089 (okay, <0.1)
#   RMSEA lower CI: 0.019 (not great - should be 0)
#   SRMR: 0.082 (okay, <0.1)
#   Akaike (AIC): 1315.527
modindices(fit4.0, sort = TRUE)
#  There are a lot of paths that could be added because this isn't a great model to start with,
#   but I will just use the OM or TN model instead, so no need to test more models.

semPaths(fit3.0, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7,
         nCharNodes = 6, node.width = 1.3, layout = "tree2", reorder = FALSE)



# Compare 2 & 3 (TN vs OM) ------------------------------------------------

summary(fit2.0, fit.measures = TRUE, standardized = TRUE)
# Mod2.0 diagnostics:
# Global fit:
#   chi-sq statistic: 31.847
#   chi-sq stat to df ratio: 1.06 (good; <2)
#   chi-sq p-value: 0.375 (good, >0.05)
#   CFI: 0.985 (good, >0.95)
#   RMSEA: 0.032 (good, <0.1)
#   RMSEA lower CI: 0.000 (good)
#   SRMR: 0.079 (good, <0.1)
#   AIC: 1310.762

summary(fit3.0, fit.measures = TRUE, standardized = TRUE)
# Mod3.0 diagnostics:
# Global fit:
#   chi-sq statistic: 26.242
#   chi-sq stat to df ratio: 0.87 (good; <2)
#   chi-sq p-value: 0.663 (good, >0.05)
#   CFI: 1.000 (good, >0.95)
#   RMSEA: 0.000 (good, <0.1)
#   RMSEA lower CI: 0.000 (good)
#   SRMR: 0.076 (good, <0.1)
#   Akaike (AIC): 1345.448



# 5 Soil mic latent, TN; Veg18 included -----------------------------------

# Overall, there really is no good justification for removing tree because you shouldn't just drop
#   non-significant paths purely because they are not significant (Goodboy & Kline 2017),
#   and this shows that removing tree makes model fit worse, anyway.

# Remove tree
mod5.0 <- '
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
fit5.0 <- sem(mod5.0, data = sem.dat)
summary(fit5.0, standardized = TRUE, fit.measures = TRUE)
# Mod5.0 diagnostics:
# Global fit:
#   chi-sq p-value is good (>0.05)
#   CFI is okay (>0.95)
#   RMSEA is okay (<0.1, lower CI is 0)
#   SRMR is okay (<0.1)
# But removing tree does not actually improve global fit (it actually gets worse)

semPaths(fit5.0, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 6,
         nCharNodes = 6, node.width = 1.3, layout = "tree2")



# 6 Soil mic as latent, OM; Veg18 & tree included; wide (no rocks) --------

# Full model, initial attempt
#   Groups Channels 13 & 12 (narrow), and Channels 19 & 21 (wide)
mod6.0 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # structure
  notree ~ wide + notree.18 + tree 
  OM_log ~ wide
  soil_microbe ~ wide
  notree.18 ~ wide
  
  # covariance
  OM_log ~~ soil_microbe
  OM_log ~~ notree
  soil_microbe ~~ notree
'
fit6.0 <- sem(mod6.0, data = sem.dat) 
summary(fit6.0, fit.measures = TRUE, standardized = TRUE)
# Mod6.0 diagnostics:
# Global fit:
#   chi-sq statistic: 57.561
#   chi-sq stat to df ratio: 1.9187 (okay; <2)
#   chi-sq p-value: 0.002 (bad, <0.05)
#   CFI: 0.776 (bad, <0.95)
#   RMSEA: 0.122 (bad, >0.1)
#   RMSEA lower CI: 0.073 (bad)
#   SRMR: 0.108 (bad, >0.1)
# I mean at least the width of the channel makes a worse fit than the actual treatment?
#   This was just messing around to see what would happen, this analysis won't be included



# 7 Herb, OM, SoMic, herb18, tree -----------------------------------------

# Full model, initial attempt
mod7.0 <- '
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
fit7.0 <- sem(mod7.0, data = sem.dat) 
summary(fit7.0, fit.measures = TRUE, standardized = TRUE)
# Mod7.0 diagnostics:
# Global fit:
#   chi-sq statistic: 21.703
#   chi-sq stat to df ratio: 0.72 (good; <2)
#   chi-sq p-value: 0.865 (good, >0.05)
#   CFI: 1.000 (good, >0.95)
#   RMSEA: 0.000 (good, <0.1)
#   RMSEA lower CI: 0.000 (good)
#   SRMR: 0.069 (good, <0.1)
#   Akaike (AIC): 1351.278
modindices(fit7.0, sort = TRUE)
#   Again is suggesting saprotrophs & OM, but I already have the OM-SoMic relationship
#     So at this point I don't really add anything or remove anything, but I have the
#     code here anyway for fun, I guess

semPaths(fit7.0, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7,
         nCharNodes = 6, node.width = 1.3, layout = "tree2", reorder = FALSE)



# 8 Herb, OM, SoMic, herb18, tree -----------------------------------------

# Full model, initial attempt
mod8.0 <- '
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
fit8.0 <- sem(mod8.0, data = sem.dat) 
summary(fit8.0, fit.measures = TRUE, standardized = TRUE)
# Mod8.0 diagnostics:
# Global fit:
#   chi-sq statistic: 25.702
#   chi-sq stat to df ratio: 0.86 (good; <2)
#   chi-sq p-value: 0.690 (good, >0.05)
#   CFI: 1.000 (good, >0.95)
#   RMSEA: 0.000 (good, <0.1)
#   RMSEA lower CI: 0.000 (good)
#   SRMR: 0.066 (good, <0.1)
#   Akaike (AIC): 1320.884
modindices(fit8.0, sort = TRUE)
#   Suggests fungi.rich ~~ TN_log, barc.rich ~~ n.cycler, chemohet ~~ n.cycler 

semPaths(fit8.0, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7,
         nCharNodes = 6, node.width = 1.3, layout = "tree2", reorder = FALSE)



save.image("RData/SEM-2.1_candidate-models.RData")
