# Purpose: Test candidate models for SEM analysis

# Findings:
#   OM & TN covary too much; have to pick just one.
#   CN ratio not good indicator (low chi-sq p-value).
#   Plant diversity and cover cannot be a single endogenous latent variable - creates
#     low chi-sq p-value.
#   Best model is probably 5.1

# Created: 2023-06-27
# Updated: 2023-07-25

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


# 1 Plants as latent variable (cover & div) -------------------------------

# Initial attempt
mod01.0 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ TN_log + OM_log
  plants =~ notree + perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + soil_chem 
  soil_chem ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit01.0 <- sem(mod01.0, data = sem.dat) # lavaan WARNING: some estimated ov variances are negative
summary(fit01.0) 
#   possibly this is because some variables are collinear


# Remove TN 
#   solves problem, but model loses important information
mod01.1 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ OM_log
  plants =~ notree + perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + soil_chem 
  soil_chem ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit01.1 <- sem(mod01.1, data = sem.dat)
summary(fit01.1) # removing TN solves negative variance warning
#   does not have good chi-sq p-value


# Change OM_log to OM_perc
#   does not solve problem
mod01.2 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ OM_perc + TN_log
  plants =~ notree + perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + soil_chem 
  soil_chem ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit01.2 <- sem(mod01.2, data = sem.dat) # lavaan WARNING: some estimated ov variances are negative
summary(fit01.2) # using OM_perc doesn't solve variance issue


# Remove OM_log
#   solves problem
mod01.3 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ TN_log
  plants =~ notree + perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + soil_chem 
  soil_chem ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit01.3 <- sem(mod01.3, data = sem.dat)
summary(fit01.3) # removing OM_log solves negative variance issue


# Break include OM & TN as separate indicator variables, no latent
#   does not solve problem
mod01.4 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  plants =~ notree + perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + TN_log + OM_log
  TN_log ~ soil_microbe + rocks
  OM_log ~ soil_microbe + rocks
  soil_microbe ~ rocks
'
fit01.4 <- sem(mod01.4, data = sem.dat) # lavaan WARNING: some estimated ov variances are negative
summary(fit01.4) # removing soil_chem as latent variable does not solve variance issue


# Allow OM & TN to covary
mod01.5 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ TN_log + OM_log
  plants =~ notree + perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + soil_chem 
  soil_chem ~ rocks + soil_microbe
  soil_microbe ~ rocks
  
  # covariance
  TN_log ~~ OM_log
'
fit01.5 <- sem(mod01.5, data = sem.dat) # lavaan WARNING: some estimated ov variances are negative
summary(fit01.5) 


# ***TN as single observed, soil microbe as direct causal, TN & soil microbe covary***
#   no neg variance issue but has bad chi-sq p-value
mod01.6 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  plants =~ notree + perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + TN_log + soil_microbe
  TN_log ~ rocks
  soil_microbe ~ rocks
  
  # covariance
  soil_microbe ~~ TN_log
'
fit01.6 <- sem(mod01.6, data = sem.dat)
summary(fit01.6) # poor chi-sq p-value




# 2 Composite indicator variables -----------------------------------------

# Initial attempt
mod02.0 <- '
  # composite variables
  soil_microbe <~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem <~ TN_log + OM_log
  plants =~ notree + perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + soil_chem 
  soil_chem ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit02.0 <- sem(mod02.0, data = sem.dat) # lavaan WARNING: Could not compute standard errors! 
#                                           The information matrix could not be inverted. 
#                                       This may be a symptom that the model is not identified.



# 3 Cover as single endogenous --------------------------------------------

# Initial attempt
mod03.0 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ TN_log + OM_log
  
  # structure
  notree ~ rocks + soil_chem 
  soil_chem ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit03.0 <- sem(mod03.0, data = sem.dat) # lavaan WARNING: some estimated ov variances are negative
summary(fit03.0) 


# Allow TN & OM to covary 
mod03.1 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ TN_log + OM_log
  
  # structure
  notree ~ rocks + soil_chem 
  soil_chem ~ rocks + soil_microbe
  soil_microbe ~ rocks
  
  # covariance
  TN_log ~~ OM_log
'
fit03.1 <- sem(mod03.1, data = sem.dat) # lavaan WARNING: some estimated ov variances are negative
summary(fit03.1) 


# ***TN as single observed (no soil chem)***
#   no neg variance issue and has good chi-sq p-value
mod03.2 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # structure
  notree ~ rocks + TN_log 
  TN_log ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit03.2 <- sem(mod03.2, data = sem.dat)
summary(fit03.2) # good chi-sq p-value


# Soil chem as latent variable with only TN
#   although not sure how this differs a lot from 3.2
mod03.3 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ TN_log
  
  # structure
  notree ~ rocks + soil_chem 
  soil_chem ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit03.3 <- sem(mod03.3, data = sem.dat)
summary(fit03.3) # good chi-sq p-value


# CN ratio instead of TN or OM
#   probably should use TN_log instead
mod03.4 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # structure
  notree ~ rocks + CN_ratio 
  CN_ratio ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit03.4<- sem(mod03.4, data = sem.dat)
summary(fit03.4) # not great chi-sq p-value


# ***TN as single observed & soil microbe as direct causal***
#   no neg variance issue and has good chi-sq p-value
mod03.5 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # structure
  notree ~ rocks + TN_log + soil_microbe
  TN_log ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit03.5 <- sem(mod03.5, data = sem.dat)
summary(fit03.5) # good chi-sq p-value


# ***TN as single observed, soil microbe as direct causal, TN & soil microbe covary***
#   no neg variance issue and has good chi-sq p-value
mod03.6 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # structure
  notree ~ rocks + TN_log + soil_microbe
  TN_log ~ rocks
  soil_microbe ~ rocks
  
  # covariance
  soil_microbe ~~ TN_log
'
fit03.6 <- sem(mod03.6, data = sem.dat)
summary(fit03.6) # good chi-sq p-value

ly <- matrix(c(0.5, 0,
               0, 2.5,
               0, -2.5,
               -0.4, -0.3,
               -0.35, -0.3,
               -0.3, -0.3,
               -0.25, -0.3,
               -0.2, -0.3,
               -0.5, 0),
             byrow = FALSE,
             ncol = 2)
semPaths(fit03.6, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7,
         nCharNodes = 6, node.width = 1.3, layout = "circle2")



# 4 Plant diversity as latent endogenous ----------------------------------

# Initial attempt
mod04.0 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ TN_log + OM_log
  plants =~ perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + soil_chem 
  soil_chem ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit04.0 <- sem(mod04.0, data = sem.dat) # lavaan WARNING: some estimated ov variances are negative
summary(fit04.0)


# TN only
mod04.1 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  plants =~ perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + TN_log 
  TN_log ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit04.1 <- sem(mod04.1, data = sem.dat) # lavaan WARNING: some estimated ov variances are negative
summary(fit04.1) # richness & Shannon might covary


# Richness & Shannon allowed to covary
#   this did not work
mod04.2 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  plants =~ perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + TN_log 
  TN_log ~ rocks + soil_microbe
  soil_microbe ~ rocks
  
  # covariance
  perveg.richness ~~ perveg.shannon
'
fit04.2 <- sem(mod04.2, data = sem.dat) # lavaan WARNING: some estimated ov variances are negative
#   lavaan WARNING:
#     Could not compute standard errors! The information matrix could
#     not be inverted. This may be a symptom that the model is not
#     identified.
summary(fit04.2) # richness & Shannon might covary



# 5 With 2018 cover and initial tree cover --------------------------------

# Initial attempt
mod05.0 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # regressions
  notree ~ rocks + notree.18 + tree 
  TN_log ~ rocks
  soil_microbe ~ rocks
  notree.18 ~ rocks + tree
  
  # covariance
  TN_log ~~ soil_microbe
  TN_log ~~ notree
  soil_microbe ~~ notree
'
fit05.0 <- sem(mod05.0, data = sem.dat)
summary(fit05.0)

semPaths(fit05.0, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7,
         nCharNodes = 6, node.width = 1.3, layout = "tree2")


# Without tree cover
mod05.1 <- '
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
fit05.1 <- sem(mod05.1, data = sem.dat)
summary(fit05.1)

semPaths(fit05.1, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 6,
         nCharNodes = 6, node.width = 1.3, layout = "tree2")


labels <- c("BAR", "FR", "Chet", "Ncyc", "Sap", "Veg", "TN",
            "Veg18", "Rocks", "SoMic")
tiff("figures/2023-07_draft-figures/SEM.tiff", width = 7, height = 5, units = "in", res = 150)
semPaths(fit05.1, "std", edge.label.cex = 0.9, residuals = FALSE, sizeMan = 6,
         nCharNodes = 0, nodeLabels = labels, node.width = 1.1, layout = "tree2")
dev.off()



# 6 Width as binary variable ----------------------------------------------

# Width & rocks as latent variable (does not work at all)
mod06.0 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  channel =~ rocks + wide
  
  # regressions
  notree ~ channel + notree.18 
  notree.18 ~ channel
  soil_microbe ~ channel
  TN_log ~ channel
  
  # covariance
  soil_microbe ~~ TN_log
  soil_microbe ~~ notree
  TN_log ~~ notree
'
fit06.0 <- sem(mod06.0, data = sem.dat)
summary(fit06.0) # does not work at all


# Wide as separate exogenous
mod06.1 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # regressions
  notree ~ rocks + wide + notree.18 
  notree.18 ~ rocks + wide
  soil_microbe ~ rocks + wide
  TN_log ~ rocks + wide
  
  # covariance
  soil_microbe ~~ TN_log
  soil_microbe ~~ notree
  TN_log ~~ notree
'
fit06.1 <- sem(mod06.1, data = sem.dat)
summary(fit06.1) # poor chi-sq p-value


# 7 Without rocks, only width ---------------------------------------------

# rocks replaced with wide
mod07.0 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # regressions
  notree ~ wide + notree.18 
  notree.18 ~ wide
  soil_microbe ~ wide
  TN_log ~ wide
  
  # covariance
  soil_microbe ~~ TN_log
  soil_microbe ~~ notree
  TN_log ~~ notree
'
fit07.0 <- sem(mod07.0, data = sem.dat)
summary(fit07.0) # poor chi-sq p-value


# 8 Herb cover ------------------------------------------------------------

# notree replaced with herb, tree cover added
mod08.0 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # regressions
  herb ~ rocks + herb.18 + tree 
  TN_log ~ rocks
  soil_microbe ~ rocks
  herb.18 ~ rocks + tree
  
  # covariance
  TN_log ~~ soil_microbe
  TN_log ~~ herb
  soil_microbe ~~ herb
'
fit08.0 <- sem(mod08.0, data = sem.dat)
summary(fit08.0)

semPaths(fit08.0, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7,
         nCharNodes = 6, node.width = 1.3, layout = "tree2")


# notree replaced with herb, tree cover removed
mod08.1 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  
  # regressions
  herb ~ rocks + herb.18 
  herb.18 ~ rocks
  soil_microbe ~ rocks
  TN_log ~ rocks
  
  # covariance
  soil_microbe ~~ TN_log
  soil_microbe ~~ herb
  TN_log ~~ herb
'
fit08.1 <- sem(mod08.1, data = sem.dat)
summary(fit08.1)

semPaths(fit08.1, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 6,
         nCharNodes = 6, node.width = 1.3, layout = "tree2")

save.image("RData/SEM-2.0_candidate-models.RData")

