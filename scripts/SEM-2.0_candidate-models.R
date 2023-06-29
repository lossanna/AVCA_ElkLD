# Purpose: SEM analysis
# Created: 2023-06-27
# Updated: 2023-06-27

library(lavaan)
library(tidyverse)

# Load data ---------------------------------------------------------------

dat.2021 <- read.csv("data/cleaned/Data-2021_clean.csv") 


# Data wrangling ----------------------------------------------------------

# Add Control/Treated as binary variable and select only variables needed for SEM
sem.dat.unscaled <- dat.2021 |> 
  mutate(rocks = case_when(
    Treatment3 == "Control" ~ 0,
    Treatment3 == "Treated" ~ 1)) |> 
  select(Sample, rocks, notree, perveg.richness, perveg.shannon,
         TN_log, CN_ratio, OM_log, OM_perc, barc.richness, fungi.richness,
         chemoheterotrophy_log, n.cycler_log, saprotroph) 

# Center and scale variables
sem.dat <- sem.dat.unscaled |> 
  mutate(rocks = as.factor(rocks),
         Sample = as.factor(Sample)) |> 
  mutate_if(is.numeric, scale) |> 
  mutate(rocks = as.numeric(rocks))


# Plants as latent variable (cover & div) ---------------------------------

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



# Composite indicator variables -------------------------------------------

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



# Cover as single endogenous ----------------------------------------------

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



# Plant diversity as latent endogenous ------------------------------------

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





save.image("RData/SEM-2.0_candidate-models.RData")

