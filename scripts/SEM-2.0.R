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
         TN_log, OM_log, OM_perc, barc.richness, fungi.richness,
         chemoheterotrophy_log, n.cycler_log, saprotroph) 

# Center and scale variables
sem.dat <- sem.dat.unscaled |> 
  mutate(rocks = as.factor(rocks),
         Sample = as.factor(Sample)) |> 
  mutate_if(is.numeric, scale) |> 
  mutate(rocks = as.numeric(rocks))


# SEM, plants as latent variable ------------------------------------------

# Initial attempt
mod1.0 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ TN_log + OM_log
  plants =~ notree + perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + soil_chem 
  soil_chem ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit1.0 <- sem(mod1.0, data = sem.dat) # lavaan WARNING: some estimated ov variances are negative
summary(fit1.0) 
#   possibly this is because some variables are collinear


# Remove TN 
#   solves problem, but model loses important information
mod1.1 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ OM_log
  plants =~ notree + perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + soil_chem 
  soil_chem ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit1.1 <- sem(mod1.1, data = sem.dat)
summary(fit1.1) # removing TN solves negative variance warning


# Change OM_log to OM_perc
#   does not solve problem
mod1.2 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ OM_perc + TN_log
  plants =~ notree + perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + soil_chem 
  soil_chem ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit1.2 <- sem(mod1.2, data = sem.dat) # lavaan WARNING: some estimated ov variances are negative
summary(fit1.2) # using OM_perc doesn't solve variance issue


# Remove OM_log
#   solves problem
mod1.3 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  soil_chem =~ TN_log
  plants =~ notree + perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + soil_chem 
  soil_chem ~ rocks + soil_microbe
  soil_microbe ~ rocks
'
fit1.3 <- sem(mod1.3, data = sem.dat)
summary(fit1.3) # removing OM_log solves negative variance issue


# Break include OM & TN as separate indicator variables, no latent
#   does not solve problem
mod1.4 <- '
  # latent variables
  soil_microbe =~ barc.richness + fungi.richness + chemoheterotrophy_log + n.cycler_log + saprotroph
  plants =~ notree + perveg.richness + perveg.shannon
  
  # structure
  plants ~ rocks + TN_log + OM_log
  TN_log ~ soil_microbe + rocks
  OM_log ~ soil_microbe + rocks
  soil_microbe ~ rocks
'
fit1.4 <- sem(mod1.4, data = sem.dat) # lavaan WARNING: some estimated ov variances are negative
summary(fit1.4) # removing soil_chem as latent variable does not solve variance issue


# Allow OM & TN to covary
mod1.5 <- '
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
fit1.5 <- sem(mod1.5, data = sem.dat) # lavaan WARNING: some estimated ov variances are negative
summary(fit1.5) 
