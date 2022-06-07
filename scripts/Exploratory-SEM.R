library(tidyverse)
library(lavaan)
library(semPlot)

# Load data ---------------------------------------------------------------

dat.2021 <- read.csv("data/cleaned/SEM-input.csv")
dat.2021$inch.trt <- as.factor(dat.2021$inch.trt)
dat.2021$up.trt <- as.factor(dat.2021$up.trt)


# Models of the same number have the same variables (except main response variable)


# Total cover -------------------------------------------------------------

# Model 1 (2021 cover)
mod.total01 <- '
  # regressions
    Cover ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ up.trt + inch.trt
'
fit.total01 <- sem(mod.total01, data = dat.2021, fixed.x = TRUE) 
  # Warning message: In lav_data_full(data = data, group = group, cluster = cluster,  :
      # lavaan WARNING: some observed variances are (at least) a factor 1000 times larger 
      # than others; use varTable(fit) to investigate
  # Discussed further here: https://groups.google.com/g/lavaan/c/FzShzsXYNco?pli=1
vartable(fit.total01)
summary(fit.total01, standardized = TRUE)

semPaths(fit.total01, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 2 (difference)
mod.total02 <- '
  # regressions
    total.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt + up.trt
'
fit.total02 <- sem(mod.total02, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.total02)
summary(fit.total02, standardized = TRUE)

semPaths(fit.total02, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 3 (cover 2021)
  # in-channel treatment effect only
mod.total03 <- '
  # regressions
    Cover ~ Width + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt
'
fit.total03 <- sem(mod.total03, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.total03)
summary(fit.total03, standardized = TRUE)

semPaths(fit.total03, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 4 (difference)
  # in-channel treatment effect only
mod.total04 <- '
  # regressions
    total.diff ~ Width + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt
'
fit.total04 <- sem(mod.total04, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.total04)
summary(fit.total04, standardized = TRUE)

semPaths(fit.total04, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 5 (cover 2021)
  # maximum variables
mod.total05 <- '
  # regressions
    Cover ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.total05 <- sem(mod.total05, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.total05)
summary(fit.total05, standardized = TRUE)

semPaths(fit.total05, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 6 (difference 2021)
  # maximum variables
mod.total06 <- '
  # regressions
    total.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.total06 <- sem(mod.total06, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.total06)
summary(fit.total06, standardized = TRUE)

semPaths(fit.total06, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 7 (cover 2021)
  # remove elevation to increase sample:variable ratio because it is not significant 
mod.total07 <- '
  # regressions
    Cover ~ Width + up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.total07 <- sem(mod.total07, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.total07)
summary(fit.total07, standardized = TRUE)

semPaths(fit.total07, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)

# Model 8 (difference 2021)
  # remove elevation to increase sample:variable ratio because it is not significant 
mod.total08 <- '
  # regressions
    total.diff ~ Width + up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.total08 <- sem(mod.total08, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.total08)
summary(fit.total08, standardized = TRUE)

semPaths(fit.total08, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 9 (cover 2021)
  # no GIS-derived models
mod.total09 <- '
  # regressions
    Cover ~ up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.total09 <- sem(mod.total09, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.total09)
summary(fit.total09, standardized = TRUE)

semPaths(fit.total09, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 10 (difference 2021)
  # no GIS-derived models
mod.total10 <- '
  # regressions
    total.diff ~ up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.total10 <- sem(mod.total10, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.total10)
summary(fit.total10, standardized = TRUE)

semPaths(fit.total10, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)



# Herbaceous --------------------------------------------------------------

# Model 1 (cover 2021)
mod.herb1 <- '
  # regressions
    Herbaceous ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ up.trt + inch.trt
'
fit.herb1 <- sem(mod.herb1, data = dat.2021, fixed.x = TRUE)
  # Warning message: In lav_data_full(data = data, group = group, cluster = cluster,  :
      # lavaan WARNING: some observed variances are (at least) a factor 1000 times larger 
      # than others; use varTable(fit) to investigate
varTable(fit.herb1)
summary(fit.herb1, standardized = TRUE)

semPaths(fit.herb1, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 2 (difference)
mod.herb2 <- '
  # regressions
    herb.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ up.trt + inch.trt
'
fit.herb2 <- sem(mod.herb2, data = dat.2021, fixed.x = TRUE)
    # Warning message: In lav_data_full(data = data, group = group, cluster = cluster,  :
      # lavaan WARNING: some observed variances are (at least) a factor 1000 times larger 
      # than others; use varTable(fit) to investigate
varTable(fit.herb2)
summary(fit.herb2, standardized = TRUE)

semPaths(fit.herb2, "std", edge.label.cex = 0.5, residuals = FALSE)


# Model 3 (difference)
mod.herb3 <- '
  # regressions
    herb.diff ~ Width + up.trt + inch.trt + Elev_Diff + TN_log
    TN_log ~ up.trt + inch.trt
'
fit.herb3 <- sem(mod.herb3, data = dat.2021, fixed.x = TRUE) # got warning about variance
varTable(fit.herb3)
summary(fit.herb3, standardized = TRUE)

semPaths(fit.herb3, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 4 (cover 2021)
mod.herb4 <- '
  # regressions
    Herbaceous ~ Width + up.trt + inch.trt + Elev_Diff + TN_log
    TN_log ~ up.trt + inch.trt
'
fit.herb4 <- sem(mod.herb4, data = dat.2021, fixed.x = TRUE) # got warning about variance
varTable(fit.herb4)
summary(fit.herb4, standardized = TRUE)

semPaths(fit.herb4, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 5 (difference 2021)
mod.herb5 <- '
  # regressions
    herb.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.herb5 <- sem(mod.herb5, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.herb5)
summary(fit.herb5, standardized = TRUE)

semPaths(fit.herb5, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)



# Woody -------------------------------------------------------------------

# Model 1 (cover 2021)
mod.wood1 <- '
  # regressions
    Woody ~ Width + up.trt + inch.trt + Elev_Diff + TN_log
    TN_log ~ up.trt + inch.trt
'
fit.wood1 <- sem(mod.wood1, data = dat.2021, fixed.x = TRUE) # got warning about variance
varTable(fit.wood1)
summary(fit.wood1, standardized = TRUE)

semPaths(fit.wood1, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 2 (difference 2021)
mod.wood2 <- '
  # regressions
    wood.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.wood2 <- sem(mod.wood2, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.wood2)
summary(fit.wood2, standardized = TRUE)

semPaths(fit.wood2, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)



# Richness ----------------------------------------------------------------

# Model 1 (richness 2021)
mod.rich1 <- '
  # regressions
    rich ~ Width + up.trt + inch.trt + Elev_Diff + TN_log
    TN_log ~ up.trt + inch.trt
'
fit.rich1 <- sem(mod.rich1, data = dat.2021, fixed.x = TRUE) 
varTable(fit.rich1)
summary(fit.rich1, standardized = TRUE)

semPaths(fit.rich1, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 2 (difference 2021)
mod.rich2 <- '
  # regressions
    rich.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.rich2 <- sem(mod.rich2, data = dat.2021, fixed.x = TRUE)
vartable(fit.rich2)
summary(fit.rich2, standardized = TRUE)

semPaths(fit.rich2, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)



# Shannon diversity -------------------------------------------------------

# Model 1 (Shannon 2021)
mod.shan1 <- '
  # regressions
    shan ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ up.trt + inch.trt
'
fit.shan1 <- sem(mod.shan1, data = dat.2021, fixed.x = TRUE)
varTable(fit.shan1)
summary(fit.shan1, standardized = TRUE)

semPaths(fit.shan1, "std", residuals = FALSE)

# Model 2 (Shannon 2021)
mod.shan2 <- '
  # regressions
    shan ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.shan2 <- sem(mod.shan2, data = dat.2021, fixed.x = TRUE) 
vartable(fit.shan2)
summary(fit.shan2, standardized = TRUE)

semPaths(fit.shan2, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 2 (difference 2021)
mod.shan3 <- '
  # regressions
    shan.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.shan3 <- sem(mod.shan3, data = dat.2021, fixed.x = TRUE) 
vartable(fit.shan3)
summary(fit.shan3, standardized = TRUE)

semPaths(fit.shan3, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Save --------------------------------------------------------------------

save.image("RData/Exploratory-SEM.RData")
