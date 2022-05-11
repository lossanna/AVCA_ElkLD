library(tidyverse)
library(lavaan)
library(semPlot)

# Load data ---------------------------------------------------------------

dat.2021 <- read.csv("data/cleaned/SEM-input.csv")
dat.2021$inch.trt <- as.factor(dat.2021$inch.trt)
dat.2021$up.trt <- as.factor(dat.2021$up.trt)


# Model number does not mean variables are preserved across all models of same number


# Total cover -------------------------------------------------------------

# Model 1 (2021 cover)
mod.total1 <- '
  # regressions
    Cover ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ up.trt + inch.trt
'
fit.total1 <- sem(mod.total1, data = dat.2021, fixed.x = TRUE) 
  # Warning message: In lav_data_full(data = data, group = group, cluster = cluster,  :
      # lavaan WARNING: some observed variances are (at least) a factor 1000 times larger 
      # than others; use varTable(fit) to investigate
  # Discussed further here: https://groups.google.com/g/lavaan/c/FzShzsXYNco?pli=1
vartable(fit.total1)
summary(fit.total1, standardized = TRUE)

semPaths(fit.total1, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 2 (2021 cover)
mod.total2 <- '
  # regressions
    Cover ~ Width + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt
'
fit.total2 <- sem(mod.total2, data = dat.2021, fixed.x = TRUE) 
    # Warning message: In lav_data_full(data = data, group = group, cluster = cluster,  :
      # lavaan WARNING: some observed variances are (at least) a factor 1000 times larger 
      # than others; use varTable(fit) to investigate
vartable(fit.total2)
summary(fit.total2, standardized = TRUE)

semPaths(fit.total2, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 3 (difference)
mod.total3 <- '
  # regressions
    total.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt + up.trt
'
fit.total3 <- sem(mod.total3, data = dat.2021, fixed.x = TRUE) 
    # Warning message: In lav_data_full(data = data, group = group, cluster = cluster,  :
      # lavaan WARNING: some observed variances are (at least) a factor 1000 times larger 
      # than others; use varTable(fit) to investigate
vartable(fit.total3)
summary(fit.total3, standardized = TRUE)

semPaths(fit.total3, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 4 (cover 2021)
mod.total4 <- '
  # regressions
    Cover ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.total4 <- sem(mod.total4, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.total4)
summary(fit.total4, standardized = TRUE)

semPaths(fit.total4, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 5 (difference 2021)
mod.total5 <- '
  # regressions
    total.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.total5 <- sem(mod.total5, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.total5)
summary(fit.total5, standardized = TRUE)

semPaths(fit.total5, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)



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


# Model 2 (Shannon 2021)
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

save.image("RData-RMarkdown/Exploratory-SEM.RData")
