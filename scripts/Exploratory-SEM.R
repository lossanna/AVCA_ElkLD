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
mod.herb01 <- '
  # regressions
    Herbaceous ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ up.trt + inch.trt
'
fit.herb01 <- sem(mod.herb01, data = dat.2021, fixed.x = TRUE) # got warning about variance
summary(fit.herb01, standardized = TRUE)

semPaths(fit.herb01, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 2 (difference)
mod.herb02 <- '
  # regressions
    herb.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt + up.trt
'
fit.herb02 <- sem(mod.herb02, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.herb02)
summary(fit.herb02, standardized = TRUE)

semPaths(fit.herb02, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 3 (cover 2021)
  # in-channel treatment effect only
mod.herb03 <- '
  # regressions
    Herbaceous ~ Width + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt
'
fit.herb03 <- sem(mod.herb03, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.herb03)
summary(fit.herb03, standardized = TRUE)

semPaths(fit.herb03, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 4 (difference)
  # in-channel treatment effect only
mod.herb04 <- '
  # regressions
    herb.diff ~ Width + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt
'
fit.herb04 <- sem(mod.herb04, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.herb04)
summary(fit.herb04, standardized = TRUE)

semPaths(fit.herb04, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 5 (cover 2021)
  # maximum variables
mod.herb05 <- '
  # regressions
    Herbaceous ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.herb05 <- sem(mod.herb05, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.herb05)
summary(fit.herb05, standardized = TRUE)

semPaths(fit.herb05, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 6 (difference 2021)
  # maximum variables
mod.herb06 <- '
  # regressions
    herb.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.herb06 <- sem(mod.herb06, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.herb06)
summary(fit.herb06, standardized = TRUE)

semPaths(fit.herb06, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 7 (cover 2021)
  # remove elevation to increase sample:variable ratio because it is not significant 
mod.herb07 <- '
  # regressions
    Herbaceous ~ Width + up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.herb07 <- sem(mod.herb07, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.herb07)
summary(fit.herb07, standardized = TRUE)

semPaths(fit.herb07, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)

# Model 8 (difference 2021)
  # remove elevation to increase sample:variable ratio because it is not significant 
mod.herb08 <- '
  # regressions
    herb.diff ~ Width + up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.herb08 <- sem(mod.herb08, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.herb08)
summary(fit.herb08, standardized = TRUE)

semPaths(fit.herb08, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 9 (cover 2021)
  # no GIS-derived models
mod.herb09 <- '
  # regressions
    Herbaceous ~ up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.herb09 <- sem(mod.herb09, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.herb09)
summary(fit.herb09, standardized = TRUE)

semPaths(fit.herb09, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 10 (difference 2021)
  # no GIS-derived models
mod.herb10 <- '
  # regressions
    herb.diff ~ up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.herb10 <- sem(mod.herb10, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.herb10)
summary(fit.herb10, standardized = TRUE)

semPaths(fit.herb10, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)



# Woody -------------------------------------------------------------------

# Model 1 (2021 cover)
mod.wood01 <- '
  # regressions
    Woody ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ up.trt + inch.trt
'
fit.wood01 <- sem(mod.wood01, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.wood01)
summary(fit.wood01, standardized = TRUE)

semPaths(fit.wood01, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 2 (difference)
mod.wood02 <- '
  # regressions
    wood.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt + up.trt
'
fit.wood02 <- sem(mod.wood02, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.wood02)
summary(fit.wood02, standardized = TRUE)

semPaths(fit.wood02, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 3 (cover 2021)
  # in-channel treatment effect only
mod.wood03 <- '
  # regressions
    Woody ~ Width + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt
'
fit.wood03 <- sem(mod.wood03, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.wood03)
summary(fit.wood03, standardized = TRUE)

semPaths(fit.wood03, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 4 (difference)
  # in-channel treatment effect only
mod.wood04 <- '
  # regressions
    wood.diff ~ Width + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt
'
fit.wood04 <- sem(mod.wood04, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.wood04)
summary(fit.wood04, standardized = TRUE)

semPaths(fit.wood04, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 5 (cover 2021)
  # maximum variables
mod.wood05 <- '
  # regressions
    Woody ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.wood05 <- sem(mod.wood05, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.wood05)
summary(fit.wood05, standardized = TRUE)

semPaths(fit.wood05, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 6 (difference 2021)
  # maximum variables
mod.wood06 <- '
  # regressions
    wood.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.wood06 <- sem(mod.wood06, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.wood06)
summary(fit.wood06, standardized = TRUE)

semPaths(fit.wood06, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 7 (cover 2021)
  # remove elevation to increase sample:variable ratio because it is not significant 
mod.wood07 <- '
  # regressions
    Woody ~ Width + up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.wood07 <- sem(mod.wood07, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.wood07)
summary(fit.wood07, standardized = TRUE)

semPaths(fit.wood07, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)

# Model 8 (difference 2021)
  # remove elevation to increase sample:variable ratio because it is not significant 
mod.wood08 <- '
  # regressions
    wood.diff ~ Width + up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.wood08 <- sem(mod.wood08, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.wood08)
summary(fit.wood08, standardized = TRUE)

semPaths(fit.wood08, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 9 (cover 2021)
  # no GIS-derived models
mod.wood09 <- '
  # regressions
    Woody ~ up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.wood09 <- sem(mod.wood09, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.wood09)
summary(fit.wood09, standardized = TRUE)

semPaths(fit.wood09, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 10 (difference 2021)
  # no GIS-derived models
mod.wood10 <- '
  # regressions
    wood.diff ~ up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.wood10 <- sem(mod.wood10, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.wood10)
summary(fit.wood10, standardized = TRUE)

semPaths(fit.wood10, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)



# Richness ----------------------------------------------------------------

# Model 1 (2021 cover)
mod.rich01 <- '
  # regressions
    rich ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ up.trt + inch.trt
'
fit.rich01 <- sem(mod.rich01, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.rich01)
summary(fit.rich01, standardized = TRUE)

semPaths(fit.rich01, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 2 (difference)
mod.rich02 <- '
  # regressions
    rich.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt + up.trt
'
fit.rich02 <- sem(mod.rich02, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.rich02)
summary(fit.rich02, standardized = TRUE)

semPaths(fit.rich02, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 3 (cover 2021)
  # in-channel treatment effect only
mod.rich03 <- '
  # regressions
    rich ~ Width + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt
'
fit.rich03 <- sem(mod.rich03, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.rich03)
summary(fit.rich03, standardized = TRUE)

semPaths(fit.rich03, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 4 (difference)
  # in-channel treatment effect only
mod.rich04 <- '
  # regressions
    rich.diff ~ Width + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt
'
fit.rich04 <- sem(mod.rich04, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.rich04)
summary(fit.rich04, standardized = TRUE)

semPaths(fit.rich04, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 5 (cover 2021)
  # maximum variables
mod.rich05 <- '
  # regressions
    rich ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.rich05 <- sem(mod.rich05, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.rich05)
summary(fit.rich05, standardized = TRUE)

semPaths(fit.rich05, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 6 (difference 2021)
# maximum variables
mod.rich06 <- '
  # regressions
    rich.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.rich06 <- sem(mod.rich06, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.rich06)
summary(fit.rich06, standardized = TRUE)

semPaths(fit.rich06, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 7 (cover 2021)
# remove elevation to increase sample:variable ratio because it is not significant 
mod.rich07 <- '
  # regressions
    rich ~ Width + up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.rich07 <- sem(mod.rich07, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.rich07)
summary(fit.rich07, standardized = TRUE)

semPaths(fit.rich07, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)

# Model 8 (difference 2021)
  # remove elevation to increase sample:variable ratio because it is not significant 
mod.rich08 <- '
  # regressions
    rich.diff ~ Width + up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.rich08 <- sem(mod.rich08, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.rich08)
summary(fit.rich08, standardized = TRUE)

semPaths(fit.rich08, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 9 (cover 2021)
  # no GIS-derived models
mod.rich09 <- '
  # regressions
    rich ~ up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.rich09 <- sem(mod.rich09, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.rich09)
summary(fit.rich09, standardized = TRUE)

semPaths(fit.rich09, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 10 (difference 2021)
  # no GIS-derived models
mod.rich10 <- '
  # regressions
    rich.diff ~ up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.rich10 <- sem(mod.rich10, data = dat.2021, fixed.x = TRUE) # got warning about variance
vartable(fit.rich10)
summary(fit.rich10, standardized = TRUE)

semPaths(fit.rich10, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)



# Shannon diversity -------------------------------------------------------

# Model 1 (2021 cover)
mod.shan01 <- '
  # regressions
    shan ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ up.trt + inch.trt
'
fit.shan01 <- sem(mod.shan01, data = dat.2021, fixed.x = TRUE) 
vartable(fit.shan01)
summary(fit.shan01, standardized = TRUE)

semPaths(fit.shan01, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 2 (difference)
mod.shan02 <- '
  # regressions
    shan.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt + up.trt
'
fit.shan02 <- sem(mod.shan02, data = dat.2021, fixed.x = TRUE) 
vartable(fit.shan02)
summary(fit.shan02, standardized = TRUE)

semPaths(fit.shan02, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 3 (cover 2021)
  # in-channel treatment effect only
mod.shan03 <- '
  # regressions
    shan ~ Width + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt
'
fit.shan03 <- sem(mod.shan03, data = dat.2021, fixed.x = TRUE) 
vartable(fit.shan03)
summary(fit.shan03, standardized = TRUE)

semPaths(fit.shan03, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


# Model 4 (difference)
  # in-channel treatment effect only
mod.shan04 <- '
  # regressions
    shan.diff ~ Width + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ inch.trt
'
fit.shan04 <- sem(mod.shan04, data = dat.2021, fixed.x = TRUE) 
vartable(fit.shan04)
summary(fit.shan04, standardized = TRUE)

semPaths(fit.shan04, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 5 (cover 2021)
  # maximum variables
mod.shan05 <- '
  # regressions
    shan ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.shan05 <- sem(mod.shan05, data = dat.2021, fixed.x = TRUE) 
vartable(fit.shan05)
summary(fit.shan05, standardized = TRUE)

semPaths(fit.shan05, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 6 (difference 2021)
  # maximum variables
mod.shan06 <- '
  # regressions
    shan.diff ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.shan06 <- sem(mod.shan06, data = dat.2021, fixed.x = TRUE) 
vartable(fit.shan06)
summary(fit.shan06, standardized = TRUE)

semPaths(fit.shan06, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 7 (cover 2021)
  # remove elevation to increase sample:variable ratio because it is not significant 
mod.shan07 <- '
  # regressions
    shan ~ Width + up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.shan07 <- sem(mod.shan07, data = dat.2021, fixed.x = TRUE) 
vartable(fit.shan07)
summary(fit.shan07, standardized = TRUE)

semPaths(fit.shan07, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)

# Model 8 (difference 2021)
  # remove elevation to increase sample:variable ratio because it is not significant 
mod.shan08 <- '
  # regressions
    shan.diff ~ Width + up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.shan08 <- sem(mod.shan08, data = dat.2021, fixed.x = TRUE) 
vartable(fit.shan08)
summary(fit.shan08, standardized = TRUE)

semPaths(fit.shan08, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 9 (cover 2021)
  # no GIS-derived models
mod.shan09 <- '
  # regressions
    shan ~ up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.shan09 <- sem(mod.shan09, data = dat.2021, fixed.x = TRUE) 
vartable(fit.shan09)
summary(fit.shan09, standardized = TRUE)

semPaths(fit.shan09, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Model 10 (difference 2021)
  # no GIS-derived models
mod.shan10 <- '
  # regressions
    shan.diff ~ up.trt + inch.trt + OM_perc + TN_log
    OM_perc ~ up.trt + inch.trt
    TN_log ~ up.trt + inch.trt
'
fit.shan10 <- sem(mod.shan10, data = dat.2021, fixed.x = TRUE) 
vartable(fit.shan10)
summary(fit.shan10, standardized = TRUE)

semPaths(fit.shan10, "std", edge.label.cex = 1.3, residuals = FALSE, sizeMan = 7)


# Save --------------------------------------------------------------------

save.image("RData/Exploratory-SEM.RData")
