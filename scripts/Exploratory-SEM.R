library(tidyverse)
library(lavaan)
library(semPlot)

# Load data ---------------------------------------------------------------

dat.2021 <- read.csv("data/cleaned/SEM-input.csv")
dat.2021$inch.trt <- as.factor(dat.2021$inch.trt)
dat.2021$up.trt <- as.factor(dat.2021$up.trt)


####### SEM (no latent variables) #########################################


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

semPaths(fit.herb2, "std", edge.label.cex = 0.5, curvePivot = TRUE, residuals = FALSE)


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

tiff("output_figs/Prelim-SEM_herb-cover_width_up-in-trt_elevation.tiff",
     units = "in", height = 4, width = 6, res = 150)
semPaths(fit.herb4, "std", edge.label.cex = 1, residuals = FALSE,
         sizeMan = 7)
dev.off()


# Shannon diversity -------------------------------------------------------

# Model 1 (Shannon 2021)
mod.shannon1 <- '
  # regressions
    shan ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ up.trt + inch.trt
'
fit.shannon1 <- sem(mod.shannon1, data = dat.2021, fixed.x = TRUE)
varTable(fit.shannon1)
summary(fit.shannon1, standardized = TRUE)

semPaths(fit.shannon1, "std", residuals = FALSE)


# Save --------------------------------------------------------------------

save.image("RData-RMarkdown/Exploratory-SEM.RData")
