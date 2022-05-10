library(tidyverse)
library(lavaan)
library(semPlot)

# Load data ---------------------------------------------------------------

dat.2021 <- read.csv("data/cleaned/SEM-input.csv")


# SEM ---------------------------------------------------------------------

# Total cover
mod.totalcover <- '
  # regressions
    Cover ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ up.trt + inch.trt
'
fit.totalcover <- sem(mod.totalcover, data = dat.2021, fixed.x = TRUE) 
  # Warning message: In lav_data_full(data = data, group = group, cluster = cluster,  :
      # lavaan WARNING: some observed variances are (at least) a factor 1000 times larger 
      # than others; use varTable(fit) to investigate
vartable(fit.totalcover)
summary(fit.totalcover, standardized = TRUE)

semPaths(fit.totalcover, "std", edge.label.cex = 0.5, curvePivot = TRUE)


# Herbaceous cover
mod.herbcover <- '
  # regressions
    Herbaceous ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ up.trt + inch.trt
'
fit.herbcover <- sem(mod.herbcover, data = dat.2021, fixed.x = TRUE)
  # Warning message: In lav_data_full(data = data, group = group, cluster = cluster,  :
      # lavaan WARNING: some observed variances are (at least) a factor 1000 times larger 
      # than others; use varTable(fit) to investigate
varTable(fit.herbcover)
summary(fit.herbcover, standardized = TRUE)

semPaths(fit.herbcover, "std", edge.label.cex = 0.5, curvePivot = TRUE)


# Shannon diversity
mod.shannon <- '
  # regressions
    shan ~ Width + up.trt + inch.trt + Elev_Diff + OM_perc
    OM_perc ~ up.trt + inch.trt
'
fit.shannon <- sem(mod.shannon, data = dat.2021, fixed.x = TRUE)
varTable(fit.shannon)
summary(fit.shannon, standardized = TRUE)

semPaths(fit.shannon, "std")


# Save --------------------------------------------------------------------

save.image("RData-RMarkdown/Exploratory-SEM.RData")
