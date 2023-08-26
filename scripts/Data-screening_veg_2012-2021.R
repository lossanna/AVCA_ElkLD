# Purpose: Data screening for temporal 2012-2021 plant data (cover & diversity).

# Checked for normality:
#   Total cover, richness, and Shannon were normally distributed for all years, and by year.
#   Herb cover was normally distributed by year, but not really for all years combined.
#   Shrub cover was usually normally distributed year, but not for all the years combined, and does not
#     normal based on histograms, only qq-plots.
#   Notree cover was normally distributed by year, and slightly skewed for all years combined.
#   Annual cover is not normally distributed by year or combined.
#   Tree cover was not really normally distributed by year or combined;
#     kind of normally distributed for average of 2012-2014 of control but not treated,
#     but cannot log-transform because of 0 values.

# Checked for outliers:
#   There were a few outliers, but none of them were extreme (all years combined) for most;
#     shrub cover had 2 extreme outliers, and invasive cover had a fair amount of outliers and
#     4 extreme outliers.

# Added notes about row numbers because the 5 missing data sheets make the numbers uneven.
#   Missing sheets (5): 3 control from 2012, 2013, 2014; 2 treated from 2013.
#   If nothing was missing: 372 sampling events in total; 186 control and samples each in total;
#     62 samples each year; 31 control and treated samples for each year.

# Created: 2023-02-01
# Last updated: 2023-08-26

library(tidyverse)
library(agricolae)
library(car)
library(rstatix)

# Load data ---------------------------------------------------------------

# CSVs have 367 rows (372 events - 5 lost sheets)
total.all <- read_csv("data/cleaned/Summarised-all_total-plant-cover.csv")
herb.all <- read_csv("data/cleaned/Summarised-all_herb-cover.csv")
shrub.all <- read_csv("data/cleaned/Summarised-all_shrub-cover.csv")
notree.all <- read_csv("data/cleaned/Summarised-all_notree-cover.csv")
tree.all <- read_csv("data/cleaned/Summarised-all_tree-cover.csv")
annual.all <- read_csv("data/cleaned/Summarised-all_annual-cover.csv")
invasive.all <- read_csv("data/cleaned/Summarised-all_invasive-cover.csv")
per.div <- read_csv("data/cleaned/Summarised-all_perennial-diversity.csv")


# Data wrangling ----------------------------------------------------------

# Convert grouping cols to factor
group.cols <- c("Year", "Channel", "Station", "Treatment1", "Treatment2", "Treatment3")

total.all[group.cols] <- lapply(total.all[group.cols], factor)
herb.all[group.cols] <- lapply(herb.all[group.cols], factor)
shrub.all[group.cols] <- lapply(shrub.all[group.cols], factor)
notree.all[group.cols] <- lapply(notree.all[group.cols], factor)
tree.all[group.cols] <- lapply(tree.all[group.cols], factor)
annual.all[group.cols] <- lapply(annual.all[group.cols], factor)
invasive.all[group.cols] <- lapply(invasive.all[group.cols], factor)
per.div[group.cols] <- lapply(per.div[group.cols], factor)


# All years, separate out control and treated
# Control
#   183 rows: 186 events, but 3 sheets from control samples were missing
total.ctrl <- total.all |> 
  filter(Treatment3 == "Control")
herb.ctrl <- herb.all |> 
  filter(Treatment3 == "Control")
shrub.ctrl <- shrub.all |> 
  filter(Treatment3 == "Control")
notree.ctrl <- notree.all |> 
  filter(Treatment3 == "Control")
tree.ctrl <- tree.all |> 
  filter(Treatment3 == "Control")
annual.ctrl <- annual.all |> 
  filter(Treatment3 == "Control")
invasive.ctrl <- invasive.all |> 
  filter(Treatment3 == "Control")
per.div.ctrl <- per.div |> 
  filter(Treatment3 == "Control")

# Treated
#   184 rows: 186 events, but 2 sheets from treated samples were missing
total.trt <- total.all |> 
  filter(Treatment3 == "Treated")
herb.trt <- herb.all |> 
  filter(Treatment3 == "Treated")
shrub.trt <- shrub.all |> 
  filter(Treatment3 == "Treated")
notree.trt <- notree.all |> 
  filter(Treatment3 == "Treated")
tree.trt <- tree.all |> 
  filter(Treatment3 == "Treated")
annual.trt <- annual.all |> 
  filter(Treatment3 == "Treated")
invasive.trt <- invasive.all |> 
  filter(Treatment3 == "Treated")
per.div.trt <- per.div |> 
  filter(Treatment3 == "Treated")



# Separate out by year (31 rows if nothing missing)
# 2012
#   30 rows: 31 samples, but 1 control sheet missing
total.ctrl.12 <- total.all |> 
  filter(Treatment3 == "Control",
         Year == "2012")
herb.ctrl.12 <- herb.all |> 
  filter(Treatment3 == "Control",
         Year == "2012")
shrub.ctrl.12 <- shrub.all |> 
  filter(Treatment3 == "Control",
         Year == "2012")
notree.ctrl.12 <- notree.all |> 
  filter(Treatment3 == "Control",
         Year == "2012")
tree.ctrl.12 <- tree.all |> 
  filter(Treatment3 == "Control",
         Year == "2012")
annual.ctrl.12 <- annual.all |> 
  filter(Treatment3 == "Control",
         Year == "2012")
invasive.ctrl.12 <- invasive.all |> 
  filter(Treatment3 == "Control",
         Year == "2012")
per.div.ctrl.12 <- per.div |> 
  filter(Treatment3 == "Control",
         Year == "2012")

total.trt.12 <- total.all |> 
  filter(Treatment3 == "Treated",
         Year == "2012")
herb.trt.12 <- herb.all |> 
  filter(Treatment3 == "Treated",
         Year == "2012")
shrub.trt.12 <- shrub.all |> 
  filter(Treatment3 == "Treated",
         Year == "2012")
notree.trt.12 <- notree.all |> 
  filter(Treatment3 == "Treated",
         Year == "2012")
tree.trt.12 <- tree.all |> 
  filter(Treatment3 == "Treated",
         Year == "2012")
annual.trt.12 <- annual.all |> 
  filter(Treatment3 == "Treated",
         Year == "2012")
invasive.trt.12 <- invasive.all |> 
  filter(Treatment3 == "Treated",
         Year == "2012")
per.div.trt.12 <- per.div |> 
  filter(Treatment3 == "Treated",
         Year == "2012")


# 2013
#   30 rows: 31 samples, but 1 control sheet missing
total.ctrl.13 <- total.all |> 
  filter(Treatment3 == "Control",
         Year == "2013")
herb.ctrl.13 <- herb.all |> 
  filter(Treatment3 == "Control",
         Year == "2013")
shrub.ctrl.13 <- shrub.all |> 
  filter(Treatment3 == "Control",
         Year == "2013")
notree.ctrl.13 <- notree.all |> 
  filter(Treatment3 == "Control",
         Year == "2013")
annual.ctrl.13 <- annual.all |> 
  filter(Treatment3 == "Control",
         Year == "2013")
invasive.ctrl.13 <- invasive.all |> 
  filter(Treatment3 == "Control",
         Year == "2013")
per.div.ctrl.13 <- per.div |> 
  filter(Treatment3 == "Control",
         Year == "2013")

#   29 rows: 31 samples, but 2 treated sheets missing
total.trt.13 <- total.all |> 
  filter(Treatment3 == "Treated",
         Year == "2013")
herb.trt.13 <- herb.all |> 
  filter(Treatment3 == "Treated",
         Year == "2013")
shrub.trt.13 <- shrub.all |> 
  filter(Treatment3 == "Treated",
         Year == "2013")
notree.trt.13 <- notree.all |> 
  filter(Treatment3 == "Treated",
         Year == "2013")
annual.trt.13 <- annual.all |> 
  filter(Treatment3 == "Treated",
         Year == "2013")
invasive.trt.13 <- invasive.all |> 
  filter(Treatment3 == "Treated",
         Year == "2013")
per.div.trt.13 <- per.div |> 
  filter(Treatment3 == "Treated",
         Year == "2013")


# 2014
#   30 rows: 31 samples but 1 control sheet missing
total.ctrl.14 <- total.all |> 
  filter(Treatment3 == "Control",
         Year == "2014")
herb.ctrl.14 <- herb.all |> 
  filter(Treatment3 == "Control",
         Year == "2014")
shrub.ctrl.14 <- shrub.all |> 
  filter(Treatment3 == "Control",
         Year == "2014")
notree.ctrl.14 <- notree.all |> 
  filter(Treatment3 == "Control",
         Year == "2014")
annual.ctrl.14 <- annual.all |> 
  filter(Treatment3 == "Control",
         Year == "2014")
invasive.ctrl.14 <- invasive.all |> 
  filter(Treatment3 == "Control",
         Year == "2014")
per.div.ctrl.14 <- per.div |> 
  filter(Treatment3 == "Control",
         Year == "2014")

total.trt.14 <- total.all |> 
  filter(Treatment3 == "Treated",
         Year == "2014")
herb.trt.14 <- herb.all |> 
  filter(Treatment3 == "Treated",
         Year == "2014")
shrub.trt.14 <- shrub.all |> 
  filter(Treatment3 == "Treated",
         Year == "2014")
notree.trt.14 <- notree.all |> 
  filter(Treatment3 == "Treated",
         Year == "2014")
annual.trt.14 <- annual.all |> 
  filter(Treatment3 == "Treated",
         Year == "2014")
invasive.trt.14 <- invasive.all |> 
  filter(Treatment3 == "Treated",
         Year == "2014")
per.div.trt.14 <- per.div |> 
  filter(Treatment3 == "Treated",
         Year == "2014")


# 2015
total.ctrl.15 <- total.all |> 
  filter(Treatment3 == "Control",
         Year == "2015")
herb.ctrl.15 <- herb.all |> 
  filter(Treatment3 == "Control",
         Year == "2015")
shrub.ctrl.15 <- shrub.all |> 
  filter(Treatment3 == "Control",
         Year == "2015")
notree.ctrl.15 <- notree.all |> 
  filter(Treatment3 == "Control",
         Year == "2015")
annual.ctrl.15 <- annual.all |> 
  filter(Treatment3 == "Control",
         Year == "2015")
invasive.ctrl.15 <- invasive.all |> 
  filter(Treatment3 == "Control",
         Year == "2015")
per.div.ctrl.15 <- per.div |> 
  filter(Treatment3 == "Control",
         Year == "2015")

total.trt.15 <- total.all |> 
  filter(Treatment3 == "Treated",
         Year == "2015")
herb.trt.15 <- herb.all |> 
  filter(Treatment3 == "Treated",
         Year == "2015")
shrub.trt.15 <- shrub.all |> 
  filter(Treatment3 == "Treated",
         Year == "2015")
notree.trt.15 <- notree.all |> 
  filter(Treatment3 == "Treated",
         Year == "2015")
annual.trt.15 <- annual.all |> 
  filter(Treatment3 == "Treated",
         Year == "2015")
invasive.trt.15 <- invasive.all |> 
  filter(Treatment3 == "Treated",
         Year == "2015")
per.div.trt.15 <- per.div |> 
  filter(Treatment3 == "Treated",
         Year == "2015")


# 2018
total.ctrl.18 <- total.all |> 
  filter(Treatment3 == "Control",
         Year == "2018")
herb.ctrl.18 <- herb.all |> 
  filter(Treatment3 == "Control",
         Year == "2018")
shrub.ctrl.18 <- shrub.all |> 
  filter(Treatment3 == "Control",
         Year == "2018")
notree.ctrl.18 <- notree.all |> 
  filter(Treatment3 == "Control",
         Year == "2018")
annual.ctrl.18 <- annual.all |> 
  filter(Treatment3 == "Control",
         Year == "2018")
invasive.ctrl.18 <- invasive.all |> 
  filter(Treatment3 == "Control",
         Year == "2018")
per.div.ctrl.18 <- per.div |> 
  filter(Treatment3 == "Control",
         Year == "2018")

total.trt.18 <- total.all |> 
  filter(Treatment3 == "Treated",
         Year == "2018")
herb.trt.18 <- herb.all |> 
  filter(Treatment3 == "Treated",
         Year == "2018")
shrub.trt.18 <- shrub.all |> 
  filter(Treatment3 == "Treated",
         Year == "2018")
notree.trt.18 <- notree.all |> 
  filter(Treatment3 == "Treated",
         Year == "2018")
annual.trt.18 <- annual.all |> 
  filter(Treatment3 == "Treated",
         Year == "2018")
invasive.trt.18 <- invasive.all |> 
  filter(Treatment3 == "Treated",
         Year == "2018")
per.div.trt.18 <- per.div |> 
  filter(Treatment3 == "Treated",
         Year == "2018")


# 2021
total.ctrl.21 <- total.all |> 
  filter(Treatment3 == "Control",
         Year == "2021")
herb.ctrl.21 <- herb.all |> 
  filter(Treatment3 == "Control",
         Year == "2021")
shrub.ctrl.21 <- shrub.all |> 
  filter(Treatment3 == "Control",
         Year == "2021")
notree.ctrl.21 <- notree.all |> 
  filter(Treatment3 == "Control",
         Year == "2021")
tree.ctrl.21 <- tree.all |> 
  filter(Treatment3 == "Control",
         Year == "2021")
annual.ctrl.21 <- annual.all |> 
  filter(Treatment3 == "Control",
         Year == "2021")
invasive.ctrl.21 <- invasive.all |> 
  filter(Treatment3 == "Control",
         Year == "2021")
per.div.ctrl.21 <- per.div |> 
  filter(Treatment3 == "Control",
         Year == "2021")

total.trt.21 <- total.all |> 
  filter(Treatment3 == "Treated",
         Year == "2021")
herb.trt.21 <- herb.all |> 
  filter(Treatment3 == "Treated",
         Year == "2021")
shrub.trt.21 <- shrub.all |> 
  filter(Treatment3 == "Treated",
         Year == "2021")
notree.trt.21 <- notree.all |> 
  filter(Treatment3 == "Treated",
         Year == "2021")
tree.trt.21 <- tree.all |> 
  filter(Treatment3 == "Treated",
         Year == "2021")
annual.trt.21 <- annual.all |> 
  filter(Treatment3 == "Treated",
         Year == "2021")
invasive.trt.21 <- invasive.all |> 
  filter(Treatment3 == "Treated",
         Year == "2021")
per.div.trt.21 <- per.div |> 
  filter(Treatment3 == "Treated",
         Year == "2021")



# Average tree cover of first 3 years
tree.ctrl.avg1214 <- tree.all |> 
  filter(Treatment3 == "Control") |> 
  filter(Year %in% c("2012", "2013", "2014")) |> 
  group_by(Sample, Channel, Station) |> 
  summarise(Cover = mean(Cover),
            .groups = "keep")

tree.trt.avg1214 <- tree.all |> 
  filter(Treatment3 == "Treated") |> 
  filter(Year %in% c("2012", "2013", "2014")) |> 
  group_by(Sample, Channel, Station) |> 
  summarise(Cover = mean(Cover),
            .groups = "keep")

tree.avg1214 <- tree.all |> 
  filter(Year %in% c("2012", "2013", "2014")) |> 
  group_by(Sample, Channel, Station, Treatment3) |> 
  summarise(Cover = mean(Cover),
            .groups = "keep")

# Compare tree cover from different times
summary(tree.ctrl.avg1214$Cover)
summary(tree.ctrl.12$Cover)
summary(tree.ctrl.21$Cover) # not good representation

tree.ctrl.compare <- bind_rows(tree.ctrl.12, tree.ctrl.21) |> 
  select(Sample, Channel, Station, Cover) |> 
  bind_rows(tree.ctrl.avg1214) |> 
  mutate(year = c(rep("2012", 30),
                  rep("2021", 31),
                  rep("2012-2014 avg", 31)))
tree.ctrl.compare |> 
  ggplot(aes(x = year, y = Cover)) +
  geom_boxplot() +
  geom_jitter()

tree.trt.compare <- bind_rows(tree.trt.12, tree.trt.21) |> 
  select(Sample, Channel, Station, Cover) |> 
  bind_rows(tree.trt.avg1214) |> 
  mutate(year = c(rep("2012", 31),
                  rep("2021", 31),
                  rep("2012-2014 avg", 31)))
tree.trt.compare |> 
  ggplot(aes(x = year, y = Cover)) +
  geom_boxplot() +
  geom_jitter()
# Will go with 2012-2014 average




# Histograms --------------------------------------------------------------

# Total cover
# All years
hist(total.ctrl$Cover, breaks = 10)
hist(total.trt$Cover, breaks = 10)

# By year
hist(total.ctrl.12$Cover, breaks = 15)
hist(total.trt.12$Cover, breaks = 15)

hist(total.ctrl.13$Cover, breaks = 15)
hist(total.trt.13$Cover, breaks = 15)

hist(total.ctrl.14$Cover, breaks = 15)
hist(total.trt.14$Cover, breaks = 15)

hist(total.ctrl.15$Cover, breaks = 15)
hist(total.trt.15$Cover, breaks = 15)

hist(total.ctrl.18$Cover, breaks = 15)
hist(total.trt.18$Cover, breaks = 15)

hist(total.ctrl.21$Cover, breaks = 15)
hist(total.trt.21$Cover, breaks = 15)


# Herb cover
# All years
hist(herb.ctrl$Cover, breaks = 15) # seems not normal
hist(herb.trt$Cover, breaks = 15) # seems not normal

# By year
hist(herb.ctrl.12$Cover, breaks = 15)
hist(herb.trt.12$Cover, breaks = 15)

hist(herb.ctrl.13$Cover, breaks = 15)
hist(herb.trt.13$Cover, breaks = 15)

hist(herb.ctrl.14$Cover, breaks = 15)
hist(herb.trt.14$Cover, breaks = 15)

hist(herb.ctrl.15$Cover, breaks = 15)
hist(herb.trt.15$Cover, breaks = 15)

hist(herb.ctrl.18$Cover, breaks = 15)
hist(herb.trt.18$Cover, breaks = 15)

hist(herb.ctrl.21$Cover, breaks = 15)
hist(herb.trt.21$Cover, breaks = 15)


# Shrub cover
# All years
hist(shrub.ctrl$Cover, breaks = 10) # not normal
hist(shrub.trt$Cover, breaks = 10) # not normal

# By year
hist(shrub.ctrl.12$Cover, breaks = 15) # not normal
hist(shrub.trt.12$Cover, breaks = 15) # not normal

hist(shrub.ctrl.13$Cover, breaks = 15) # not normal
hist(shrub.trt.13$Cover, breaks = 15) # not normal

hist(shrub.ctrl.14$Cover, breaks = 15) # not normal
hist(shrub.trt.14$Cover, breaks = 15) # not normal

hist(shrub.ctrl.15$Cover, breaks = 15) # not normal
hist(shrub.trt.15$Cover, breaks = 15) # not normal

hist(shrub.ctrl.18$Cover, breaks = 15) # not normal 
hist(shrub.trt.18$Cover, breaks = 15) # not normal

hist(shrub.ctrl.21$Cover, breaks = 15) # the only one that might be normal
hist(shrub.trt.21$Cover, breaks = 15) # not normal


# Notree cover
# All years
hist(notree.ctrl$Cover, breaks = 15) # slightly skewed?
hist(notree.trt$Cover, breaks = 10) # right tail?

# By year
hist(notree.ctrl.12$Cover, breaks = 15)
hist(notree.trt.12$Cover, breaks = 15)

hist(notree.ctrl.13$Cover, breaks = 15)
hist(notree.trt.13$Cover, breaks = 15)

hist(notree.ctrl.14$Cover, breaks = 15)
hist(notree.trt.14$Cover, breaks = 15)

hist(notree.ctrl.15$Cover, breaks = 15)
hist(notree.trt.15$Cover, breaks = 15)

hist(notree.ctrl.18$Cover, breaks = 15)
hist(notree.trt.18$Cover, breaks = 15)

hist(notree.ctrl.21$Cover, breaks = 15)
hist(notree.trt.21$Cover, breaks = 15)


# Tree cover
hist(tree.ctrl.avg1214$Cover, breaks = 15) # not normal
hist(tree.trt.avg1214$Cover, breaks = 15)


# Annual cover
# All years
hist(annual.ctrl$Cover, breaks = 15) # definitely not normal
hist(annual.trt$Cover, breaks = 15) # definitely not normal

# By year
#   none of these are normal either
hist(annual.ctrl.12$Cover, breaks = 15)
hist(annual.trt.12$Cover, breaks = 15)

hist(annual.ctrl.13$Cover, breaks = 15)
hist(annual.trt.13$Cover, breaks = 15)

hist(annual.ctrl.14$Cover, breaks = 15)
hist(annual.trt.14$Cover, breaks = 15)

hist(annual.ctrl.15$Cover, breaks = 15)
hist(annual.trt.15$Cover, breaks = 15)

hist(annual.ctrl.18$Cover, breaks = 15)
hist(annual.trt.18$Cover, breaks = 15)

hist(annual.ctrl.21$Cover, breaks = 15)
hist(annual.trt.21$Cover, breaks = 15)


# Invasive cover
# All years
#   none of this is normal
hist(invasive.ctrl$Cover, breaks = 10)
hist(invasive.trt$Cover, breaks = 10)

# By year
#   none of this is normal 
hist(invasive.ctrl.12$Cover, breaks = 15)
hist(invasive.trt.12$Cover, breaks = 15)

hist(invasive.ctrl.13$Cover, breaks = 15)
hist(invasive.trt.13$Cover, breaks = 15)

hist(invasive.ctrl.14$Cover, breaks = 15)
hist(invasive.trt.14$Cover, breaks = 15)

hist(invasive.ctrl.15$Cover, breaks = 15)
hist(invasive.trt.15$Cover, breaks = 15)

hist(invasive.ctrl.18$Cover, breaks = 15)
hist(invasive.trt.18$Cover, breaks = 15)

hist(invasive.ctrl.21$Cover, breaks = 15)
hist(invasive.trt.21$Cover, breaks = 15)



# Quantile-quantile plots -------------------------------------------------

# Total cover
# All years
qqPlot(total.ctrl$Cover) # almost normal?
qqPlot(total.trt$Cover) # almost normal?

# By year
#   all normal
qqPlot(total.ctrl.12$Cover)
qqPlot(total.trt.12$Cover)

qqPlot(total.ctrl.13$Cover)
qqPlot(total.trt.13$Cover)

qqPlot(total.ctrl.14$Cover)
qqPlot(total.trt.14$Cover)

qqPlot(total.ctrl.15$Cover)
qqPlot(total.trt.15$Cover)

qqPlot(total.ctrl.18$Cover)
qqPlot(total.trt.18$Cover)

qqPlot(total.ctrl.21$Cover)
qqPlot(total.trt.21$Cover)


# Herb cover
# All years
qqPlot(herb.ctrl$Cover) # not normal
qqPlot(herb.trt$Cover) # not normal

# By year
#   all normal, except 2 (marked)
qqPlot(herb.ctrl.12$Cover)
qqPlot(herb.trt.12$Cover)

qqPlot(herb.ctrl.13$Cover)
qqPlot(herb.trt.13$Cover)

qqPlot(herb.ctrl.14$Cover) # almost normal
qqPlot(herb.trt.14$Cover)

qqPlot(herb.ctrl.15$Cover) # almost normal
qqPlot(herb.trt.15$Cover)

qqPlot(herb.ctrl.18$Cover)
qqPlot(herb.trt.18$Cover)

qqPlot(herb.ctrl.21$Cover)
qqPlot(herb.trt.21$Cover)


# Shrub cover
# All years
qqPlot(shrub.ctrl$Cover) # not normal
qqPlot(shrub.trt$Cover) # not normal

# By year
#   most are normal; 3 are not
qqPlot(shrub.ctrl.12$Cover) # normal
qqPlot(shrub.trt.12$Cover) # normal

qqPlot(shrub.ctrl.13$Cover) # normal
qqPlot(shrub.trt.13$Cover) # normal

qqPlot(shrub.ctrl.14$Cover) # normal
qqPlot(shrub.trt.14$Cover) # not normal

qqPlot(shrub.ctrl.15$Cover) # not normal
qqPlot(shrub.trt.15$Cover) # normal

qqPlot(shrub.ctrl.18$Cover) # not normal
qqPlot(shrub.trt.18$Cover) # normal

qqPlot(shrub.ctrl.21$Cover) # normal
qqPlot(shrub.trt.21$Cover) # normal


# Notree cover
# All years
qqPlot(notree.ctrl$Cover) # almost normal?
qqPlot(notree.trt$Cover) # almost normal?

# By year
#   all are normal
qqPlot(notree.ctrl.12$Cover)
qqPlot(notree.trt.12$Cover)

qqPlot(notree.ctrl.13$Cover)
qqPlot(notree.trt.13$Cover)

qqPlot(notree.ctrl.14$Cover)
qqPlot(notree.trt.14$Cover)

qqPlot(notree.ctrl.15$Cover)
qqPlot(notree.trt.15$Cover)

qqPlot(notree.ctrl.18$Cover)
qqPlot(notree.trt.18$Cover)

qqPlot(notree.ctrl.21$Cover)
qqPlot(notree.trt.21$Cover)


# Tree cover
# All years
qqPlot(tree.ctrl$Cover) # not normal
qqPlot(tree.trt$Cover) # not normal

# First and last year
qqPlot(tree.ctrl.12$Cover) # not normal
qqPlot(tree.trt.12$Cover) # not normal
qqPlot(tree.ctrl.21$Cover) # kind of normal?
qqPlot(tree.trt.21$Cover) # not normal

# Average 2012-2014
qqPlot(tree.ctrl.avg1214$Cover) # mostly normal
qqPlot(tree.trt.avg1214$Cover) # not normal
#                                   but cannot log transform because of 0s
qqPlot(c(tree.ctrl.avg1214$Cover, tree.trt.avg1214$Cover)) # not that normal


# Annual cover
# All years
qqPlot(annual.ctrl$Cover) # super not normal
qqPlot(annual.trt$Cover) # super not normal

# By year
#   mostly not normal
qqPlot(annual.ctrl.12$Cover) # not normal
qqPlot(annual.trt.12$Cover) # not normal

qqPlot(annual.ctrl.13$Cover) # not normal
qqPlot(annual.trt.13$Cover) # not normal

qqPlot(annual.ctrl.14$Cover) # almost normal?
qqPlot(annual.trt.14$Cover) # not normal

qqPlot(annual.ctrl.15$Cover) # not normal
qqPlot(annual.trt.15$Cover) # not normal

qqPlot(annual.ctrl.18$Cover) # not normal
qqPlot(annual.trt.18$Cover) # not normal

qqPlot(annual.ctrl.21$Cover) # almost normal?
qqPlot(annual.trt.21$Cover) # normal


# Invasive cover
# All years
qqPlot(invasive.ctrl$Cover) # super not normal
qqPlot(invasive.trt$Cover) # super not normal

# By year
qqPlot(invasive.ctrl.12$Cover) # normal (enough)
qqPlot(invasive.trt.12$Cover) # almost normal

qqPlot(invasive.ctrl.13$Cover) # almost normal (except 1)
qqPlot(invasive.trt.13$Cover) # almost normal?

qqPlot(invasive.ctrl.14$Cover) # almost normal (except 1)
qqPlot(invasive.trt.14$Cover) # almost normal

qqPlot(invasive.ctrl.15$Cover) # almost normal
qqPlot(invasive.trt.15$Cover) # normal

qqPlot(invasive.ctrl.18$Cover) # normal
qqPlot(invasive.trt.18$Cover) # almost normal

qqPlot(invasive.ctrl.21$Cover) # almost normal
qqPlot(invasive.trt.21$Cover) # almost normal


# Richness
# All years
#   normal enough
qqPlot(per.div.ctrl$rich)
qqPlot(per.div.trt$rich)

# By year
#   all normal
qqPlot(per.div.ctrl.12$rich)
qqPlot(per.div.trt.12$rich)

qqPlot(per.div.ctrl.13$rich)
qqPlot(per.div.trt.13$rich)

qqPlot(per.div.ctrl.14$rich)
qqPlot(per.div.trt.14$rich)

qqPlot(per.div.ctrl.15$rich)
qqPlot(per.div.trt.15$rich)

qqPlot(per.div.ctrl.18$rich)
qqPlot(per.div.trt.18$rich)

qqPlot(per.div.ctrl.21$rich)
qqPlot(per.div.trt.21$rich)


# Shannon
# All years
#   both normal
qqPlot(per.div.ctrl$shan)
qqPlot(per.div.trt$shan)

# By year
#   all normal
qqPlot(per.div.ctrl.12$shan)
qqPlot(per.div.trt.12$shan)

qqPlot(per.div.ctrl.13$shan)
qqPlot(per.div.trt.13$shan)

qqPlot(per.div.ctrl.14$shan)
qqPlot(per.div.trt.14$shan)

qqPlot(per.div.ctrl.15$shan)
qqPlot(per.div.trt.15$shan)

qqPlot(per.div.ctrl.18$shan)
qqPlot(per.div.trt.18$shan)

qqPlot(per.div.ctrl.21$shan)
qqPlot(per.div.trt.21$shan)



# Check for outliers ------------------------------------------------------

total.all |> 
  select(Cover, Treatment3, Year) |> 
  group_by(Treatment3, Year) |> 
  identify_outliers(Cover)

herb.all |> 
  select(Cover, Treatment3, Year) |> 
  group_by(Treatment3, Year) |> 
  identify_outliers(Cover)

shrub.all |> 
  select(Cover, Treatment3, Year) |> 
  group_by(Treatment3, Year) |> 
  identify_outliers(Cover) # 2 extreme outliers

notree.all |> 
  select(Cover, Treatment3) |> 
  group_by(Treatment3) |> 
  identify_outliers(Cover)

tree.avg1214 |> 
  select(Cover, Treatment3) |> 
  group_by(Treatment3) |> 
  identify_outliers(Cover)

invasive.all |> 
  select(Cover, Treatment3, Year) |> 
  group_by(Treatment3, Year) |> 
  identify_outliers(Cover) |> 
  print(n = 22) # fair amount of outliers, 4 extreme outliers

per.div |> 
  select(rich, Treatment3, Year) |> 
  group_by(Treatment3, Year) |> 
  identify_outliers(rich)

per.div |> 
  select(shan, Treatment3, Year) |> 
  group_by(Treatment3, Year) |> 
  identify_outliers(shan)



save.image("RData/Data-screening_veg_2012-2021.RData")
