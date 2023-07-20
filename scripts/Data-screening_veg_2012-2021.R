# Purpose: Data screening for temporal 2012-2021 plant data (cover & diversity).

# Checked for normality:
#   Total cover, richness, and Shannon were normally distributed for all years, and by year.
#   Herb cover was normally distributed by year, but not really for all years combined.
#   Notree cover was normally distributed by year, and slightly skewed for all years combined.
#   Tree cover was not really normally distributed by year or combined;
#     kind of normally distributed for average of 2012-2014 of control but not treated,
#     but cannot log-transform because of 0 values.

# Checked for outliers:
#   There were a few outliers, but none of them were extreme (all years combined).

# Added notes about row numbers because the 5 missing data sheets make the numbers uneven.
#   Missing sheets (5): 3 control from 2012, 2013, 2014; 2 treated from 2013.
#   If nothing was missing: 372 sampling events in total; 186 control and samples each in total;
#     62 samples each year; 31 control and treated samples for each year.

# Created: 2023-02-01
# Last updated: 2023-07-14

library(tidyverse)
library(agricolae)
library(car)
library(rstatix)

# Load data ---------------------------------------------------------------

# CSVs have 367 rows (372 events - 5 lost sheets)
total.all <- read_csv("data/cleaned/Summarised-all_total-plant-cover.csv")
herb.all <- read_csv("data/cleaned/Summarised-all_herb-cover.csv")
notree.all <- read_csv("data/cleaned/Summarised-all_notree-cover.csv")
tree.all <- read_csv("data/cleaned/Summarised-all_tree-cover.csv")
annual.all <- read_csv("data/cleaned/Summarised-all_annual-cover.csv")
per.div <- read_csv("data/cleaned/Summarised-all_perennial-diversity.csv")


# Data wrangling ----------------------------------------------------------

# Convert grouping cols to factor
group.cols <- c("Year", "Channel", "Station", "Treatment1", "Treatment2", "Treatment3")

total.all[group.cols] <- lapply(total.all[group.cols], factor)
herb.all[group.cols] <- lapply(herb.all[group.cols], factor)
notree.all[group.cols] <- lapply(notree.all[group.cols], factor)
tree.all[group.cols] <- lapply(tree.all[group.cols], factor)
annual.all[group.cols] <- lapply(annual.all[group.cols], factor)
per.div[group.cols] <- lapply(per.div[group.cols], factor)

# All years, separate out control and treated
# Control
#   183 rows: 186 events, but 3 sheets from control samples were missing
total.ctrl <- total.all |> 
  filter(Treatment3 == "Control")
herb.ctrl <- herb.all |> 
  filter(Treatment3 == "Control")
notree.ctrl <- notree.all |> 
  filter(Treatment3 == "Control")
tree.ctrl <- tree.all |> 
  filter(Treatment3 == "Control")
annual.ctrl <- annual.all |> 
  filter(Treatment3 == "Control")
per.div.ctrl <- per.div |> 
  filter(Treatment3 == "Control")


# Treated
#   184 rows: 186 events, but 2 sheets from treated samples were missing
total.trt <- total.all |> 
  filter(Treatment3 == "Treated")
herb.trt <- herb.all |> 
  filter(Treatment3 == "Treated")
notree.trt <- notree.all |> 
  filter(Treatment3 == "Treated")
tree.trt <- tree.all |> 
  filter(Treatment3 == "Treated")
annual.trt <- annual.all |> 
  filter(Treatment3 == "Treated")
per.div.trt <- per.div |> 
  filter(Treatment3 == "Treated")


# Separate out by year
# 2012
#   30 rows: 31 samples, but 1 control sheet missing
total.ctrl.12 <- total.all |> 
  filter(Treatment3 == "Control",
         Year == "2012")
herb.ctrl.12 <- herb.all |> 
  filter(Treatment3 == "Control",
         Year == "2012")
notree.ctrl.12 <- notree.all |> 
  filter(Treatment3 == "Control",
         Year == "2012")
tree.ctrl.12 <- tree.all |> 
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
notree.trt.12 <- notree.all |> 
  filter(Treatment3 == "Treated",
         Year == "2012")
tree.trt.12 <- tree.all |> 
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
notree.ctrl.13 <- notree.all |> 
  filter(Treatment3 == "Control",
         Year == "2013")
per.div.ctrl.13 <- per.div |> 
  filter(Treatment3 == "Control",
         Year == "2013")

# 29 rows: 31 samples, but 2 treated sheets missing
total.trt.13 <- total.all |> 
  filter(Treatment3 == "Treated",
         Year == "2013")
herb.trt.13 <- herb.all |> 
  filter(Treatment3 == "Treated",
         Year == "2013")
notree.trt.13 <- notree.all |> 
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
notree.ctrl.14 <- notree.all |> 
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
notree.trt.14 <- notree.all |> 
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
notree.ctrl.15 <- notree.all |> 
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
notree.trt.15 <- notree.all |> 
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
notree.ctrl.18 <- notree.all |> 
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
notree.trt.18 <- notree.all |> 
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
notree.ctrl.21 <- notree.all |> 
  filter(Treatment3 == "Control",
         Year == "2021")
tree.ctrl.21 <- tree.all |> 
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
notree.trt.21 <- notree.all |> 
  filter(Treatment3 == "Treated",
         Year == "2021")
tree.trt.21 <- tree.all |> 
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




# Check distribution ------------------------------------------------------

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
hist(herb.ctrl$Cover, breaks = 15) # seems not normal
hist(herb.trt$Cover, breaks = 10) # seems not normal


# Notree cover
hist(notree.ctrl$Cover, breaks = 15) # slightly skewed?
hist(notree.trt$Cover, breaks = 10) # right tail?


# Annual cover
hist(annual.ctrl$Cover, breaks = 15) # definitely not normal
hist(annual.trt$Cover, breaks = 15) # definitely not normal



# Check normality ---------------------------------------------------------

# Total cover
# All years
qqPlot(total.ctrl$Cover) # almost normal?
qqPlot(total.trt$Cover) # almost normal?

# By year
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
qqPlot(herb.ctrl.12$Cover)
qqPlot(herb.trt.12$Cover)

qqPlot(herb.ctrl.13$Cover)
qqPlot(herb.trt.13$Cover)

qqPlot(herb.ctrl.14$Cover)
qqPlot(herb.trt.14$Cover)

qqPlot(herb.ctrl.15$Cover)
qqPlot(herb.trt.15$Cover)

qqPlot(herb.ctrl.18$Cover)
qqPlot(herb.trt.18$Cover)

qqPlot(herb.ctrl.21$Cover)
qqPlot(herb.trt.21$Cover)


# Notree cover
# All years
qqPlot(notree.ctrl$Cover) # almost normal?
qqPlot(notree.trt$Cover) # almost normal?

# By year
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


# Annual
qqPlot(annual.ctrl$Cover) # super not normal
qqPlot(annual.trt$Cover) # super not normal

# Richness
# All years
qqPlot(per.div.ctrl$rich)
qqPlot(per.div.trt$rich)

# By year
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
qqPlot(per.div.ctrl$shan)
qqPlot(per.div.trt$shan)

# By year
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

notree.all |> 
  select(Cover, Treatment3) |> 
  group_by(Treatment3) |> 
  identify_outliers(Cover)

tree.avg1214 |> 
  select(Cover, Treatment3, Year) |> 
  group_by(Treatment3, Year) |> 
  identify_outliers(Cover)

per.div |> 
  select(rich, Treatment3, Year) |> 
  group_by(Treatment3, Year) |> 
  identify_outliers(rich)

per.div |> 
  select(shan, Treatment3, Year) |> 
  group_by(Treatment3, Year) |> 
  identify_outliers(shan)



save.image("RData/Data-screening_veg_2012-2021.RData")
