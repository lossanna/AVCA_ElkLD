# Purpose: Data screening for temporal 2012-2021 plant data (cover & diversity).
# Checked for normality:
#   Total cover, richness, and Shannon were normally distributed for all years, and by year.
#   Herb cover was normally distributed by year, but not really for all years combined.
# Checked for outliers:
#   There were a few outliers, but none of them were extreme (all years combined).
# Created: 2023-02-01
# Last updated: 2023-03-31


library(tidyverse)
library(agricolae)
library(car)
library(rstatix)

# Load data ---------------------------------------------------------------

total.all <- read_csv("data/cleaned/Summarised-all_total-plant-cover.csv")
herb.all <- read_csv("data/cleaned/Summarised-all_herb-cover.csv")
notree.all <- read_csv("data/cleaned/Summarised-all_notree-cover.csv")
per.div <- read_csv("data/cleaned/Summarised-all_perennial-diversity.csv")

total.pd <- read_csv("data/cleaned/Percent-difference_total-cover.csv")
herb.pd <- read_csv("data/cleaned/Percent-difference_herb-cover.csv")
notree.pd <- read_csv("data/cleaned/Percent-difference_notree-cover.csv")
rich.pd <- read_csv("data/cleaned/Percent-difference_rich.csv")
shan.pd <- read_csv("data/cleaned/Percent-difference_shan.csv")


# Data wrangling ----------------------------------------------------------

# Convert grouping cols to factor
group.cols <- c("Year", "Channel", "Station", "Treatment1", "Treatment2", "Treatment3")

total.all[group.cols] <- lapply(total.all[group.cols], factor)
herb.all[group.cols] <- lapply(herb.all[group.cols], factor)
notree.all[group.cols] <- lapply(notree.all[group.cols], factor)
per.div[group.cols] <- lapply(per.div[group.cols], factor)

# All years, separate out control and treated
# Control
total.ctrl <- total.all |> 
  filter(Treatment3 == "Control")
herb.ctrl <- herb.all |> 
  filter(Treatment3 == "Control")
notree.ctrl <- notree.all |> 
  filter(Treatment3 == "Control")
per.div.ctrl <- per.div |> 
  filter(Treatment3 == "Control")

total.ctrl.pd <- total.pd |> 
  filter(Treatment3 == "Control")
herb.ctrl.pd <- herb.pd |> 
  filter(Treatment3 == "Control")
notree.ctrl.pd <- notree.pd |> 
  filter(Treatment3 == "Control")
rich.ctrl.pd <- rich.pd |> 
  filter(Treatment3 == "Control")
shan.ctrl.pd <- shan.pd |> 
  filter(Treatment3 == "Control")


# Treated
total.trt <- total.all |> 
  filter(Treatment3 == "Treated")
herb.trt <- herb.all |> 
  filter(Treatment3 == "Treated")
notree.trt <- notree.all |> 
  filter(Treatment3 == "Treated")
per.div.trt <- per.div |> 
  filter(Treatment3 == "Treated")

total.trt.pd <- total.pd |> 
  filter(Treatment3 == "Treated")
herb.trt.pd <- herb.pd |> 
  filter(Treatment3 == "Treated")
notree.trt.pd <- notree.pd |> 
  filter(Treatment3 == "Treated")
rich.trt.pd <- rich.pd |> 
  filter(Treatment3 == "Treated")
shan.trt.pd <- shan.pd |> 
  filter(Treatment3 == "Treated")


# Separate out by year
# 2012
total.ctrl.12 <- total.all |> 
  filter(Treatment3 == "Control",
         Year == "2012")
herb.ctrl.12 <- herb.all |> 
  filter(Treatment3 == "Control",
         Year == "2012")
notree.ctrl.12 <- notree.all |> 
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
per.div.trt.12 <- per.div |> 
  filter(Treatment3 == "Treated",
         Year == "2012")

# 2013
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
per.div.trt.21 <- per.div |> 
  filter(Treatment3 == "Treated",
         Year == "2021")



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
hist(herb.ctrl$Cover, breaks = 10) # seems not normal
hist(herb.trt$Cover, breaks = 10) # seems not normal



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


# Percent difference
qqPlot(total.ctrl.pd$dCover) # not normal?
qqPlot(total.trt.pd$dCover) # kind of not normal?
qqPlot(herb.ctrl.pd$dCover) # almost normal?
qqPlot(herb.trt.pd$dCover) # almost normal?
qqPlot(notree.ctrl.pd$dCover) # not normal
qqPlot(notree.trt.pd$dCover) # not normal
qqPlot(rich.ctrl.pd$dRichness) # almost normal?
qqPlot(rich.trt.pd$dRichness) # not normal?
qqPlot(shan.ctrl.pd$dShannon) # def not normal
qqPlot(shan.trt.pd$dShannon) # def not normal


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
