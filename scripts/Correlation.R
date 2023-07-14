# Purpose: Explore correlation between notree averages and precip, and 
#   elevation difference and percent difference (but not sure this will really work).
# Created: 2023-05-24
# Last updated: 2023-07-14

library(tidyverse)

# Load data ---------------------------------------------------------------

# Precip & veg cover
notree.avg <- read.csv("data/cleaned/Treatment3-average_notree-cover.csv")
precip <- read.table("data/PimaCounty_precip/PimaCounty_precip_2012-2021.txt",
                     sep = "\t", header = TRUE) 

# Percent change & elevation
elev <- read.csv("data/cleaned/Cross-section-elevation_clean.csv")
firstlast <- read.csv("data/cleaned/Percent-difference_first-last.csv", )


# Data wrangling ----------------------------------------------------------

# Precip & veg cover
precip.sample <- precip |> 
  filter(!str_detect(year.xaxis, c("2020|2016|2017|2019"))) |> 
  mutate(Year = gsub("-.*", "", year.xaxis)) |> 
  select(Year, Precip_cum)

notree.ctrl <- notree.avg |> 
  mutate(Year = as.character(Year)) |> 
  filter(Treatment3 == "Control") |> 
  left_join(precip.sample)

notree.trt <- notree.avg |> 
  mutate(Year = as.character(Year)) |> 
  filter(Treatment3 == "Treated") |> 
  left_join(precip.sample)

# Percent change & elevation
dat.elev <- left_join(elev, firstlast)


# Visualization -----------------------------------------------------------

# Precip & veg cover
notree.ctrl |> 
  ggplot(aes(x = Precip_cum, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Cummulative summer precipitation (in.)",
       y = "Grass, forb & shrub cover (%)",
       title = "Control")

notree.trt |> 
  ggplot(aes(x = Precip_cum, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Cummulative summer precipitation (in.)",
       y = "Grass, forb & shrub cover (%)",
       title = "Treated")


# Percent difference and elevation change
dat.elev |> 
  ggplot(aes(x = dElev, y = notree.pd, color = Treatment3)) +
  geom_point() +
  geom_smooth(method = "lm")

dat.elev |> 
  ggplot(aes(x = dElev, y = notree.pd, color = Channel)) +
  geom_point() +
  geom_smooth(method = "lm")

