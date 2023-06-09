# Purpose: Maybe look at relationship between elevation difference and percent difference,
#   but not sure this will really work.
# Created: 2023-05-24
# Last updated: 2023-05-24

library(tidyverse)

# Load data ---------------------------------------------------------------

elev <- read.csv("data/cleaned/Cross-section-elevation_clean.csv")
firstlast <- read.csv("data/cleaned/Percent-difference_first-last.csv", )

dat <- left_join(elev, firstlast)


# Visualization -----------------------------------------------------------

# Percent difference and elevation change
dat |> 
  ggplot(aes(x = dElev, y = notree.pd, color = Treatment3)) +
  geom_point() +
  geom_smooth(method = "lm")

dat |> 
  ggplot(aes(x = dElev, y = notree.pd, color = Channel)) +
  geom_point() +
  geom_smooth(method = "lm")

