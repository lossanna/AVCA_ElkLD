library(tidyverse)

# Load data ---------------------------------------------------------------

elev <- read.csv("data/cleaned/Cross-section-elevation_clean.csv")
firstlast <- read.csv("data/cleaned/Percent-difference_first-last.csv", )

dat <- left_join(elev, firstlast)

# Visualization -----------------------------------------------------------

dat |> 
  ggplot(aes(x = dElev, y = notree.pd, color = Treatment3)) +
  geom_point() +
  geom_smooth(method = "lm")

dat |> 
  ggplot(aes(x = dElev, y = notree.pd, color = Channel)) +
  geom_point() +
  geom_smooth(method = "lm")

