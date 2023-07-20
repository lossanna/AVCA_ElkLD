# Purpose: Explore correlation between notree averages and precip, and 
#   elevation difference and percent difference (but not sure this will really work).
# Created: 2023-05-24
# Last updated: 2023-07-20

library(tidyverse)
library(ggpubr)

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
notree.ctrl.plot <- notree.ctrl |> 
  ggplot(aes(x = Precip_cum, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Cummulative summer precipitation (in.)",
       y = "Grass, forb & shrub cover (%)",
       title = "Control") +
  theme_bw() +
  stat_regline_equation(label.x = 7.5, label.y = 46.5) +
  stat_cor(label.x = 7.5, label.y = 43.5) 
notree.ctrl.plot
  

notree.trt.plot <- notree.trt |> 
  ggplot(aes(x = Precip_cum, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Cummulative summer precipitation (in.)",
       y = "Grass, forb & shrub cover (%)",
       title = "Treated") +
  theme_bw() +
  stat_regline_equation(label.x = 7.5, label.y = 46.5) +
  stat_cor(label.x = 7.5, label.y = 43.5) 
notree.trt.plot

tiff("figures/2023-07_draft-figures/Corr-precip-veg.tiff", height = 5, width = 11, units = "in", res = 150)
annotate_figure(ggarrange(notree.ctrl.plot, notree.trt.plot,
                          nrow = 1, ncol = 2,
                          labels = c("(A)", "(B)")),
                top = text_grob("Correlation between precipitation and vegetation cover \n",
                                size = 15,
                                hjust = 1.1))
dev.off()


# Percent difference and elevation change
dat.elev |> 
  ggplot(aes(x = dElev, y = notree.pd, color = Treatment3)) +
  geom_point() +
  geom_smooth(method = "lm")

dat.elev |> 
  ggplot(aes(x = dElev, y = notree.pd, color = Channel)) +
  geom_point() +
  geom_smooth(method = "lm")

