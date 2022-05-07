library(tidyverse)
library(readxl)

# Load data ---------------------------------------------------------------

ground.all <- read.csv("data/cleaned/Summarised-all_ground-cover.csv")
total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
fungr.all <- read.csv("data/cleaned/Summarised-all_functional-group-cover.csv")
gfst.all <- read.csv("data/cleaned/Summarised-all_grass-forb-shrub-tree-cover.csv")
woody.all <- read.csv("data/cleaned/Summarised-all_woody-herb-cover.csv")
inwood.all <- read.csv("data/cleaned/Summarised-all_invasive-woody-cover.csv")
ingfst.all <- read.csv("data/cleaned/Summarised-all_invasive-grassforbshrubtree-cover.csv")
innat.all <- read.csv("data/cleaned/Summarised-all_invasive-native-cover.csv")


# Find difference in cover from 2012 to 2021 ------------------------------

ground.wide <- ground.all %>% 
  filter(Year != "2012-03-01") %>% 
  filter(Common %in% c("Soil", "Gravel", "Rock")) %>% 
  pivot_wider(names_from = Year, values_from = Cover) %>% 
  mutate(d2012_2021 = `2021-11-01` - `2012-11-01`)

total.wide <- total.all %>% 
  filter(Year != "2012-03-01") %>% 
  pivot_wider(names_from = Year, values_from = Cover) %>% 
  mutate(d2012_2021 = `2021-11-01` - `2012-11-01`)
