library(tidyverse)

# Load data ---------------------------------------------------------------

ground.all <- read.csv("data/cleaned/Summarised-all_ground-cover.csv")
total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
fungr.all <- read.csv("data/cleaned/Summarised-all_functional-group-cover.csv")
gfst.all <- read.csv("data/cleaned/Summarised-all_grass-forb-shrub-tree-cover.csv")
woody.all <- read.csv("data/cleaned/Summarised-all_woody-herb-cover.csv")
inwood.all <- read.csv("data/cleaned/Summarised-all_invasive-woody-cover.csv")
ingfst.all <- read.csv("data/cleaned/Summarised-all_invasive-grassforbshrubtree-cover.csv")
innat.all <- read.csv("data/cleaned/Summarised-all_invasive-native-cover.csv")
per.diversity <- read.csv("data/cleaned/All-Nov_perennial-diversity.csv")


# Find difference in cover from 2012 to 2021 ------------------------------

soil.wide <- ground.all %>% 
  filter(Year != "2012-03-01") %>% 
  filter(Common == "Soil") %>% 
  select(-PlotTimeID) |> 
  pivot_wider(names_from = Year, values_from = Cover) %>% 
  mutate(soil.diff = `2021-11-01` - `2012-11-01`) %>% 
  select(-`2012-11-01`, -`2013-11-01`, -`2014-11-01`, 
         -`2015-11-01`, -`2018-11-01`, -`2021-11-01`)

total.wide <- total.all %>% 
  filter(Year != "2012-03-01") %>% 
  select(-PlotTimeID) |> 
  pivot_wider(names_from = Year, values_from = Cover) %>% 
  mutate(total.diff = `2021-11-01` - `2012-11-01`) %>% 
  select(-`2012-11-01`, -`2013-11-01`, -`2014-11-01`, 
         -`2015-11-01`, -`2018-11-01`, -`2021-11-01`)

richness.wide <- per.diversity %>% 
  select(-shan) %>% 
  pivot_wider(names_from = Year, values_from = rich) %>% 
  mutate(rich.diff = `2021-11-01` - `2012-11-01`) %>% 
  select(-`2012-11-01`, -`2013-11-01`, -`2014-11-01`, 
         -`2015-11-01`, -`2018-11-01`, -`2021-11-01`)

shannon.wide <- per.diversity %>% 
  select(-rich) %>% 
  pivot_wider(names_from = Year, values_from = shan) %>% 
  mutate(shan.diff = `2021-11-01` - `2012-11-01`) %>% 
  select(-`2012-11-01`, -`2013-11-01`, -`2014-11-01`, 
         -`2015-11-01`, -`2018-11-01`, -`2021-11-01`)

herb.wide <- woody.all %>% 
  filter(Year != "2012-03-01",
         woody == "Herbaceous") %>% 
  select(-PlotTimeID) |> 
  pivot_wider(names_from = Year, values_from = Cover) %>% 
  mutate(herb.diff = `2021-11-01` - `2012-11-01`) %>% 
  select(-`2012-11-01`, -`2013-11-01`, -`2014-11-01`, 
         -`2015-11-01`, -`2018-11-01`, -`2021-11-01`)

wood.wide <- woody.all %>% 
  filter(Year != "2012-03-01",
         woody == "Woody") %>% 
  select(-PlotTimeID) |> 
  pivot_wider(names_from = Year, values_from = Cover) %>% 
  replace(is.na(.), 0) %>% # NA indicates woody plants were not measured that year; hence, 0
                                # except for C19 2+4, which is actually NA since Nov 2012 data sheet is missing
  mutate(wood.diff = `2021-11-01` - `2012-11-01`) %>% 
  select(-`2012-11-01`, -`2013-11-01`, -`2014-11-01`, 
         -`2015-11-01`, -`2018-11-01`, -`2021-11-01`)
wood.wide$wood.diff[wood.wide$Station == "Station 02 + 04"] <- NA # manually fix C19 2+4 (should be NA)


# Combine -----------------------------------------------------------------

diff.12.21 <- total.wide %>% 
  left_join(richness.wide) %>% 
  left_join(shannon.wide) %>% 
  left_join(herb.wide) %>% 
  select(-woody) %>% 
  left_join(wood.wide) %>% # Nov 2012 data sheet is missing for C19 2+4 so they are all NA
  select(-woody)


# Save --------------------------------------------------------------------

write.csv(diff.12.21,
          file = "data/cleaned/Difference_2012-2021.csv",
          row.names = FALSE)

save.image("RData/Difference_2012-2021.RData")
