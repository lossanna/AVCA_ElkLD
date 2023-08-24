# Purpose: Species cover is measured in 4-6 quadrats. Cover needs to be averaged (summarised).
# For perennial diversity, quadrats need to be converted to richness and Shannon diversity
# across all quadrats.

# Columns added for grouping: channel and station treatment, Treatment 1-3,
#   lifeform with native status).
# Columns added for formatting year/date as just year (20XX), and 
#   year formatted for x-axis when graphing (20XX-01-01).
# These are the cleaned data sheets for plant cover and diversity from 2012-2021.

# Created: ~2022-01 (pre-GitHub; probably created in early Jan)
# Last updated: 2023-08-24


library(tidyverse)
library(vegan)

# Load data ---------------------------------------------------------------

all.c12 <- read.csv("data/cleaned/C12-cover.csv")
all.c13 <- read.csv("data/cleaned/C13-cover.csv")
all.c19 <- read.csv("data/cleaned/C19-cover.csv")
all.c21 <- read.csv("data/cleaned/C21-cover.csv")


# Create PlotTimeID -------------------------------------------------------

# PlotTimeID is a unique ID for each sample (station) each year
#   There are 372 sampling events (62 samples x 6 years)
all.c <- bind_rows(all.c12, all.c13, all.c19, all.c21) %>% 
  select(Year, Station) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Year) |> 
  filter(Year != "2012-03-01") 
all.c$PlotTimeID <- c(1:nrow(all.c)) 
all.c <- all.c %>% 
  separate(Station, c("Channel", "Station"), "_") %>% 
  select(PlotTimeID, Year, Channel, Station)
all.c$Year <- gsub("-.*", "", all.c$Year)

# There was a total of 372 sampling events, but 5 data sheets were lost:
#   Sample 58 from 2013 (Channel 12 Station 4 + 65); PlotTimeID 72
#   Sample 48 from 2013 (Channel 13 Station 4 + 78); PlotTimeID 92
#   Sample 36 from 2013 (Channel 13 Station 1 + 77); PlotTimeID 80
#   Sample 16 from 2014 (Channel 19 Station 1 + 4); PlotTimeID 155
#   Sample 17 from 2012 (Channel 19 Station 2 + 4); PlotTimeID 32
# And for one event, there was 0 plant cover (this row needs to be added back in):
#     Sample 8 from 2015 (Channel 21 Station 7 + 66); PlotTimeID 241 

# Hence, summarised dataframes should have 367 rows



# Summarise by plant species ----------------------------------------------

# Remove ground cover data for all channels and average cover across quadrats
plant.c13 <- all.c13 %>% 
  filter(!Common %in% c("Rock", "Gravel", "Soil", "Litter", "Biocrust")) %>% 
  group_by(Station, Year, Functional, Native, Common, Scientific) %>% 
  summarise(Cover = mean(Cover), .groups = "keep")

plant.c21 <- all.c21 %>% 
  filter(!Common %in% c("Rock", "Gravel", "Soil", "Litter", "Biocrust")) %>% 
  group_by(Station, Year, Functional, Native, Common, Scientific) %>% 
  summarise(Cover = mean(Cover), .groups = "keep")

plant.c19 <- all.c19 %>% 
  filter(!Common %in% c("Rock", "Gravel", "Soil", "Litter", "Biocrust")) %>% 
  group_by(Station, Year, Functional, Native, Common, Scientific) %>% 
  summarise(Cover = mean(Cover), .groups = "keep")

plant.c12 <- all.c12 %>% 
  filter(!Common %in% c("Rock", "Gravel", "Soil", "Litter", "Biocrust")) %>% 
  group_by(Station, Year, Functional, Native, Common, Scientific) %>% 
  summarise(Cover = mean(Cover), .groups = "keep")

# Combine channels and remove rows of 0% cover
#   Data sheets had common species already listed for ease, but since we are not interested in
#     tracking specific species, the 0s are not actually data
plant.all <- rbind(plant.c12, plant.c13, plant.c19, plant.c21)
plant.all <- plant.all %>% 
  filter(Cover > 0) %>% 
  separate(Station, c("Channel", "Station"), "_")



# Add and edit grouping columns ------------------------------------------

# Add station and channel treatment cols
plant.all <- plant.all |> 
  mutate(station.trt = case_when(
    str_detect(Station, "ORD") ~ "One rock dam",
    str_detect(Station, "BAF") ~ "Baffle",
    TRUE ~ "No treatment"))

# Add channel treatment
plant.all <- plant.all |> 
  mutate(channel.trt = case_when(
    str_detect(Channel, "12") ~ "Channel 12: Control",
    str_detect(Channel, "13") ~ "Channel 13: In-channel treatment",
    str_detect(Channel, "19") ~ "Channel 19: Upland treatment",
    TRUE ~ "Channel 21: In-channel treatment"))

# Add Treatment1 (station treatment, differentiate ORD & BAF)
plant.all <- plant.all |> 
  mutate(Treatment1 = case_when(
    str_detect(Channel, "19") ~ "Upland",
    TRUE ~ station.trt)) |> 
  mutate(Treatment1 = as.factor(Treatment1))

# Add Treatment2 (In-channel, Upland, Control)
plant.all$Treatment2 <- as.factor(gsub("^.*?: ", "", plant.all$channel.trt))

# Add Treatment3 (Treated, Control)
plant.all <- plant.all |> 
  mutate(Treatment3 = case_when(
    str_detect(channel.trt, "In-channel treatment") ~ "Treated",
    TRUE ~ "Control")) |> 
  mutate(Treatment3 = as.factor(Treatment3))

# Add woody/herbaceous
plant.all <- plant.all |> 
  mutate(woody = case_when(
    str_detect(Functional, c("grass|forb")) ~ "Herbaceous",
    TRUE ~ "Woody")) 

# Add tree/not tree
plant.all <- plant.all |> 
  mutate(tree = case_when(
    Functional == "Tree" ~ "tree",
    TRUE ~ "not tree"))

# Remove March 2012 sampling
plant.all <- plant.all |> 
  filter(Year != "2012-03-01")

# Add year.xaxis 
plant.all <-plant.all |> 
  mutate(year.xaxis = case_when(
    Year == "2012-11-01" ~ "2012-01-01",
    Year == "2013-11-01" ~ "2013-01-01",
    Year == "2014-11-01" ~ "2014-01-01",
    Year == "2015-11-01" ~ "2015-01-01",
    Year == "2018-11-01" ~ "2018-01-01",
    Year == "2021-11-01" ~ "2021-01-01")) |> 
  mutate(year.xaxis = as.Date(year.xaxis))

# Shorten Year to 4 digits
plant.all$Year <- as.factor(gsub("-.*", "", plant.all$Year))

# Add numeric sample ID
sampling <- bind_rows(all.c12, all.c13, all.c19, all.c21) |> 
  select(Station) |> 
  distinct(.keep_all = TRUE) |> 
  arrange(Station) |> 
  separate(Station, c("Channel", "Station"), "_") |> 
  arrange(desc(Channel)) |> 
  mutate(Sample = 1:62) |> 
  select(Sample, Channel, Station)
plant.all <- left_join(sampling, plant.all)

# Add PlotTimeID
plant.all <- left_join(all.c, plant.all)
plant.all |> 
  filter(is.na(Cover)) # inspect NAs 
#                       (they are the missing data sheet and 0 plant cover event; can be removed)
plant.all <- plant.all |> 
  filter(!is.na(Cover))



# Summarise by total, herb, notree, tree, shrub, invasive, annual ---------

# Create row for PlotTimeID 241 that must be added back in manually
plottime.241.plant <- data.frame(PlotTimeID = 241,
                        Sample = 8,
                        Channel = "Channel 21",
                        Station = "Station 07 + 66 BAF",
                        Year = "2015",
                        year.xaxis = as.Date("2015-01-01"),
                        station.trt = "Baffle",
                        channel.trt = "Channel 21: In-channel treatment",
                        Treatment1 = "Baffle",
                        Treatment2 = "In-channel treatment",
                        Treatment3 = "Treated",
                        Cover = 0)
  
# Summarise and add PlotTimeID 241 row
# Total
total.all <- plant.all %>% 
  group_by(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
           Treatment1, Treatment2, Treatment3) %>% 
  summarise(Cover = sum(Cover), .groups = "keep") |> 
  ungroup() |> 
  select(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
         Treatment1, Treatment2, Treatment3, Cover) |> 
  bind_rows(plottime.241.plant) |> 
  arrange(PlotTimeID)

# Herb
herb.all <- plant.all %>% 
  group_by(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
           Treatment1, Treatment2, Treatment3, woody) %>% 
  summarise(Cover = sum(Cover), .groups = "keep") |> 
  filter(woody == "Herbaceous") |> 
  ungroup() |> 
  select(-woody) |> 
  ungroup() |> 
  select(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
         Treatment1, Treatment2, Treatment3, Cover) |> 
  bind_rows(plottime.241.plant) |> 
  arrange(PlotTimeID)

# Notree
notree.all <- plant.all %>% 
  filter(tree == "not tree") %>% 
  group_by(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
           Treatment1, Treatment2, Treatment3) %>% 
  summarise(Cover = sum(Cover), .groups = "keep") |> 
  ungroup() |> 
  select(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
         Treatment1, Treatment2, Treatment3, Cover) |> 
  bind_rows(plottime.241.plant) |> 
  arrange(PlotTimeID)

# Tree
tree.all <- plant.all |> 
  filter(tree == "tree") |> 
  group_by(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
           Treatment1, Treatment2, Treatment3) %>% 
  summarise(Cover = sum(Cover), .groups = "keep") |> 
  ungroup() |> 
  select(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
         Treatment1, Treatment2, Treatment3, Cover) |> 
  bind_rows(plottime.241.plant) |> 
  arrange(PlotTimeID) # only 260 rows; sometimes there were no trees

tree.missing.plottimeid <- setdiff(total.all$PlotTimeID, tree.all$PlotTimeID)
tree.missing <- total.all |> 
  filter(PlotTimeID %in% tree.missing.plottimeid) |> 
  select(-Cover) # add back missing rows based on sampling info from total.all df
tree.missing$Cover <- rep(0, nrow(tree.missing))
tree.all <- tree.all |> 
  bind_rows(tree.missing) |> 
  arrange(PlotTimeID)

# Annual
annual.all <- plant.all |> 
  filter(Functional %in% c("Annual grass", "Annual forb")) |> 
  group_by(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
           Treatment1, Treatment2, Treatment3) %>% 
  summarise(Cover = sum(Cover), .groups = "keep") |> 
  ungroup() |> 
  select(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
         Treatment1, Treatment2, Treatment3, Cover) |> 
  bind_rows(plottime.241.plant) |> 
  arrange(PlotTimeID) # only 286 rows; sometimes there were no annuals

annual.missing.plottimeid <- setdiff(total.all$PlotTimeID, annual.all$PlotTimeID)
annual.missing <- total.all |> 
  filter(PlotTimeID %in% annual.missing.plottimeid) |> 
  select(-Cover) # add back missing rows based on sampling info from total.all df
annual.missing$Cover <- rep(0, nrow(annual.missing))
annual.all <- annual.all |> 
  bind_rows(annual.missing) |> 
  arrange(PlotTimeID)

# Invasive
invasive.all <- plant.all |> 
  filter(Native == "Invasive") |> 
  group_by(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
           Treatment1, Treatment2, Treatment3) %>% 
  summarise(Cover = sum(Cover), .groups = "keep") |> 
  ungroup() |> 
  select(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
         Treatment1, Treatment2, Treatment3, Cover) |> 
  bind_rows(plottime.241.plant) |> 
  arrange(PlotTimeID) # only 325 rows; sometimes there were no invasives

invasive.missing.plottimeid <- setdiff(total.all$PlotTimeID, invasive.all$PlotTimeID)
invasive.missing <- total.all |> 
  filter(PlotTimeID %in% invasive.missing.plottimeid) |> 
  select(-Cover) # add back missing rows based on sampling info from total.all df
invasive.missing$Cover <- rep(0, nrow(invasive.missing))
invasive.all <- invasive.all |> 
  bind_rows(invasive.missing) |> 
  arrange(PlotTimeID)


# Shrub
shrub.all <- plant.all %>% 
  group_by(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
           Treatment1, Treatment2, Treatment3, Functional) %>% 
  summarise(Cover = sum(Cover), .groups = "keep") |> 
  filter(Functional == "Shrub") |> 
  ungroup() |> 
  select(-Functional) |> 
  ungroup() |> 
  select(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
         Treatment1, Treatment2, Treatment3, Cover) |> 
  bind_rows(plottime.241.plant) |> 
  arrange(PlotTimeID)  # only 325 rows; sometimes there were no shrubs

shrub.missing.plottimeid <- setdiff(total.all$PlotTimeID, shrub.all$PlotTimeID)
shrub.missing <- total.all |> 
  filter(PlotTimeID %in% shrub.missing.plottimeid) |> 
  select(-Cover) # add back missing rows based on sampling info from total.all df
shrub.missing$Cover <- rep(0, nrow(shrub.missing))
shrub.all <- shrub.all |> 
  bind_rows(shrub.missing) |> 
  arrange(PlotTimeID)



# Richness and Shannon ----------------------------------------------------

# Remove annuals
plant.per <- plant.all |> 
  filter(!str_detect(Functional, "Annual"))

# By treatment and station
richness <- plant.per %>%  
  group_by(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
           Treatment1, Treatment2, Treatment3) %>% 
  summarise(rich = n_distinct(Common),
            .groups = "keep") 

shannon <- plant.per %>%  
  group_by(PlotTimeID, Sample, Channel, Station, Year, year.xaxis, station.trt, channel.trt,
           Treatment1, Treatment2, Treatment3) %>% 
  summarise(shan = diversity(Cover),
            .groups = "keep")

per.div <- left_join(richness, shannon) # 365 rows; missing PlotTimeID 241 and one other
setdiff(total.all$PlotTimeID, per.div$PlotTimeID) # also missing PlotTimeID 298
filter(plant.all, PlotTimeID == 298) # no perennial species measured for 298

# Add row for PlotTimeID 241 & 298 
add.plottime.div <- data.frame(PlotTimeID = c(241, 298),
                               Sample = c(8, 3),
                               Channel = rep("Channel 21", 2),
                               Station = c("Station 07 + 66 BAF",
                                           "Station 02 + 15 BAF"),
                               Year = c("2015", "2018"),
                               year.xaxis = c(as.Date("2015-01-01"),
                                              as.Date("2018-01-01")),
                               station.trt = rep("Baffle", 2),
                               channel.trt = rep("Channel 21: In-channel treatment", 2),
                               Treatment1 = rep("Baffle", 2),
                               Treatment2 = rep("In-channel treatment", 2),
                               Treatment3 = rep("Treated", 2),
                               rich = c(0, 0),
                               shan = c(0, 0))

per.div <- per.div |> 
  rbind(add.plottime.div) |> 
  arrange(PlotTimeID)



# Save dataframes ---------------------------------------------------------

write_csv(plant.all,
          file = "data/cleaned/Summarised-all_plant-species-cover.csv")
write_csv(total.all,
          file = "data/cleaned/Summarised-all_total-plant-cover.csv")
write_csv(herb.all,
          file = "data/cleaned/Summarised-all_herb-cover.csv")
write_csv(notree.all,
          file = "data/cleaned/Summarised-all_notree-cover.csv")
write_csv(tree.all,
          file = "data/cleaned/Summarised-all_tree-cover.csv")
write_csv(annual.all,
          file = "data/cleaned/Summarised-all_annual-cover.csv")
write_csv(invasive.all,
          file = "data/cleaned/Summarised-all_invasive-cover.csv")
write_csv(shrub.all,
          file = "data/cleaned/Summarised-all_shrub-cover.csv")
write_csv(per.div,
          file = "data/cleaned/Summarised-all_perennial-diversity.csv")



# Check sums --------------------------------------------------------------

check.sum <- total.all |> 
  select(PlotTimeID, Sample, Cover) |> 
  rename(total = Cover)
check.sum$herb <- herb.all$Cover
check.sum$notree <- notree.all$Cover
check.sum$tree <- tree.all$Cover
check.sum$shrub <- shrub.all$Cover

# Total - notree 
check.sum$totalMnotree <- check.sum$total - check.sum$notree
range(check.sum$totalMnotree) # total always greater

# notree + tree difference with total
check.sum$notreePtreeDiff <- (check.sum$notree + check.sum$tree) - check.sum$total
range(check.sum$notreePtreeDiff) # only differ by very small amounts due to rounding

# herb + shrub difference with notree
check.sum$herbPshrubDiff <- (check.sum$herb + check.sum$shrub) - check.sum$notree
range(check.sum$herbPshrubDiff) # only differ by very small amounts due to rounding

save.image("RData/Summarise-all-channels.RData")

