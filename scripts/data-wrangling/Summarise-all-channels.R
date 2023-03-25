# Purpose: Species cover is measured in 4-6 quadrats. Cover needs to be averaged (summarised).
# For perennial diversity, quadrats need to be converted to richness and Shannon diversity
# across all quadrats.

# Columns added for grouping: channel and station treatment, Treatment 1-3,
#   lifeform with native status).
# Columns added for formatting year/date as just year (20XX), and 
#   year formatted for x-axis when graphing (20XX-01-01).
# These are the cleaned data sheets for plant cover and diversity from 2012-2021.


library(tidyverse)
library(vegan)

# Load data ---------------------------------------------------------------

all.c12 <- read.csv("data/cleaned/C12-cover.csv")
all.c13 <- read.csv("data/cleaned/C13-cover.csv")
all.c19 <- read.csv("data/cleaned/C19-cover.csv")
all.c21 <- read.csv("data/cleaned/C21-cover.csv")


# Summarise by plant species ----------------------------------------------

# Remove ground cover data for all channels
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

# Add grouping cols
# Add station and channel treatment cols
plant.all <- plant.all |> 
  mutate(station.trt = case_when(
    str_detect(Station, "ORD") ~ "One rock dam",
    str_detect(Station, "BAF") ~ "Baffle",
    TRUE ~ "No treatment"))

# Add channel treatment
plant.all <- plant.all |> 
  mutate(channel.trt = case_when(
    str_detect(Channel, "12") ~ "Channel 12: No treatment",
    str_detect(Station, "13") ~ "Channel 13: In-channel treatment",
    str_detect(Station, "19") ~ "Channel 19: Upland treatment",
    TRUE ~ "Channel 21: In-channel treatment"))

# Add Treatment1
plant.all <- plant.all |> 
  mutate(Treatment1 = case_when(
    str_detect(Station, "19") ~ "Upland",
    TRUE ~ station.trt)) |> 
  mutate(Treatment1 = as.factor(Treatment1))

# Add Treatment2
plant.all$Treatment2 <- as.factor(gsub("^.*?: ", "", plant.all$channel.trt))

# Add Treatment3
plant.all <- plant.all |> 
  mutate(Treatment3 = case_when(
    str_detect("In-channel treatment", channel.trt) ~ "Treated",
    TRUE ~ "Control")) |> 
  mutate(Treatment3 = as.factor(Treatment3))

# Add woody/herbaceous
plant.all <- plant.all |> 
  mutate(woody = case_when(
    str_detect(Functional, c("grass|forb")) ~ "Herbaceous",
    TRUE ~ "Woody")) 

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


# Summarise by total and herb ---------------------------------------------

total.all <- plant.all %>% 
  group_by(Channel, Station, Year, station.trt, channel.trt,
           Treatment1, Treatment2, Treatment3, year.xaxis) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")

herb.all <- plant.all %>% 
  group_by(Channel, Station, Year, station.trt, channel.trt, woody,
           Treatment1, Treatment2, Treatment3, year.xaxis) %>% 
  summarise(Cover = sum(Cover), .groups = "keep") |> 
  filter(woody == "Herbaceous")


# Richness and Shannon ----------------------------------------------------

# Remove annuals
plant.per <- plant.all |> 
  filter(!str_detect(Functional, "Annual"))

# By treatment and station
richness <- plant.per %>%  
  group_by(Channel, Station, Year, station.trt, channel.trt,
           Treatment1, Treatment2, Treatment3, year.xaxis) %>% 
  summarise(rich = n_distinct(Common),
            .groups = "keep") 

shannon <- plant.per %>%  
  group_by(Channel, Station, Year, station.trt, channel.trt,
           Treatment1, Treatment2, Treatment3, year.xaxis) %>% 
  summarise(shan = diversity(Cover),
            .groups = "keep")

per.div <- left_join(richness, shannon)


# Assign record number for each sampling event ----------------------------

# Create ID
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

# Add to ID dataframes (remove NAs because some data sheets were lost)
plant.all <- left_join(all.c, plant.all) %>% 
  filter(!is.na(Cover))
total.all <- left_join(all.c, total.all) %>% 
  filter(!is.na(Cover))
herb.all <- left_join(all.c, herb.all) %>% 
  filter(!is.na(Cover))
per.div <- left_join(all.c, per.div) |> 
  filter(!is.na(shan))



# Save dataframes ---------------------------------------------------------

write_csv(plant.all,
          file = "data/cleaned/Summarised-all_plant-species-cover.csv")
write_csv(total.all,
          file = "data/cleaned/Summarised-all_total-plant-cover.csv")
write_csv(herb.all,
          file = "data/cleaned/Summarised-all_herb-cover.csv")
write_csv(per.div,
          file = "data/cleaned/Summarised-all_perennial-diversity.csv")


save.image("RData/Summarise-all-channels.RData")

