# Purpose: Create cleaned data that can be published.
#   Remove extra cols (like station.trt and Treatment1-2) and have record of removing them.

#   Data that can be used as is (literally just copied and pasted from data/cleaned/ to data/publish/):
#     data/cleaned/sequencing/bac-arc_clean_asv.txt
#     data/cleaned/sequencing/fungi_clean_asv.txt

# Created: 2023-08-29
# Last updated: 2023-09-05


library(tidyverse)
library(readxl)

# Load data ---------------------------------------------------------------

# Data that needs changing
herb.all <- read_csv("data/cleaned/Summarised-all_herb-cover.csv") 
shrub.all <- read_csv("data/cleaned/Summarised-all_shrub-cover.csv")
notree.all <- read_csv("data/cleaned/Summarised-all_notree-cover.csv")
invasive.all <- read_csv("data/cleaned/Summarised-all_invasive-cover.csv")
plant.all <- read_csv("data/cleaned/Summarised-all_plant-species-cover.csv")
per.div <- read_csv("data/cleaned/Summarised-all_perennial-diversity.csv")

dat.2021 <- read.csv("data/cleaned/Data-2021_clean.csv") 

meta <- read.table(file = "data/cleaned/sequencing/sequencing_metadata.txt",
                   header = TRUE,
                   sep = "\t")



# Data wrangling ----------------------------------------------------------

# Cover & diversity
herb.pub <- herb.all |> 
  select(Sample, Year, year.xaxis, Treatment3, Cover) |> 
  rename(Treatment = Treatment3) |> 
  arrange(Sample) |> 
  arrange(Year)

shrub.pub <- shrub.all |> 
  select(Sample, Year, year.xaxis, Treatment3, Cover) |> 
  rename(Treatment = Treatment3) |> 
  arrange(Sample) |> 
  arrange(Year)

notree.pub <- notree.all |> 
  select(Sample, Year, year.xaxis, Treatment3, Cover) |> 
  rename(Treatment = Treatment3) |> 
  arrange(Sample) |> 
  arrange(Year)

invasive.pub <- invasive.all |> 
  select(Sample, Year, year.xaxis, Treatment3, Cover) |> 
  rename(Treatment = Treatment3) |> 
  arrange(Sample) |> 
  arrange(Year)

plant.pub <- plant.all |> 
  select(Sample, Year, Treatment3, Functional, Native, Common, Scientific, Cover) |> 
  rename(Treatment = Treatment3) |> 
  arrange(Sample) |> 
  arrange(Year)

perdiv.pub <- per.div |> 
  select(Sample, Year, year.xaxis, Treatment3, rich, shan) |> 
  rename(Treatment = Treatment3) |> 
  arrange(Sample) |> 
  arrange(Year)


# 2021 data
dat.2021.pub <- dat.2021 |> 
  select(-Name, -Channel, -Station, -CN_ratio, -barc.shannon, -barc.betadisp.3,
         -fungi.shannon, -fungi.betadisp.3, -dElev, -dElev_corrected) |> 
  rename(Treatment = Treatment3,
         dElev_corrected = dElev_corrected2)


# Sequencing
meta <- meta |> 
  select(Sample, Treatment3) |> 
  rename(Treatment = Treatment3)


# Write to CSV ------------------------------------------------------------

write_csv(herb.pub,
          file = "data/publish/Herb-cover_2012-2021.csv")
write_csv(shrub.pub,
          file = "data/publish/Shrub-cover_2012-2021.csv")
write_csv(notree.pub,
          file = "data/publish/Herb-and-shrub-cover_2012-2021.csv")
write_csv(invasive.pub,
          file = "data/publish/Invasive-cover_2012-2021.csv")
write_csv(plant.pub,
          file = "data/publish/Species-cover_2012-2021.csv")
write_csv(perdiv.pub,
          file = "data/publish/Perennial-plant-diversity_2012-2021.csv")

write_csv(dat.2021.pub,
          file = "data/publish/Veg-soil-elev_2021.csv")

write_csv(meta,
          file = "data/publish/sequencing_metadata.csv")
