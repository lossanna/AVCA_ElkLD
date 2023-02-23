library(tidyverse)

# Load data ---------------------------------------------------------------

total.sum <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
meta.raw <- read.table("data/cleaned/sequencing/sequencing_metadata.txt",
                   sep = "\t", header = TRUE)

# Data wrangling ----------------------------------------------------------

meta <- meta.raw |> 
  select(Sample, Name)
meta$Sample[1:9] <- paste0(0, meta$Sample[1:9])
meta$Sample <- paste0("x", meta$Sample)

total.diff <- total.sum |> 
  select(Year, Channel, Station, Cover) |> 
  filter(!str_detect(Year, "-03")) |> 
  mutate(Year = gsub("-.*", "", Year))
total.diff$Name <- paste0(total.diff$Channel, ", ", total.diff$Station)
total.diff <- total.diff |> 
  select(-Channel, -Station) |> 
  mutate(Year = as.numeric(Year))

total.diff <- left_join(total.diff, meta)
total.diff <- total.diff |> 
  select(-Name)

test <- total.diff |> 
  pivot_wider(names_from = Sample, values_from = Cover)

test <- total.diff |> 
  group_by(Sample) |> 
  mutate(d12.13 = diff(Cover[Year %in% 2012:2021]))
