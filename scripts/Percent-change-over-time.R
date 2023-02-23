library(tidyverse)

# Load data ---------------------------------------------------------------

total.sum <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
meta.raw <- read.table("data/cleaned/sequencing/sequencing_metadata.txt",
                   sep = "\t", header = TRUE)

# Data wrangling ----------------------------------------------------------

# Format meta to connect Names with Sample no.
meta <- meta.raw |> 
  select(Sample, Name)


# Total cover -------------------------------------------------------------

# Remove March samples, format to join with names from meta
total.change <- total.sum |> 
  select(Year, Channel, Station, Cover) |> 
  filter(!str_detect(Year, "-03")) |> 
  mutate(Year = gsub("-.*", "", Year))
total.change$Name <- paste0(total.change$Channel, ", ", total.change$Station)
total.change <- total.change |> 
  select(-Channel, -Station) |> 
  mutate(Year = as.numeric(Year))

# Add Sample no. from meta
total.change <- left_join(total.change, meta)
total.change <- total.change |> 
  select(-Name)

total.change.long <- total.change

# Pivot wider so every column is a sample and every row is a year
total.change.wide <- total.change.long |> 
  pivot_wider(names_from = Sample, values_from = Cover)
total.change <- total.change.wide |> 
  select(-Year)

# Convert to time series object
total.change <- as.matrix(total.change)
totalts <- ts(total.change, 1, 6, frequency = 1)

# Calculate percent change 
total.pd <- totalts / stats::lag(totalts, -1) -1

# Reformat as dataframe, add years, names & Treatment3
total.pd <- as.data.frame(total.pd)
total.pd$Year <- c("2012-2013", "2013-2014", "2014-2015", "2015-2018", "2018-2021")
total.pd <- total.pd |> 
  pivot_longer(!Year, names_to = "Sample", values_to = "dCover")
total.pd$Sample <- gsub("^.*?\\.", "", total.pd$Sample)
total.pd$Sample <- as.numeric(total.pd$Sample)
total.pd <- left_join(total.pd, meta)
total.pd <- total.pd |> 
  mutate(Treatment3 = case_when(
    str_detect(total.pd$Name, "Channel 12|Channel 19") ~ "Control",
    str_detect(total.pd$Name, "Channel 13|Channel 21") ~ "Treated"))



# Percent change in total cover, 2012-2015
total.change.12.15 <- total.change.wide[1:4, -c(1)]
total.change.12.15 <- as.matrix(total.change.12.15)
totalts1215 <- ts(total.change.12.15, 1, 4, frequency = 1)

total.pd1215 <- totalts1215 / stats::lag(totalts1215, -1) -1
total.pd1215 <- as.data.frame(total.pd1215)
total.pd1215$Year <- c("2012-2013", "2013-2014", "2015-2016")
total.pd1215 <- total.pd1215 |> 
  pivot_longer(!Year, names_to = "Sample", values_to = "dCover")
total.pd1215$Sample <- gsub("^.*?\\.", "", total.pd1215$Sample)
total.pd1215 <- left_join(total.pd1215, meta)





save.image("RData/Percent-change-over-time.RData")
