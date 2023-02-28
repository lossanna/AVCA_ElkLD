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
  # separate 2012-2015 and 2015-2021
total.change.wide <- total.change.long |> 
  pivot_wider(names_from = Sample, values_from = Cover)
total.change1 <- total.change.wide[1:4, -c(1)]
total.change2 <- total.change.wide[4:6, -c(1)]

# Convert to time series object
total.change1 <- as.matrix(total.change1)
totalts1 <- ts(total.change1, 1, 4, frequency = 1)
total.change2 <- as.matrix(total.change2)
totalts2 <- ts(total.change2, 1, 3, frequency = 1)

# Calculate percent change 
total.pd1 <- log(totalts1) - log(stats::lag(totalts1, -1)) # 1-year interval for 2012-2015
total.pd2 <- (log(totalts2) - log(stats::lag(totalts2 -2))) / 3 # 3-year interval for 2015-2021

# Reformat as dataframe, add years, names & Treatment3
total.pd <- rbind(total.pd1, total.pd2)
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
    str_detect(total.pd$Name, "Channel 13|Channel 21") ~ "Treated")) |> 
  arrange(Sample)

write.csv(total.pd,
          file = "data/cleaned/Percent-difference_total-cover.csv")



save.image("RData/Percent-change-over-time.RData")

