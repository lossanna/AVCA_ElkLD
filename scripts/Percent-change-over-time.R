library(tidyverse)

# Load data ---------------------------------------------------------------

total.sum <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
herb.sum <- read.csv("data/cleaned/Summarised-all_woody-herb-cover.csv") |> 
  filter(woody == "Herbaceous")
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
total.pd1 <- log(totalts1) - log(stats::lag(totalts1, - 1)) # 1-year interval for 2012-2015
total.pd2 <- (log(totalts2) - log(stats::lag(totalts2 - 1))) / 3 # 3-year interval for 2015-2021

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


# Plot by Treatment3
ggplot(total.pd, aes(x = Treatment3, y = dCover)) +
  geom_boxplot() +
  geom_jitter() 
  
t.test(filter(total.pd, Treatment3 == "Treated")$dCover, 
       filter(total.pd, Treatment3 == "Control")$dCover) # NS



# Herbaceous cover --------------------------------------------------------

# Remove March samples, format to join with names from meta
herb.change <- herb.sum |> 
  select(Year, Channel, Station, Cover) |> 
  filter(!str_detect(Year, "-03")) |> 
  mutate(Year = gsub("-.*", "", Year))
herb.change$Name <- paste0(herb.change$Channel, ", ", herb.change$Station)
herb.change <- herb.change |> 
  select(-Channel, -Station) |> 
  mutate(Year = as.numeric(Year))

# Add Sample no. from meta
herb.change <- left_join(herb.change, meta)
herb.change <- herb.change |> 
  select(-Name)
herb.change.long <- herb.change

# Pivot wider so every column is a sample and every row is a year
  # separate 2012-2015 and 2015-2021
herb.change.wide <- herb.change.long |> 
  pivot_wider(names_from = Sample, values_from = Cover)
herb.change1 <- herb.change.wide[1:4, -c(1)]
herb.change2 <- herb.change.wide[4:6, -c(1)]

# Convert to time series object
herb.change1 <- as.matrix(herb.change1)
herbts1 <- ts(herb.change1, 1, 4, frequency = 1)
herb.change2 <- as.matrix(herb.change2)
herbts2 <- ts(herb.change2, 1, 3, frequency = 1)

# Calculate percent change 
herb.pd1 <- log(herbts1) - log(stats::lag(herbts1, - 1)) # 1-year interval for 2012-2015
herb.pd2 <- (log(herbts2) - log(stats::lag(herbts2 - 1))) / 3 # 3-year interval for 2015-2021

# Reformat as dataframe, add years, names & Treatment3
herb.pd <- rbind(herb.pd1, herb.pd2)
herb.pd <- as.data.frame(herb.pd)
herb.pd$Year <- c("2012-2013", "2013-2014", "2014-2015", "2015-2018", "2018-2021")
herb.pd <- herb.pd |> 
  pivot_longer(!Year, names_to = "Sample", values_to = "dCover")
herb.pd$Sample <- gsub("^.*?\\.", "", herb.pd$Sample)
herb.pd$Sample <- as.numeric(herb.pd$Sample)
herb.pd <- left_join(herb.pd, meta)
herb.pd <- herb.pd |> 
  mutate(Treatment3 = case_when(
    str_detect(herb.pd$Name, "Channel 12|Channel 19") ~ "Control",
    str_detect(herb.pd$Name, "Channel 13|Channel 21") ~ "Treated")) |> 
  arrange(Sample)

write.csv(herb.pd,
          file = "data/cleaned/Percent-difference_herb-cover.csv")


# Plot by Treatment3
ggplot(herb.pd, aes(x = Treatment3, y = dCover)) +
  geom_boxplot() +
  geom_jitter() 

t.test(filter(herb.pd, Treatment3 == "Treated")$dCover, 
       filter(herb.pd, Treatment3 == "Control")$dCover) # NS


save.image("RData/Percent-change-over-time.RData")

