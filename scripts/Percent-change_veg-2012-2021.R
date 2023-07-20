# Status of this: not sure how to deal with 0 values

# Created: 2023-07-20

library(tidyverse)
library(car)

# Load data ---------------------------------------------------------------

total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
herb.all <- read.csv("data/cleaned/Summarised-all_herb-cover.csv") 
notree.all <- read.csv("data/cleaned/Summarised-all_notree-cover.csv")
per.div <- read.csv("data/cleaned/Summarised-all_perennial-diversity.csv")

# Change 0 values to NA to prevent dividing by 0
total.all$Cover[total.all$Cover == 0] <- NA
herb.all$Cover[herb.all$Cover == 0] <- NA
notree.all$Cover[notree.all$Cover == 0] <- NA
per.div$rich[per.div$rich == 0] <- NA
per.div$shan[per.div$shan == 0] <- NA

# Create general metadata without year information for each sample
grouping.cols <- total.all |> 
  select(Sample, Channel, Station, Treatment3) |> 
  distinct(.keep_all = TRUE) 




# Total cover -------------------------------------------------------------

# Select cols to pivot
total.long <- total.all |> 
  select(Sample, Year, Cover) 

# Pivot wider so every column is a sample and every row is a year
total.wide <- total.long |> 
  pivot_wider(names_from = Sample, values_from = Cover)

# Convert to time series object
total.ts <- total.wide[ , -1]
total.ts <- as.matrix(total.ts)
total.ts <- ts(total.ts, 1, 6, frequency = 1)

# Calculate percent change 
total.pc <- ((total.ts - stats::lag(total.ts)) / stats::lag(total.ts) * 100)

# Reformat as dataframe, add years, names & Treatment3
total.pc <- as.data.frame(total.pc)
total.pc$Year <- c("2012-2013", "2013-2014", "2014-2015", "2015-2018", "2018-2021")
total.pc <- total.pc |> 
  pivot_longer(!Year, names_to = "Sample", values_to = "dCover")
total.pc$Sample <- gsub("^.*\\.", "", total.pc$Sample)
total.pc$Sample <- as.numeric(total.pc$Sample)
total.pc <- left_join(grouping.cols, total.pc) |> 
  arrange(Sample) |> 
  select(Sample, Year, Channel, Station, Treatment3, dCover)

# Explore distribution
qqPlot(filter(total.pc, Treatment3 == "Treated")$dCover) # 
qqPlot(filter(total.lc, Treatment3 == "Control")$dCover) # not really normal?

hist()

write_csv(total.pc,
          file = "data/cleaned/Percent-change_total-cover.csv")
