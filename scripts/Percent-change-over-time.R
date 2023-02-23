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

total.diff.long <- total.diff

total.diff <- total.diff.long |> 
  pivot_wider(names_from = Sample, values_from = Cover)

total.diff.12.15 <- total.diff[1:4, -c(1)]
total.diff.12.15 <- as.matrix(total.diff.12.15)
ts1215 <- ts(total.diff.12.15, 1, 4, frequency = 1)

pcdf1215 <- ts1215 / stats::lag(ts1215, -1) -1
pcdf1215 <- as.data.frame(pcdf1215)
pcdf1215$Year <- c("2012-2013", "2013-2014", "2015-2016")
pcdf1215 <- pcdf1215 |> 
  pivot_longer(!Year, names_to = "Sample", values_to = "dCover")
pcdf1215$Sample <- gsub("^.*?\\.", "", pcdf1215$Sample)
pcdf1215 <- left_join(pcdf1215, meta)

save.image("RData/Percent-change-over-time.RData")
