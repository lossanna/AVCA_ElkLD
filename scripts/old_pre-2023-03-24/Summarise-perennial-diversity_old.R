# Purpose: Cover is measured by species in 4-6 quadrats, which need to be 
#   converted to richness and Shannon diversity across all quadrats (summarised).
#   Also, add additional columns for grouping (channel and station treatment,
#     lifeform with native status).
# This is the cleaned data for perennial plant diversity from 2012-2021 that were used in older, 
#   initial analysis and are compatible with/needed for scripts (in scripts/old_pre-2023-03-24/ folder): 
#     Perennial-diversity-by-year-and-channel_v1.R
#     Perennial-diversity-by-year-and-channel_v2.R


library(tidyverse)
library(vegan)

# Load data ---------------------------------------------------------------

plant.all <- read.csv("data/cleaned/Summarised-all_plant-species-cover.csv")
meta <- read.table("data/cleaned/sequencing/sequencing_metadata.txt",
                   sep = "\t",
                   header = TRUE)

# Data wrangling ----------------------------------------------------------

# Add year as date and character, and retain Nov samples for perennial species only
year <- function(x) {
  x <- x %>% 
    mutate(year.date = as.Date(x$Year))
  
  x[ , "year.xaxis"] <- NA
  for(i in 1:nrow(x)) {
    if(x$Year[i] == "2012-11-01") {
      x$year.xaxis[i] <- "2012-01-01"
    } else if(x$Year[i] == "2013-11-01") {
      x$year.xaxis[i] <- "2013-01-01"
    } else if(x$Year[i] == "2014-11-01") {
      x$year.xaxis[i] <- "2014-01-01"
    } else if(x$Year[i] == "2015-11-01") {
      x$year.xaxis[i] <- "2015-01-01"
    } else if(x$Year[i] == "2018-11-01") {
      x$year.xaxis[i] <- "2018-01-01"
    } else if(x$Year[i] == "2021-11-01") {
      x$year.xaxis[i] <- "2021-01-01"
    } else {
      x$year.xaxis[i] <- "2012-03-01"
    }
  }
  x$year.xaxis <- as.Date(x$year.xaxis)
  
  x <- x %>% 
    filter(year.xaxis != "2012-03-01") %>% 
    filter(!str_detect(Functional, "Annual"))
  x$Year <- as.factor(gsub("-.*", "", x$Year))
  
  
  return(x)
}

plant.per <- year(plant.all)



# Richness and Shannon ----------------------------------------------------

# By treatment and station
richness <- plant.per %>%  
  group_by(Channel, Station, Year, year.date, year.xaxis, 
           channel.trt, station.trt) %>% 
  summarise(rich = n_distinct(Common),
            .groups = "keep") 

shannon <- plant.per %>%  
  group_by(Channel, Station, Year, year.date, year.xaxis, 
           channel.trt, station.trt) %>% 
  summarise(shan = diversity(Cover),
            .groups = "keep")

per.div <- left_join(richness, shannon)

write.csv(per.div,
          file = "data/cleaned/old-summarised/Summarised-all_perennial-diversity.csv",
          row.names = FALSE)
