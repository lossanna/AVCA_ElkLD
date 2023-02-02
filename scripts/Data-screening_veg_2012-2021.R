library(tidyverse)
library(agricolae)
library(car)

# Load data ---------------------------------------------------------------

plant.all <- read.csv("data/cleaned/Summarised-all_plant-species-cover.csv")
ground.all <- read.csv("data/cleaned/Summarised-all_ground-cover.csv")
total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
fungr.all <- read.csv("data/cleaned/Summarised-all_functional-group-cover.csv")
gfst.all <- read.csv("data/cleaned/Summarised-all_grass-forb-shrub-tree-cover.csv")
woody.all <- read.csv("data/cleaned/Summarised-all_woody-herb-cover.csv")
inwood.all <- read.csv("data/cleaned/Summarised-all_invasive-woody-cover.csv")
ingfst.all <- read.csv("data/cleaned/Summarised-all_invasive-grassforbshrubtree-cover.csv")
innat.all <- read.csv("data/cleaned/Summarised-all_invasive-native-cover.csv")


# Functions ---------------------------------------------------------------

# Add year as date and character, and retain Nov samples only
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
    filter(year.xaxis != "2012-03-01") 
  x$Year <- as.factor(gsub("-.*", "", x$Year))
  
  return(x)
}

# Check normality and homogeneity of variance
box.norm.homvar <- function(x, channel) {
  
  boxplot(Cover ~ Year, data = filter(x, Channel == channel))
  
  plot(tapply(filter(x, Channel == channel)$Cover,
              filter(x, Channel == channel)$Year, var),
       tapply(filter(x, Channel == channel)$Cover,
              filter(x, Channel == channel)$Year, mean))
  
  plot(aov(Cover ~ Year, data = filter(x, Channel == channel)))
  
}


# Data wrangling ----------------------------------------------------------

plant.all <- year(plant.all)
ground.all <- year(ground.all)
total.all <- year(total.all)
fungr.all <- year(fungr.all)
gfst.all <- year(gfst.all)
woody.all <- year(woody.all)
inwood.all <- year(inwood.all)
ingfst.all <- year(ingfst.all)
innat.all <- year(innat.all)


# Total cover -------------------------------------------------------------

box.norm.homvar(total.all, "Channel 12")
box.norm.homvar(total.all, "Channel 13")
box.norm.homvar(total.all, "Channel 19")
box.norm.homvar(total.all, "Channel 21")
