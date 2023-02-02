library(tidyverse)
library(agricolae)
library(car)
library(vegan)

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

# Cover as response variable
box.norm.homvar <- function(x, channel) {
  
  boxplot(Cover ~ Year, data = filter(x, Channel == channel))
  
  plot(tapply(filter(x, Channel == channel)$Cover,
              filter(x, Channel == channel)$Year, var),
       tapply(filter(x, Channel == channel)$Cover,
              filter(x, Channel == channel)$Year, mean))
  
  plot(aov(Cover ~ Year, data = filter(x, Channel == channel)))
  
}

# Richness as response variable
box.norm.homvar.rich <- function(x, channel) {
  
  boxplot(rich ~ Year, data = filter(x, Channel == channel))
  
  plot(tapply(filter(x, Channel == channel)$rich,
              filter(x, Channel == channel)$Year, var),
       tapply(filter(x, Channel == channel)$rich,
              filter(x, Channel == channel)$Year, mean))
  
  plot(aov(rich ~ Year, data = filter(x, Channel == channel)))
  
}

# Diversity as response variable
box.norm.homvar.shan <- function(x, channel) {
  
  boxplot(shan ~ Year, data = filter(x, Channel == channel))
  
  plot(tapply(filter(x, Channel == channel)$shan,
              filter(x, Channel == channel)$Year, var),
       tapply(filter(x, Channel == channel)$shan,
              filter(x, Channel == channel)$Year, mean))
  
  plot(aov(shan ~ Year, data = filter(x, Channel == channel)))
  
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


# Functional group (gfst) -------------------------------------------------

box.norm.homvar(gfst.all, "Channel 12")
box.norm.homvar(gfst.all, "Channel 13")
box.norm.homvar(gfst.all, "Channel 19")
box.norm.homvar(gfst.all, "Channel 21")

# Grass
gfst.g <- gfst.all %>% 
  filter(gfst == "Grass")
box.norm.homvar(gfst.g, "Channel 12")
box.norm.homvar(gfst.g, "Channel 13")
box.norm.homvar(gfst.g, "Channel 19")
box.norm.homvar(gfst.g, "Channel 21")

# Forb
gfst.f <- gfst.all %>% 
  filter(gfst == "Forb")
box.norm.homvar(gfst.f, "Channel 12")
box.norm.homvar(gfst.f, "Channel 13")
box.norm.homvar(gfst.f, "Channel 19")
box.norm.homvar(gfst.f, "Channel 21")

# Shrub
gfst.s <- gfst.all %>% 
  filter(gfst == "Shrub")
box.norm.homvar(gfst.s, "Channel 12")
box.norm.homvar(gfst.s, "Channel 13")
box.norm.homvar(gfst.s, "Channel 19")
box.norm.homvar(gfst.s, "Channel 21")

# Tree
gfst.t <- gfst.all %>% 
  filter(gfst == "Tree")
box.norm.homvar(gfst.t, "Channel 12")
box.norm.homvar(gfst.t, "Channel 13")
box.norm.homvar(gfst.t, "Channel 19")
box.norm.homvar(gfst.t, "Channel 21")  


# Woody/herbaceous --------------------------------------------------------

box.norm.homvar(woody.all, "Channel 12")
box.norm.homvar(woody.all, "Channel 13")
box.norm.homvar(woody.all, "Channel 19")
box.norm.homvar(woody.all, "Channel 21")

# Herbaceous
woody.herb <- woody.all %>% 
  filter(woody == "Herbaceous")
box.norm.homvar(woody.herb, "Channel 12")
box.norm.homvar(woody.herb, "Channel 13")
box.norm.homvar(woody.herb, "Channel 19")
box.norm.homvar(woody.herb, "Channel 21")

# Woody
woody.wood <- woody.all %>% 
  filter(woody == "Woody")
box.norm.homvar(woody.wood, "Channel 12")
box.norm.homvar(woody.wood, "Channel 13")
box.norm.homvar(woody.wood, "Channel 19")
box.norm.homvar(woody.wood, "Channel 21")



# Perennial richness ------------------------------------------------------

# Define richness
richness <- plant.all %>%  
  group_by(Channel, Station, Year, year.date, year.xaxis, 
           channel.trt, station.trt) %>% 
  summarise(rich = n_distinct(Common),
            .groups = "keep")

box.norm.homvar.rich(richness, "Channel 12")
box.norm.homvar.rich(richness, "Channel 13")
box.norm.homvar.rich(richness, "Channel 19")
box.norm.homvar.rich(richness, "Channel 21")


# Perennial diversity -----------------------------------------------------

# Define Shannon diversity
shannon <- plant.all %>%  
  group_by(Channel, Station, Year, year.date, year.xaxis, 
           channel.trt, station.trt) %>% 
  summarise(shan = diversity(Cover),
            .groups = "keep")

box.norm.homvar.shan(shannon, "Channel 12")
box.norm.homvar.shan(shannon, "Channel 13")
box.norm.homvar.shan(shannon, "Channel 19")
box.norm.homvar.shan(shannon, "Channel 21")



save.image("RData/Data-screening_veg_2012-2021.RData")
