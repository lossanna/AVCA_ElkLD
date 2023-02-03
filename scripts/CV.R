library(tidyverse)
library(cvequality)

# Load data ---------------------------------------------------------------

total.all <- read_csv("data/cleaned/Summarised-all_total-plant-cover.csv")


# Data wrangling ----------------------------------------------------------

total.all <- total.all %>% 
  mutate(Year = gsub("-.*", "", total.all$Year),
         Treatment = gsub("^.*?: ", "", total.all$channel.trt))


# Total plant cover -------------------------------------------------------

# Between all channels
with(total.all, asymptotic_test(Cover, Channel))

# Between Channels 12 and 13
total1213.all <- total.all %>% 
  filter(Channel %in% c("Channel 12", "Channel 13"))
with(total1213.all, asymptotic_test(Cover, Channel)) # NS

# Between Channels 19 and 13
total1913.all <- total.all %>% 
  filter(Channel %in% c("Channel 19", "Channel 13"))
with(total1913.all, asymptotic_test(Cover, Channel)) # p = 0.001092162

# Between Channels 12 and 21
total1221.all <- total.all %>% 
  filter(Channel %in% c("Channel 12", "Channel 21"))
with(total1221.all, asymptotic_test(Cover, Channel)) # p = 0.03117101

# Between Channels 19 and 21
total1921.all <- total.all %>% 
  filter(Channel %in% c("Channel 19", "Channel 21"))
with(total1921.all, asymptotic_test(Cover, Channel)) # NS

# Between Channels 12 and 19
total1219.all <- total.all %>% 
  filter(Channel %in% c("Channel 12", "Channel 19"))
with(total1219.all, asymptotic_test(Cover, Channel)) # NS

# Between Channels 13 and 21
total1321.all <- total.all %>% 
  filter(Channel %in% c("Channel 13", "Channel 21"))
with(total1321.all, asymptotic_test(Cover, Channel)) # p = 0.0004935618




save.image("RData/CV.RData")
