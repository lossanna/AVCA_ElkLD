library(tidyverse)
library(cvequality)

# Load data ---------------------------------------------------------------

total.all <- read_csv("data/cleaned/Summarised-all_total-plant-cover.csv")

precip <- read.table("data/PimaCounty_precip/PimaCounty_precip_2012-2021.txt",
                     sep = "\t", header = TRUE)
precip$year.xaxis <- as.Date(precip$year.xaxis)


# Data wrangling ----------------------------------------------------------

total.all <- total.all %>% 
  mutate(Year = gsub("-.*", "", total.all$Year),
         Treatment2 = gsub("^.*?: ", "", total.all$channel.trt))


# By Treatment2 -----------------------------------------------------------

# Total plant cover
with(total.all, asymptotic_test(Cover, Treatment2)) # p = 0.04598678

totalin.cn.all <- total.all %>% 
  filter(Treatment2 %in% c("In-channel treatment", "No treatment"))
with(totalin.cn.all, asymptotic_test(Cover, Treatment2)) # p = 0.01270811

totalin.up.all <- total.all %>% 
  filter(Treatment2 %in% c("Upland treatment", "In-channel treatment"))
with(totalin.up.all, asymptotic_test(Cover, Treatment2)) # NS

totalup.cn.all <- total.all %>% 
  filter(Treatment2 %in% c("Upland treatment", "No treatment"))
with(totalup.cn.all, asymptotic_test(Cover, Treatment2)) # NS


# 2012-2015 precipitation
(6.46 - 10.98) / 10.98 # 41% decrease
total.12.15 <- total.all %>% 
  filter(Year %in% c("2012", "2013", "2014", "2015"))

with(total.12.15, asymptotic_test(Cover, Treatment2)) # NS

totalin.cn.12.15 <- total.12.15 %>% 
  filter(Treatment2 %in% c("In-channel treatment", "No treatment"))
with(totalin.cn.12.15, asymptotic_test(Cover, Treatment2)) # NS

totalin.up.12.15 <- total.12.15 %>% 
  filter(Treatment2 %in% c("Upland treatment", "In-channel treatment"))
with(totalin.up.12.15, asymptotic_test(Cover, Treatment2)) # NS

totalup.cn.12.15 <- total.12.15 %>% 
  filter(Treatment2 %in% c("Upland treatment", "No treatment"))
with(totalup.cn.12.15, asymptotic_test(Cover, Treatment2)) # NS



# By channel --------------------------------------------------------------

# Total plant cover
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
