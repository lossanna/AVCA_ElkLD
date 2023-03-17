# Purpose: Analysis and figures for the AVCA data sharing community event on 2022-01-18, updated Sept 2022.

# Needed to be updated because previously, I had moved some data into different rows in the Excel sheets, 
#   but then realized later that was a misinterpretation and the data were entered correctly.
#   The original analysis are in a separate repo, as I did this pre-GitHub and didn't want to transfer
#   incorrect analysis. This script can stand on its own in this repo.


library(tidyverse)
library(vegan)
library(plotrix)
library(RColorBrewer)
library(readxl)

# Load data ---------------------------------------------------------------

fungr.all <- read.csv("data/cleaned/Summarised-all_functional-group-cover.csv")
gfst.all <- read.csv("data/cleaned/Summarised-all_grass-forb-shrub-tree-cover.csv")
ground.all <- read.csv("data/cleaned/Summarised-all_ground-cover.csv")
ingfst.all <- read.csv("data/cleaned/Summarised-all_invasive-grassforbshrubtree-cover.csv")
innat.all <- read.csv("data/cleaned/Summarised-all_invasive-native-cover.csv")
inwood.all <- read.csv("data/cleaned/Summarised-all_invasive-woody-cover.csv")
plant.all <- read.csv("data/cleaned/Summarised-all_plant-species-cover.csv")
total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
woody.all <- read.csv("data/cleaned/Summarised-all_woody-herb-cover.csv")

# Exclude March sampling with missing data sheets
plant.all.nov <- plant.all %>% 
  filter(Year != "2012-03-01")

# Rain data
rain <- read_xlsx("data/Excel_LO_edited/Elk_LD Rain data_LO.xlsx", sheet = "R_LO")
rain$Date <- as.Date(rain$Date, format = "%Y-%m-%d")


# Common species ----------------------------------------------------------

common.plants <- plant.all %>% 
  ungroup() %>% 
  filter(Cover >= 15) 
common.plants <- count(common.plants, Common)
common.plants <- common.plants[order(common.plants$n, decreasing = TRUE), ]




###### Entire channel averages ############################################

# For ANOVA and KW analysis, see Cover-by-year-and-channel.R and Perennial-diversity-by-year-and-channel.R
# For test of difference in CV, see CV.R



# Total plant cover -------------------------------------------------------

total.channel <- total.all %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(Year != "2012-03-01")


for(i in 1:nrow(total.channel)) {
  if(total.channel$Year[i] == "2012-11-01") {
    total.channel$Year[i] <- "2012-01-01"
  } else if(total.channel$Year[i] == "2013-11-01") {
    total.channel$Year[i] <- "2013-01-01"
  } else if(total.channel$Year[i] == "2014-11-01") {
    total.channel$Year[i] <- "2014-01-01"
  } else if(total.channel$Year[i] == "2015-11-01") {
    total.channel$Year[i] <- "2015-01-01"
  } else if(total.channel$Year[i] == "2018-11-01") {
    total.channel$Year[i] <- "2018-01-01"
  } else {
    total.channel$Year[i] <- "2021-01-01"
  }
}

total.channel$Channel <- factor(total.channel$Channel, 
                                levels = c("Channel 12", "Channel 19",
                                           "Channel 13", "Channel 21"))
total.channel$Year <- as.Date(total.channel$Year)

total.plot.ds <- ggplot(total.channel, aes(x = Year, y = mean, 
                                           group = Channel, color = Channel, shape = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  scale_color_manual(values = c("red", "#1F78B4", "#33A02C", "#33A02C")) +
  scale_shape_manual(values = c(17, 16, 15, 16)) +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
total.plot.ds


total.channel[ , "channel.trt"] <- NA
for(i in 1:nrow(total.channel)) {
  if(total.channel$Channel[i] == "Channel 12") {
    total.channel$channel.trt[i] <- "Channel 12: No treatment"
  } else if(total.channel$Channel[i] == "Channel 13") {
    total.channel$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(total.channel$Channel[i] == "Channel 19") {
    total.channel$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    total.channel$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

total.plot.fw.ds <- ggplot(total.channel, aes(x = Year, y = mean, 
                                              group = channel.trt, color = channel.trt)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
total.plot.fw.ds

tiff("output_figs/2022-01-28_Data-sharing_updated-2022-09/Total-plant-cover_FW.tiff", 
     units = "in", height = 5, width = 7, res = 300)
total.plot.fw.ds
dev.off()


total.table.ds <- data.frame(Channel = c("Channel 12", "Channel 19", "Channel 13", "Channel 21"),
                             Cover_2012 = c(filter(total.channel, Year == "2012-01-01" & Channel == "Channel 12")$mean,
                                            filter(total.channel, Year == "2012-01-01" & Channel == "Channel 19")$mean,
                                            filter(total.channel, Year == "2012-01-01" & Channel == "Channel 13")$mean,
                                            filter(total.channel, Year == "2012-01-01" & Channel == "Channel 21")$mean),
                             Cover_2021 = c(filter(total.channel, Year == "2021-01-01" & Channel == "Channel 12")$mean,
                                            filter(total.channel, Year == "2021-01-01" & Channel == "Channel 19")$mean,
                                            filter(total.channel, Year == "2021-01-01" & Channel == "Channel 13")$mean,
                                            filter(total.channel, Year == "2021-01-01" & Channel == "Channel 21")$mean))
total.table.ds$Cover_2012 <- round(total.table.ds$Cover_2012, 0)
total.table.ds$Cover_2021 <- round(total.table.ds$Cover_2021, 0)


# Channels 19 & 13
total1913.channel <- total.channel %>% 
  filter(Channel %in% c("Channel 19", "Channel 13"))

total1913.plot.ds <- ggplot(total1913.channel, aes(x = Year, y = mean, 
                                                   group = Channel, color = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover: 19 & 13") +
  scale_color_manual(values = c("#1F78B4", "#33A02C")) +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
total1913.plot.ds

tiff("output_figs/2022-01-28_Data-sharing_updated-2022-09/Total1913-plant-cover.tiff", 
     units = "in", height = 4, width = 6, res = 300)
total1913.plot.ds
dev.off()


# Channels 12 & 21
total1221.channel <- total.channel %>% 
  filter(Channel %in% c("Channel 12", "Channel 21"))

total1221.plot.ds <- ggplot(total1221.channel, aes(x = Year, y = mean, 
                                                   group = Channel, color = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover: 12 & 21") +
  scale_color_manual(values = c("red", "#33A02C")) +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
total1221.plot.ds

tiff("output_figs/2022-01-28_Data-sharing_updated-2022-09/Total1221-plant-cover.tiff", 
     units = "in", height = 4, width = 6, res = 300)
total1221.plot.ds
dev.off()



# Ground cover ------------------------------------------------------------

ground.channel <- ground.all %>% 
  group_by(Channel, Year, Common) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(ground.channel)) {
  if(ground.channel$Year[i] == "2012-11-01") {
    ground.channel$Year[i] <- "2012-01-01"
  } else if(ground.channel$Year[i] == "2013-11-01") {
    ground.channel$Year[i] <- "2013-01-01"
  } else if(ground.channel$Year[i] == "2014-11-01") {
    ground.channel$Year[i] <- "2014-01-01"
  } else if(ground.channel$Year[i] == "2015-11-01") {
    ground.channel$Year[i] <- "2015-01-01"
  } else if(ground.channel$Year[i] == "2018-11-01") {
    ground.channel$Year[i] <- "2018-01-01"
  } else {
    ground.channel$Year[i] <- "2021-01-01"
  }
}

ground.channel$Year <- as.Date(ground.channel$Year)

ground.channel[ , "channel.trt"] <- NA
for(i in 1:nrow(ground.channel)) {
  if(ground.channel$Channel[i] == "Channel 12") {
    ground.channel$channel.trt[i] <- "Channel 12: No treatment"
  } else if(ground.channel$Channel[i] == "Channel 13") {
    ground.channel$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(ground.channel$Channel[i] == "Channel 19") {
    ground.channel$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    ground.channel$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

ground.channel.grs <- ground.channel %>% 
  filter(Common %in% c("Gravel", "Rock", "Soil"))

ground.grs.plot.ds <- ggplot(ground.channel.grs, aes(x = Year, y = mean, 
                                                     group = Common, color = Common, shape = Common)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Gravel, rock, and soil cover") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ground.grs.plot.ds

tiff("output_figs/2022-01-28_Data-sharing_updated-2022-09/GravelRockSoil-cover.tiff", 
     units = "in", height = 5, width = 7, res = 300)
ground.grs.plot.ds
dev.off()


ground.channel.soil <- ground.channel %>% 
  filter(Common == "Soil")

ground.soil.plot.ds <- ggplot(ground.channel.soil, aes(x = Year, y = mean, 
                                                       group = channel.trt, color = channel.trt)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Soil cover") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
ground.soil.plot.ds

tiff("output_figs/2022-01-28_Data-sharing_updated-2022-09/Soil-cover.tiff", 
     units = "in", height = 5, width = 7, res = 300)
ground.soil.plot.ds
dev.off()


soil.table.ds <- data.frame(Channel = c("Channel 12", "Channel 19", "Channel 13", "Channel 21"),
                            Cover_2012 = c(filter(ground.channel.soil, Year == "2012-01-01" & Channel == "Channel 12")$mean,
                                           filter(ground.channel.soil, Year == "2012-01-01" & Channel == "Channel 19")$mean,
                                           filter(ground.channel.soil, Year == "2012-01-01" & Channel == "Channel 13")$mean,
                                           filter(ground.channel.soil, Year == "2012-01-01" & Channel == "Channel 21")$mean),
                            Cover_2021 = c(filter(ground.channel.soil, Year == "2021-01-01" & Channel == "Channel 12")$mean,
                                           filter(ground.channel.soil, Year == "2021-01-01" & Channel == "Channel 19")$mean,
                                           filter(ground.channel.soil, Year == "2021-01-01" & Channel == "Channel 13")$mean,
                                           filter(ground.channel.soil, Year == "2021-01-01" & Channel == "Channel 21")$mean))
soil.table.ds$Cover_2012 <- round(soil.table.ds$Cover_2012, 0)
soil.table.ds$Cover_2021 <- round(soil.table.ds$Cover_2021, 0)



# Functional group (gfst) -------------------------------------------------

gfst.channel <- gfst.all %>% 
  group_by(Channel, Year, gfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(gfst.channel)) {
  if(gfst.channel$Year[i] == "2012-11-01") {
    gfst.channel$Year[i] <- "2012-01-01"
  } else if(gfst.channel$Year[i] == "2013-11-01") {
    gfst.channel$Year[i] <- "2013-01-01"
  } else if(gfst.channel$Year[i] == "2014-11-01") {
    gfst.channel$Year[i] <- "2014-01-01"
  } else if(gfst.channel$Year[i] == "2015-11-01") {
    gfst.channel$Year[i] <- "2015-01-01"
  } else if(gfst.channel$Year[i] == "2018-11-01") {
    gfst.channel$Year[i] <- "2018-01-01"
  } else {
    gfst.channel$Year[i] <- "2021-01-01"
  }
}

gfst.channel$Year <- as.Date(gfst.channel$Year)

gfst.channel[ , "channel.trt"] <- NA
for(i in 1:nrow(gfst.channel)) {
  if(gfst.channel$Channel[i] == "Channel 12") {
    gfst.channel$channel.trt[i] <- "Channel 12: No treatment"
  } else if(gfst.channel$Channel[i] == "Channel 13") {
    gfst.channel$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(gfst.channel$Channel[i] == "Channel 19") {
    gfst.channel$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    gfst.channel$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

gfst.channel$gfst <- factor(gfst.channel$gfst, 
                            levels = c("Grass", "Forb", "Shrub", "Tree"))

gfst.plot.ds <- ggplot(gfst.channel, aes(x = Year, y = mean, 
                                         group = gfst, color = gfst, shape = gfst)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE),
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Grass, forb, shrub, and tree cover") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
gfst.plot.ds


# Functional group (gfs) --------------------------------------------------

gfs.channel <- gfst.all %>% 
  group_by(Channel, Year, gfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(gfst != "Tree") %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(gfs.channel)) {
  if(gfs.channel$Year[i] == "2012-11-01") {
    gfs.channel$Year[i] <- "2012-01-01"
  } else if(gfs.channel$Year[i] == "2013-11-01") {
    gfs.channel$Year[i] <- "2013-01-01"
  } else if(gfs.channel$Year[i] == "2014-11-01") {
    gfs.channel$Year[i] <- "2014-01-01"
  } else if(gfs.channel$Year[i] == "2015-11-01") {
    gfs.channel$Year[i] <- "2015-01-01"
  } else if(gfs.channel$Year[i] == "2018-11-01") {
    gfs.channel$Year[i] <- "2018-01-01"
  } else {
    gfs.channel$Year[i] <- "2021-01-01"
  }
}

gfs.channel$Year <- as.Date(gfs.channel$Year)

gfs.channel[ , "channel.trt"] <- NA
for(i in 1:nrow(gfs.channel)) {
  if(gfs.channel$Channel[i] == "Channel 12") {
    gfs.channel$channel.trt[i] <- "Channel 12: No treatment"
  } else if(gfs.channel$Channel[i] == "Channel 13") {
    gfs.channel$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(gfs.channel$Channel[i] == "Channel 19") {
    gfs.channel$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    gfs.channel$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

gfs.channel$gfst <- factor(gfs.channel$gfst, 
                           levels = c("Grass", "Forb", "Shrub"))

gfs.plot.ds <- ggplot(gfs.channel, aes(x = Year, y = mean, 
                                       group = gfst, color = gfst, shape = gfst)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Grass, forb, and shrub cover") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
gfs.plot.ds

tiff("output_figs/2022-01-28_Data-sharing_updated-2022-09/GrassForbShrub-cover.tiff", 
     units = "in", height = 5, width = 7, res = 300)
gfs.plot.ds
dev.off()


# Woody/herbaceous --------------------------------------------------------

woody.channel <- woody.all %>% 
  group_by(Channel, Year, woody) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(woody.channel)) {
  if(woody.channel$Year[i] == "2012-11-01") {
    woody.channel$Year[i] <- "2012-01-01"
  } else if(woody.channel$Year[i] == "2013-11-01") {
    woody.channel$Year[i] <- "2013-01-01"
  } else if(woody.channel$Year[i] == "2014-11-01") {
    woody.channel$Year[i] <- "2014-01-01"
  } else if(woody.channel$Year[i] == "2015-11-01") {
    woody.channel$Year[i] <- "2015-01-01"
  } else if(woody.channel$Year[i] == "2018-11-01") {
    woody.channel$Year[i] <- "2018-01-01"
  } else {
    woody.channel$Year[i] <- "2021-01-01"
  }
}

woody.channel$Year <- as.Date(woody.channel$Year)

woody.channel[ , "channel.trt"] <- NA
for(i in 1:nrow(woody.channel)) {
  if(woody.channel$Channel[i] == "Channel 12") {
    woody.channel$channel.trt[i] <- "Channel 12: No treatment"
  } else if(woody.channel$Channel[i] == "Channel 13") {
    woody.channel$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(woody.channel$Channel[i] == "Channel 19") {
    woody.channel$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    woody.channel$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

woody.plot.ds <- ggplot(woody.channel, aes(x = Year, y = mean, 
                                           group = woody, color = woody, shape = woody)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Woody and herbaceous cover") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
woody.plot.ds



# Herbaceous --------------------------------------------------------------

herb.channel <- woody.all %>% 
  group_by(Channel, Year, woody) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(woody == "Herbaceous") %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(herb.channel)) {
  if(herb.channel$Year[i] == "2012-11-01") {
    herb.channel$Year[i] <- "2012-01-01"
  } else if(herb.channel$Year[i] == "2013-11-01") {
    herb.channel$Year[i] <- "2013-01-01"
  } else if(herb.channel$Year[i] == "2014-11-01") {
    herb.channel$Year[i] <- "2014-01-01"
  } else if(herb.channel$Year[i] == "2015-11-01") {
    herb.channel$Year[i] <- "2015-01-01"
  } else if(herb.channel$Year[i] == "2018-11-01") {
    herb.channel$Year[i] <- "2018-01-01"
  } else {
    herb.channel$Year[i] <- "2021-01-01"
  }
}

herb.channel$Year <- as.Date(herb.channel$Year)

herb.channel[ , "channel.trt"] <- NA
for(i in 1:nrow(herb.channel)) {
  if(herb.channel$Channel[i] == "Channel 12") {
    herb.channel$channel.trt[i] <- "Channel 12: No treatment"
  } else if(herb.channel$Channel[i] == "Channel 13") {
    herb.channel$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(herb.channel$Channel[i] == "Channel 19") {
    herb.channel$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    herb.channel$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

herb.plot.ds <- ggplot(herb.channel, aes(x = Year, y = mean, 
                                         group = channel.trt, color = channel.trt)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous cover") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~channel.trt)
herb.plot.ds

tiff("output_figs/2022-01-28_Data-sharing_updated-2022-09/Herbaceous-cover.tiff", 
     units = "in", height = 5, width = 7, res = 300)
herb.plot.ds
dev.off()


herb.table.ds <- data.frame(Channel = c("Channel 12", "Channel 19", "Channel 13", "Channel 21"),
                            Cover_2012 = c(filter(herb.channel, Year == "2012-01-01" & Channel == "Channel 12")$mean,
                                           filter(herb.channel, Year == "2012-01-01" & Channel == "Channel 19")$mean,
                                           filter(herb.channel, Year == "2012-01-01" & Channel == "Channel 13")$mean,
                                           filter(herb.channel, Year == "2012-01-01" & Channel == "Channel 21")$mean),
                            Cover_2021 = c(filter(herb.channel, Year == "2021-01-01" & Channel == "Channel 12")$mean,
                                           filter(herb.channel, Year == "2021-01-01" & Channel == "Channel 19")$mean,
                                           filter(herb.channel, Year == "2021-01-01" & Channel == "Channel 13")$mean,
                                           filter(herb.channel, Year == "2021-01-01" & Channel == "Channel 21")$mean))
herb.table.ds$Cover_2012 <- round(herb.table.ds$Cover_2012, 0)
herb.table.ds$Cover_2021 <- round(herb.table.ds$Cover_2021, 0)



# Invasive/native and gfs -------------------------------------------------

ingfs.channel <- ingfst.all %>%  
  group_by(Channel, Year, ingfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(ingfst != "Native tree") %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(ingfs.channel)) {
  if(ingfs.channel$Year[i] == "2012-11-01") {
    ingfs.channel$Year[i] <- "2012-01-01"
  } else if(ingfs.channel$Year[i] == "2013-11-01") {
    ingfs.channel$Year[i] <- "2013-01-01"
  } else if(ingfs.channel$Year[i] == "2014-11-01") {
    ingfs.channel$Year[i] <- "2014-01-01"
  } else if(ingfs.channel$Year[i] == "2015-11-01") {
    ingfs.channel$Year[i] <- "2015-01-01"
  } else if(ingfs.channel$Year[i] == "2018-11-01") {
    ingfs.channel$Year[i] <- "2018-01-01"
  } else {
    ingfs.channel$Year[i] <- "2021-01-01"
  }
}

ingfs.channel$Year <- as.Date(ingfs.channel$Year)

ingfs.channel.known <- ingfs.channel %>% 
  filter(ingfst %in% c("Invasive forb", "Invasive grass", "Native forb",
                       "Native grass", "Native shrub"))

ingfs.channel.known$ingfst <- factor(ingfs.channel.known$ingfst,
                                     levels = c("Native grass", "Invasive grass",
                                                "Native forb", "Invasive forb",
                                                "Native shrub"))
brewer.pal(3, "Dark2")
brewer.pal(3, "Set2")

ingfs.known.plot.ds <- ggplot(ingfs.channel.known, aes(x = Year, y = mean, 
                                                       group = ingfst, color = ingfst, shape = ingfst)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Grass, forb, and shrub cover") +
  scale_color_manual(values = c("#1B9E77", "#66C2A5", "#D95F02", "#FC8D62", "#7570B3")) +
  scale_shape_manual(values = c(16, 17, 16, 17, 16)) +
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ingfs.known.plot.ds



# Invasive/native and grass/forb ------------------------------------------

ingf.channel <- ingfst.all %>%  
  group_by(Channel, Year, ingfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(!ingfst %in% c("Native tree", "Native shrub")) %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(ingf.channel)) {
  if(ingf.channel$Year[i] == "2012-11-01") {
    ingf.channel$Year[i] <- "2012-01-01"
  } else if(ingf.channel$Year[i] == "2013-11-01") {
    ingf.channel$Year[i] <- "2013-01-01"
  } else if(ingf.channel$Year[i] == "2014-11-01") {
    ingf.channel$Year[i] <- "2014-01-01"
  } else if(ingf.channel$Year[i] == "2015-11-01") {
    ingf.channel$Year[i] <- "2015-01-01"
  } else if(ingf.channel$Year[i] == "2018-11-01") {
    ingf.channel$Year[i] <- "2018-01-01"
  } else {
    ingf.channel$Year[i] <- "2021-01-01"
  }
}

ingfs.channel$Year <- as.Date(ingfs.channel$Year)

ingfs.channel.known <- ingfs.channel %>% 
  filter(ingfst %in% c("Invasive forb", "Invasive grass", "Native forb",
                       "Native grass"))

ingfs.channel.known$ingfst <- factor(ingfs.channel.known$ingfst,
                                     levels = c("Native grass", "Invasive grass",
                                                "Native forb", "Invasive forb",
                                                "Native shrub"))

ingf.known.plot.ds <- ggplot(ingfs.channel.known, aes(x = Year, y = mean, 
                                                      group = ingfst, color = ingfst, shape = ingfst)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Grass and forb cover") +
  scale_color_manual(values = c("#1B9E77", "#66C2A5", "#D95F02", "#FC8D62")) +
  scale_shape_manual(values = c(16, 17, 16, 17)) +
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ingf.known.plot.ds

tiff("output_figs/2022-01-28_Data-sharing_updated-2022-09/InNatGF-cover.tiff", 
     units = "in", height = 5, width = 7, res = 300)
ingf.known.plot.ds
dev.off()



# Richness ----------------------------------------------------------------

# By channel and station
richness <- plant.all.nov %>%  
  group_by(Year, Station, Channel) %>% 
  summarise(rich = n_distinct(Common),
            .groups = "keep")

# Channel summarised (stations averaged)
richness.channel <- richness %>% 
  group_by(Year, Channel) %>% 
  summarise(mean = mean(rich),
            SD = sd(rich),
            SE = std.error(rich),
            .groups = "keep") %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(richness.channel)) {
  if(richness.channel$Year[i] == "2012-11-01") {
    richness.channel$Year[i] <- "2012-01-01"
  } else if(richness.channel$Year[i] == "2013-11-01") {
    richness.channel$Year[i] <- "2013-01-01"
  } else if(richness.channel$Year[i] == "2014-11-01") {
    richness.channel$Year[i] <- "2014-01-01"
  } else if(richness.channel$Year[i] == "2015-11-01") {
    richness.channel$Year[i] <- "2015-01-01"
  } else if(richness.channel$Year[i] == "2018-11-01") {
    richness.channel$Year[i] <- "2018-01-01"
  } else {
    richness.channel$Year[i] <- "2021-01-01"
  }
}

richness.channel$Year <- as.Date(richness.channel$Year)

richness.channel$Channel <- factor(richness.channel$Channel,
                                   levels = c("Channel 12", "Channel 19",
                                              "Channel 13", "Channel 21"))

richness.plot.ds <- ggplot(richness.channel, aes(x = Year, y = mean, 
                                                 group = Channel, color = Channel, shape = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Number of species") +
  ggtitle("Species richness") +
  scale_color_manual(values = c("red", "#1F78B4", "#33A02C", "#33A02C")) +
  scale_shape_manual(values = c(17, 16, 15, 16)) +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
richness.plot.ds


richness.channel[ , "channel.trt"] <- NA
for(i in 1:nrow(richness.channel)) {
  if(richness.channel$Channel[i] == "Channel 12") {
    richness.channel$channel.trt[i] <- "Channel 12: No treatment"
  } else if(richness.channel$Channel[i] == "Channel 13") {
    richness.channel$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(richness.channel$Channel[i] == "Channel 19") {
    richness.channel$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    richness.channel$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

richness.plot.fw.ds <- ggplot(richness.channel, aes(x = Year, y = mean, 
                                                    group = channel.trt, color = channel.trt)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Number of species") +
  ggtitle("Perennial species richness") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~channel.trt)
richness.plot.fw.ds

tiff("output_figs/2022-01-28_Data-sharing_updated-2022-09/Richness_FW.tiff", 
     units = "in", height = 5, width = 7, res = 300)
richness.plot.fw.ds
dev.off()


richness.table.ds <- data.frame(Channel = c("Channel 12", "Channel 19", "Channel 13", "Channel 21"),
                                Cover_2012 = c(filter(richness.channel, Year == "2012-01-01" & Channel == "Channel 12")$mean,
                                               filter(richness.channel, Year == "2012-01-01" & Channel == "Channel 19")$mean,
                                               filter(richness.channel, Year == "2012-01-01" & Channel == "Channel 13")$mean,
                                               filter(richness.channel, Year == "2012-01-01" & Channel == "Channel 21")$mean),
                                Cover_2021 = c(filter(richness.channel, Year == "2021-01-01" & Channel == "Channel 12")$mean,
                                               filter(richness.channel, Year == "2021-01-01" & Channel == "Channel 19")$mean,
                                               filter(richness.channel, Year == "2021-01-01" & Channel == "Channel 13")$mean,
                                               filter(richness.channel, Year == "2021-01-01" & Channel == "Channel 21")$mean))
richness.table.ds$Cover_2012 <- round(richness.table.ds$Cover_2012, 0)
richness.table.ds$Cover_2021 <- round(richness.table.ds$Cover_2021, 0)




# Shannon diversity -------------------------------------------------------

# By channel and station
shannon <- plant.all.nov %>%  
  group_by(Year, Station, Channel) %>% 
  summarise(shan = diversity(Cover),
            .groups = "keep")

# Channel summarised (stations averaged)
shannon.channel <- shannon %>% 
  group_by(Year, Channel) %>% 
  summarise(mean = mean(shan),
            SD = sd(shan),
            SE = std.error(shan),
            .groups = "keep") %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(shannon.channel)) {
  if(shannon.channel$Year[i] == "2012-11-01") {
    shannon.channel$Year[i] <- "2012-01-01"
  } else if(shannon.channel$Year[i] == "2013-11-01") {
    shannon.channel$Year[i] <- "2013-01-01"
  } else if(shannon.channel$Year[i] == "2014-11-01") {
    shannon.channel$Year[i] <- "2014-01-01"
  } else if(shannon.channel$Year[i] == "2015-11-01") {
    shannon.channel$Year[i] <- "2015-01-01"
  } else if(shannon.channel$Year[i] == "2018-11-01") {
    shannon.channel$Year[i] <- "2018-01-01"
  } else {
    shannon.channel$Year[i] <- "2021-01-01"
  }
}

shannon.channel$Year <- as.Date(shannon.channel$Year)

shannon.channel$Channel <- factor(shannon.channel$Channel,
                                  levels = c("Channel 12", "Channel 19",
                                             "Channel 13", "Channel 21"))

shannon.plot.ds <- ggplot(shannon.channel, aes(x = Year, y = mean, 
                                               group = Channel, color = Channel, shape = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Diversity") +
  scale_color_manual(values = c("red", "#1F78B4", "#33A02C", "#33A02C")) +
  scale_shape_manual(values = c(17, 16, 15, 16)) +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
shannon.plot.ds

shannon.channel[ , "channel.trt"] <- NA
for(i in 1:nrow(shannon.channel)) {
  if(shannon.channel$Channel[i] == "Channel 12") {
    shannon.channel$channel.trt[i] <- "Channel 12: No treatment"
  } else if(shannon.channel$Channel[i] == "Channel 13") {
    shannon.channel$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(shannon.channel$Channel[i] == "Channel 19") {
    shannon.channel$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    shannon.channel$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

shannon.plot.fw.ds <- ggplot(shannon.channel, aes(x = Year, y = mean, 
                                                  group = channel.trt, color = channel.trt)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial plant diversity") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~channel.trt)
shannon.plot.fw.ds

tiff("output_figs/2022-01-28_Data-sharing_updated-2022-09/Shannon_FW.tiff", 
     units = "in", height = 5, width = 7, res = 300)
shannon.plot.fw.ds
dev.off()


shannon.table.ds <- data.frame(Channel = c("Channel 12", "Channel 19", "Channel 13", "Channel 21"),
                               Cover_2012 = c(filter(shannon.channel, Year == "2012-01-01" & Channel == "Channel 12")$mean,
                                              filter(shannon.channel, Year == "2012-01-01" & Channel == "Channel 19")$mean,
                                              filter(shannon.channel, Year == "2012-01-01" & Channel == "Channel 13")$mean,
                                              filter(shannon.channel, Year == "2012-01-01" & Channel == "Channel 21")$mean),
                               Cover_2021 = c(filter(shannon.channel, Year == "2021-01-01" & Channel == "Channel 12")$mean,
                                              filter(shannon.channel, Year == "2021-01-01" & Channel == "Channel 19")$mean,
                                              filter(shannon.channel, Year == "2021-01-01" & Channel == "Channel 13")$mean,
                                              filter(shannon.channel, Year == "2021-01-01" & Channel == "Channel 21")$mean))
shannon.table.ds$Cover_2012 <- round(shannon.table.ds$Cover_2012, 3)
shannon.table.ds$Cover_2021 <- round(shannon.table.ds$Cover_2021, 3)




# Rain --------------------------------------------------------------------

rain.plot.ds <- ggplot(rain, aes(x = Date, y = Avg)) +
  geom_line(size = 0.9) +
  geom_point(size = 2.5) + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Precipitation (in.)") +
  ggtitle("Rainfall") +
  theme_bw(base_size = 14)
rain.plot.ds

tiff("output_figs/2022-01-28_Data-sharing_updated-2022-09/Rain.tiff", 
     units = "in", height = 5, width = 7, res = 300)
rain.plot.ds
dev.off()


# Save data ---------------------------------------------------------------

save.image("RData/2022-01-28_Data-sharing_updated-2022-09.RData")
