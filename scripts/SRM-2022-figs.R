library(tidyverse)
library(vegan)
library(plotrix)
library(agricolae)
library(FSA)
library(rcompanion)
 
# Load data ---------------------------------------------------------------

load("RData/Cover-by-year-and-channel.RData")
load("RData/Perennial-diversity-by-year-and-channel.RData")


# Common species ----------------------------------------------------------

common.plants <- plant.all %>% 
  ungroup() %>% 
  filter(Cover >= 15) 
common.plants <- count(common.plants, Common)
common.plants <- common.plants[order(common.plants$n, decreasing = TRUE), ]


###### Entire channel averages ############################################


# Total plant cover -------------------------------------------------------

total.plot.srm22 <- ggplot(total.channel.nov, aes(x = Year, y = mean, 
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
  theme_bw(base_size = 18) +
  theme(legend.position = "none") 
total.plot.srm22

tiff("output_figs/SRM_2022/Total plant cover.tiff", units = "in", height = 5.5, width = 10, res = 300)
total.plot.srm22
dev.off()

total12.letters
total13.letters
total19.letters
# Channel 21 NS


total.table.srm22 <- data.frame(Channel = c("Channel 12", "Channel 19", "Channel 13", "Channel 21"),
                             Cover_2012 = c(filter(total.channel.nov, Year == "2012-01-01" & Channel == "Channel 12")$mean,
                                            filter(total.channel.nov, Year == "2012-01-01" & Channel == "Channel 19")$mean,
                                            filter(total.channel.nov, Year == "2012-01-01" & Channel == "Channel 13")$mean,
                                            filter(total.channel.nov, Year == "2012-01-01" & Channel == "Channel 21")$mean),
                             Cover_2021 = c(filter(total.channel.nov, Year == "2021-01-01" & Channel == "Channel 12")$mean,
                                            filter(total.channel.nov, Year == "2021-01-01" & Channel == "Channel 19")$mean,
                                            filter(total.channel.nov, Year == "2021-01-01" & Channel == "Channel 13")$mean,
                                            filter(total.channel.nov, Year == "2021-01-01" & Channel == "Channel 21")$mean))
total.table.srm22$Cover_2012 <- round(total.table.srm22$Cover_2012, 0)
total.table.srm22$Cover_2021 <- round(total.table.srm22$Cover_2021, 0)



# Herbaceous cover --------------------------------------------------------

herb.plot.srm22 <- ggplot(herb.channel.nov, aes(x = Year, y = mean, 
                                              group = channel.trt, color = channel.trt)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous cover") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  facet_wrap(~channel.trt)
herb.plot.srm22

tiff("output_figs/SRM_2022/Herbaceous cover.tiff", units = "in", height = 5.5, width = 10, res = 300)
herb.plot.srm22
dev.off()

herb12.letters
herb13.letters
herb19.letters
hsd.herb21



# Invasive/native and woody/herbaceous ------------------------------------

inwood.known.plot.srm22 <- ggplot(inwood.channel.known.nov, aes(x = Year, y = mean, 
                                                              group = inwood, color = inwood, shape = inwood)) +
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
  theme_bw(base_size = 18) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
inwood.known.plot.srm22

tiff("output_figs/SRM_2022/Inwood cover.tiff", units = "in", height = 7, width = 10, res = 300)
inwood.known.plot.srm22
dev.off()

# Invasive herb
# KW for all
# NS for C12, C13, C21
ivherb19.letters

# Native herb
hsd.ntherb12
ntherb13.letters
ntherb19.letters
# C21 KW NS


# Species richness --------------------------------------------------------

richness.plot.srm22 <- ggplot(richness.channel.year, aes(x = Year, y = mean, 
                                                      group = channel.trt, color = channel.trt)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Number of species") +
  ggtitle("Perennial species richness") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  facet_wrap(~channel.trt)
richness.plot.srm22

tiff("output_figs/SRM_2022/Richess.tiff", units = "in", height = 5.5, width = 10, res = 300)
richness.plot.srm22
dev.off()

hsd.richness12
hsd.richness13
richness19.letters
richness21.letters


# Shannon diversity -------------------------------------------------------


shannon.plot.srm22 <- ggplot(shannon.channel.year, aes(x = Year, y = mean, 
                                                    group = channel.trt, color = channel.trt)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial plant diversity") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  facet_wrap(~channel.trt)
shannon.plot.srm22

tiff("output_figs/SRM_2022/Shannon.tiff", units = "in", height = 5.5, width = 10, res = 300)
shannon.plot.srm22
dev.off()

# NS for all
# KW for C19; ANOVA for all others


# Soil cover --------------------------------------------------------------

ground.soil.plot.srm22 <- ggplot(ground.channel.nov.soil, aes(x = Year, y = mean, 
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
  theme_bw(base_size = 18) +
  theme(legend.position = "none")
ground.soil.plot.srm22

tiff("output_figs/SRM_2022/Soil cover.tiff", units = "in", height = 5.5, width = 10, res = 300)
ground.soil.plot.srm22
dev.off()

soil12.letters
hsd.soil13
hsd.soil19
soil21.letters
