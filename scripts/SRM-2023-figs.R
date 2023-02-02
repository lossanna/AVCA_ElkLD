library(tidyverse)
library(vegan)
library(plotrix)
library(agricolae)
 
# Load data ---------------------------------------------------------------

load("RData/Perennial-diversity-by-year-and-channel_v2.RData")
load("RData/Cover-by-year-and-channel_v2.RData")



# Common species ----------------------------------------------------------

common.plants <- plant.all %>% 
  filter(Cover >= 15) 
common.plants <- count(common.plants, Common)
common.plants <- common.plants[order(common.plants$n, decreasing = TRUE), ]


###### Entire channel averages ############################################


# Total plant cover -------------------------------------------------------

letters <- data.frame(label = c(total13.letters$groups,
                                total19.letters$groups),
                      channel.trt = c(rep("Channel 13: In-channel treatment", 6),
                                      rep("Channel 19: Upland treatment", 6)),
                      x = rep(total.channel$year.xaxis[1:6], 2),
                      y = c(70, 60, 70, 65, 60, 55,
                            50, 45, 48, 43, 55, 60))

anova.lab <- data.frame(label = rep("ANOVA", 2),
                        channel.trt = c("Channel 13: In-channel treatment",
                                        "Channel 19: Upland treatment"),
                        x = c(rep(as.Date("2020-01-01"), 2)),
                        y = c(23, 23))

total.plot.srm23 <- ggplot(total.channel, aes(x = year.xaxis, y = mean, 
                                                group = channel.trt, 
                                                color = channel.trt)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  geom_text(data = anova.lab,
            mapping = aes(x = x, y = y, label = label),
            size = 3.5, color = "gray30")
total.plot.srm23

tiff("output_figs/SRM_2023/Total plant cover.tiff", units = "in", height = 5.5, width = 10, res = 300)
total.plot.srm23
dev.off()


total.table.srm23 <- data.frame(Channel = c("Channel 12", "Channel 19", "Channel 13", "Channel 21"),
                             Cover_2012 = c(filter(total.channel, Year == "2012" & Channel == "Channel 12")$mean,
                                            filter(total.channel, Year == "2012" & Channel == "Channel 19")$mean,
                                            filter(total.channel, Year == "2012" & Channel == "Channel 13")$mean,
                                            filter(total.channel, Year == "2012" & Channel == "Channel 21")$mean),
                             Cover_2021 = c(filter(total.channel, Year == "2021" & Channel == "Channel 12")$mean,
                                            filter(total.channel, Year == "2021" & Channel == "Channel 19")$mean,
                                            filter(total.channel, Year == "2021" & Channel == "Channel 13")$mean,
                                            filter(total.channel, Year == "2021" & Channel == "Channel 21")$mean))
total.table.srm23$Cover_2012 <- round(total.table.srm23$Cover_2012, 0)
total.table.srm23$Cover_2021 <- round(total.table.srm23$Cover_2021, 0)



# Herbaceous cover --------------------------------------------------------

letters <- data.frame(label = c(herb12.letters$groups, 
                                herb13.letters$groups,
                                herb19.letters$groups,
                                herb21.letters$groups),
                      channel.trt = c(rep("Channel 12: No treatment", 6),
                                      rep("Channel 13: In-channel treatment", 6),
                                      rep("Channel 19: Upland treatment", 6),
                                      rep("Channel 21: In-channel treatment", 6)),
                      x = rep(total.channel$year.xaxis[1:6], 4),
                      y = c(20, 25, 15, 25, 30, 23,
                            20, 15, 22, 20, 38, 33,
                            20, 12, 25, 8, 13, 28,
                            15, 13, 23, 18, 25, 25))

anova.lab <- data.frame(label = c(rep("ANOVA", 4)),
                        channel.trt = c("Channel 12: No treatment",
                                        "Channel 13: In-channel treatment",
                                        "Channel 19: Upland treatment",
                                        "Channel 21: In-channel treatment"),
                        x = c(rep(as.Date("2020-01-01"), 4)),
                        y = c(8, 8, 8, 8))

herb.plot.srm23 <- ggplot(herb.channel, aes(x = year.xaxis, y = mean, 
                                            group = channel.trt, color = channel.trt)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous cover") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~channel.trt) +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  geom_text(data = anova.lab,
            mapping = aes(x = x, y = y, label = label),
            size = 3, color = "gray30")
herb.plot.srm23

tiff("output_figs/SRM_2023/Herbaceous cover.tiff", units = "in", height = 5.5, width = 10, res = 300)
herb.plot.srm23
dev.off()



# Invasive/native ---------------------------------------------------------


# Invasive/native and woody/herbaceous ------------------------------------

inwood.known.plot.srm23 <- ggplot(inwood.channel.known.nov, aes(x = Year, y = mean, 
                                                              group = inwood, color = inwood, shape = inwood)) +
  geom_line(linewidth = 1) +
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
inwood.known.plot.srm23

tiff("output_figs/SRM_2023/Inwood cover.tiff", units = "in", height = 7, width = 10, res = 300)
inwood.known.plot.srm23
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

richness.plot.srm23 <- ggplot(richness.channel.year, aes(x = Year, y = mean, 
                                                      group = channel.trt, color = channel.trt)) +
  geom_line(linewidth = 1) +
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
richness.plot.srm23

tiff("output_figs/SRM_2023/Richess.tiff", units = "in", height = 5.5, width = 10, res = 300)
richness.plot.srm23
dev.off()

hsd.richness12
hsd.richness13
richness19.letters
richness21.letters


# Shannon diversity -------------------------------------------------------


shannon.plot.srm23 <- ggplot(shannon.channel.year, aes(x = Year, y = mean, 
                                                    group = channel.trt, color = channel.trt)) +
  geom_line(linewidth = 1) +
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
shannon.plot.srm23

tiff("output_figs/SRM_2023/Shannon.tiff", units = "in", height = 5.5, width = 10, res = 300)
shannon.plot.srm23
dev.off()

# NS for all
# KW for C19; ANOVA for all others


# Soil cover --------------------------------------------------------------

ground.soil.plot.srm23 <- ggplot(ground.channel.nov.soil, aes(x = Year, y = mean, 
                                                              group = channel.trt, color = channel.trt)) +
  geom_line(linewidth = 1) +
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
ground.soil.plot.srm23

tiff("output_figs/SRM_2023/Soil cover.tiff", units = "in", height = 5.5, width = 10, res = 300)
ground.soil.plot.srm23
dev.off()

soil12.letters
hsd.soil13
hsd.soil19
soil21.letters
