library(tidyverse)
library(vegan)
library(plotrix)
library(agricolae)
library(ggpubr)
 
# Load data ---------------------------------------------------------------

load("RData/Perennial-diversity-by-year-and-channel_v2.RData")
load("RData/Cover-by-year-and-channel_v2.RData")
load("RData/ANOVA-between-channels_2021.RData")


# Common species ----------------------------------------------------------

common.plants <- plant.all %>% 
  filter(Cover >= 15) 
common.plants <- count(common.plants, Common)
common.plants <- common.plants[order(common.plants$n, decreasing = TRUE), ]


# Total plant cover -------------------------------------------------------

letters <- data.frame(label = c(total13.letters$groups,
                                total19.letters$groups),
                      channel.trt = c(rep("Channel 13: In-channel treatment", 6),
                                      rep("Channel 19: Upland treatment", 6)),
                      x = rep(total.channel$year.xaxis[1:6], 2),
                      y = c(70, 60, 70, 65, 65, 55,
                            55, 45, 48, 38, 55, 60))

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

tiff("output_figs/SRM_2023/Total-plant-cover.tiff", units = "in", height = 5.5, width = 10, res = 300)
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
                      y = c(18, 23, 14, 23, 27, 23,
                            18, 13, 21, 19, 38, 33,
                            20, 12, 26, 8, 14, 28,
                            13, 11, 21, 18, 24, 24))

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
            size = 3.5, color = "gray30")
herb.plot.srm23

tiff("output_figs/SRM_2023/Herbaceous-cover.tiff", units = "in", height = 5.5, width = 10, res = 300)
herb.plot.srm23
dev.off()



# Invasive/native ---------------------------------------------------------

letters <- data.frame(label = c(native13.letters$groups,
                                native19.letters$groups,
                                invas19.letters$groups.up),
                      channel.trt = c(rep("Channel 13: In-channel treatment", 6),
                                      rep("Channel 19: Upland treatment", 12)),
                      x = rep(total.channel$year.xaxis[1:6], 3),
                      y = c(63, 60, 64, 60, 60, 42,
                            70, 64, 64, 37, 50, 60,
                            11, 11, 11, 11, 15, 20))

anova.lab <- data.frame(label = c(rep("ANOVA", 2)),
                        channel.trt = c("Channel 13: In-channel treatment",
                                        "Channel 19: Upland treatment"),
                        x = c(rep(as.Date("2020-01-01"), 2)),
                        y = c(75, 75))

innat.known.plot.srm23 <- ggplot(innat.channel.known, aes(x = year.xaxis, y = mean)) +
  geom_line(linewidth = 1, aes(color = Native)) +
  geom_point(size = 3, aes(color = Native, shape = Native)) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE, color = Native, shape = Native)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by native status") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom") +
  facet_wrap(~channel.trt) +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label, group = channel.trt),
            color = "black") +
  geom_text(data = anova.lab,
            mapping = aes(x = x, y = y, label = label, group = channel.trt),
            size = 3.5, color = "gray30")
innat.known.plot.srm23

tiff("output_figs/SRM_2023/Invasive-native-cover.tiff", units = "in", height = 5.5, width = 10, res = 300)
innat.known.plot.srm23
dev.off()


# Invasive/native for herbaceous ------------------------------------------

letters <- data.frame(label = c(ntherb13.letters$groups,
                                ntherb19.letters$groups,
                                ivherb19.letters$groups.up),
                      channel.trt = c(rep("Channel 13: In-channel treatment", 6),
                                      rep("Channel 19: Upland treatment", 12)),
                      x = rep(total.channel$year.xaxis[1:6], 3),
                      y = c(14, 10, 15, 13, 21, 17,
                            23, 16, 27, 13, 17, 19,
                            1.5, 1.5, 1.5, 1.5, 3, 8))

anova.lab <- data.frame(label = c(rep("ANOVA", 2)),
                        channel.trt = c("Channel 13: In-channel treatment",
                                        "Channel 19: Upland treatment"),
                        x = c(rep(as.Date("2020-01-01"), 2)),
                        y = c(28, 28))

inwood.known.herb.plot.srm23 <- ggplot(inwood.channel.known.herb, 
                                         aes(x = year.xaxis, y = mean)) +
  geom_line(linewidth = 1, aes(color = inwood)) +
  geom_point(size = 3, aes(color = inwood, shape = inwood)) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE, color = inwood, shape = inwood)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous cover by native status") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom") +
  facet_wrap(~channel.trt) +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label, group = channel.trt),
            color = "black") +
  geom_text(data = anova.lab,
            mapping = aes(x = x, y = y, label = label, group = channel.trt),
            size = 3.5, color = "gray30")
inwood.known.herb.plot.srm23

tiff("output_figs/SRM_2023/Invasive-native-cover_herbaceous.tiff", units = "in", height = 5.5, width = 10, res = 300)
inwood.known.herb.plot.srm23
dev.off()


# Species richness --------------------------------------------------------

letters <- data.frame(label = c(richness13.letters$groups,
                                                    richness19.letters$groups,
                                                    richness21.letters$groups),
                                          channel.trt = c(rep("Channel 13: In-channel treatment", 6),
                                                          rep("Channel 19: Upland treatment", 6),
                                                          rep("Channel 21: In-channel treatment", 6)),
                                          x = rep(richness.channel$year.xaxis[1:6], 3),
                                          y = c(9.3, 8.2, 8.2, 9, 10.6, 9,
                                                10.9, 9.5, 9.5, 10.1, 8.2, 8.1,
                                                7.6, 7.9, 9.8, 8.5, 9.7, 6.9))

anova.lab <- data.frame(label = c(rep("ANOVA", 3)),
                        channel.trt = c("Channel 13: In-channel treatment", 
                                        "Channel 19: Upland treatment",
                                        "Channel 21: In-channel treatment"),
                        x = c(rep(as.Date("2020-01-01"), 3)),
                        y = c(10.5, 10.5, 10.5))

richness.plot.srm23 <- ggplot(richness.channel, aes(x = year.xaxis, y = mean, 
                                                      group = channel.trt, 
                                                      color = channel.trt)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Number of species") +
  ggtitle("Perennial species richness") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~channel.trt) +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  geom_text(data = anova.lab,
            mapping = aes(x = x, y = y, label = label),
            size = 3.5, color = "gray30")
richness.plot.srm23

tiff("output_figs/SRM_2023/Richess.tiff", units = "in", height = 5.5, width = 10, res = 300)
richness.plot.srm23
dev.off()


# Shannon diversity -------------------------------------------------------

shannon.plot.srm23 <- ggplot(shannon.channel, aes(x = year.xaxis, y = mean, 
                                            group = channel.trt, 
                                            color = channel.trt)) +
  geom_line(linewidth = 1) +
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
shannon.plot.srm23

tiff("output_figs/SRM_2023/Shannon.tiff", units = "in", height = 5.5, width = 10, res = 300)
shannon.plot.srm23
dev.off()



# Soil cover --------------------------------------------------------------

letters <- data.frame(label = c(soil12.letters$groups, 
                                soil13.letters$groups,
                                soil19.letters$groups,
                                soil21.letters$groups),
                      channel.trt = c(rep("Channel 12: No treatment", 6),
                                      rep("Channel 13: In-channel treatment", 6),
                                      rep("Channel 19: Upland treatment", 6),
                                      rep("Channel 21: In-channel treatment", 6)),
                      x = rep(total.channel$year.xaxis[1:6], 4),
                      y = c(40, 52, 42, 60, 38, 53,
                            65, 61, 57, 39, 32, 35,
                            49, 54, 38, 63, 48, 39,
                            53, 59, 50, 47, 53, 25))

anova.lab <- data.frame(label = c(rep("ANOVA", 4)),
                        channel.trt = c("Channel 12: No treatment",
                                        "Channel 13: In-channel treatment",
                                        "Channel 19: Upland treatment",
                                        "Channel 21: In-channel treatment"),
                        x = c(rep(as.Date("2020-01-01"), 4)),
                        y = c(65, 65, 65, 65))

ground.soil.plot.srm23 <- ggplot(ground.channel.soil, 
                                   aes(x = year.xaxis, y = mean, 
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
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  geom_text(data = anova.lab,
            mapping = aes(x = x, y = y, label = label),
            size = 3, color = "gray30")
ground.soil.plot.srm23

tiff("output_figs/SRM_2023/Soil-cover.tiff", units = "in", height = 5.5, width = 10, res = 300)
ground.soil.plot.srm23
dev.off()


# 2021 ANOVA comparisons --------------------------------------------------

tiff("output_figs/SRM_2023/Soil2021.tiff", units = "in", height = 7, width = 8, res = 300)
ggarrange(tn2021.plot, tc2021.plot, om2021.plot, barc.rich2021.plot,
          ncol = 2, nrow = 2)
dev.off()

tiff("output_figs/SRM_2023/Plant2021.tiff", units = "in", height = 7, width = 8, res = 300)
ggarrange(total2021.plot, herb2021.plot, rich2021.plot, shan2021.plot,
          ncol = 2, nrow = 2)
dev.off()


save.image("RData/SRM-2023-figs.RData")
