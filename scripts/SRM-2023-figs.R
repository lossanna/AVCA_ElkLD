library(tidyverse)
library(vegan)
library(plotrix)
library(agricolae)
library(ggpubr)
 
# Load data ---------------------------------------------------------------

load("RData/Perennial-diversity-by-year-and-channel_v2.RData")
load("RData/Cover-by-year-and-channel_v2.RData")
load("RData/ANOVA-by-treatment2_2021.RData")

precip <- read.table("data/PimaCounty_precip/PimaCounty_precip_2012-2021.txt",
                     sep = "\t", header = TRUE)
precip$year.xaxis <- as.Date(precip$year.xaxis)


# Common species ----------------------------------------------------------

common.plants <- plant.all %>% 
  filter(Cover >= 15) 
common.plants <- count(common.plants, Common)
common.plants <- common.plants[order(common.plants$n, decreasing = TRUE), ]


# Precipitation -----------------------------------------------------------

precip.srm23 <- ggplot(precip, aes(x = year.xaxis, y = Precip_cum)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Precipitation (in)") +
  ggtitle("Cumulative summer precipitation") +
  theme_bw(base_size = 14) +
  scale_y_continuous(limits = c(0, 14)) +
  theme(axis.text.x = element_text(color = "#000000"))
precip.srm23

tiff("output_figs/SRM_2023/Precip.tiff", units = "in", height = 5.5, width = 10, res = 300)
precip.srm23
dev.off()
  


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
                        y = c(7, 7))

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
            size = 3.5, color = "gray30") +
  scale_y_continuous(limits = c(0, 85)) +
  theme(axis.text.x = element_text(color = "#000000"))
total.plot.srm23

tiff("output_figs/SRM_2023/Total-plant-cover.tiff", units = "in", height = 5.5, width = 10, res = 300)
total.plot.srm23
dev.off()



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
                        y = c(4, 4, 4, 4))

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
            size = 3.5, color = "gray30") +
  scale_y_continuous(limits = c(0, 40)) +
  theme(axis.text.x = element_text(color = "#000000"))
herb.plot.srm23

tiff("output_figs/SRM_2023/Herbaceous-cover.tiff", units = "in", height = 5.5, width = 10, res = 300)
herb.plot.srm23
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
            size = 3.5, color = "gray30") +
  theme(axis.text.x = element_text(color = "#000000"))
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
  facet_wrap(~channel.trt) +
  theme(axis.text.x = element_text(color = "#000000"))
shannon.plot.srm23

tiff("output_figs/SRM_2023/Shannon.tiff", units = "in", height = 5.5, width = 10, res = 300)
shannon.plot.srm23
dev.off()


# 2021 NMDS ---------------------------------------------------------------

meta <- meta %>% 
  mutate(Treatment2 = factor(meta$Treatment2, 
                             levels = c("In-channel treatment", "Upland treatment",
                                        "No treatment")))

nmds2021 <- meta %>% 
  ggplot(aes(x = NMDS1, y = NMDS2, color = Treatment2, shape = Treatment2)) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(15, 17, 18)) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
nmds2021

tiff("output_figs/SRM_2023/NMDS.tiff", units = "in", height = 5.5, width = 10, res = 300)
nmds2021
dev.off()


# 2021 ANOVA comparisons --------------------------------------------------

# Total N 
letters <- data.frame(label = tn.letters$groups,
                      x = 1:3,
                      y = rep(0.64, 3))
tn2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = TN_ppt)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Total N (mg/g soil)") +
  ggtitle("Soil nitrogen") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "#000000"))
tn2021.plot

# Total C 
letters <- data.frame(label = tc.letters$groups,
                      x = 1:3,
                      y = rep(6.5, 3))
tc2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = TC_ppt)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Total C (mg/g soil)") +
  ggtitle("Soil carbon") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "#000000"))
tc2021.plot

# Organic matter
letters <- data.frame(label = om.letters$groups,
                      x = 1:3,
                      y = rep(2.1, 3))
om2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = OM_perc)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Organic matter (%)") +
  ggtitle("Soil organic matter") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "#000000"))
om2021.plot

# Soil bac arc richness 
barc.rich2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = Richness.barc)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("No. of species (ASVs)") +
  ggtitle("Soil bacterial & archaeal richness") +
  theme(axis.text.x = element_text(color = "#000000"))
barc.rich2021.plot

# Soil bac arc diversity 
barc.shan2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = Shannon.barc)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Shannon diversity index") + 
  ggtitle("Soil bacterial & archaeal diversity") +
  theme(axis.text.x = element_text(color = "#000000"))
barc.shan2021.plot

# Combine soil graphs 
tiff("output_figs/SRM_2023/Soil2021.tiff", units = "in", height = 7, width = 9, res = 300)
ggarrange(tn2021.plot, tc2021.plot, om2021.plot, barc.rich2021.plot,
          ncol = 2, nrow = 2)
dev.off()



# Total plant cover
total2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = Cover)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  theme(axis.text.x = element_text(color = "#000000"))
total2021.plot

# Herbaceous cover
letters <- data.frame(label = herb.letters$groups,
                      x = 1:3,
                      y = rep(75, 3))
herb2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = Herbaceous)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous plant cover") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "#000000"))
herb2021.plot

# Perennial plant richness
rich2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = rich)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("No. of species") +
  ggtitle("Perennial plant richness") +
  theme(axis.text.x = element_text(color = "#000000"))
rich2021.plot

# Perennial plant diversity 
shan2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = shan)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial plant diversity") +
  theme(axis.text.x = element_text(color = "#000000"))
shan2021.plot


# Combine plant graphs 
tiff("output_figs/SRM_2023/Plant2021.tiff", units = "in", height = 7, width = 9, res = 300)
ggarrange(total2021.plot, herb2021.plot, rich2021.plot, shan2021.plot,
          ncol = 2, nrow = 2)
dev.off()




# Save --------------------------------------------------------------------

save.image("RData/SRM-2023-figs.RData")


# Others ------------------------------------------------------------------

# Plots that look nice but won't be used in the talk probably


# Soil cover 
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


# Invasive/native 
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


# Invasive/native for herbaceous
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




