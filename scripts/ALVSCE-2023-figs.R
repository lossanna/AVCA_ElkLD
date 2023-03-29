# Purpose: Make figures for ALVSCE poster forum (presented 2023-03-30).

library(tidyverse)

# Load data ---------------------------------------------------------------

total.all <- read_csv("data/cleaned/Summarised-all_total-plant-cover.csv")
per.div <- read_csv("data/cleaned/Summarised-all_perennial-diversity.csv")
herb.all <- read_csv("data/cleaned/Summarised-all_herb-cover.csv")

total.avg <- read_csv("data/cleaned/Treatment3-average_total-cover.csv")
herb.avg <- read_csv("data/cleaned/Treatment3-average_herb-cover.csv")
rich.avg <- read_csv("data/cleaned/Treatment3-average_richness.csv")
shan.avg <- read_csv("data/cleaned/Treatment3-average_shannon.csv")

total.cv <- read_csv("data/cleaned/CV-2012-2021_total-cover.csv")
herb.cv <- read_csv("data/cleaned/CV-2012-2021_herb-cover.csv")
rich.cv <- read_csv("data/cleaned/CV-2012-2021_richness.csv")
shan.cv <- read_csv("data/cleaned/CV-2012-2021_shannon.csv")

dat.2021 <- read_csv("data/cleaned/Data-2021_clean.csv")


# Temporal veg data 2012-2021 ---------------------------------------------

# Total cover
tiff("output_figs/ALVSCE_2023/Total-cover_2012-2021.tiff", width = 6, height = 5, units = "in", res = 300)
ggplot(total.avg, aes(x = year.xaxis, y = mean, 
                                    group = Treatment3, 
                                    color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE),
                width = 40) +
  scale_x_date(date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover, 2012-2021") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())
dev.off()


# Richness
tiff("output_figs/ALVSCE_2023/Richness_2012-2021.tiff", width = 6, height = 5, units = "in", res = 300)
ggplot(rich.avg, aes(x = year.xaxis, y = mean,
                     group = Treatment3,
                      color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE),
                width = 40) +
  scale_x_date(date_labels = "%Y") +
  xlab(NULL) +
  ylab("No. of species") +
  ggtitle("Perennial plant richness, 2012-2021") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())
dev.off()



# 16S NMDS
dat.2021 %>% 
  ggplot(aes(x = NMDS1, y = NMDS2, color = Treatment3, shape = Treatment3)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  labs(x = "Axis 1",
       y = "Axis 2",
       title = "Bacteria & archaea NMDS",
       color = "Treatment",
       shape = "Treatment") +
  theme(legend.position = "bottom") +
  geom_text(aes(x = 0.3, y = -0.55, label = "PERMANOVA, p < 0.05"),
            size = 2.5, color = "gray30") +
  geom_text(aes(x = 0.35, y = -0.65, label = "Stress = 0.168"),
            size = 2.5, color = "gray30") # only 3% explained by Treatment3 lol
