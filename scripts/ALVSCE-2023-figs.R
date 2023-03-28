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


# Total cover 2012-2021 ---------------------------------------------------

ggplot(total.avg, aes(x = year.xaxis, y = mean, 
                                    group = Treatment3, 
                                    color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover, 2012-2021") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())


ggplot(total.all, aes(x = year.xaxis, y = Cover, 
                      fill = Treatment3, 
                      color = Treatment3)) +
  geom_boxplot() +
  geom_jitter() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover, 2012-2021") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())
