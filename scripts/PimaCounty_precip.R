# Purpose: Graph precipitation data from Pima County ALERT system, sensor #6380,
#   sourced from https://webcms.pima.gov/government/flood_control/services/precipitation_and_streamflow_data/.
# Create supp figure for publication (will not be publishing code)

# Created: 2023-02-02
# Last updated: 2023-08-27

library(tidyverse)
library(plotrix)

# Load data ---------------------------------------------------------------

precip <- read.table("data/PimaCounty_precip/PimaCounty_precip_2012-2021.txt",
                     sep = "\t", header = TRUE)


# Data wrangling ----------------------------------------------------------

precip$year.xaxis <- as.Date(precip$year.xaxis)
precip$Precip_cum_cm <- precip$Precip_cum * 2.54

precip.sample <- precip |> 
  filter(!str_detect(year.xaxis, c("2020|2016|2017|2019")))

precip <- precip |> 
  arrange(Date0)


# Averages ----------------------------------------------------------------

summary(precip$Precip_cum)
summary(precip$Precip_cum_cm)


# Percent change of extreme years -----------------------------------------

# 2018-2019
(precip[8, 9] - precip[7, 9]) / precip[7, 9] * 100 # -74.6826

# 2019-2020
(precip[9, 9] - precip[8, 9]) / precip[8, 9] * 100 # 79.941

# 2020-2021
(precip[10, 9] - precip[9, 9]) / precip[9, 9] * 100 # 87.86885

# 2019-2021
(precip[10, 9] - precip[8, 9]) / precip[8, 9] * 100 # 238.0531



# Graph -------------------------------------------------------------------

# Every year
# Inches
ggplot(precip, aes(x = year.xaxis, y = Precip_cum)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  xlab(NULL) +
  ylab("Precipitation (in.)") +
  ggtitle("Cumulative summer precipitation (June-October)") +
  theme_bw(base_size = 14) +
  scale_y_continuous(limits = c(0, 14)) +
  theme(axis.text.x = element_text(color = "black")) 

# Centimeters
tiff("figures/2023-07_draft-figures/Precipitation-line-graph.tiff", width = 6, height = 4, units = "in", res = 150)
ggplot(precip, aes(x = year.xaxis, y = Precip_cum_cm)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  xlab(NULL) +
  ylab("Precipitation (cm)") +
  ggtitle("Cumulative summer precipitation (June-October)") +
  theme_bw(base_size = 14) +
  scale_y_continuous(limits = c(0, 35)) +
  theme(axis.text.x = element_text(color = "black")) 
dev.off()

tiff("figures/2023-09_publish-figures/FigS1_Precipitation.tiff", width = 6, height = 4, units = "in", res = 300)
ggplot(precip, aes(x = year.xaxis, y = Precip_cum_cm)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  xlab(NULL) +
  ylab("Precipitation (cm)") +
  ggtitle("Cumulative summer precipitation (June-October)") +
  theme_bw(base_size = 14) +
  scale_y_continuous(limits = c(0, 35)) +
  theme(axis.text.x = element_text(color = "black")) 
dev.off()


# Sampled years only
ggplot(precip.sample, aes(x = year.xaxis, y = Precip_cum)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  xlab(NULL) +
  ylab("Precipitation (in)") +
  ggtitle("Cumulative summer precipitation") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 14))

