# Purpose: Graph precipitation data from Pima County ALERT system, sensor #6380,
#   sourced from https://webcms.pima.gov/government/flood_control/services/precipitation_and_streamflow_data/.
# Created: 2023-02-02
# Last updated: 2023-07-14

library(tidyverse)
library(plotrix)

# Load data ---------------------------------------------------------------

precip <- read.table("data/PimaCounty_precip/PimaCounty_precip_2012-2021.txt",
                     sep = "\t", header = TRUE)


# Data wrangling ----------------------------------------------------------

precip$year.xaxis <- as.Date(precip$year.xaxis)

precip.sample <- precip |> 
  filter(!str_detect(year.xaxis, c("2020|2016|2017|2019")))


# Graph -------------------------------------------------------------------

# Every year
tiff("figures/2023-07_draft-figures/Precipitation-line-graph.tiff", width = 6, height = 4, units = "in", res = 150)
ggplot(precip, aes(x = year.xaxis, y = Precip_cum)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  xlab(NULL) +
  ylab("Precipitation (in.)") +
  ggtitle("Cumulative summer precipitation (June-October)") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 14)) +
  theme(axis.text.x = element_text(color = "black"))
dev.off()

# Sampled years only
ggplot(precip.sample, aes(x = year.xaxis, y = Precip_cum)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Precipitation (in)") +
  ggtitle("Cumulative summer precipitation") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 14))

