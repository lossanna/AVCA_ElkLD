# Purpose: Calculate CV of each sample across all years, and compare CVs of Treated vs. Control.
#   CV is a measure of stability and resistance to change.

# Tested normality distribution and log-transformed herb cover, richness & Shannon.
#   Compared with t-test, using log-transformation if needed.
#   
# Did not find any significant differences in CVs for total, herb, notree, richness, or Shannon.

# Created: 2022-02-15
# Last updated: 2023-07-20

library(tidyverse)
library(car)
library(scales)
library(ggpubr)
library(agricolae)

# Load data ---------------------------------------------------------------

total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
herb.all <- read.csv("data/cleaned/Summarised-all_herb-cover.csv")
notree.all <- read.csv("data/cleaned/Summarised-all_notree-cover.csv")
per.div <- read.csv("data/cleaned/Summarised-all_perennial-diversity.csv")


# Total cover -------------------------------------------------------------

# Find CV for each sample over time
total.sample <- total.all |> 
  group_by(Sample, Channel, Station, Treatment3) |> 
  summarise(CV = sd(Cover) / mean(Cover),
            .groups = "keep")

# Explore distribution
qqPlot(filter(total.sample, Treatment3 == "Treated")$CV) # normal
qqPlot(filter(total.sample, Treatment3 == "Control")$CV) # normal

# Write to csv
write_csv(total.sample,
          "data/cleaned/CV-2012-2021_total-cover.csv")

# Compare
t.test(filter(total.sample, Treatment3 == "Treated")$CV,
       filter(total.sample, Treatment3 == "Control")$CV) # NS, p = 0.259

# Plot
total.plot.cv <- total.sample |> 
  ggplot(aes(x = Treatment3, y = CV, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Total cover CV",
       x = NULL) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent)
total.plot.cv


# Herb cover --------------------------------------------------------------

# Find CV for each sample over time
herb.sample <- herb.all |> 
  group_by(Sample, Channel, Station, Treatment3) |> 
  summarise(CV = sd(Cover) / mean(Cover),
            .groups = "keep")

# Explore distribution
qqPlot(filter(herb.sample, Treatment3 == "Treated")$CV) # normal
qqPlot(filter(herb.sample, Treatment3 == "Control")$CV) # not quite normal?

# Log transformation sort of helps Control
qqPlot(log(filter(herb.sample, Treatment3 == "Control")$CV)) # more normal?
qqPlot(log(filter(herb.sample, Treatment3 == "Treated")$CV)) # normal

herb.sample <- herb.sample |> 
  mutate(CV_log = log(CV))

# Write to csv
write_csv(herb.sample,
          "data/cleaned/CV-2012-2021_herb-cover.csv")

# Compare
wilcox.test(filter(herb.sample, Treatment3 == "Treated")$CV,
            filter(herb.sample, Treatment3 == "Control")$CV) # NS, p = 0.148

t.test(filter(herb.sample, Treatment3 == "Treated")$CV_log,
       filter(herb.sample, Treatment3 == "Control")$CV_log) # NS, p = 0.122

# Plot
herb.plot.cv <- herb.sample |> 
  ggplot(aes(x = Treatment3, y = CV, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Herbaceous cover CV",
       x = NULL) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent)
herb.plot.cv



# Notree cover ------------------------------------------------------------

# Find CV for each sample over time
notree.sample <- notree.all |> 
  group_by(Sample, Channel, Station, Treatment3) |> 
  summarise(CV = sd(Cover) / mean(Cover),
            .groups = "keep")

# Explore distribution
qqPlot(filter(notree.sample, Treatment3 == "Treated")$CV) # normal
qqPlot(filter(notree.sample, Treatment3 == "Control")$CV) # normal

# Write to csv
write_csv(notree.sample,
          "data/cleaned/CV-2012-2021_notree-cover.csv")

# Compare
t.test(filter(notree.sample, Treatment3 == "Treated")$CV,
       filter(notree.sample, Treatment3 == "Control")$CV) # NS, p = 0.882

# Plot
notree.plot.cv <- notree.sample |> 
  ggplot(aes(x = Treatment3, y = CV)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment3)) +
  geom_jitter(size = 2,
              alpha = 0.8,
              aes(color = Treatment3)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = " Vegetation cover",
       x = NULL,
       y = "Coefficient of variation") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(color = "black")) +
  geom_text(aes(x = 0.9, y = 0.95, label = "t-test, p = 0.882"),
            color = "gray30",
            size = 2.5) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in"))
notree.plot.cv


# Compare by each channel
summary(aov(notree.sample$CV ~ notree.sample$Channel))
anova.notree.bychannel <- aov(notree.sample$CV ~ notree.sample$Channel)
hsd.notree.bychannel <- HSD.test(anova.notree.bychannel, trt = "notree.sample$Channel")
hsd.notree.bychannel

notree.sample |> 
  ggplot(aes(x = Channel, y = CV)) +
  geom_boxplot() +
  geom_jitter()



# Richness ----------------------------------------------------------------

# Find CV for each sample over time
rich.sample <- per.div |> 
  group_by(Sample, Channel, Station, Treatment3) |> 
  summarise(CV = sd(rich) / mean(rich),
            .groups = "keep")

# Explore distribution
qqPlot(filter(rich.sample, Treatment3 == "Treated")$CV) # almost normal?
qqPlot(filter(rich.sample, Treatment3 == "Control")$CV) # normal

# Log transformation helps Treated 
qqPlot(log(filter(rich.sample, Treatment3 == "Treated")$CV)) # normal now
qqPlot(log(filter(rich.sample, Treatment3 == "Control")$CV)) # normal

rich.sample <- rich.sample |> 
  mutate(CV_log = log(CV))

# Write to csv
write_csv(rich.sample,
          "data/cleaned/CV-2012-2021_richness.csv")

# Compare
t.test(filter(rich.sample, Treatment3 == "Treated")$CV_log,
       filter(rich.sample, Treatment3 == "Control")$CV_log) # NS, p = 0.093

# Plot
rich.plot.cv <- rich.sample |> 
  ggplot(aes(x = Treatment3, y = CV)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment3)) +
  geom_jitter(size = 2,
              alpha = 0.8,
              aes(color = Treatment3)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Perennial plant species \nrichness",
       x = NULL,
       y = "Coefficient of variation") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(color = "black")) +
  geom_text(aes(x = 0.9, y = 0.65, label = "t-test, p = 0.093"),
            color = "gray30",
            size = 2.5) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in"))
rich.plot.cv



# Shannon -----------------------------------------------------------------

# Find CV for each sample over time
shan.sample <- per.div |> 
  group_by(Sample, Channel, Station, Treatment3) |> 
  summarise(CV = sd(shan) / mean(shan),
            .groups = "keep")

# Explore distribution
qqPlot(filter(shan.sample, Treatment3 == "Treated")$CV) # normal
qqPlot(filter(shan.sample, Treatment3 == "Control")$CV) # almost normal

# Log transformation helps Control
qqPlot(log(filter(shan.sample, Treatment3 == "Control")$CV)) # normal now
qqPlot(log(filter(shan.sample, Treatment3 == "Treated")$CV)) # normal

shan.sample <- shan.sample |> 
  mutate(CV_log = log(CV))

# Write to csv
write_csv(shan.sample,
          "data/cleaned/CV-2012-2021_shannon.csv")

# Compare
t.test(filter(shan.sample, Treatment3 == "Treated")$CV_log,
       filter(shan.sample, Treatment3 == "Control")$CV_log) # NS, p = 0.075

# Plot
shan.plot.cv <- shan.sample |> 
  ggplot(aes(x = Treatment3, y = CV)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment3)) +
  geom_jitter(size = 2,
              alpha = 0.8,
              aes(color = Treatment3)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Perennial plant diversity",
       x = NULL,
       y = "Coefficient of variation") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(color = "black")) +
  geom_text(aes(x = 0.9, y = 0.83, label = "t-test, p = 0.075"),
            color = "gray30",
            size = 2.5) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in"))
shan.plot.cv



# Combine notree, richness & Shannon --------------------------------------

tiff("figures/2023-07_draft-figures/CV.tiff", units = "in", height = 4.7, width = 8, res = 150)
ggarrange(notree.plot.cv, rich.plot.cv, shan.plot.cv,
          ncol = 3, nrow = 1,
          labels = c("(A)", "(B)", "(C)")) 

dev.off()



# Within-channel variation over time --------------------------------------

# Notree
# Find CV of each channel by year
notree.channel <- notree.all |> 
  group_by(Channel, Year, year.xaxis, Treatment3) |> 
  summarise(CV = sd(Cover) / mean(Cover),
            .groups = "keep") |> 
  mutate(Year = as.character(Year),
         year.xaxis = as.Date(year.xaxis))

# Plot over time
notree.channel |> 
  ggplot(aes(x = year.xaxis, y = CV, group = Channel, color = Treatment3))  +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Coefficient of variation") +
  ggtitle("Within-channel variation over time") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_y_continuous(labels = percent) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
#   Interpretation: I think we would want to see treated channels becoming more uniform over time?

# Explore distribution
qqPlot(notree.channel$CV) # normal

# Compare channels
summary(aov(CV ~ Channel, data = notree.channel))
anova.notree.channel <- aov(notree.channel$CV ~ notree.channel$Channel)
hsd.notree.channel <- HSD.test(anova.notree.channel, trt = "notree.channel$Channel")
hsd.notree.channel
# Channel 21         0.6500014      a
# Channel 19         0.5093929     ab
# Channel 13         0.3871000     bc
# Channel 12         0.3545642      c
#   Interpretation: Channels 21 & 19 were initially more variable
#     but we are interested in how in-channel variation changes over time


save.image("RData/CV_veg-2012-2021.RData")

