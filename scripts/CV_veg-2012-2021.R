# Purpose: Calculate CV of each sample across all years, and compare CVs of Treated vs. Control.
#   CV is a measure of stability and resistance to change.
# Did not find any significant differences in CVs for total cover, herb cover, richness, or Shannon.
# Created: 2022-02-15
# Last updated: 2023-05-26


library(tidyverse)
library(car)
library(scales)

# Load data ---------------------------------------------------------------

total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
herb.all <- read.csv("data/cleaned/Summarised-all_herb-cover.csv")
notree.all <- read.csv("data/cleaned/Summarised-all_notree-cover.csv")
per.div <- read.csv("data/cleaned/Summarised-all_perennial-diversity.csv")


# Total cover -------------------------------------------------------------

total.sample <- total.all |> 
  group_by(Sample, Channel, Station, Treatment3) |> 
  summarise(CV = sd(Cover) / mean(Cover),
            .groups = "keep")

write_csv(total.sample,
          "data/cleaned/CV-2012-2021_total-cover.csv")

summary(filter(total.sample, Treatment3 == "Treated")$CV)
summary(filter(total.sample, Treatment3 == "Control")$CV)

qqPlot(filter(total.sample, Treatment3 == "Treated")$CV) # normal
qqPlot(filter(total.sample, Treatment3 == "Control")$CV) # normal

# Compare
t.test(filter(total.sample, Treatment3 == "Treated")$CV,
       filter(total.sample, Treatment3 == "Control")$CV) # NS

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

herb.sample <- herb.all |> 
  group_by(Sample, Channel, Station, Treatment3) |> 
  summarise(CV = sd(Cover) / mean(Cover),
            .groups = "keep")

write_csv(herb.sample,
          "data/cleaned/CV-2012-2021_herb-cover.csv")

summary(filter(herb.sample, Treatment3 == "Treated")$CV)
summary(filter(herb.sample, Treatment3 == "Control")$CV)

qqPlot(filter(herb.sample, Treatment3 == "Treated")$CV) # normal
qqPlot(filter(herb.sample, Treatment3 == "Control")$CV) # kind of normal?

# Compare
wilcox.test(filter(herb.sample, Treatment3 == "Treated")$CV,
            filter(herb.sample, Treatment3 == "Control")$CV) # NS

t.test(filter(herb.sample, Treatment3 == "Treated")$CV,
       filter(herb.sample, Treatment3 == "Control")$CV) # NS

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

notree.sample <- notree.all |> 
  group_by(Sample, Channel, Station, Treatment3) |> 
  summarise(CV = sd(Cover) / mean(Cover),
            .groups = "keep")

write_csv(notree.sample,
          "data/cleaned/CV-2012-2021_notree-cover.csv")

summary(filter(notree.sample, Treatment3 == "Treated")$CV)
summary(filter(notree.sample, Treatment3 == "Control")$CV)

qqPlot(filter(notree.sample, Treatment3 == "Treated")$CV) # normal
qqPlot(filter(notree.sample, Treatment3 == "Control")$CV) # normal

# Compare
t.test(filter(notree.sample, Treatment3 == "Treated")$CV,
       filter(notree.sample, Treatment3 == "Control")$CV) # NS

# Plot
notree.plot.cv <- notree.sample |> 
  ggplot(aes(x = Treatment3, y = CV, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Grass, forb & shrub CV",
       x = NULL) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent)
notree.plot.cv



# Richness ----------------------------------------------------------------

rich.sample <- per.div |> 
  group_by(Sample, Channel, Station, Treatment3) |> 
  summarise(CV = sd(rich) / mean(rich),
            .groups = "keep")

write_csv(rich.sample,
          "data/cleaned/CV-2012-2021_richness.csv")

summary(filter(rich.sample, Treatment3 == "Treated")$CV)
summary(filter(rich.sample, Treatment3 == "Control")$CV)

qqPlot(filter(rich.sample, Treatment3 == "Treated")$CV) # normal
qqPlot(filter(rich.sample, Treatment3 == "Control")$CV) # normal

# Compare
t.test(filter(rich.sample, Treatment3 == "Treated")$CV,
       filter(rich.sample, Treatment3 == "Control")$CV) # NS

# Plot
rich.plot.cv <- rich.sample |> 
  ggplot(aes(x = Treatment3, y = CV, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Richness CV",
       x = NULL) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent)
rich.plot.cv



# Shannon -----------------------------------------------------------------

shan.sample <- per.div |> 
  group_by(Sample, Channel, Station, Treatment3) |> 
  summarise(CV = sd(shan) / mean(shan),
            .groups = "keep")

write_csv(shan.sample,
          "data/cleaned/CV-2012-2021_shannon.csv")

summary(filter(shan.sample, Treatment3 == "Treated")$CV)
summary(filter(shan.sample, Treatment3 == "Control")$CV)

qqPlot(filter(shan.sample, Treatment3 == "Treated")$CV) # normal
qqPlot(filter(shan.sample, Treatment3 == "Control")$CV) # almost normal

# Compare
t.test(filter(shan.sample, Treatment3 == "Treated")$CV,
       filter(shan.sample, Treatment3 == "Control")$CV) # NS

# Plot
shan.plot.cv <- shan.sample |> 
  ggplot(aes(x = Treatment3, y = CV, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Shannon diversity CV",
       x = NULL) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent)
shan.plot.cv


save.image("RData/CV_veg-2023-2021.RData")
