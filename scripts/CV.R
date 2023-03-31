# Purpose: Calculate CV of each sample across all years, and compare CVs of Treated vs. Control.
#   CV is a measure of stability and resistance to change.
# Did not find any significant differences in CVs for total cover, herb cover, richness, or Shannon.


library(tidyverse)
library(cvequality)
library(car)

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
total.sample |> 
ggplot(aes(x = Treatment3, y = CV)) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("Total cover")


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
herb.sample |> 
  ggplot(aes(x = Treatment3, y = CV)) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("Herbaceous cover")



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
notree.sample |> 
  ggplot(aes(x = Treatment3, y = CV)) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("Grass, forb & shrub cover")


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
rich.sample |> 
  ggplot(aes(x = Treatment3, y = CV)) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("Perennial richness")



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
shan.sample |> 
  ggplot(aes(x = Treatment3, y = CV)) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("Perennial Shannon")



save.image("RData/CV.RData")
