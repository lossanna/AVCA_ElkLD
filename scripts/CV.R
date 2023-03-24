# Purpose: Compare CVs across different channel and treatment groupings to look for a
#   resistance effect (idea is that rocks lower CV even during times of variable rainfall).

library(tidyverse)
library(cvequality)

# Load data ---------------------------------------------------------------

total.all.raw <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
per.div.raw <- read.csv("data/cleaned/Summarised-all_perennial-diversity.csv")
herb.all.raw <- read.csv("data/cleaned/Summarised-all_woody-herb-cover.csv")

precip <- read.table("data/PimaCounty_precip/PimaCounty_precip_2012-2021.txt",
                     sep = "\t", header = TRUE)
precip$year.xaxis <- as.Date(precip$year.xaxis)

meta <- read.table("data/cleaned/sequencing/sequencing_metadata.txt",
                       sep = "\t", header = TRUE)


# Data wrangling ----------------------------------------------------------

total.all <- total.all.raw %>% 
  filter(!str_detect(Year, "03-01")) |> 
  left_join(meta) |> 
  select(-contains("trt"), -PlotTimeID) |> 
  mutate(Year = gsub("-.*", "", Year))

herb.all <- herb.all.raw %>% 
  filter(!str_detect(Year, "03-01")) |> 
  filter(woody == "Herbaceous") |> 
  left_join(meta) |> 
  select(-contains("trt"), -PlotTimeID, -woody) |> 
  mutate(Year = gsub("-.*", "", Year))

per.div <- left_join(per.div.raw, meta)



# By Treatment3 (each sample) ---------------------------------------------

# Total cover
total.sample <- total.all |> 
  group_by(Sample, Channel, Station, Name, Treatment3) |> 
  summarise(CV = sd(Cover) / mean(Cover),
            .groups = "keep")

summary(filter(total.sample, Treatment3 == "Treated")$CV)
summary(filter(total.sample, Treatment3 == "Control")$CV)

t.test(filter(total.sample, Treatment3 == "Treated")$CV,
       filter(total.sample, Treatment3 == "Control")$CV) # NS

total.sample |> 
ggplot(aes(x = Treatment3, y = CV)) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("Total cover")


# Herbaceous cover
herb.sample <- herb.all |> 
  group_by(Sample, Channel, Station, Name, Treatment3) |> 
  summarise(CV = sd(Cover) / mean(Cover),
            .groups = "keep")

summary(filter(herb.sample, Treatment3 == "Treated")$CV)
summary(filter(herb.sample, Treatment3 == "Control")$CV)

t.test(filter(herb.sample, Treatment3 == "Treated")$CV,
       filter(herb.sample, Treatment3 == "Control")$CV) # NS

herb.sample |> 
  ggplot(aes(x = Treatment3, y = CV)) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("Herbaceous cover")


# Perennial richness
rich.sample <- per.div |> 
  group_by(Sample, Channel, Station, Name, Treatment3) |> 
  summarise(CV = sd(rich) / mean(rich),
            .groups = "keep")

summary(filter(rich.sample, Treatment3 == "Treated")$CV)
summary(filter(rich.sample, Treatment3 == "Control")$CV)

t.test(filter(rich.sample, Treatment3 == "Treated")$CV,
       filter(rich.sample, Treatment3 == "Control")$CV) # NS

rich.sample |> 
  ggplot(aes(x = Treatment3, y = CV)) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("Perennial richness")


# Perennial diversity
shan.sample <- per.div |> 
  group_by(Sample, Channel, Station, Name, Treatment3) |> 
  summarise(CV = sd(shan) / mean(shan),
            .groups = "keep")

summary(filter(shan.sample, Treatment3 == "Treated")$CV)
summary(filter(shan.sample, Treatment3 == "Control")$CV)

t.test(filter(shan.sample, Treatment3 == "Treated")$CV,
       filter(shan.sample, Treatment3 == "Control")$CV) # NS

shan.sample |> 
  ggplot(aes(x = Treatment3, y = CV)) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("Perennial Shannon")


# By Treatment3 (all) -----------------------------------------------------

## Total plant cover
# All years
with(total.all, asymptotic_test(Cover, Treatment3)) # NS

# 2012-2015 precipitation
(6.46 - 10.98) / 10.98 # 41% decrease
total.12.15 <- total.all %>% 
  filter(Year %in% c("2012", "2013", "2014", "2015"))
with(total.12.15, asymptotic_test(Cover, Treatment3)) # NS

# Richness
with(per.div, asymptotic_test(rich, Treatment3)) # 0.03851548

# Shannon
with(per.div, asymptotic_test(shan, Treatment2)) # 0.006889485



save.image("RData/CV.RData")
