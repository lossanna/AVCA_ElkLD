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



# By Treatment2 -----------------------------------------------------------

## Total plant cover ------------------------------------------------------

# All years
with(total.all, asymptotic_test(Cover, Treatment2)) # p = 0.04598678

totalin.cn.all <- total.all %>% 
  filter(Treatment2 %in% c("In-channel treatment", "Control"))
with(totalin.cn.all, asymptotic_test(Cover, Treatment2)) # p = 0.01270811

totalin.up.all <- total.all %>% 
  filter(Treatment2 %in% c("Upland treatment", "In-channel treatment"))
with(totalin.up.all, asymptotic_test(Cover, Treatment2)) # NS

totalup.cn.all <- total.all %>% 
  filter(Treatment2 %in% c("Upland treatment", "Control"))
with(totalup.cn.all, asymptotic_test(Cover, Treatment2)) # NS


# 2012-2015 precipitation
(6.46 - 10.98) / 10.98 # 41% decrease
total.12.15 <- total.all %>% 
  filter(Year %in% c("2012", "2013", "2014", "2015"))

with(total.12.15, asymptotic_test(Cover, Treatment2)) # NS



## Richness ---------------------------------------------------------------

# All years
with(per.div, asymptotic_test(rich, Treatment2)) # p = 0.01160437

richin.cn.all <- per.div %>% 
  filter(Treatment2 %in% c("In-channel treatment", "Control"))
with(richin.cn.all, asymptotic_test(rich, Treatment2)) # p = 0.002754744

richin.up.all <- per.div %>% 
  filter(Treatment2 %in% c("Upland treatment", "In-channel treatment"))
with(richin.up.all, asymptotic_test(rich, Treatment2)) # NS

richup.cn.all <- per.div %>% 
  filter(Treatment2 %in% c("Upland treatment", "Control"))
with(richup.cn.all, asymptotic_test(rich, Treatment2)) # p = 0.0307329


# 2012-2015 precipitation
(6.46 - 10.98) / 10.98 # 41% decrease
rich.12.15 <- per.div %>% 
  filter(Year %in% c("2012", "2013", "2014", "2015"))

with(rich.12.15, asymptotic_test(rich, Treatment2)) # NS




## Shannon ----------------------------------------------------------------

# All years
with(per.div, asymptotic_test(shan, Treatment2)) # p = 0.006889485

shanin.cn.all <- per.div %>% 
  filter(Treatment2 %in% c("In-channel treatment", "Control"))
with(shanin.cn.all, asymptotic_test(shan, Treatment2)) # p = 0.005136371

shanin.up.all <- per.div %>% 
  filter(Treatment2 %in% c("Upland treatment", "In-channel treatment"))
with(shanin.up.all, asymptotic_test(shan, Treatment2)) # p = 0.04515136

shanup.cn.all <- per.div %>% 
  filter(Treatment2 %in% c("Upland treatment", "Control"))
with(shanup.cn.all, asymptotic_test(shan, Treatment2)) # NS


# 2012-2015 precipitation
(6.46 - 10.98) / 10.98 # 41% decrease
shan.12.15 <- per.div %>% 
  filter(Year %in% c("2012", "2013", "2014", "2015"))

with(shan.12.15, asymptotic_test(shan, Treatment2)) # p = 0.008836917

shanin.cn.12.15 <- shan.12.15 %>% 
  filter(Treatment2 %in% c("In-channel treatment", "Control"))
with(shanin.cn.12.15, asymptotic_test(shan, Treatment2)) # p = 0.006691954

shanin.up.12.15 <- shan.12.15 %>% 
  filter(Treatment2 %in% c("Upland treatment", "In-channel treatment"))
with(shanin.up.12.15, asymptotic_test(shan, Treatment2)) # NS

shanup.cn.12.15 <- shan.12.15 %>% 
  filter(Treatment2 %in% c("Upland treatment", "Control"))
with(shanup.cn.12.15, asymptotic_test(shan, Treatment2)) # NS



# By channel --------------------------------------------------------------

# Total plant cover
# Between all channels
with(total.all, asymptotic_test(Cover, Channel))

# Between Channels 12 and 13
total1213.all <- total.all %>% 
  filter(Channel %in% c("Channel 12", "Channel 13"))
with(total1213.all, asymptotic_test(Cover, Channel)) # NS

# Between Channels 19 and 13
total1913.all <- total.all %>% 
  filter(Channel %in% c("Channel 19", "Channel 13"))
with(total1913.all, asymptotic_test(Cover, Channel)) # p = 0.001092162

# Between Channels 12 and 21
total1221.all <- total.all %>% 
  filter(Channel %in% c("Channel 12", "Channel 21"))
with(total1221.all, asymptotic_test(Cover, Channel)) # p = 0.03117101

# Between Channels 19 and 21
total1921.all <- total.all %>% 
  filter(Channel %in% c("Channel 19", "Channel 21"))
with(total1921.all, asymptotic_test(Cover, Channel)) # NS

# Between Channels 12 and 19
total1219.all <- total.all %>% 
  filter(Channel %in% c("Channel 12", "Channel 19"))
with(total1219.all, asymptotic_test(Cover, Channel)) # NS

# Between Channels 13 and 21
total1321.all <- total.all %>% 
  filter(Channel %in% c("Channel 13", "Channel 21"))
with(total1321.all, asymptotic_test(Cover, Channel)) # p = 0.0004935618




save.image("RData/CV.RData")
