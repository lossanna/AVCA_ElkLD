library(tidyverse)
library(car)
library(agricolae)

# Load data ---------------------------------------------------------------

dat.2021 <- read.csv("data/cleaned/Data-2021_clean.csv")


# Total N -----------------------------------------------------------------

t.test(filter(dat.2021, Treatment3 == "Control")$TN_log,
       filter(dat.2021, Treatment3 == "Treated")$TN_log) # NS

ggplot(dat.2021, aes(x = Treatment3, y = TN_ppt)) +
  geom_boxplot() +
  geom_jitter()


# Total C -----------------------------------------------------------------

t.test(filter(dat.2021, Treatment3 == "Control")$TC_log,
       filter(dat.2021, Treatment3 == "Treated")$TC_log) # NS

ggplot(dat.2021, aes(x = Treatment3, y = TC_ppt)) +
  geom_boxplot() +
  geom_jitter()



# C:N ratio ---------------------------------------------------------------

t.test(filter(dat.2021, Treatment3 == "Control")$CN_ratio,
       filter(dat.2021, Treatment3 == "Treated")$CN_ratio) # NS

ggplot(dat.2021, aes(x = Treatment3, y = CN_ratio)) +
  geom_boxplot() +
  geom_jitter()


# Organic matter ----------------------------------------------------------

t.test(filter(dat.2021, Treatment3 == "Control")$OM_perc,
       filter(dat.2021, Treatment3 == "Treated")$OM_perc) # NS

ggplot(dat.2021, aes(x = Treatment3, y = OM_perc)) +
  geom_boxplot() +
  geom_jitter()


# Soil bac arc richness ---------------------------------------------------

t.test(filter(dat.2021, Treatment3 == "Control")$Richness.barc,
       filter(dat.2021, Treatment3 == "Treated")$Richness.barc) # NS

ggplot(dat.2021, aes(x = Treatment3, y = Richness.barc)) +
  geom_boxplot() +
  geom_jitter()


# Soil bac arc diversity --------------------------------------------------

t.test(filter(dat.2021, Treatment3 == "Control")$Shannon.barc,
       filter(dat.2021, Treatment3 == "Treated")$Shannon.barc) # NS

ggplot(dat.2021, aes(x = Treatment3, y = Shannon.barc)) +
  geom_boxplot() +
  geom_jitter()



# Total plant cover -------------------------------------------------------

t.test(filter(dat.2021, Treatment3 == "Control")$Cover,
       filter(dat.2021, Treatment3 == "Treated")$Cover) # NS

ggplot(dat.2021, aes(x = Treatment3, y = Cover)) +
  geom_boxplot() +
  geom_jitter()


# Herbaceous cover --------------------------------------------------------

t.test(filter(dat.2021, Treatment3 == "Control")$Herbaceous,
       filter(dat.2021, Treatment3 == "Treated")$Herbaceous) # NS

ggplot(dat.2021, aes(x = Treatment3, y = Herbaceous)) +
  geom_boxplot() +
  geom_jitter()


# Perennial plant richness ------------------------------------------------

t.test(filter(dat.2021, Treatment3 == "Control")$rich,
       filter(dat.2021, Treatment3 == "Treated")$rich) # NS

ggplot(dat.2021, aes(x = Treatment3, y = rich)) +
  geom_boxplot() +
  geom_jitter()



# Perennial plant diversity -----------------------------------------------

t.test(filter(dat.2021, Treatment3 == "Control")$shan,
       filter(dat.2021, Treatment3 == "Treated")$shan) # NS

ggplot(dat.2021, aes(x = Treatment3, y = shan)) +
  geom_boxplot() +
  geom_jitter()
