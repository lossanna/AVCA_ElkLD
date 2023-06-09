# Purpose: Compare 2021 soil and plant data by Treatment3 (Treated/Control).
#   Used t-test because determined all were normally distributed in Data-screening_2021.R
# Created: 2023-02-02
# Last updated: 2023-03-27

library(tidyverse)
library(car)
library(agricolae)

# Load data ---------------------------------------------------------------

dat.2021 <- read.csv("data/cleaned/Data-2021_clean.csv")


# Total N -----------------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$TN_log,
       filter(dat.2021, Treatment3 == "Treated")$TN_log) # NS

# Plot
tn.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = TN_ppt, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Total soil nitrogen",
       x = NULL,
       y = "Total N (mg/g)") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
tn.plot.21


# Total C -----------------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$TC_log,
       filter(dat.2021, Treatment3 == "Treated")$TC_log) # NS

# Plot
tc.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = TC_ppt, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Total soil carbon",
       x = NULL,
       y = "Total C (mg/g)") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
tc.plot.21



# C:N ratio ---------------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$CN_ratio,
       filter(dat.2021, Treatment3 == "Treated")$CN_ratio) # NS

# Plot
cn.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = CN_ratio, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Carbon:Nitrogen ratio",
       x = NULL,
       y = "C:N") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
cn.plot.21


# Organic matter ----------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$OM_perc,
       filter(dat.2021, Treatment3 == "Treated")$OM_perc) # NS

# Plot
om.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = OM_perc, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Soil organic matter",
       x = NULL,
       y = "Organic matter (%)") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
om.plot.21


# Soil bac arc richness ---------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$Richness.barc,
       filter(dat.2021, Treatment3 == "Treated")$Richness.barc) # NS

# Plot
barcrich.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = Richness.barc, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Bacteria & archaea richness",
       x = NULL,
       y = "No. of ASVs") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
barcrich.plot.21


# Soil bac arc diversity --------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$Shannon.barc,
       filter(dat.2021, Treatment3 == "Treated")$Shannon.barc) # NS

# Plot
barcshan.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = Shannon.barc, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Bacteria & archaea diversity",
       x = NULL,
       y = "Shannon diversity index") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
barcshan.plot.21


# Soil bac arc beta dispersion --------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$betadisper.treatment3,
       filter(dat.2021, Treatment3 == "Treated")$betadisper.treatment3) # NS

# Plot
barcbetadisp.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = betadisper.treatment3, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Bacteria & archaea beta dispersion",
       x = NULL,
       y = "Beta dispersion") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
barcbetadisp.plot.21


# Total plant cover -------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$Cover,
       filter(dat.2021, Treatment3 == "Treated")$Cover) # NS

# Plot
total.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = Cover, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Total plant cover",
       x = NULL,
       y = "Cover (%)") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
total.plot.21


# Herbaceous cover --------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$Herbaceous,
       filter(dat.2021, Treatment3 == "Treated")$Herbaceous) # NS

# Plot
herb.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = herb, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Herbaceous cover",
       x = NULL,
       y = "Cover (%)") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
herb.plot.21


# Notree cover ------------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$notree,
       filter(dat.2021, Treatment3 == "Treated")$notree) # NS

# Plot
notree.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = notree, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Grass, forb & shrub cover",
       x = NULL,
       y = "Cover (%)") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
notree.plot.21


# Perennial plant richness ------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$rich,
       filter(dat.2021, Treatment3 == "Treated")$rich) # NS

# Plot
rich.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = rich, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Perennial plant richness",
       x = NULL,
       y = "No. of species") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
rich.plot.21



# Perennial plant diversity -----------------------------------------------

t.test(filter(dat.2021, Treatment3 == "Control")$shan,
       filter(dat.2021, Treatment3 == "Treated")$shan) # NS

# Plot
shan.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = shan, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Perennial plant diversity",
       x = NULL,
       y = "Shannon diversity index") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
shan.plot.21


save.image("RData/T-test-by-Treatment3_2021.RData")
