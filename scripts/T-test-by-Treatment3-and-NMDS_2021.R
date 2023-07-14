# Purpose: Compare 2021 soil and plant data by Treatment3 (Treated/Control) and write out figures.
#   Used t-test because determined all were normally distributed in Data-screening_2021.R
#     (although some were log-transformed; t-test is on log-transformation, plot back-transformed).
# 
# Created: 2023-02-02
# Last updated: 2023-03-27

library(tidyverse)
library(car)
library(agricolae)
library(metagenomeSeq)

# Load data ---------------------------------------------------------------

dat.2021 <- read.csv("data/cleaned/Data-2021_clean.csv")


# Total N -----------------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$TN_log,
       filter(dat.2021, Treatment3 == "Treated")$TN_log) # NS, p = 0.49

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
       filter(dat.2021, Treatment3 == "Treated")$TC_log) # NS, p = 0.5

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
       filter(dat.2021, Treatment3 == "Treated")$CN_ratio) # NS, p = 0.607

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
t.test(filter(dat.2021, Treatment3 == "Control")$OM_log,
       filter(dat.2021, Treatment3 == "Treated")$OM_log) # NS, p = 0.33

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



# Barc richness -----------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$barc.richness,
       filter(dat.2021, Treatment3 == "Treated")$barc.richness) # NS, p = 0.69

# Plot
barc.rich.plot.21 <- dat.2021 |> 
  ggplot(aes(Treatment3, barc.richness), color = Treatment3) +
  geom_jitter(aes(color = Treatment3), 
              alpha = 0.8, 
              size = 3) +
  geom_boxplot(aes(fill = Treatment3), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("No. of ASVs") +
  ggtitle("Bacteria & archaea richness") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000"))
barc.rich.plot.21

tiff("figures/2023-07_draft-figures/Richness-barc.tiff", width = 6, height = 4, units = "in", res = 150)
barcrich.plot.21
dev.off()



# Barc NMDS ---------------------------------------------------------------

# See 16S_prelim-stats.R script for stress and PERMANOVA analysis

# Plot
barc.nmds.plot.21 <- dat.2021 %>% 
  ggplot(aes(x = barc.NMDS1, y = barc.NMDS2, color = Treatment3, shape = Treatment3)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  labs(x = "Axis 1",
       y = "Axis 2",
       title = "Bacteria & archaea NMDS",
       color = "Treatment",
       shape = "Treatment") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  geom_text(aes(x = 0.3, y = -0.55, label = "PERMANOVA, p = 0.026"),
            size = 3, color = "gray30") +
  geom_text(aes(x = 0.35, y = -0.65, label = "Stress = 0.168"),
            size = 3, color = "gray30") # only 3% explained by Treatment3 lol
barc.nmds.plot.21

tiff("figures/2023-07_draft-figures/NMDS-barc.tiff", height = 5, width = 6, units = "in", res = 150)
barc.nmds.plot.21
dev.off()




# Fungi richness ----------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$fungi.richness,
       filter(dat.2021, Treatment3 == "Treated")$fungi.richness) # NS, p = 0.94

# Plot
fungi.rich.plot.21 <- dat.2021 %>% 
  ggplot(aes(Treatment3, fungi.richness), color = Treatment3) +
  geom_jitter(aes(color = Treatment3), 
              alpha = 0.8, 
              size = 3) +
  geom_boxplot(aes(fill = Treatment3), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("No. of ASVs") +
  ggtitle("Fungi richness") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000"))
fungi.rich.plot.21

tiff("figures/2023-07_draft-figures/Richness-fungi.tiff", width = 6, height = 4, units = "in", res = 150)
fungi.rich.plot.21
dev.off()



# Fungi NMDS --------------------------------------------------------------

# See ITS_prelim-stats.R script for stress and PERMANOVA analysis

# Plot
fungi.nmds.plot.21 <- dat.2021 %>% 
  ggplot(aes(x = fungi.NMDS1, y = fungi.NMDS2, color = Treatment3, shape = Treatment3)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  labs(x = "Axis 1",
       y = "Axis 2",
       title = "Fungi NMDS",
       color = "Treatment",
       shape = "Treatment") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  geom_text(aes(x = 0.7, y = -0.7, label = "PERMANOVA, p = 0.008"),
            size = 3, color = "gray30") +
  geom_text(aes(x = 0.78, y = -0.8, label = "Stress = 0.237"),
            size = 3, color = "gray30") # only 2% explained by Treatment3 lol
fungi.nmds.plot.21

tiff("figures/2023-07_draft-figures/NMDS-fungi.tiff", height = 5, width = 6, units = "in", res = 150)
fungi.nmds.plot.21
dev.off()


# Total plant cover -------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$total,
       filter(dat.2021, Treatment3 == "Treated")$total) # NS, p = 0.99

# Plot
total.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = total, fill = Treatment3, color = Treatment3)) +
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
t.test(filter(dat.2021, Treatment3 == "Control")$herb,
       filter(dat.2021, Treatment3 == "Treated")$herb) # NS, p = 0.256

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
       filter(dat.2021, Treatment3 == "Treated")$notree) # NS, p = 0.368

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
t.test(filter(dat.2021, Treatment3 == "Control")$perveg.richness,
       filter(dat.2021, Treatment3 == "Treated")$perveg.richness) # NS, p = 0.13

# Plot
rich.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = perveg.richness, fill = Treatment3, color = Treatment3)) +
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

t.test(filter(dat.2021, Treatment3 == "Control")$perveg.shannon,
       filter(dat.2021, Treatment3 == "Treated")$perveg.shannon) # NS, p = 0.22

# Plot
shan.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = perveg.shannon, fill = Treatment3, color = Treatment3)) +
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


save.image("RData/T-test-by-Treatment3-and-NMDS_2021.RData")
