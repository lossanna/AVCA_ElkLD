# Purpose: Make figures for ALVSCE poster forum (presented 2023-03-30).

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(scales)

# Load data ---------------------------------------------------------------

total.avg <- read_csv("data/cleaned/Treatment3-average_total-cover.csv")
herb.avg <- read_csv("data/cleaned/Treatment3-average_herb-cover.csv")
rich.avg <- read_csv("data/cleaned/Treatment3-average_richness.csv")

total.cv <- read_csv("data/cleaned/CV-2012-2021_total-cover.csv")
herb.cv <- read_csv("data/cleaned/CV-2012-2021_herb-cover.csv")
rich.cv <- read_csv("data/cleaned/CV-2012-2021_richness.csv")

dat.2021 <- read_csv("data/cleaned/Data-2021_clean.csv")


# Temporal veg data 2012-2021 ---------------------------------------------

# Total cover
tiff("output_figs/ALVSCE_2023/Total-cover_2012-2021.tiff", width = 6, height = 5, units = "in", res = 300)
ggplot(total.avg, aes(x = year.xaxis, y = mean, 
                                    group = Treatment3, 
                                    color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE),
                width = 40) +
  scale_x_date(date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover, 2012-2021") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())
dev.off()


# Herbaceous cover
tiff("output_figs/ALVSCE_2023/Herb-cover_2012-2021.tiff", width = 6, height = 5, units = "in", res = 300)
ggplot(herb.avg, aes(x = year.xaxis, y = mean, 
                      group = Treatment3, 
                      color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE),
                width = 40) +
  scale_x_date(date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous plant cover, 2012-2021") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())
dev.off()


# Richness
tiff("output_figs/ALVSCE_2023/Richness_2012-2021.tiff", width = 6, height = 5, units = "in", res = 300)
ggplot(rich.avg, aes(x = year.xaxis, y = mean,
                     group = Treatment3,
                      color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE),
                width = 40) +
  scale_x_date(date_labels = "%Y") +
  xlab(NULL) +
  ylab("No. of species") +
  ggtitle("Perennial plant richness, 2012-2021") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())
dev.off()


# CV for temporal veg -----------------------------------------------------

# Total cover
tiff("output_figs/ALVSCE_2023/CV_total-cover.tiff", width = 6, height = 5, units = "in", res = 300)
total.cv |> 
  ggplot(aes(x = Treatment3, y = CV, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Coefficient of variation for total cover, 2012-2021",
       x = NULL) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent)
dev.off()


# Herbaceous  
tiff("output_figs/ALVSCE_2023/CV_herb-cover.tiff", width = 6, height = 5, units = "in", res = 300)
herb.cv |> 
  ggplot(aes(x = Treatment3, y = CV, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Coefficient of variation for herb cover, 2012-2021",
       x = NULL) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent)
dev.off()


# Soil & plant 2021 data --------------------------------------------------

# 16S NMDS
tiff("output_figs/ALVSCE_2023/NMDS.tiff", width = 6, height = 5, units = "in", res = 300)
dat.2021 %>% 
  ggplot(aes(x = NMDS1, y = NMDS2, color = Treatment3, shape = Treatment3)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  labs(x = "Axis 1",
       y = "Axis 2",
       title = "Bacteria & archaea NMDS",
       color = "Treatment",
       shape = "Treatment") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  geom_text(aes(x = 0.3, y = -0.55, label = "PERMANOVA, p < 0.05"),
            size = 2.5, color = "gray30") +
  geom_text(aes(x = 0.35, y = -0.65, label = "Stress = 0.168"),
            size = 2.5, color = "gray30") # only 3% explained by Treatment3 lol
dev.off()


# PCA
# Control
dat.pca.ctrl <- dat.2021 |> 
  filter(Treatment3 == "Control") |> 
  select(Cover, rich, shan, TN_log, TC_log, OM_perc, Richness.barc, Shannon.barc) |> 
  rename(TN = TN_log,
         TC = TC_log,
         OM = OM_perc,
         Richness = rich,
         Shannon = shan)
pca.ctrl3 <- PCA(dat.pca.ctrl, scale.unit = TRUE, graph = FALSE)

tiff("output_figs/ALVSCE_2023/PCA-correlation-ctrl.tiff", width = 6, height = 5, units = "in", res = 300)
fviz_pca_var(pca.ctrl3,
             repel = TRUE,
             col.var = "red") +
  labs(title = "PCA for Control, 2021 plant & soil")
dev.off()


# Treated
dat.pca.trt <- dat.2021 |> 
  filter(Treatment3 == "Treated") |> 
  select(Cover, rich, shan, TN_log, TC_log, OM_perc, Richness.barc, Shannon.barc) |> 
  rename(TN = TN_log,
         TC = TC_log,
         OM = OM_perc,
         Richness = rich,
         Shannon = shan)
pca.trt3 <- PCA(dat.pca.trt, scale.unit = TRUE, graph = FALSE)

tiff("output_figs/ALVSCE_2023/PCA-correlation-trt.tiff", width = 6, height = 5, units = "in", res = 300)
fviz_pca_var(pca.trt3,
             repel = TRUE,
             col.var = "#1F78B4") +
  labs(title = "PCA for Treated, 2021 plant & soil")
dev.off()
