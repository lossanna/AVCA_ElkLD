library(tidyverse)
library(vegan)
library(plotrix)
library(agricolae)
library(ggpubr)
 
# Load data ---------------------------------------------------------------

precip <- read.table("data/PimaCounty_precip/PimaCounty_precip_2012-2021.txt",
                     sep = "\t", header = TRUE)
precip$year.xaxis <- as.Date(precip$year.xaxis)

total.avg <- read_csv("data/cleaned/Treatment2-average_total-cover.csv") %>% 
  mutate(year.date = as.Date(year.date),
         year.xaxis = as.Date(year.xaxis))

richness.avg <- read_csv("data/cleaned/Treatment2-average_richness.csv") %>% 
  mutate(year.date = as.Date(year.date),
         year.xaxis = as.Date(year.xaxis))

shannon.avg <- read_csv("data/cleaned/Treatment2-average_Shannon.csv") %>% 
  mutate(year.date = as.Date(year.date),
         year.xaxis = as.Date(year.xaxis))

meta <- read.table("data/cleaned/sequencing/bac_arc_diversity.txt",
                    sep = "\t", header = TRUE)

dat.2021 <- read_csv("data/cleaned/SEM-input.csv")



# Precipitation -----------------------------------------------------------

precip.sample <- precip[1:6, ]

precip.srm23 <- ggplot(precip.sample, aes(x = year.xaxis, y = Precip_cum)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Precipitation (in)") +
  ggtitle("Cumulative summer precipitation") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 14)) +
  theme(axis.text.x = element_text(color = "#000000")) +
  labs(caption = "Data from Pima County precipitation gauge #6380") +
  theme(plot.caption = element_text(size = 6))
precip.srm23

tiff("output_figs/SRM_2023/Precip.tiff", units = "in", height = 3, width = 4, res = 300)
precip.srm23
dev.off()
  

ggplot(precip, aes(x = year.xaxis, y = Precip_cum)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Precipitation (in)") +
  ggtitle("Cumulative summer precipitation") +
  theme_bw() +
  scale_y_continuous(limits = c(0, 14)) +
  theme(axis.text.x = element_text(color = "#000000")) +
  labs(caption = "Data from Pima County precipitation gauge #6380") +
  theme(plot.caption = element_text(size = 6))


# Total plant cover -------------------------------------------------------

total.plot.srm23 <- ggplot(total.avg, aes(x = year.xaxis, y = mean, 
                                    group = Treatment2, 
                                    color = Treatment2)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Treatment2) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000"))
total.plot.srm23

tiff("output_figs/SRM_2023/Total-plant-cover.tiff", units = "in", height = 4, width = 11, res = 300)
total.plot.srm23
dev.off()

tiff("output_figs/SRM_2023/Total-plant-cover_narrow.tiff", units = "in", height = 5, width = 7, res = 300)
total.plot.srm23
dev.off()



# Species richness --------------------------------------------------------


richness.plot.srm23 <- ggplot(richness.avg, aes(x = year.xaxis, y = mean, 
                                                group = Treatment2, 
                                                color = Treatment2)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Number of species") +
  ggtitle("Perennial species richness") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~Treatment2) +
  theme(axis.text.x = element_text(color = "#000000"))
richness.plot.srm23

tiff("output_figs/SRM_2023/Richess.tiff", units = "in", height = 4, width = 11, res = 300)
richness.plot.srm23
dev.off()

tiff("output_figs/SRM_2023/Richess_narrow.tiff", units = "in", height = 5, width = 7, res = 300)
richness.plot.srm23
dev.off()


# Shannon diversity -------------------------------------------------------

shannon.plot.srm23 <- ggplot(shannon.avg, aes(x = year.xaxis, y = mean, 
                                              group = Treatment2, 
                                              color = Treatment2)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial plant diversity") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~Treatment2) +
  theme(axis.text.x = element_text(color = "#000000"))
shannon.plot.srm23

tiff("output_figs/SRM_2023/Shannon.tiff", units = "in", height = 4, width = 11, res = 300)
shannon.plot.srm23
dev.off()



# 2021 data wrangling -----------------------------------------------------

# Add trt.short col
dat.2021 <- dat.2021 %>% 
  mutate(trt.short = case_when(
    str_detect(channel.trt, "In-channel") ~ "In-channel",
    str_detect(channel.trt, "Upland") ~ "Upland",
    str_detect(channel.trt, "No treat") ~ "Control")) %>% 
  mutate(TN_ppt = TN_perc * 10,
         TC_ppt = TC_perc * 10) 


meta <- meta %>% 
  mutate(trt.short = case_when(
    str_detect(Treatment2, "In-channel") ~ "In-channel",
    str_detect(Treatment2, "Upland") ~ "Upland",
    str_detect(Treatment2, "No treat") ~ "Control")) 
  



# 2021 soil chemistry -----------------------------------------------------

# Total N
letters <- data.frame(label = c("b", "a", "a"),
                      x = 1:3,
                      y = rep(0.59, 3))

tn2021.plot <- ggplot(dat.2021, aes(x = trt.short, y = TN_ppt)) +
  geom_boxplot(aes(fill = trt.short),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = trt.short),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Total N (mg/g soil)") +
  ggtitle("Soil nitrogen") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "#000000")) +
  geom_text(aes(x = 1.1, y = 0.68, label = "ANOVA, p < 0.05"),
          size = 2.3, color = "gray30") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
tn2021.plot


# Total C 
letters <- data.frame(label = c("b", "a", "a"),
                      x = 1:3,
                      y = rep(6.5, 3))

tc2021.plot <- ggplot(dat.2021, aes(x = trt.short, y = TC_ppt)) +
  geom_boxplot(aes(fill = trt.short),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = trt.short),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Total C (mg/g soil)") +
  ggtitle("Soil carbon") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "#000000")) +
  geom_text(aes(x = 1.1, y = 7.25, label = "ANOVA, p < 0.05"),
            size = 2.3, color = "gray30") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
tc2021.plot



# Organic matter
letters <- data.frame(label = c("c", "b", "a"),
                      x = 1:3,
                      y = rep(2.04, 3))
om2021.plot <- ggplot(dat.2021, aes(x = trt.short, y = OM_perc)) +
  geom_boxplot(aes(fill = trt.short),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = trt.short),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Organic matter (%)") +
  ggtitle("Soil organic matter") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "#000000")) +
  geom_text(aes(x = 1, y = 2.3, label = "ANOVA, p < 0.05"),
            size = 2.3, color = "gray30") +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
om2021.plot

# Combine soil chemistry graphs 
tiff("output_figs/SRM_2023/Soil-chem-2021.tiff", units = "in", height = 4.7, width = 8, res = 300)
soilchem2021.plot <- ggarrange(tn2021.plot, tc2021.plot, om2021.plot, 
          ncol = 3, nrow = 1) 
annotate_figure(soilchem2021.plot,
                bottom = "Treatment")
dev.off()



# 2021 soil microbial -----------------------------------------------------

# Soil bac arc richness 
barc.rich2021.plot <- ggplot(meta, aes(x = trt.short, y = Richness)) +
  geom_boxplot(aes(fill = trt.short),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = trt.short),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("No. of species (ASVs)") +
  ggtitle("Bacterial & archaeal richness") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
barc.rich2021.plot

tiff("output_figs/SRM_2023/Soil-richness.tiff", units = "in", height = 4, width = 4.5, res = 300)
barc.rich2021.plot
dev.off()


# Soil bac arc diversity 
barc.shan2021.plot <- ggplot(meta, aes(x = trt.short, y = Shannon)) +
  geom_boxplot(aes(fill = trt.short),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = trt.short),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Shannon diversity index") + 
  ggtitle("Bacterial & archaeal diversity") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
barc.shan2021.plot

# Combine soil graphs 
tiff("output_figs/SRM_2023/Soil-microbial-2021.tiff", units = "in", height = 4.7, width = 7, res = 300)
soilbarc2021.plot <- ggarrange(barc.rich2021.plot, barc.shan2021.plot,
          ncol = 2, nrow = 1)
annotate_figure(soilbarc2021.plot,
                bottom = "Treatment")
dev.off()



# 2021 plant  -------------------------------------------------------------

# Total plant cover
total2021.plot <- ggplot(dat.2021, aes(x = trt.short, y = Cover)) +
  geom_boxplot(aes(fill = trt.short),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = trt.short),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
total2021.plot

# Perennial plant richness
rich2021.plot <- ggplot(dat.2021, aes(x = trt.short, y = rich)) +
  geom_boxplot(aes(fill = trt.short),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = trt.short),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("No. of species") +
  ggtitle("Perennial plant richness") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
rich2021.plot

# Perennial plant diversity 
shan2021.plot <- ggplot(dat.2021, aes(x = trt.short, y = shan)) +
  geom_boxplot(aes(fill = trt.short),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = trt.short),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial plant diversity") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) 
shan2021.plot


# Combine plant graphs 
tiff("output_figs/SRM_2023/Plant2021.tiff", units = "in", height = 4.7, width = 8, res = 300)
plant2021.plot <- ggarrange(total2021.plot, rich2021.plot, shan2021.plot,
          ncol = 3, nrow = 1)
annotate_figure(plant2021.plot,
                bottom = "Treatment")
dev.off()



# 2021 NMDS and beta dispersion -------------------------------------------

# NMDS
nmds2021 <- meta %>% 
  ggplot(aes(x = NMDS1, y = NMDS2, color = trt.short, shape = trt.short)) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(18, 15, 17)) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4")) +
  theme_bw(base_size = 14) +
  labs(x = "Axis 1",
       y = "Axis 2",
       title = "Bacteria & archaea NMDS",
       color = "Treatment",
       shape = "Treatment") +
  theme(legend.position = "bottom") +
  geom_text(aes(x = 0.3, y = -0.55, label = "PERMANOVA, p < 0.05"),
            size = 2.5, color = "gray30") +
  geom_text(aes(x = 0.35, y = -0.65, label = "Stress = 0.168"),
            size = 2.5, color = "gray30") 
nmds2021

# Beta dispersion
letters <- data.frame(label = c("a", "a", "b"),
                      x = 1:3,
                      y = c(rep(0.55, 3)))

betadisp2021 <- meta %>% 
  ggplot(aes(trt.short, betadisper.treatment2)) +
  geom_jitter(aes(color = trt.short,
                  shape = trt.short), 
              alpha = 0.8, 
              size = 3) +
  geom_boxplot(aes(fill = trt.short), 
               alpha = 0.3, 
               outlier.shape = NA) +
  scale_shape_manual(values = c(18, 15, 17)) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4")) +
  xlab(NULL) +
  ylab("Beta dispersion") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  ggtitle("Beta diversity") +
  geom_text(aes(x = 3.1, y = 0.22, label = "ANOVA, p < 0.05"),
            size = 2.5, color = "gray30")
betadisp2021


# Combine plots
tiff("output_figs/SRM_2023/NMDS_beta-dispersion.tiff", units = "in", height = 5, width = 8.5, res = 300)
ggarrange(nmds2021, betadisp2021,
          ncol = 2, nrow = 1, common.legend = TRUE,
          legend = "bottom", widths = c(1.5, 1))
dev.off()



# Save --------------------------------------------------------------------

save.image("RData/SRM-national-2023-figs.RData")

