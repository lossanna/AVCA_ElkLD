# Purpose: Compare 2021 soil and plant data by Treatment3 (Treated/Control) and write out figures.
#   Used t-test because determined all were normally distributed in Data-screening_2021.R
#     (although some were log-transformed; t-test is on log-transformation, plot back-transformed).
# 
# Created: 2023-02-02
# Last updated: 2023-07-21

library(tidyverse)
library(car)
library(agricolae)
library(ggpubr)
library(scales)

# Load data ---------------------------------------------------------------

dat.2021 <- read.csv("data/cleaned/Data-2021_clean.csv")


# Total N -----------------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$TN_log,
       filter(dat.2021, Treatment3 == "Treated")$TN_log) # NS, p = 0.490

summary(filter(dat.2021, Treatment3 == "Control")$TN_ppt)
summary(filter(dat.2021, Treatment3 == "Treated")$TN_ppt)

# Plot
tn.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = TN_ppt)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment3)) +
  geom_jitter(size = 2,
              alpha = 0.8,
              aes(color = Treatment3)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Total soil nitrogen",
       x = NULL,
       y = "Total N (mg/g)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000"))  +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  geom_text(aes(x = 2.16, y = 63, label = "t-test, p = 0.490"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, linetype = "dashed")
tn.plot.21

# Correlation with n-cyclers
summary(lm(dat.2021$n.cycler_log ~ dat.2021$TN_log))

ggplot(dat.2021, aes(x = TN_log, y = n.cycler_log)) +
  geom_point() +
  geom_smooth(method = "lm") +
  xlab("TN (log)") +
  ylab("N-cycler (log)") +
  theme_bw() +
  theme(axis.text.x = element_text(color = "black")) +
  stat_regline_equation(label.x = 1.2, label.y = 0.6) +
  stat_cor(label.x = 1.2, label.y = 0.4)


# Total C -----------------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$TC_log,
       filter(dat.2021, Treatment3 == "Treated")$TC_log) # NS, p = 0.500

summary(filter(dat.2021, Treatment3 == "Control")$TC_ppt)
summary(filter(dat.2021, Treatment3 == "Treated")$TC_ppt)

# Plot
tc.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = TC_ppt)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment3)) +
  geom_jitter(size = 2,
              alpha = 0.8,
              aes(color = Treatment3)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Total soil carbon",
       x = NULL,
       y = "Total C (mg/g)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000"))  +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  geom_text(aes(x = 2.16, y = 670, label = "t-test, p = 0.500"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, linetype = "dashed")
tc.plot.21



# C:N ratio ---------------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$CN_ratio,
       filter(dat.2021, Treatment3 == "Treated")$CN_ratio) # NS, p = 0.607

# Plot
cn.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = CN_ratio)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment3)) +
  geom_jitter(size = 2,
              alpha = 0.8,
              aes(color = Treatment3)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Carbon:Nitrogen ratio",
       x = NULL,
       y = "C:N") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000"))
cn.plot.21


# Organic matter ----------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$OM_log,
       filter(dat.2021, Treatment3 == "Treated")$OM_log) # NS, p = 0.332

summary(filter(dat.2021, Treatment3 == "Control")$OM_perc)
summary(filter(dat.2021, Treatment3 == "Treated")$OM_perc)

# Plot
om.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = OM_perc)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment3)) +
  geom_jitter(size = 2,
              alpha = 0.8,
              aes(color = Treatment3)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Soil organic matter",
       x = NULL,
       y = "Organic matter (%)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000"))  +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  geom_text(aes(x = 2.16, y = 2.14, label = "t-test, p = 0.332"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, linetype = "dashed")
om.plot.21



# Combine soil chem -------------------------------------------------------

# TN, TC, OM
tiff("figures/2023-07_draft-figures/Soil-chem-2021.tiff", units = "in", height = 4.7, width = 8, res = 150)
ggarrange(tn.plot.21, tc.plot.21, om.plot.21, 
          ncol = 3, nrow = 1,
          labels = c("(A)", "(B)", "(C)")) 

dev.off()



# Barc richness -----------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$barc.richness,
       filter(dat.2021, Treatment3 == "Treated")$barc.richness) # NS, p = 0.686

# Plot
barc.rich.plot.21 <- dat.2021 |> 
  ggplot(aes(Treatment3, barc.richness)) +
  geom_jitter(aes(color = Treatment3), 
              alpha = 0.8, 
              size = 2) +
  geom_boxplot(aes(fill = Treatment3), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("No. of ASVs") +
  ggtitle("Bacterial & archaeal richness") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000")) +
  geom_text(aes(x = 2.3, y = 1100, label = "t-test, p = 0.686"),
            color = "gray30",
            size = 2.5) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, linetype = "dashed")
barc.rich.plot.21



# Fungi richness ----------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$fungi.richness,
       filter(dat.2021, Treatment3 == "Treated")$fungi.richness) # NS, p = 0.938

# Plot
fungi.rich.plot.21 <- dat.2021 %>% 
  ggplot(aes(Treatment3, fungi.richness)) +
  geom_jitter(aes(color = Treatment3), 
              alpha = 0.8, 
              size = 2) +
  geom_boxplot(aes(fill = Treatment3), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("No. of ASVs") +
  ggtitle("Fungal richness") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000")) +
  geom_text(aes(x = 2.3, y = 70, label = "t-test, p = 0.938"),
            color = "gray30",
            size = 2.5) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, linetype = "dashed")
fungi.rich.plot.21


# Combine soil richness ---------------------------------------------------

tiff("figures/2023-07_draft-figures/Soil-richness-2021.tiff", units = "in", height = 4.7, width = 7, res = 150)
ggarrange(barc.rich.plot.21, fungi.rich.plot.21,
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"))

dev.off()



# Barc NMDS ---------------------------------------------------------------

# See 16S_prelim-stats.R script for stress and PERMANOVA analysis

# Plot
barc.nmds.plot.21 <- dat.2021 %>% 
  ggplot(aes(x = barc.NMDS1, y = barc.NMDS2, color = Treatment3, shape = Treatment3)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  labs(x = "Axis 1",
       y = "Axis 2",
       title = "Bacteria & archaea",
       color = "Treatment",
       shape = "Treatment") +
  theme(legend.position = "bottom") +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  theme(legend.title = element_blank()) +
  geom_text(aes(x = 0.3, y = -0.55, label = "PERMANOVA, p = 0.026"),
            size = 3, color = "gray30") +
  geom_text(aes(x = 0.35, y = -0.65, label = "Stress = 0.168"),
            size = 3, color = "gray30") # only 3% explained by Treatment3 lol
barc.nmds.plot.21


# Fungi NMDS --------------------------------------------------------------

# See ITS_prelim-stats.R script for stress and PERMANOVA analysis

# Plot
fungi.nmds.plot.21 <- dat.2021 %>% 
  ggplot(aes(x = fungi.NMDS1, y = fungi.NMDS2, color = Treatment3, shape = Treatment3)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  labs(x = "Axis 1",
       y = "Axis 2",
       title = "Fungi",
       color = "Treatment",
       shape = "Treatment") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  geom_text(aes(x = 0.83, y = -0.7, label = "PERMANOVA, p = 0.008"),
            size = 3, color = "gray30") +
  geom_text(aes(x = 0.92, y = -0.8, label = "Stress = 0.237"),
            size = 3, color = "gray30") # only 2% explained by Treatment3 lol
fungi.nmds.plot.21


# Combine NMDS ------------------------------------------------------------

tiff("figures/2023-07_draft-figures/Soil-NMDS-2021.tiff", height = 5, width = 11, units = "in", res = 150)
ggarrange(barc.nmds.plot.21, fungi.nmds.plot.21,
          nrow = 1, ncol = 2,
          labels = c("(A)", "(B)"),
          common.legend = TRUE, legend = "bottom")
dev.off()



# Chemoheterotrophs -------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$chemoheterotrophy_log,
       filter(dat.2021, Treatment3 == "Treated")$chemoheterotrophy_log) # NS, p = 0.931

# Plot
chemohet.plot.21 <- dat.2021 %>% 
  ggplot(aes(Treatment3, chemoheterotrophy_perc)) +
  geom_jitter(aes(color = Treatment3), 
              alpha = 0.8, 
              size = 2) +
  geom_boxplot(aes(fill = Treatment3), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("Relative abundance (%)") +
  ggtitle("Chemoheterotrophic \nbacteria & archaea") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  geom_text(aes(x = 2.16, y = 28.7, label = "t-test, p = 0.931"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, linetype = "dashed")
chemohet.plot.21



# N-cyclers ---------------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$n.cycler_log,
       filter(dat.2021, Treatment3 == "Treated")$n.cycler_log) # NS, p = 0.490

# Plot
ncycler.plot.21 <- dat.2021 %>% 
  ggplot(aes(Treatment3, n.cycler_perc)) +
  geom_jitter(aes(color = Treatment3), 
              alpha = 0.8, 
              size = 2) +
  geom_boxplot(aes(fill = Treatment3), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("Relative abundance (%)") +
  ggtitle("Nitrogen-cycling \nbacteria & archaea") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  geom_text(aes(x = 2.16, y = 13.6, label = "t-test, p = 0.490"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, linetype = "dashed")
ncycler.plot.21



# Saprotrophs -------------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$saprotroph,
       filter(dat.2021, Treatment3 == "Treated")$saprotroph) # NS, p = 0.272

# Plot
sapro.plot.21 <- dat.2021 %>% 
  ggplot(aes(Treatment3, saprotroph)) +
  geom_jitter(aes(color = Treatment3), 
              alpha = 0.8, 
              size = 2) +
  geom_boxplot(aes(fill = Treatment3), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("Relative abundance (%)") +
  ggtitle("Saprotrophic fungi") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  geom_text(aes(x = 2.16, y = 42, label = "t-test, p = 0.272"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, linetype = "dashed")
sapro.plot.21



# Combine FAPROTAX and FUNGuild -------------------------------------------

tiff("figures/2023-07_draft-figures/FAPROTAX-FUNGuild-2021.tiff", units = "in", height = 4.7, width = 8, res = 150)
ggarrange(chemohet.plot.21, ncycler.plot.21, sapro.plot.21, 
          ncol = 3, nrow = 1,
          labels = c("(A)", "(B)", "(C)")) 

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
  theme_bw() +
  theme(legend.position = "none") 
herb.plot.21


# Notree cover ------------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$notree,
       filter(dat.2021, Treatment3 == "Treated")$notree) # NS, p = 0.368

# Plot
notree.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = notree)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment3)) +
  geom_jitter(size = 2,
              aes(color = Treatment3)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Vegetation cover",
       x = NULL,
       y = "Cover (%)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  geom_text(aes(x = 0.9, y = 95, label = "t-test, p = 0.368"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, linetype = "dashed")
notree.plot.21


# Perennial plant richness ------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment3 == "Control")$perveg.richness,
       filter(dat.2021, Treatment3 == "Treated")$perveg.richness) # NS, p = 0.135

# Plot
rich.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = perveg.richness)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment3)) +
  geom_jitter(size = 2,
              aes(color = Treatment3)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Perennial plant species \nrichness",
       x = NULL,
       y = "No. of species") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  geom_text(aes(x = 0.9, y = 12.8, label = "t-test, p = 0.135"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, linetype = "dashed")
rich.plot.21



# Perennial plant diversity -----------------------------------------------

t.test(filter(dat.2021, Treatment3 == "Control")$perveg.shannon,
       filter(dat.2021, Treatment3 == "Treated")$perveg.shannon) # NS, p = 0.221

# Plot
shan.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment3, y = perveg.shannon)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment3)) +
  geom_jitter(size = 2,
              aes(color = Treatment3)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Perennial plant diversity",
       x = NULL,
       y = "Shannon diversity index") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  geom_text(aes(x = 0.9, y = 2.2, label = "t-test, p = 0.221"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = .75, linetype = "dashed")
shan.plot.21



# Combine 2021 veg  -------------------------------------------------------

tiff("figures/2023-07_draft-figures/Notree-2021.tiff", units = "in", height = 4.7, width = 8, res = 150)
ggarrange(notree.plot.21, rich.plot.21, shan.plot.21, 
          ncol = 3, nrow = 1,
          labels = c("(A)", "(B)", "(C)")) 

dev.off()


save.image("RData/T-test-by-Treatment3-and-NMDS_2021.RData")
