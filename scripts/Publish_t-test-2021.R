# Purpose: Create 2021 figures for publication, and code for published R Markdown.
#   Main figures: none
#   Supp figures: elevation, TN/TC/OM, barc richness/fungi richness, chet/ncycler/sapro
#   Code: t-test/Mann-Whitney and plot for soil chem & microbial (not elevation)

# Created: 2023-08-28
# Last updated: 2023-11-28

library(tidyverse)
library(ggpubr)

# Load data ---------------------------------------------------------------

dat.2021 <- read.csv("data/publish/Veg-soil-elev_2021.csv")


# Channel elevation change ------------------------------------------------

elev <- dat.2021 |> 
  filter(!is.na(dElev_corrected))

# Number of samples that increased
count(filter(dat.2021, Treatment == "Control"), dElev_corrected) # 27 0s
27/31

count(filter(dat.2021, Treatment == "Treated"), dElev_corrected)
7/19
summary(filter(dat.2021, Treatment == "Treated")$dElev_corrected)

# Mann-Whitney
wilcox.test(filter(dat.2021, Treatment == "Control")$dElev_corrected,
            filter(dat.2021, Treatment == "Treated")$dElev_corrected,
            paired = FALSE, exact = FALSE) # p-value = 6.48e-05

# Plot
letters <- data.frame(x = c(1, 2),
                      y = c(0.45, 0.45),
                      label = c("b", "a"),
                      Treatment = c("Control", "Treated"))

dElev.corrected.plot <- elev |> 
  ggplot(aes(x = Treatment, y = dElev_corrected)) +
  geom_boxplot(aes(fill = Treatment),
               alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment),
              size = 1,
              alpha = 0.8) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Change in channel elevation, 2011-2019",
       x = NULL,
       y = "Elevation change (m)") +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "black")) +
  geom_text(aes(x = 2.3, y = -0.05, label = "Mann-Whitney, p < 0.001"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = 0.75, linetype = "dashed")
dElev.corrected.plot

# Initial submission
tiff("figures/2023-09_publish-figures/FigS3_Elevation.tiff", width = 6, height = 4, units = "in", res = 300)
dElev.corrected.plot
dev.off()

# Revision 1
tiff("figures/2023-12_publish-figures/FigS2_Elevation.tiff", width = 5, height = 3.5, units = "in", res = 300)
dElev.corrected.plot
dev.off()



# Total N -----------------------------------------------------------------

# Mann-Whitney
wilcox.test(filter(dat.2021, Treatment == "Control")$TN_ppt,
            filter(dat.2021, Treatment == "Treated")$TN_ppt,
            exact = FALSE) # p-value = 0.1731

# Plot
tn.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment, y = TN_ppt)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment)) +
  geom_jitter(size = 1,
              alpha = 0.8,
              aes(color = Treatment)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Total soil nitrogen",
       x = NULL,
       y = "Total N (mg/g)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000"))  +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  geom_text(aes(x = 2, y = 63, label = "Mann-Whitney, \np = 0.173"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = 0.75, linetype = "dashed") +
  theme(plot.title = element_text(size = 12))
tn.plot.21


# Total C -----------------------------------------------------------------

# Mann-Whitney
wilcox.test(filter(dat.2021, Treatment == "Control")$TC_ppt,
            filter(dat.2021, Treatment == "Treated")$TC_ppt,
            exact = FALSE) # p-value = 0.2397

# Plot
tc.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment, y = TC_ppt)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment)) +
  geom_jitter(size = 1,
              alpha = 0.8,
              aes(color = Treatment)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Total soil carbon",
       x = NULL,
       y = "Total C (mg/g)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000"))  +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  geom_text(aes(x = 2, y = 670, label = "Mann-Whitney, \np = 0.240"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = 0.75, linetype = "dashed") +
  theme(plot.title = element_text(size = 12))
tc.plot.21



# Organic matter ----------------------------------------------------------

# Mann-Whitney
wilcox.test(filter(dat.2021, Treatment == "Control")$OM_perc,
            filter(dat.2021, Treatment == "Treated")$OM_perc) # p-value = 0.4332

# Plot
om.plot.21 <- dat.2021 |> 
  ggplot(aes(x = Treatment, y = OM_perc)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment)) +
  geom_jitter(size = 1,
              alpha = 0.8,
              aes(color = Treatment)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Soil organic matter",
       x = NULL,
       y = "Organic matter (%)") +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000"))  +
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "in")) +
  geom_text(aes(x = 2, y = 2.14, label = "Mann-Whitney, \np = 0.433"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = 0.75, linetype = "dashed") +
  theme(plot.title = element_text(size = 12))
om.plot.21



# Combine soil chem -------------------------------------------------------

# TN, TC, OM
# Initial submission
tiff("figures/2023-09_publish-figures/FigS6_Soil-chem.tiff", units = "in", height = 4, width = 7, res = 300)
ggarrange(tn.plot.21, tc.plot.21, om.plot.21, 
          ncol = 3, nrow = 1,
          labels = c("(A)", "(B)", "(C)")) 

dev.off()

# Revision 1
tiff("figures/2023-12_publish-figures/FigS5_Soil-chem.tiff", units = "in", height = 4, width = 7, res = 300)
ggarrange(tn.plot.21, tc.plot.21, om.plot.21, 
          ncol = 3, nrow = 1,
          labels = c("(A)", "(B)", "(C)")) 

dev.off()



# Barc richness -----------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment == "Control")$barc.richness,
       filter(dat.2021, Treatment == "Treated")$barc.richness) # NS, p = 0.686

# Plot
barc.rich.plot.21 <- dat.2021 |> 
  ggplot(aes(Treatment, barc.richness)) +
  geom_jitter(aes(color = Treatment), 
              alpha = 0.8, 
              size = 1) +
  geom_boxplot(aes(fill = Treatment), 
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
  geom_text(aes(x = 2.1, y = 1100, label = "t-test, p = 0.686"),
            color = "gray30",
            size = 2.5) +
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in")) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = 0.75, linetype = "dashed") +
  theme(plot.title = element_text(size = 11.5))
barc.rich.plot.21



# Fungi richness ----------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment == "Control")$fungi.richness,
       filter(dat.2021, Treatment == "Treated")$fungi.richness) # NS, p = 0.938

# Plot
fungi.rich.plot.21 <- dat.2021 %>% 
  ggplot(aes(Treatment, fungi.richness)) +
  geom_jitter(aes(color = Treatment), 
              alpha = 0.8, 
              size = 1) +
  geom_boxplot(aes(fill = Treatment), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Fungal richness") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000")) +
  geom_text(aes(x = 2.1, y = 70, label = "t-test, p = 0.938"),
            color = "gray30",
            size = 2.5) +
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in")) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = 0.75, linetype = "dashed") +
  theme(plot.title = element_text(size = 12))
fungi.rich.plot.21


# Combine soil richness ---------------------------------------------------

# Initial submission
tiff("figures/2023-09_publish-figures/FigS7_Soil-richness.tiff", units = "in", height = 4, width = 5.5, res = 300)
ggarrange(barc.rich.plot.21, fungi.rich.plot.21,
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"))

dev.off()

# Revision 1
tiff("figures/2023-12_publish-figures//FigS6_Soil-richness.tiff", units = "in", height = 4, width = 5.5, res = 300)
ggarrange(barc.rich.plot.21, fungi.rich.plot.21,
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)"))

dev.off()




# Chemoheterotrophs -------------------------------------------------------

# Mann-Whitney
wilcox.test(filter(dat.2021, Treatment == "Control")$chemoheterotrophy_perc,
            filter(dat.2021, Treatment == "Treated")$chemoheterotrophy_perc) # p = 0.8449

# Plot
chemohet.plot.21 <- dat.2021 %>% 
  ggplot(aes(Treatment, chemoheterotrophy_perc)) +
  geom_jitter(aes(color = Treatment), 
              alpha = 0.8, 
              size = 1) +
  geom_boxplot(aes(fill = Treatment), 
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
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in")) +
  geom_text(aes(x = 2, y = 28.7, label = "Mann-Whitney, \np = 0.845"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = 0.75, linetype = "dashed") +
  theme(plot.title = element_text(size = 12))
chemohet.plot.21



# N-cyclers ---------------------------------------------------------------

# Mann-Whitney
wilcox.test(filter(dat.2021, Treatment == "Control")$n.cycler_perc,
            filter(dat.2021, Treatment == "Treated")$n.cycler_perc) # p-value = 0.5854

# Plot
ncycler.plot.21 <- dat.2021 %>% 
  ggplot(aes(Treatment, n.cycler_perc)) +
  geom_jitter(aes(color = Treatment), 
              alpha = 0.8, 
              size = 1) +
  geom_boxplot(aes(fill = Treatment), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Nitrogen-cycling \nbacteria & archaea") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.25, "in")) +
  geom_text(aes(x = 2, y = 13.6, label = "Mann-Whitney, \np = 0.585"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = 0.75, linetype = "dashed") +
  theme(plot.title = element_text(size = 12))
ncycler.plot.21



# Saprotrophs -------------------------------------------------------------

# T-test
t.test(filter(dat.2021, Treatment == "Control")$saprotroph,
       filter(dat.2021, Treatment == "Treated")$saprotroph) # NS, p = 0.272

# Plot
sapro.plot.21 <- dat.2021 %>% 
  ggplot(aes(Treatment, saprotroph)) +
  geom_jitter(aes(color = Treatment), 
              alpha = 0.8, 
              size = 1) +
  geom_boxplot(aes(fill = Treatment), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab(NULL) +
  ggtitle("Saprotrophic fungi") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000")) +
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.25, "in")) +
  geom_text(aes(x = 2, y = 42, label = "t-test, p = 0.272"),
            color = "gray30",
            size = 2.5) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = 0.75, linetype = "dashed") +
  theme(plot.title = element_text(size = 12))
sapro.plot.21



# Combine FAPROTAX and FUNGuild -------------------------------------------

# Initial submission
tiff("figures/2023-09_publish-figures/FigS8_Soil-functional.tiff", units = "in", height = 4, width = 7, res = 300)
ggarrange(chemohet.plot.21, ncycler.plot.21, sapro.plot.21, 
          ncol = 3, nrow = 1,
          labels = c("(A)", "(B)", "(C)")) 

dev.off()

# Revision 1
tiff("figures/2023-12_publish-figures/FigS7_Soil-functional.tiff", units = "in", height = 4, width = 7, res = 300)
ggarrange(chemohet.plot.21, ncycler.plot.21, sapro.plot.21, 
          ncol = 3, nrow = 1,
          labels = c("(A)", "(B)", "(C)")) 

dev.off()


save.image("RData/Publish_t-test-2021.RData")
