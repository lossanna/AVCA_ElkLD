# Purpose: Create NMDS figure and code for basic amplicon sequence analysis.
#   Main figure: combined NMDS of fungi & barc
#   Code: normalization, richness, Shannon, NMDS, adonis2, plot

# Created: 2023-08-30
# Last updated: 2023-08-30

library(tidyverse)
library(ggpubr)
library(metagenomeSeq)
library(vegan)

# Load data ---------------------------------------------------------------

barc.asv <- read.table("data/publish/bac-arc_clean_asv.txt", sep = "\t",
                       header = T, row.names = 1)
fungi.asv <- read.table("data/publish/fungi_clean_asv.txt",
                      sep = "\t", header = T, row.names = 1)
meta <- read.csv("data/publish/sequencing_metadata.csv")

# Use dat.2021 for "official" NMDS values from 16S_prelim-stats.R and ITS_prelim-stats.R
dat.2021 <- read.csv("data/publish/Veg-soil-elev_2021.csv")


# 16S ---------------------------------------------------------------------

# Note:Calculating Bray-Curtis distance and NMDS changes a little every time, 
#   so NMDS values and stress and adonis2() output will vary.

# "Official" output from 16S_prelim-stats:
#   NMDS stres = 0.1684425
#   adonis2(formula = barc.dist ~ meta$Treatment3)
#                   Df SumOfSqs      R2      F Pr(>F)  
#   meta$Treatment3  1   0.2559 0.02881 1.7799  0.029 *
#   Residual        60   8.6275 0.97119                
#   Total           61   8.8835 1.00000        

# Code:
# Normalization
barc.MR <- newMRexperiment(t(barc.asv))
p <- cumNormStat(barc.MR)
barc.MR <- cumNorm(barc.MR, p = p)
barc.norm <- t(MRcounts(barc.MR, norm = T, log = F))

# Richness and Shannon
meta$barc.richness <- specnumber(barc.norm)
meta$barc.shannon <- diversity(barc.norm, index = "shannon")

# NMDS ordination
barc.dist <- vegdist(barc.norm, method = "bray")
barc.nmds <- metaMDS(barc.dist, k = 2)
barc.nmds$stress

meta0$barc.NMDS1 <- barc.nmds$points[ , 1]
meta$barc.NMDS2 <- barc.nmds$points[ , 2]

# Test community similarity differences
adonis2(barc.dist ~ meta$Treatment) 


# Plot (use dat.2021 for same NMDS values):
barc.nmds.plot.21 <- dat.2021 %>% 
  ggplot(aes(x = barc.NMDS1, y = barc.NMDS2, color = Treatment, shape = Treatment)) +
  geom_point() +
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
  geom_text(aes(x = 0.2, y = -0.55, label = "PERMANOVA, p = 0.029"),
            size = 2.5, color = "gray30") +
  geom_text(aes(x = 0.25, y = -0.65, label = "Stress = 0.168"),
            size = 2.5, color = "gray30") +
  theme(plot.title = element_text(size = 12))
barc.nmds.plot.21


# ITS ---------------------------------------------------------------------

# Note:Calculating Bray-Curtis distance and NMDS changes a little every time, 
#   so NMDS values and stress and adonis2() output will vary.

# "Official" output from ITS_prelim-stats:
#   NMDS stres = 0.2359848
#   adonis2(formula = fungi.dist ~ meta$Treatment3)
#                   Df SumOfSqs      R2     F Pr(>F)  
#   meta$Treatment3  1   0.4511 0.02371 1.457  0.011 *
#   Residual        60  18.5783 0.97629               
#   Total           61  19.0294 1.00000   

# Code:
# Normalization 
fungi.MR <- newMRexperiment(t(fungi.asv))
p <- cumNormStat(fungi.MR)
fungi.MR <- cumNorm(fungi.MR, p = p)
fungi.norm <- t(MRcounts(fungi.MR, norm = T, log = F))

# Richness and Shannon
meta$fungi.richness <- specnumber(fungi.norm)
meta$fungi.shannon <- diversity(fungi.norm, index = "shannon")

# NMDS ordination
fungi.dist <- vegdist(fungi.norm, method = "bray")
fungi.nmds <- metaMDS(fungi.dist, k = 2)
fungi.nmds$stress

meta$fungi.NMDS1 <- fungi.nmds$points[ , 1]
meta$fungi.NMDS2 <- fungi.nmds$points[ , 2]

# Test community similarity differences
adonis2(fungi.dist ~ meta$Treatment)


# Plot (use dat.2021 for same NMDS values):
fungi.nmds.plot.21 <- dat.2021 %>% 
  ggplot(aes(x = fungi.NMDS1, y = fungi.NMDS2, color = Treatment, shape = Treatment)) +
  geom_point() +
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
  geom_text(aes(x = 0.73, y = -0.7, label = "PERMANOVA, p = 0.011"),
            size = 2.5, color = "gray30") +
  geom_text(aes(x = 0.82, y = -0.8, label = "Stress = 0.236"),
            size = 2.5, color = "gray30") +
  theme(plot.title = element_text(size = 12))
fungi.nmds.plot.21


# Combine NMDS ------------------------------------------------------------

tiff("figures/2023-09_publish-figures/Soil-NMDS.tiff", height = 4, width = 7, units = "in", res = 1000)
ggarrange(barc.nmds.plot.21, fungi.nmds.plot.21,
          nrow = 1, ncol = 2,
          labels = c("(A)", "(B)"),
          common.legend = TRUE, legend = "bottom")
dev.off()
