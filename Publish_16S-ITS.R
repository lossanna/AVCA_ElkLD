# Purpose: Create NMDS figure and code for basic amplicon sequence analysis.
#   Main figure: combined NMDS of fungi & barc
#   Code: normalization, richness, Shannon, NMDS, adonis2, plot

# Created: 2023-08-30
# Last updated: 2023-08-12

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

# Use dat.2021 for "official" NMDS values from 16S_prelim-stats.R
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
meta$Richness <- specnumber(barc.norm)
meta$Shannon <- diversity(barc.norm, index = "shannon")

# NMDS ordination
barc.dist <- vegdist(barc.norm, method = "bray")
barc.nmds <- metaMDS(barc.dist, k = 2)
barc.nmds$stress

meta$NMDS1 <- barc.nmds$points[ , 1]
meta$NMDS2 <- barc.nmds$points[ , 2]

# Test community similarity differences
adonis2(barc.dist ~ meta$Treatment) 


# Plot (use dat.2021 for same NMDS values)
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
  geom_text(aes(x = 0.3, y = -0.55, label = "PERMANOVA, p = 0.029"),
            size = 3, color = "gray30") +
  geom_text(aes(x = 0.35, y = -0.65, label = "Stress = 0.168"),
            size = 3, color = "gray30")
barc.nmds.plot.21

