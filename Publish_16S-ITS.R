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

barc.asv <- 