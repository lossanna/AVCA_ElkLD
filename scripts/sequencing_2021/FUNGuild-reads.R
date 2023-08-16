# Purpose: Calculate how many reads were assigned FUNGuild guilds.

# The output results table tells what percentage of ASVs were assigned a FUNGuild guild,
#   but most of the reads come from a small group of ASVs, so considering the weight of
#   each ASV via its reads is a better for determining how much of the soil microbiome
#   was categorized.

# Created: 2023-08-15
# Updated: 2023-08-15

library(tidyverse)

# Load data ---------------------------------------------------------------

fungi.trophic <- read.csv("data/cleaned/sequencing/FUNGuild-proportions-trophic_clean.csv")

funguild.raw <- read.table("data/cleaned/sequencing/FUNGuild-output.txt",
                           sep = "\t", header = T, row.names = 1)

fungi.asv <- read.table("data/cleaned/sequencing/fungi_clean_asv.txt",
                        sep = "\t", header = TRUE, row.names = 1)

# Data wrangling ----------------------------------------------------------

# Keep only those listed as Highly Probable and Probable
unique(funguild.raw$confidenceRanking)
funguild.prob <- funguild.raw |> 
  filter(confidenceRanking %in% c("Probable", "Highly Probable")) 
funguild.prob$ASV <- rownames(funguild.prob)
nrow(funguild.prob) / nrow(funguild.raw) # 41% of ASVs

# Make list of FUNGuild-assigned ASVs
asv.assigned <- funguild.prob$ASV

# Sum reads across all samples
asv.counts <- as.data.frame(colSums(fungi.asv)) |> 
  rename(reads = `colSums(fungi.asv)`) |> 
  mutate(perc = reads / sum(reads) * 100)
sum(asv.counts$perc[1:1000]) # 92% of reads from 1000 ASVs

# Calculate percentage of FUNGuild-assigned reads across all samples
asv.assigned.counts <- asv.counts |> 
  mutate(asv = rownames(asv.counts)) |> 
  filter(asv %in% asv.assigned)
sum(asv.assigned.counts$perc) # ~ 40.7% of reads assigned a FAPROTAX group
