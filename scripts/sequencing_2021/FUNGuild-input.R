# Purpose: Format tables for FUNGuild input
# Created: 2023-06-26
# Updated: 2023-06-26

library(tidyverse)

# Load data ---------------------------------------------------------------

fungi.asv.raw <- read.table("data/cleaned/sequencing/fungi_clean_asv.txt",
                            sep = "\t", header = T, row.names = 1)
fungi.tax <- read.table("data/cleaned/sequencing/fungi_clean_tax.txt",
                        sep = "\t", header = T, row.names = 1)

# Format data for FUNGuild ------------------------------------------------

# Collapse taxonomy info into single column
tax <- fungi.tax %>% 
  mutate(D0 = rep("k__", nrow(fungi.tax)),
         D1 = rep("p__", nrow(fungi.tax)),
         D2 = rep("c__", nrow(fungi.tax)),
         D3 = rep("o__", nrow(fungi.tax)),
         D4 = rep("f__", nrow(fungi.tax)),
         D5 = rep("g__", nrow(fungi.tax)),
         D6 = rep("s__", nrow(fungi.tax))) %>% 
  mutate(Kingdom = paste0(D0, Kingdom),
         Phylum = paste0(D1, Phylum),
         Class = paste0(D2, Class),
         Order = paste0(D3, Order),
         Family = paste0(D4, Family),
         Genus = paste0(D5, Genus),
         Species = paste0(D6, Species)) %>% 
  select(-starts_with("D")) %>% 
  mutate(taxonomy = paste(Kingdom, Phylum, Class, Order, Family, Genus, Species, sep = ";")) %>% 
  select(taxonomy)

# Each row is an ASV/OTU, and each col is a sample
fungi.asv <- t(fungi.asv.raw)

# Add taxonomy col
funguild <- bind_cols(fungi.asv, tax)
funguild$asv <- row.names(funguild)
funguild <- funguild[ , c(64, 1:63)]
rownames(funguild) <- c()

# Write table
write.table(funguild, 
            file = "data/cleaned/sequencing/FUNGuild-input.tsv",
            quote = F, 
            sep = "\t", 
            row.names = F)

write.table(funguild, 
            file = "hpc-amplicon-sequencing/FUNGuild_1.2/FUNGuild-input.tsv",
            quote = F, 
            sep = "\t", 
            row.names = F)
