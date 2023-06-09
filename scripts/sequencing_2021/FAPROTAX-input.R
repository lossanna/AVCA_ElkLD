# Purpose: Format tables for FAPROTAX input

library(tidyverse)

# load data ---------------------------------------------------------------

ex <- read_tsv("amplicon-sequencing/FAPROTAX_1.2.6/example_otu_faprotax.tsv") # example
barc.asv.raw <- read.table("data/cleaned/sequencing/bac-arc_clean_asv.txt")
barc.tax <- read.table("data/cleaned/sequencing/bac-arc_clean_tax.txt", 
                       sep = "\t", header = T, row.names = 1)

# Format data for FAPROTAX ------------------------------------------------

# Collapse taxonomy info into single column
tax <- barc.tax %>% 
  mutate(D0 = rep("D_0__", nrow(barc.tax)),
         D1 = rep("D_1__", nrow(barc.tax)),
         D2 = rep("D_2__", nrow(barc.tax)),
         D3 = rep("D_3__", nrow(barc.tax)),
         D4 = rep("D_4__", nrow(barc.tax)),
         D5 = rep("D_5__", nrow(barc.tax)),
         D6 = rep("D_6__", nrow(barc.tax))) %>% 
  mutate(Kingdom = paste0(D0, Kingdom),
         Phylum = paste0(D1, Phylum),
         Class = paste0(D2, Class),
         Order = paste0(D3, Order),
         Family = paste0(D4, Family),
         Genus = paste0(D5, Genus),
         Species = paste0(D6, Species)) %>% 
  select(-starts_with("D")) %>% 
  mutate(taxonomy = paste(Kingdom, Phylum, Class, Order, Family, Genus, Species, sep = "; ")) %>% 
  select(taxonomy)

# Each row is an ASV/OTU, and each col is a sample
barc.asv <- t(barc.asv.raw)

# Add taxonomy col
fapro <- bind_cols(barc.asv, tax)
fapro$asv <- row.names(fapro)
fapro <- fapro[ , c(64, 1:63)]
rownames(fapro) <- c()

# Write table
write.table(fapro, 
            file = "data/cleaned/sequencing/faprotax-input.tsv",
            quote = F, 
            sep = "\t", 
            row.names = F)

write.table(fapro, 
            file = "amplicon-sequencing/FAPROTAX_1.2.6/faprotax-input.tsv",
            quote = F, 
            sep = "\t", 
            row.names = F)
