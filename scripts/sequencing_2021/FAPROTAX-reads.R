# Purpose: Calculate how many reads were assigned FAPROTAX groups.

# The output results table tells what percentage of ASVs were assigned a FAPROTAX group,
#   but most of the reads come from a small group of ASVs, so considering the weight of
#   each ASV via its reads is a better for determining how much of the soil microbiome
#   was categorized.

# Created: 2023-07-19
# Updated: 2023-07-19

library(tidyverse)
library(readxl)

# Load data ---------------------------------------------------------------

barc.asv <- read.table("data/cleaned/sequencing/bac-arc_clean_asv.txt",
                       sep = "\t", header = TRUE, row.names = 1)
barc.tax <- read.table("data/cleaned/sequencing/bac-arc_clean_tax.txt",
                       sep = "\t", header = TRUE, row.names = 1)
faprotax.tax.raw <- read_xlsx("data/cleaned/sequencing/faprotax-asv.xlsx",
                              sheet = "Sheet1")
faprotax.input <- read.csv("data/cleaned/sequencing/faprotax-input.csv")


# Data wrangling ----------------------------------------------------------

# Make a list of taxonomies that were assigned FAPROTAX group
faprotax.tax <- faprotax.tax.raw |> 
  pivot_longer(everything(), names_to = "group", values_to = "taxonomy") |> 
  filter(!is.na(taxonomy)) |> 
  distinct(.keep_all = TRUE)

# Connect FAPROTAX-assigned taxonomies to their ASVs
asv.tax.fapro <- faprotax.input |> 
  select(asv, taxonomy) |> 
  left_join(faprotax.tax, relationship = "many-to-many") |> # some taxonomies were assigned to multiple ASVs, which is okay
  filter(!is.na(group))

# Make list of FAPROTAX-assigned ASVs
asv.assigned <- unique(asv.tax.fapro$asv)

# Sum reads across all samples
asv.counts <- as.data.frame(colSums(barc.asv)) |> 
  rename(reads = `colSums(barc.asv)`) |> 
  mutate(perc = reads / sum(reads) * 100)
sum(asv.counts$perc[1:1000]) # 75% of reads from 1000 ASVs

# Calculate percentage of FAPROTAX-assigned reads across all samples
asv.assigned.counts <- asv.counts |> 
  mutate(asv = rownames(asv.counts)) |> 
  filter(asv %in% asv.assigned)
sum(asv.assigned.counts$perc) # ~ 24.7% of reads assigned a FAPROTAX group

