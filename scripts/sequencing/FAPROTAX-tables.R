library(tidyverse)


# load data ---------------------------------------------------------------

ex <- read_tsv("amplicon-sequencing/FAPROTAX_1.2.6/example_otu_faprotax.tsv") # example
barc.asv <- read.table("data/cleaned/sequencing/bac_arc_clean_asv.txt")
barc.tax <- read.table("data/cleaned/sequencing/bac_arc_clean_tax.txt", header = T, row.names = 1)
