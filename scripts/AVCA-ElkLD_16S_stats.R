library(metagenomeSeq)
library(vegan)
library(agricolae)
library(FSA)
library(rcompanion)
library(tidyverse)
library(ggpubr)
library(reshape2)


# Load data ---------------------------------------------------------------

# Read ASV table, taxonomy table and ASV_rep
barc.asv.raw <- read.table("amplicon-sequencing/FASTQ_16S_raw/16S_demultiplexed/16S_asv_table.txt", 
                       sep = "\t", header = TRUE, row.names = 1)
barc.tax.raw <- read.table("amplicon-sequencing/FASTQ_16S_raw/16S_demultiplexed/16S_taxa_table.txt", 
                       sep = "\t", header = TRUE, row.names = 1)
barc.rep.raw <- read.table("amplicon-sequencing/FASTQ_16S_raw/16S_demultiplexed/16S_ASV_rep.txt", 
                       sep = "\t", header = TRUE, row.names = 1)


# Generate clean tables ---------------------------------------------------

# Remove chloroplasts and mitochondria
grep("Chloroplast", barc.tax.raw$Order)
grep("Mitochondria", barc.tax.raw$Family)

barc.asv <- barc.asv.raw[ , -grep("Chloroplast", barc.tax.raw$Order)]
barc.tax <- barc.tax.raw[-grep("Chloroplast", barc.tax.raw$Order), ]
barc.asv <- barc.asv.raw[ , -grep("Mitochondria", barc.tax.raw$Family)]
barc.tax <- barc.tax.raw[-grep("Mitochondria", barc.tax.raw$Family), ]

# Remove potential lab contaminations
barc.ext01.control <- barc.asv[which(rownames(barc.asv) == "avca-elkld_blank1"), ]
barc.ext02.control <- barc.asv[which(rownames(barc.asv) == "avca-elkld_blank2"), ]
barc.ext03.control <- barc.asv[which(rownames(barc.asv) == "avca-elkld_blank3"), ]
barc.ext04.control <- barc.asv[which(rownames(barc.asv) == "avca-elkld_blank4"), ]
barc.ext05.control <- barc.asv[which(rownames(barc.asv) == "avca-elkld_PCRZymo"), ]
barc.ext06.control <- barc.asv[which(rownames(barc.asv) == "avca-elkld_PRCblank"), ]

# Which contaminants should be removed?
rm.contaminants <- unique(c(which(barc.ext01.control > 0.01 * sum(barc.ext01.control)),
                            which(barc.ext02.control > 0.01 * sum(barc.ext02.control)),
                            which(barc.ext03.control > 0.01 * sum(barc.ext03.control)),
                            which(barc.ext04.control > 0.01 * sum(barc.ext04.control)),
                            which(barc.ext05.control > 0.01 * sum(barc.ext05.control)),
                            which(barc.ext06.control > 0.01 * sum(barc.ext06.control))))
length(rm.contaminants)

# Remove contaminants from ASV table and taxonomy table
barc.asv <- barc.asv[ , -rm.contaminants]
barc.tax <- barc.tax[-rm.contaminants, ]

# Remove control samples from ASV table
rownames(barc.asv)
barc.asv <- barc.asv[-c(63:68), ]
rownames(barc.asv)

# Remove empty ASVs
barc.asv <- subset(barc.asv, select = colSums(barc.asv)!=0)
barc.tax <- barc.tax[colnames(barc.asv), ]

# Remove ASVs without kingdom information
which(is.na(barc.tax$Kingdom))

barc.asv <- barc.asv[ , -which(is.na(barc.tax$Kingdom))]
barc.tax <- barc.tax[-which(is.na(barc.tax$Kingdom)), ]

# Remove ASV IDs from ASV.rep
barc.rep <- subset(barc.rep.raw, 
                      as.character(barc.rep.raw$ASV) %in% rownames(barc.tax))

# Save the clean asv table, taxonomy table and ASV.rep
write.table(barc.asv, 
            file = "data/cleaned/bac_arc_clean_asv.txt", 
            quote = F, 
            sep = "\t", 
            col.names = NA)
write.table(barc.tax, 
            file = "data/cleaned/bac_arc_clean_tax.txt", 
            quote = F, 
            sep = "\t", 
            col.names = NA)
write.table(barc.rep, 
            file = "data/cleaned/bac_arc_clean_rep.txt", 
            quote = F, 
            sep = "\t", 
            col.names = NA)


# Remove useless objects
rm(barc.ext01.control, 
   barc.ext02.control, 
   barc.ext03.control,
   barc.ext04.control,
   barc.ext05.control,
   barc.ext06.control,
   rm.contaminants)


# Organize taxonomy table
barc.tax.ordered <- barc.tax
barc.tax.ordered <- barc.tax.ordered[order(barc.tax.ordered$Species, decreasing = FALSE), ]
barc.tax.ordered <- barc.tax.ordered[order(barc.tax.ordered$Genus, decreasing = FALSE), ]
barc.tax.ordered <- barc.tax.ordered[order(barc.tax.ordered$Family, decreasing = FALSE), ]
barc.tax.ordered <- barc.tax.ordered[order(barc.tax.ordered$Order, decreasing = FALSE), ]
barc.tax.ordered <- barc.tax.ordered[order(barc.tax.ordered$Class, decreasing = FALSE), ]
barc.tax.ordered <- barc.tax.ordered[order(barc.tax.ordered$Phylum, decreasing = FALSE), ]
barc.tax.ordered <- barc.tax.ordered[order(barc.tax.ordered$Kingdom, decreasing = FALSE), ]

write.table(barc.tax.ordered,
            file = "data/cleaned/bac_arc_clean_tax_ordered.txt",
            quote = F, 
            sep = "\t", 
            col.names = NA)

# Taxonomy table without ASV labels
barc.tax.unique <- barc.tax.ordered
rownames(barc.tax.unique) <- c()
barc.tax.unique <- unique(barc.tax.unique)

write.table(barc.tax.unique,
            file = "data/cleaned/bac_arc_clean_tax_unique.txt",
            quote = F, 
            sep = "\t", 
            col.names = NA)
