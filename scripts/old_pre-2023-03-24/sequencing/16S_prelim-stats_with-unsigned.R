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

# Remove potential lab contamination
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
barc.asv.us <- barc.asv[-c(63:68), ] # keep unsigned
                      # Note: ".us" added to object name means that the "unsigned" sample is included
rownames(barc.asv.us)

# Remove empty ASVs
barc.asv.us <- subset(barc.asv.us, select = colSums(barc.asv.us)!=0)
barc.tax <- barc.tax[colnames(barc.asv.us), ]

# Remove ASVs without kingdom information
which(is.na(barc.tax$Kingdom))

barc.asv.us <- barc.asv.us[ , -which(is.na(barc.tax$Kingdom))]
barc.tax <- barc.tax[-which(is.na(barc.tax$Kingdom)), ]

# Remove ASV IDs from ASV.rep
barc.rep <- subset(barc.rep.raw, 
                   as.character(barc.rep.raw$ASV) %in% rownames(barc.tax))

# Remove useless objects
rm(barc.ext01.control, 
   barc.ext02.control, 
   barc.ext03.control,
   barc.ext04.control,
   barc.ext05.control,
   barc.ext06.control,
   rm.contaminants)



# Statistical analysis ----------------------------------------------------

# Note: ".us" added to object name means that the "unsigned" sample is included

# Read metadata
meta.us <- read.table(file = "data/cleaned/sequencing/sequencing_metadata_with-unsigned.txt.txt",
                      header = TRUE,
                      sep = "\t")

# Check number of sequences per sample
hist(rowSums(barc.asv.us))
summary(rowSums(barc.asv.us)) # min 77212 - max 646735; median 137230 

# Normalization (metagenomeSeq), with unsigned
barc.MR.us <- newMRexperiment(t(barc.asv.us))
p <- cumNormStat(barc.MR.us)
barc.MR.us <- cumNorm(barc.MR.us, p = p)
barc.norm.us <- t(MRcounts(barc.MR.us, norm = T, log = F))

# Richness and Shannon
meta.us$Richness <- specnumber(barc.norm.us)
meta.us$Shannon <- diversity(barc.norm.us, index = "shannon")


# Phylum ------------------------------------------------------------------

# Generate phylum level profile
barc.phylum.us <- as.data.frame(t(barc.asv.us))
barc.phylum.us$phylum <- barc.tax$Phylum
barc.phylum.us <- aggregate(. ~ phylum, data = barc.phylum.us, sum) # group ASVs of same phylum and sum reads
rownames(barc.phylum.us) <- barc.phylum.us$phylum
barc.phylum.us <- barc.phylum.us[ , -1]
barc.phylum.us <- (sweep(as.matrix(barc.phylum.us), 2, rowSums(barc.asv.us), "/")) * 100 # change reads to relative proportions
barc.phylum.us <- as.data.frame(t(barc.phylum.us))
barc.phylum.us$Unclassified = 100 - rowSums(barc.phylum.us)


# Create dominant phyla table
barc.phylum.us.d <- as.data.frame(t(barc.phylum.us))
unclassified.row <- c("Unclassified")
barc.phylum.us.un <- barc.phylum.us.d[unclassified.row, ]
barc.phylum.us.d$avg <- rowMeans(barc.phylum.us.d)
barc.phylum.us.d <- barc.phylum.us.d[rownames(barc.phylum.us.d) != "Unclassified", ]
barc.phylum.us.nd <- barc.phylum.us.d[which(barc.phylum.us.d$avg < 1), ]
barc.phylum.us.d <- barc.phylum.us.d[-which(barc.phylum.us.d$avg < 1), ]
barc.phylum.us.d <- barc.phylum.us.d[order(barc.phylum.us.d$avg, decreasing = T), ]
barc.phylum.us.nd.sum <- colSums(barc.phylum.us.nd) 
barc.phylum.us.nd.sum <- as.data.frame(t(barc.phylum.us.nd.sum))
rownames(barc.phylum.us.nd.sum) <- c("Others")
barc.phylum.us.d <- rbind(barc.phylum.us.d, barc.phylum.us.nd.sum)
barc.phylum.us.d <- select(barc.phylum.us.d, -avg)
barc.phylum.us.d <- rbind(barc.phylum.us.d, barc.phylum.us.un)
barc.phylum.us.d <- as.data.frame(t(barc.phylum.us.d))

rm(barc.phylum.us.nd,
   barc.phylum.us.nd.sum,
   barc.phylum.us.un,
   unclassified.row)

# Average proportions by Channel (dominant phyla)
barc.phylum.us.dc <- as.data.frame(t(barc.phylum.us.d))
colnames(barc.phylum.us.dc) <- meta.us$Channel
barc.phylum.us.dc <- barc.phylum.us.dc[ , order(names(barc.phylum.us.dc))]
barc.phylum.us.dc$Channel_12 <- rowMeans(barc.phylum.us.dc[ , 1:14])
barc.phylum.us.dc$Channel_13 <- rowMeans(barc.phylum.us.dc[ , 15:30])
barc.phylum.us.dc$Channel_19 <- rowMeans(barc.phylum.us.dc[ , 31:47])
barc.phylum.us.dc$Channel_21 <- rowMeans(barc.phylum.us.dc[ , 48:62])
barc.phylum.us.dc <- barc.phylum.us.dc[ , -c(1:62)]
barc.phylum.us.dc <- as.data.frame(t(barc.phylum.us.dc))
barc.phylum.us.dc <- barc.phylum.us.dc[c(2:5, 1), ]

# Reshape data for stacked bar plot
barc.phylum.us.bar <- melt(barc.phylum.us.dc)
names(barc.phylum.us.bar) <- c("phylum", "proportion")

# Change rep() parameters based on number of cols
dim(barc.phylum.us.dc) # times = col number
barc.phylum.us.bar$Channel <- rep(c("Channel 12", 
                                    "Channel 13", 
                                    "Channel 19", 
                                    "Channel 21", 
                                    "Unsigned"), 
                                  times = 13)

barc.phylum.us.bar %>% 
  ggplot(aes(x = Channel, y = proportion, fill = phylum)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  ylab("Relative abundance (%)") +
  theme_bw(base_size = 15)+
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c(brewer.pal(n = 8, "Set1"), 
                               brewer.pal(n = 4, "Dark2"),
                               "#999999"))
                               


# Family ------------------------------------------------------------------

# Generate family level profile
barc.family.us <- as.data.frame(t(barc.asv.us))
barc.family.us$family <- barc.tax$Family
barc.family.us <- aggregate(. ~ family, data = barc.family.us, sum) # group ASVs of same family and sum reads
rownames(barc.family.us) <- barc.family.us$family
barc.family.us <- barc.family.us[ , -1]
barc.family.us <- (sweep(as.matrix(barc.family.us), 2, rowSums(barc.asv.us), "/")) * 100 # change reads to relative proportions
barc.family.us <- as.data.frame(t(barc.family.us))
barc.family.us$Unclassified = 100 - rowSums(barc.family.us)

# Create dominant phyla table
barc.family.us.d <- as.data.frame(t(barc.family.us))
unclassified.row <- c("Unclassified")
barc.family.us.un <- barc.family.us.d[unclassified.row, ]
barc.family.us.d$avg <- rowMeans(barc.family.us.d)
barc.family.us.d <- barc.family.us.d[rownames(barc.family.us.d) != "Unclassified", ]
barc.family.us.nd <- barc.family.us.d[which(barc.family.us.d$avg < 1), ]
barc.family.us.d <- barc.family.us.d[-which(barc.family.us.d$avg < 1), ]
barc.family.us.d <- barc.family.us.d[order(barc.family.us.d$avg, decreasing = T), ]
barc.family.us.nd.sum <- colSums(barc.family.us.nd) 
barc.family.us.nd.sum <- as.data.frame(t(barc.family.us.nd.sum))
rownames(barc.family.us.nd.sum) <- c("Others")
barc.family.us.d <- rbind(barc.family.us.d, barc.family.us.nd.sum)
barc.family.us.d <- select(barc.family.us.d, -avg)
barc.family.us.d <- rbind(barc.family.us.d, barc.family.us.un)
barc.family.us.d <- as.data.frame(t(barc.family.us.d))

rm(barc.family.us.nd,
   barc.family.us.nd.sum,
   barc.family.us.un,
   unclassified.row)

# Average proportions by Channel (dominant phyla)
barc.family.us.dc <- as.data.frame(t(barc.family.us.d))
colnames(barc.family.us.dc) <- meta.us$Channel
barc.family.us.dc <- barc.family.us.dc[ , order(names(barc.family.us.dc))]
barc.family.us.dc$Channel_12 <- rowMeans(barc.family.us.dc[ , 1:14])
barc.family.us.dc$Channel_13 <- rowMeans(barc.family.us.dc[ , 15:30])
barc.family.us.dc$Channel_19 <- rowMeans(barc.family.us.dc[ , 31:47])
barc.family.us.dc$Channel_21 <- rowMeans(barc.family.us.dc[ , 48:62])
barc.family.us.dc <- barc.family.us.dc[ , -c(1:62)]
barc.family.us.dc <- as.data.frame(t(barc.family.us.dc))
barc.family.us.dc <- barc.family.us.dc[c(2:5, 1), ]

# Reshape data for stacked bar plot
barc.family.us.bar <- melt(barc.family.us.dc)
names(barc.family.us.bar) <- c("family", "proportion")

# Change rep() parameters based on number of cols
dim(barc.family.us.dc) # times = col number
barc.family.us.bar$Channel <- rep(c("Channel 12", 
                                    "Channel 13", 
                                    "Channel 19", 
                                    "Channel 21", 
                                    "Unsigned"), 
                                  times = 25)

barc.family.us.bar %>% 
  ggplot(aes(x = Channel, y = proportion, fill = family)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  ylab("Relative abundance (%)") +
  theme_bw(base_size = 15)+
  theme(legend.title = element_blank()) 


save.image("RData/16S_prelim-stats_with-unasigned.RData")
