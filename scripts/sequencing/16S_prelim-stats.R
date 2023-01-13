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
pipeline.stats <- read.table("amplicon-sequencing/FASTQ_16S_raw/16S_demultiplexed/16S_sequence_pipeline_stats.txt",
                             sep = "\t", header = TRUE, row.names = 1)


# Investigate pipeline stats ----------------------------------------------

summary(pipeline.stats$InputRetained)

# Less than 50% retained:
pipeline.stats %>% 
  filter(InputRetained < 50) # unsigned sample only


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
barc.asv <- barc.asv[-c(63:69), ] # removed blanks and "unsigned"
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
            file = "data/cleaned/sequencing/bac_arc_clean_asv.txt", 
            quote = F, 
            sep = "\t", 
            col.names = NA)
write.table(barc.tax, 
            file = "data/cleaned/sequencing/bac_arc_clean_tax.txt", 
            quote = F, 
            sep = "\t", 
            col.names = NA)
write.table(barc.rep, 
            file = "data/cleaned/sequencing/bac_arc_clean_rep.txt", 
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


# Create taxonomy table of unique taxa
barc.tax.unique <- barc.tax
barc.tax.unique <- barc.tax.unique[order(barc.tax.unique$Species, decreasing = FALSE), ]
barc.tax.unique <- barc.tax.unique[order(barc.tax.unique$Genus, decreasing = FALSE), ]
barc.tax.unique <- barc.tax.unique[order(barc.tax.unique$Family, decreasing = FALSE), ]
barc.tax.unique <- barc.tax.unique[order(barc.tax.unique$Order, decreasing = FALSE), ]
barc.tax.unique <- barc.tax.unique[order(barc.tax.unique$Class, decreasing = FALSE), ]
barc.tax.unique <- barc.tax.unique[order(barc.tax.unique$Phylum, decreasing = FALSE), ]
barc.tax.unique <- barc.tax.unique[order(barc.tax.unique$Kingdom, decreasing = FALSE), ]
rownames(barc.tax.unique) <- c()
barc.tax.unique <- unique(barc.tax.unique)

write.table(barc.tax.unique,
            file = "data/cleaned/sequencing/bac_arc_clean_tax_unique.txt",
            quote = F, 
            sep = "\t", 
            col.names = NA)


# Sequence length & number of reads ---------------------------------------

seq.length <- as.vector(barc.rep$rep)
table(nchar(seq.length))
median(nchar(seq.length)) # 233 bp

# Total reads
sum(rowSums(barc.asv)) # 8419103

# Reads per sample
mean(rowSums(barc.asv)) # 135792
sd(rowSums(barc.asv)) # 66948.55
summary(rowSums(barc.asv))
# Min.   1st Qu.  Median  Mean    3rd Qu.   Max. 
# 77212  124231   137163  135792  148312    184961


# Statistical analysis ----------------------------------------------------

# Read metadata
meta <- read.table(file = "data/cleaned/sequencing/sequencing_metadata.txt",
                   header = TRUE,
                   sep = "\t")
meta <- meta[-63, ] # remove "unsigned" row; see other script for analysis including "unsigned"
  
# Check number of sequences per sample
hist(rowSums(barc.asv))
summary(rowSums(barc.asv)) # min 77212 - max 184961; median 137167 

# Normalization (metagenomeSeq)
barc.MR <- newMRexperiment(t(barc.asv))
p <- cumNormStat(barc.MR)
barc.MR <- cumNorm(barc.MR, p = p)
barc.norm <- t(MRcounts(barc.MR, norm = T, log = F))

# Richness and Shannon
meta$Richness <- specnumber(barc.norm)
meta$Shannon <- diversity(barc.norm, index = "shannon")

write.table(meta, 
            file = "data/cleaned/sequencing/bac_arc_richness_Shannon.txt", 
            quote = F, 
            sep = "\t", 
            col.names = NA)


# NMDS & beta dispersion --------------------------------------------------

# NMDS ordination
barc.dist <- vegdist(barc.norm, method = "bray")
barc.nmds <- metaMDS(barc.dist, k = 2)
barc.nmds$stress # 0.1680366 

meta$NMDS1 <- barc.nmds$points[ , 1]
meta$NMDS2 <- barc.nmds$points[ , 2]


# Test community similarity differences
adonis2(barc.dist ~ meta$Channel) # p < 0.001, 15% of variability explained by Channel
adonis2(barc.dist ~ meta$Treatment) # p < 0.001, 13% of variability explained by Treatment

# Plot NMDS
meta %>% 
ggplot(aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = Channel)) +
  stat_ellipse(aes(color = Channel))

meta %>% 
  ggplot(aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = Treatment)) +
  stat_ellipse(aes(color = Treatment))


# Beta dispersion 
barc.betadisper <- betadisper(barc.dist, 
                              group = meta$Channel, 
                              type = "centroid")

anova(barc.betadisper) # p = 0.0001425 

meta$betadisper <- barc.betadisper$distances

betadisper.hsd <- HSD.test(aov(betadisper ~ Channel, data = meta), trt = "Channel")
betadisper.hsd
# Channel 19  0.3847392      a
# Channel 21  0.3649528      a
# Channel 13  0.3279800     ab
# Channel 12  0.2824097      b


# Plot beta dispersion boxplot
meta %>% 
  ggplot(aes(Channel, betadisper), color = Channel) +
  geom_jitter(aes(color = Channel), 
              alpha = 0.8, 
              size = 4) +
  geom_boxplot(aes(fill = Channel), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("Beta dispersion") 



# Shannon diversity -------------------------------------------------------

shapiro.test(meta$Shannon) # p-value = 9.452e-05
kruskal.test(Shannon ~ Channel, data = meta) # p-value = 0.2663

# Plot Shannon diversity
meta %>% 
  ggplot(aes(Channel, Shannon), color = Channel) +
  geom_jitter(aes(color = Channel), 
              alpha = 0.8, 
              size = 4) +
  geom_boxplot(aes(fill = Channel), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("Bacteria & archaea Shannon diversity")



# Richness ----------------------------------------------------------------

shapiro.test(meta$Richness) # p-value = 0.9817
richness.anova <- aov(meta$Richness ~ meta$Channel, data = meta) 
summary(richness.anova) # p = 0.348

# Plot richness
meta %>% 
  ggplot(aes(Channel, Richness), color = Channel) +
  geom_jitter(aes(color = Channel), 
              alpha = 0.8, 
              size = 4) +
  geom_boxplot(aes(fill = Channel), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("Bacteria & archaea richness")



# Phylum ------------------------------------------------------------------

# Generate phylum level profile
barc.phylum <- as.data.frame(t(barc.asv))
barc.phylum$phylum <- barc.tax$Phylum
barc.phylum <- aggregate(. ~ phylum, data = barc.phylum, sum) # group ASVs of same phylum and sum reads
rownames(barc.phylum) <- barc.phylum$phylum
barc.phylum <- barc.phylum[ , -1]
barc.phylum <- (sweep(as.matrix(barc.phylum), 2, rowSums(barc.asv), "/")) * 100 # change reads to relative proportions
barc.phylum <- as.data.frame(t(barc.phylum))
barc.phylum$Unclassified = 100 - rowSums(barc.phylum)

write.table(barc.phylum,
            file = "data/cleaned/sequencing/bac_arc_phylum.txt",
            quote = F,
            sep ="\t",
            col.names = NA)


# Average proportions by Channel (all phyla)
barc.phylum <- read.table("data/cleaned/sequencing/bac_arc_phylum.txt", 
                             sep = "\t", header = T, row.names = 1)
barc.phylum <- as.data.frame(t(barc.phylum))
colnames(barc.phylum) <- meta$Channel
barc.phylum <- barc.phylum[ , order(names(barc.phylum))]
barc.phylum$Channel_12 <- rowMeans(barc.phylum[ , 1:14])
barc.phylum$Channel_13 <- rowMeans(barc.phylum[ , 15:30])
barc.phylum$Channel_19 <- rowMeans(barc.phylum[ , 31:47])
barc.phylum$Channel_21 <- rowMeans(barc.phylum[ , 48:62])
barc.phylum <- barc.phylum[ , -c(1:62)]
barc.phylum <- as.data.frame(t(barc.phylum))

write.table(barc.phylum,
            file = "data/cleaned/sequencing/bac_arc_phyla_avg-ch.txt",
            quote = F,
            sep ="\t",
            col.names = NA)

# Create dominant phyla table
barc.phylum <- read.table("data/cleaned/sequencing/bac_arc_phylum.txt", 
                          sep = "\t", header = T, row.names = 1)
barc.phylum.d <- as.data.frame(t(barc.phylum))
unclassified.row <- c("Unclassified")
barc.phylum.un <- barc.phylum.d[unclassified.row, ]
barc.phylum.d$avg <- rowMeans(barc.phylum.d)
barc.phylum.d <- barc.phylum.d[rownames(barc.phylum.d) != "Unclassified", ]
barc.phylum.nd <- barc.phylum.d[which(barc.phylum.d$avg < 1), ]
barc.phylum.d <- barc.phylum.d[-which(barc.phylum.d$avg < 1), ]
barc.phylum.d <- barc.phylum.d[order(barc.phylum.d$avg, decreasing = T), ]
barc.phylum.nd.sum <- colSums(barc.phylum.nd) 
barc.phylum.nd.sum <- as.data.frame(t(barc.phylum.nd.sum))
rownames(barc.phylum.nd.sum) <- c("Others")
barc.phylum.d <- rbind(barc.phylum.d, barc.phylum.nd.sum)
barc.phylum.d <- select(barc.phylum.d, -avg)
barc.phylum.d <- rbind(barc.phylum.d, barc.phylum.un)
barc.phylum.d <- as.data.frame(t(barc.phylum.d))

write.table(barc.phylum.d,
            file = "data/cleaned/sequencing/bac_arc_dominant_phyla_sample.txt",
            quote = F,
            sep ="\t",
            col.names = NA) # > 1% abundance

rm(barc.phylum.nd,
   barc.phylum.nd.sum,
   barc.phylum.un,
   unclassified.row)

# Average proportions by Channel (dominant phyla)
barc.phylum.dc <- read.table("data/cleaned/sequencing/bac_arc_dominant_phyla_sample.txt", 
                                sep = "\t", header = T, row.names = 1)
barc.phylum.dc <- as.data.frame(t(barc.phylum.dc))
colnames(barc.phylum.dc) <- meta$Channel
barc.phylum.dc <- barc.phylum.dc[ , order(names(barc.phylum.dc))]
barc.phylum.dc$Channel_12 <- rowMeans(barc.phylum.dc[ , 1:14])
barc.phylum.dc$Channel_13 <- rowMeans(barc.phylum.dc[ , 15:30])
barc.phylum.dc$Channel_19 <- rowMeans(barc.phylum.dc[ , 31:47])
barc.phylum.dc$Channel_21 <- rowMeans(barc.phylum.dc[ , 48:62])
barc.phylum.dc <- barc.phylum.dc[ , -c(1:62)]
barc.phylum.dc <- as.data.frame(t(barc.phylum.dc))

write.table(barc.phylum.dc,
            file = "data/cleaned/sequencing/bac_arc_dominant_phyla_avg-ch.txt",
            quote = F,
            sep ="\t",
            col.names = NA) # > 1% abundance

# Reshape data for stacked bar plot
barc.phylum.bar <- melt(barc.phylum.dc)
names(barc.phylum.bar) <- c("phylum", "proportion")

# Change rep() parameters based on number of cols
dim(barc.phylum.dc) # times = col number
barc.phylum.bar$Channel <- rep(c("Channel 12", 
                                    "Channel 13", 
                                    "Channel 19", 
                                    "Channel 21"), 
                                     times = 13)

barc.phylum.bar %>% 
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
barc.family <- as.data.frame(t(barc.asv))
barc.family$family <- barc.tax$Family
barc.family <- aggregate(. ~ family, data = barc.family, sum) # group ASVs of same family and sum reads
rownames(barc.family) <- barc.family$family
barc.family <- barc.family[ , -1]
barc.family <- (sweep(as.matrix(barc.family), 2, rowSums(barc.asv), "/")) * 100 # change reads to relative proportions
barc.family <- as.data.frame(t(barc.family))
barc.family$Unclassified = 100 - rowSums(barc.family)

write.table(barc.family,
            file = "data/cleaned/sequencing/bac_arc_family.txt",
            quote = F,
            sep ="\t",
            col.names = NA)


# Average proportions by Channel (all phyla)
barc.family <- read.table("data/cleaned/sequencing/bac_arc_family.txt", 
                             sep = "\t", header = T, row.names = 1)
barc.family <- as.data.frame(t(barc.family))
colnames(barc.family) <- meta$Channel
barc.family <- barc.family[ , order(names(barc.family))]
barc.family$Channel_12 <- rowMeans(barc.family[ , 1:14])
barc.family$Channel_13 <- rowMeans(barc.family[ , 15:30])
barc.family$Channel_19 <- rowMeans(barc.family[ , 31:47])
barc.family$Channel_21 <- rowMeans(barc.family[ , 48:62])
barc.family <- barc.family[ , -c(1:62)]
barc.family <- as.data.frame(t(barc.family))

write.table(barc.family,
            file = "data/cleaned/sequencing/bac_arc_phyla_avg-ch.txt",
            quote = F,
            sep ="\t",
            col.names = NA)

# Create dominant phyla table
barc.family <- read.table("data/cleaned/sequencing/bac_arc_family.txt", 
                             sep = "\t", header = T, row.names = 1)
barc.family.d <- as.data.frame(t(barc.family))
unclassified.row <- c("Unclassified")
barc.family.un <- barc.family.d[unclassified.row, ]
barc.family.d$avg <- rowMeans(barc.family.d)
barc.family.d <- barc.family.d[rownames(barc.family.d) != "Unclassified", ]
barc.family.nd <- barc.family.d[which(barc.family.d$avg < 1), ]
barc.family.d <- barc.family.d[-which(barc.family.d$avg < 1), ]
barc.family.d <- barc.family.d[order(barc.family.d$avg, decreasing = T), ]
barc.family.nd.sum <- colSums(barc.family.nd) 
barc.family.nd.sum <- as.data.frame(t(barc.family.nd.sum))
rownames(barc.family.nd.sum) <- c("Others")
barc.family.d <- rbind(barc.family.d, barc.family.nd.sum)
barc.family.d <- select(barc.family.d, -avg)
barc.family.d <- rbind(barc.family.d, barc.family.un)
barc.family.d <- as.data.frame(t(barc.family.d))

write.table(barc.family.d,
            file = "data/cleaned/sequencing/bac_arc_dominant_phyla_sample.txt",
            quote = F,
            sep ="\t",
            col.names = NA) # > 1% abundance

rm(barc.family.nd,
   barc.family.nd.sum,
   barc.family.un,
   unclassified.row)

# Average proportions by Channel (dominant phyla)
barc.family.dc <- read.table("data/cleaned/sequencing/bac_arc_dominant_phyla_sample.txt", 
                                sep = "\t", header = T, row.names = 1)
barc.family.dc <- as.data.frame(t(barc.family.dc))
colnames(barc.family.dc) <- meta$Channel
barc.family.dc <- barc.family.dc[ , order(names(barc.family.dc))]
barc.family.dc$Channel_12 <- rowMeans(barc.family.dc[ , 1:14])
barc.family.dc$Channel_13 <- rowMeans(barc.family.dc[ , 15:30])
barc.family.dc$Channel_19 <- rowMeans(barc.family.dc[ , 31:47])
barc.family.dc$Channel_21 <- rowMeans(barc.family.dc[ , 48:62])
barc.family.dc <- barc.family.dc[ , -c(1:62)]
barc.family.dc <- as.data.frame(t(barc.family.dc))

write.table(barc.family.dc,
            file = "data/cleaned/sequencing/bac_arc_dominant_phyla_avg-ch.txt",
            quote = F,
            sep ="\t",
            col.names = NA) # > 1% abundance

# Reshape data for stacked bar plot
barc.family.bar <- melt(barc.family.dc)
names(barc.family.bar) <- c("family", "proportion")

# Change rep() parameters based on number of cols
dim(barc.family.dc) # times = col number
barc.family.bar$Channel <- rep(c("Channel 12", 
                                    "Channel 13", 
                                    "Channel 19", 
                                    "Channel 21"), 
                                  times = 25)

barc.family.bar %>% 
  ggplot(aes(x = Channel, y = proportion, fill = family)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  ylab("Relative abundance (%)") +
  theme_bw(base_size = 15)+
  theme(legend.title = element_blank()) 


save.image("RData/16S_prelim-stats.RData")
