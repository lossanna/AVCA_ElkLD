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
fungi.asv.raw <- read.table("amplicon-sequencing/FASTQ_ITS_raw/ITS_demultiplexed/ITS_asv_table.txt", 
                           sep = "\t", header = TRUE, row.names = 1)
fungi.tax.raw <- read.table("amplicon-sequencing/FASTQ_ITS_raw/ITS_demultiplexed/ITS_taxa_table.txt", 
                           sep = "\t", header = TRUE, row.names = 1)
fungi.rep.raw <- read.table("amplicon-sequencing/FASTQ_ITS_raw/ITS_demultiplexed/ITS_ASV_rep.txt", 
                           sep = "\t", header = TRUE, row.names = 1)
pipeline.stats <- read.table("amplicon-sequencing/FASTQ_ITS_raw/ITS_demultiplexed/ITS_sequence_pipeline_stats.txt",
                             sep = "\t", header = TRUE, row.names = 1)


# Investigate pipeline stats ----------------------------------------------

summary(pipeline.stats$InputRetained)

# Less than 50% retained:
pipeline.stats %>% 
  filter(InputRetained < 50) 

# Less than 50% retained after filtering:
pipeline.stats %>% 
  filter(FilteredRetained < 50)
  # Sample 11 from Channel 21 had only 25% retained
  # Sample 42 from Channel 13 had only 27% retained


# Generate clean tables ---------------------------------------------------

# Remove potential lab contamination
fungi.ext01.control <- fungi.asv.raw[which(rownames(fungi.asv.raw) == "avca-elkld_blank1"), ]
fungi.ext02.control <- fungi.asv.raw[which(rownames(fungi.asv.raw) == "avca-elkld_blank2"), ]
fungi.ext03.control <- fungi.asv.raw[which(rownames(fungi.asv.raw) == "avca-elkld_blank3"), ]
fungi.ext04.control <- fungi.asv.raw[which(rownames(fungi.asv.raw) == "avca-elkld_blank4"), ]
fungi.ext05.control <- fungi.asv.raw[which(rownames(fungi.asv.raw) == "avca-elkld_PCRZymo"), ]
fungi.ext06.control <- fungi.asv.raw[which(rownames(fungi.asv.raw) == "avca-elkld_PRCblank"), ]

# Which contaminants should be removed?
rm.contaminants <- unique(c(which(fungi.ext01.control > 0.01 * sum(fungi.ext01.control)),
                            which(fungi.ext02.control > 0.01 * sum(fungi.ext02.control)),
                            which(fungi.ext03.control > 0.01 * sum(fungi.ext03.control)),
                            which(fungi.ext04.control > 0.01 * sum(fungi.ext04.control)),
                            which(fungi.ext05.control > 0.01 * sum(fungi.ext05.control)),
                            which(fungi.ext06.control > 0.01 * sum(fungi.ext06.control))))
length(rm.contaminants)

# Remove contaminants from ASV table and taxonomy table
fungi.asv <- fungi.asv.raw[ , -rm.contaminants]
fungi.tax <- fungi.tax.raw[-rm.contaminants, ]

# Remove control samples from ASV table
rownames(fungi.asv)
fungi.asv <- fungi.asv[-c(63:68), ] # removed blanks and "unsigned"
rownames(fungi.asv)

# Remove empty ASVs
fungi.asv <- subset(fungi.asv, select = colSums(fungi.asv)!=0)
fungi.tax <- fungi.tax[colnames(fungi.asv), ]

# Remove ASV IDs from ASV.rep
fungi.rep <- subset(fungi.rep.raw, 
                   as.character(fungi.rep.raw$ASV) %in% rownames(fungi.tax))

# Save the clean asv table, taxonomy table and ASV.rep
write.table(fungi.asv, 
            file = "data/cleaned/sequencing/fungi_clean_asv.txt", 
            quote = F, 
            sep = "\t", 
            col.names = NA)
write.table(fungi.tax, 
            file = "data/cleaned/sequencing/fungi_clean_tax.txt", 
            quote = F, 
            sep = "\t", 
            col.names = NA)
write.table(fungi.rep, 
            file = "data/cleaned/sequencing/fungi_clean_rep.txt", 
            quote = F, 
            sep = "\t", 
            col.names = NA)

# Remove useless objects
rm(fungi.ext01.control, 
   fungi.ext02.control, 
   fungi.ext03.control,
   fungi.ext04.control,
   fungi.ext05.control,
   fungi.ext06.control,
   rm.contaminants)


# Create taxonomy table of unique taxa
fungi.tax.unique <- fungi.tax
fungi.tax.unique <- fungi.tax.unique[order(fungi.tax.unique$Species, decreasing = FALSE), ]
fungi.tax.unique <- fungi.tax.unique[order(fungi.tax.unique$Genus, decreasing = FALSE), ]
fungi.tax.unique <- fungi.tax.unique[order(fungi.tax.unique$Family, decreasing = FALSE), ]
fungi.tax.unique <- fungi.tax.unique[order(fungi.tax.unique$Order, decreasing = FALSE), ]
fungi.tax.unique <- fungi.tax.unique[order(fungi.tax.unique$Class, decreasing = FALSE), ]
fungi.tax.unique <- fungi.tax.unique[order(fungi.tax.unique$Phylum, decreasing = FALSE), ]
fungi.tax.unique <- fungi.tax.unique[order(fungi.tax.unique$Kingdom, decreasing = FALSE), ]
rownames(fungi.tax.unique) <- c()
fungi.tax.unique <- unique(fungi.tax.unique)

write.table(fungi.tax.unique,
            file = "data/cleaned/sequencing/fungi_clean_tax_unique.txt",
            quote = F, 
            sep = "\t", 
            col.names = NA)


# Sequence length & number of reads ---------------------------------------

seq.length <- as.vector(fungi.rep$rep)
table(nchar(seq.length)) # 208 bp most common
median(nchar(seq.length)) # 216 bp

# Total reads
sum(rowSums(fungi.asv)) # 5113868

# Reads per sample
mean(rowSums(fungi.asv)) # 82481.74
sd(rowSums(fungi.asv)) # 24375.54
summary(rowSums(fungi.asv))
# Min.    1st Qu.  Median   Mean    3rd Qu. Max. 
# 21287   68629   78378     82482   96509   144592 


# Statistical analysis ----------------------------------------------------

# Read metadata
meta <- read.table(file = "data/cleaned/sequencing/sequencing_metadata.txt",
                   header = TRUE,
                   sep = "\t")
meta <- meta[-63, ] # remove "unsigned" row; see other script for analysis including "unsigned"

# Check number of sequences per sample
hist(rowSums(fungi.asv))
summary(rowSums(fungi.asv)) # min 21287 - max 144592; median 78378 

# Normalization (metagenomeSeq)
fungi.MR <- newMRexperiment(t(fungi.asv))
p <- cumNormStat(fungi.MR)
fungi.MR <- cumNorm(fungi.MR, p = p)
fungi.norm <- t(MRcounts(fungi.MR, norm = T, log = F))

# Richness and Shannon
meta$Richness <- specnumber(fungi.norm)
meta$Shannon <- diversity(fungi.norm, index = "shannon")

write.table(meta, 
            file = "data/cleaned/sequencing/fungi_richness_Shannon.txt", 
            quote = F, 
            sep = "\t", 
            col.names = NA)


# NMDS & beta dispersion --------------------------------------------------

# NMDS ordination
fungi.dist <- vegdist(fungi.norm, method = "bray")
fungi.nmds <- metaMDS(fungi.dist, k = 2)
fungi.nmds$stress # 0.2369334 

meta$NMDS1 <- fungi.nmds$points[ , 1]
meta$NMDS2 <- fungi.nmds$points[ , 2]


# Test community similarity differences
adonis2(fungi.dist ~ meta$Channel) # p < 0.001, 9% of variability explained by Channel
adonis2(fungi.dist ~ meta$Treatment) # p < 0.001, 8% of variability explained by Treatment

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
fungi.betadisper <- betadisper(fungi.dist, 
                              group = meta$Channel, 
                              type = "centroid")

anova(fungi.betadisper) # p = 0.0002724 

meta$betadisper <- fungi.betadisper$distances

betadisper.hsd <- HSD.test(aov(betadisper ~ Channel, data = meta), trt = "Channel")
betadisper.hsd
# Channel 19  0.5667671      a
# Channel 21  0.5361576     ab
# Channel 13  0.5076694     bc
# Channel 12  0.4803405      c


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

shapiro.test(meta$Shannon) # p-value = 0.0004526
kruskal.test(Shannon ~ Channel, data = meta) # p-value = 0.01909
dt <- dunnTest(Shannon ~ Channel, data = meta, method = "bh")
dt <- dt$res
cldList(comparison = dt$Comparison, p.value = dt$P.adj, threshold  = 0.05)
meta %>% 
  group_by(Channel) %>%
  drop_na(Shannon) %>% 
  summarise(mean = mean(Shannon)) %>% 
  arrange(desc(mean)) 
# 1 Channel 13  4.72  a
# 2 Channel 12  4.41  ab
# 3 Channel 19  4.23  b
# 4 Channel 21  4.19  b

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

shapiro.test(meta$Richness) # p-value = 0.9629
richness.anova <- aov(meta$Richness ~ meta$Channel, data = meta) 
summary(richness.anova) # p = 0.00171
richness.hsd <- HSD.test(richness.anova, trt = "meta$Channel")
richness.hsd
# Channel 13      471.3750      a
# Channel 12      458.1429     ab
# Channel 19      364.8235     bc
# Channel 21      333.1333      c

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
fungi.phylum <- as.data.frame(t(fungi.asv))
fungi.phylum$phylum <- fungi.tax$Phylum
fungi.phylum <- aggregate(. ~ phylum, data = fungi.phylum, sum) # group ASVs of same phylum and sum reads
rownames(fungi.phylum) <- fungi.phylum$phylum
fungi.phylum <- fungi.phylum[ , -1]
fungi.phylum <- (sweep(as.matrix(fungi.phylum), 2, rowSums(fungi.asv), "/")) * 100 # change reads to relative proportions
fungi.phylum <- as.data.frame(t(fungi.phylum))
fungi.phylum$Unclassified = 100 - rowSums(fungi.phylum)

write.table(fungi.phylum,
            file = "data/cleaned/sequencing/fungi_phylum.txt",
            quote = F,
            sep ="\t",
            col.names = NA)


# Average proportions by Channel (all phyla)
fungi.phylum <- read.table("data/cleaned/sequencing/fungi_phylum.txt", 
                          sep = "\t", header = T, row.names = 1)
fungi.phylum <- as.data.frame(t(fungi.phylum))
colnames(fungi.phylum) <- meta$Channel
fungi.phylum <- fungi.phylum[ , order(names(fungi.phylum))]
fungi.phylum$Channel_12 <- rowMeans(fungi.phylum[ , 1:14])
fungi.phylum$Channel_13 <- rowMeans(fungi.phylum[ , 15:30])
fungi.phylum$Channel_19 <- rowMeans(fungi.phylum[ , 31:47])
fungi.phylum$Channel_21 <- rowMeans(fungi.phylum[ , 48:62])
fungi.phylum <- fungi.phylum[ , -c(1:62)]
fungi.phylum <- as.data.frame(t(fungi.phylum))

write.table(fungi.phylum,
            file = "data/cleaned/sequencing/fungi_phyla_avg-ch.txt",
            quote = F,
            sep ="\t",
            col.names = NA)

# Create dominant phyla table
fungi.phylum <- read.table("data/cleaned/sequencing/fungi_phylum.txt", 
                          sep = "\t", header = T, row.names = 1)
fungi.phylum.d <- as.data.frame(t(fungi.phylum))
unclassified.row <- c("Unclassified")
fungi.phylum.un <- fungi.phylum.d[unclassified.row, ]
fungi.phylum.d$avg <- rowMeans(fungi.phylum.d)
fungi.phylum.d <- fungi.phylum.d[rownames(fungi.phylum.d) != "Unclassified", ]
fungi.phylum.nd <- fungi.phylum.d[which(fungi.phylum.d$avg < 1), ]
fungi.phylum.d <- fungi.phylum.d[-which(fungi.phylum.d$avg < 1), ]
fungi.phylum.d <- fungi.phylum.d[order(fungi.phylum.d$avg, decreasing = T), ]
fungi.phylum.nd.sum <- colSums(fungi.phylum.nd) 
fungi.phylum.nd.sum <- as.data.frame(t(fungi.phylum.nd.sum))
rownames(fungi.phylum.nd.sum) <- c("Others")
fungi.phylum.d <- rbind(fungi.phylum.d, fungi.phylum.nd.sum)
fungi.phylum.d <- select(fungi.phylum.d, -avg)
fungi.phylum.d <- rbind(fungi.phylum.d, fungi.phylum.un)
fungi.phylum.d <- as.data.frame(t(fungi.phylum.d))

write.table(fungi.phylum.d,
            file = "data/cleaned/sequencing/fungi_dominant_phyla_sample.txt",
            quote = F,
            sep ="\t",
            col.names = NA) # > 1% abundance

rm(fungi.phylum.nd,
   fungi.phylum.nd.sum,
   fungi.phylum.un,
   unclassified.row)

# Average proportions by Channel (dominant phyla)
fungi.phylum.dc <- read.table("data/cleaned/sequencing/fungi_dominant_phyla_sample.txt", 
                             sep = "\t", header = T, row.names = 1)
fungi.phylum.dc <- as.data.frame(t(fungi.phylum.dc))
colnames(fungi.phylum.dc) <- meta$Channel
fungi.phylum.dc <- fungi.phylum.dc[ , order(names(fungi.phylum.dc))]
fungi.phylum.dc$Channel_12 <- rowMeans(fungi.phylum.dc[ , 1:14])
fungi.phylum.dc$Channel_13 <- rowMeans(fungi.phylum.dc[ , 15:30])
fungi.phylum.dc$Channel_19 <- rowMeans(fungi.phylum.dc[ , 31:47])
fungi.phylum.dc$Channel_21 <- rowMeans(fungi.phylum.dc[ , 48:62])
fungi.phylum.dc <- fungi.phylum.dc[ , -c(1:62)]
fungi.phylum.dc <- as.data.frame(t(fungi.phylum.dc))

write.table(fungi.phylum.dc,
            file = "data/cleaned/sequencing/fungi_dominant_phyla_avg-ch.txt",
            quote = F,
            sep ="\t",
            col.names = NA) # > 1% abundance

# Reshape data for stacked bar plot
fungi.phylum.bar <- melt(fungi.phylum.dc)
names(fungi.phylum.bar) <- c("phylum", "proportion")

# Change rep() parameters based on number of cols
dim(fungi.phylum.dc) # times = col number
fungi.phylum.bar$Channel <- rep(c("Channel 12", 
                                 "Channel 13", 
                                 "Channel 19", 
                                 "Channel 21"), 
                               times = 5)

fungi.phylum.bar %>% 
  ggplot(aes(x = Channel, y = proportion, fill = phylum)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  ylab("Relative abundance (%)") +
  theme_bw(base_size = 15)+
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c(brewer.pal("Set1"),
                               "#999999"))
                               


# Family ------------------------------------------------------------------

# Generate family level profile
fungi.family <- as.data.frame(t(fungi.asv))
fungi.family$family <- fungi.tax$Family
fungi.family <- aggregate(. ~ family, data = fungi.family, sum) # group ASVs of same family and sum reads
rownames(fungi.family) <- fungi.family$family
fungi.family <- fungi.family[ , -1]
fungi.family <- (sweep(as.matrix(fungi.family), 2, rowSums(fungi.asv), "/")) * 100 # change reads to relative proportions
fungi.family <- as.data.frame(t(fungi.family))
fungi.family$Unclassified = 100 - rowSums(fungi.family)

write.table(fungi.family,
            file = "data/cleaned/sequencing/fungi_family.txt",
            quote = F,
            sep ="\t",
            col.names = NA)


# Average proportions by Channel (all phyla)
fungi.family <- read.table("data/cleaned/sequencing/fungi_family.txt", 
                          sep = "\t", header = T, row.names = 1)
fungi.family <- as.data.frame(t(fungi.family))
colnames(fungi.family) <- meta$Channel
fungi.family <- fungi.family[ , order(names(fungi.family))]
fungi.family$Channel_12 <- rowMeans(fungi.family[ , 1:14])
fungi.family$Channel_13 <- rowMeans(fungi.family[ , 15:30])
fungi.family$Channel_19 <- rowMeans(fungi.family[ , 31:47])
fungi.family$Channel_21 <- rowMeans(fungi.family[ , 48:62])
fungi.family <- fungi.family[ , -c(1:62)]
fungi.family <- as.data.frame(t(fungi.family))

write.table(fungi.family,
            file = "data/cleaned/sequencing/fungi_phyla_avg-ch.txt",
            quote = F,
            sep ="\t",
            col.names = NA)

# Create dominant phyla table
fungi.family <- read.table("data/cleaned/sequencing/fungi_family.txt", 
                          sep = "\t", header = T, row.names = 1)
fungi.family.d <- as.data.frame(t(fungi.family))
unclassified.row <- c("Unclassified")
fungi.family.un <- fungi.family.d[unclassified.row, ]
fungi.family.d$avg <- rowMeans(fungi.family.d)
fungi.family.d <- fungi.family.d[rownames(fungi.family.d) != "Unclassified", ]
fungi.family.nd <- fungi.family.d[which(fungi.family.d$avg < 1), ]
fungi.family.d <- fungi.family.d[-which(fungi.family.d$avg < 1), ]
fungi.family.d <- fungi.family.d[order(fungi.family.d$avg, decreasing = T), ]
fungi.family.nd.sum <- colSums(fungi.family.nd) 
fungi.family.nd.sum <- as.data.frame(t(fungi.family.nd.sum))
rownames(fungi.family.nd.sum) <- c("Others")
fungi.family.d <- rbind(fungi.family.d, fungi.family.nd.sum)
fungi.family.d <- select(fungi.family.d, -avg)
fungi.family.d <- rbind(fungi.family.d, fungi.family.un)
fungi.family.d <- as.data.frame(t(fungi.family.d))

write.table(fungi.family.d,
            file = "data/cleaned/sequencing/fungi_dominant_phyla_sample.txt",
            quote = F,
            sep ="\t",
            col.names = NA) # > 1% abundance

rm(fungi.family.nd,
   fungi.family.nd.sum,
   fungi.family.un,
   unclassified.row)

# Average proportions by Channel (dominant phyla)
fungi.family.dc <- read.table("data/cleaned/sequencing/fungi_dominant_phyla_sample.txt", 
                             sep = "\t", header = T, row.names = 1)
fungi.family.dc <- as.data.frame(t(fungi.family.dc))
colnames(fungi.family.dc) <- meta$Channel
fungi.family.dc <- fungi.family.dc[ , order(names(fungi.family.dc))]
fungi.family.dc$Channel_12 <- rowMeans(fungi.family.dc[ , 1:14])
fungi.family.dc$Channel_13 <- rowMeans(fungi.family.dc[ , 15:30])
fungi.family.dc$Channel_19 <- rowMeans(fungi.family.dc[ , 31:47])
fungi.family.dc$Channel_21 <- rowMeans(fungi.family.dc[ , 48:62])
fungi.family.dc <- fungi.family.dc[ , -c(1:62)]
fungi.family.dc <- as.data.frame(t(fungi.family.dc))

write.table(fungi.family.dc,
            file = "data/cleaned/sequencing/fungi_dominant_phyla_avg-ch.txt",
            quote = F,
            sep ="\t",
            col.names = NA) # > 1% abundance

# Reshape data for stacked bar plot
fungi.family.bar <- melt(fungi.family.dc)
names(fungi.family.bar) <- c("family", "proportion")

# Change rep() parameters based on number of cols
dim(fungi.family.dc) # times = col number
fungi.family.bar$Channel <- rep(c("Channel 12", 
                                 "Channel 13", 
                                 "Channel 19", 
                                 "Channel 21"), 
                               times = 25)

fungi.family.bar %>% 
  ggplot(aes(x = Channel, y = proportion, fill = family)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  ylab("Relative abundance (%)") +
  theme_bw(base_size = 15)+
  theme(legend.title = element_blank()) 


save.image("RData/AVCA-ElkLD_ITS_stats.RData")
