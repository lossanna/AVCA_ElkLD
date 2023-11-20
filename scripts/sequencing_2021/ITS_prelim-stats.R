# Purpose: Run ITS stats - create clean ASV tables, look at NMDS and beta dispersion,
#   calculate richness and diversity, create stacked bar charts of dominant phyla.
#   Write out clean data tables. NMDS & richness figures written out in T-test-by-Treatment3_2021.R.
# Post-2023-03-34 analysis only includes grouping by Channel and Treatment3.

# Bray-Curtis distance and NMDS should not be run again, because values differ slightly every time.
#   The current version saved in ITS_prelim-stats.RData is the "official" version that is then used for 
#   downstream analysis/model output/plots.

# Created: 2023-01-12
# Last updated: 2023-11-20

library(metagenomeSeq)
library(vegan)
library(agricolae)
library(FSA)
library(rcompanion)
library(tidyverse)
library(ggpubr)
library(reshape2)
library(car)


# Load data ---------------------------------------------------------------

# Read ASV table, taxonomy table and ASV_rep
fungi.asv.raw <- read.table("hpc-amplicon-sequencing/FASTQ_ITS_raw/ITS_demultiplexed/ITS_asv_table.txt", 
                           sep = "\t", header = TRUE, row.names = 1)
fungi.tax.raw <- read.table("hpc-amplicon-sequencing/FASTQ_ITS_raw/ITS_demultiplexed/ITS_taxa_table.txt", 
                           sep = "\t", header = TRUE, row.names = 1)
fungi.rep.raw <- read.table("hpc-amplicon-sequencing/FASTQ_ITS_raw/ITS_demultiplexed/ITS_ASV_rep.txt", 
                           sep = "\t", header = TRUE, row.names = 1)
pipeline.stats <- read.table("hpc-amplicon-sequencing/FASTQ_ITS_raw/ITS_demultiplexed/ITS_sequence_pipeline_stats.txt",
                             sep = "\t", header = TRUE, row.names = 1)


# Investigate pipeline stats ----------------------------------------------

summary(pipeline.stats$InputRetained)

# Less than 50% retained:
pipeline.stats %>% 
  filter(InputRetained < 50) # all of them

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
length(rm.contaminants) # 10

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



# NMDS & beta dispersion --------------------------------------------------

# NMDS ordination
fungi.dist <- vegdist(fungi.norm, method = "bray")
fungi.nmds <- metaMDS(fungi.dist, k = 2)
fungi.nmds$stress # ~0.2359848 (varies)

meta$NMDS1 <- fungi.nmds$points[ , 1]
meta$NMDS2 <- fungi.nmds$points[ , 2]


# Test community similarity differences
adonis2(fungi.dist ~ meta$Channel) # p < 0.001, 9% of variability explained by Channel
adonis2(fungi.dist ~ meta$Treatment3) # p = 0.011, 2% of variability explained by Treatment

# Plot NMDS
# By Channel
meta %>% 
  ggplot(aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = Channel)) +
  stat_ellipse(aes(color = Channel))

meta %>% 
  ggplot(aes(x = NMDS1, y = NMDS2, color = Channel, shape = Channel)) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(15:18)) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_minimal() +
  theme(legend.title = element_blank())


# By Treatment3
meta %>% 
  ggplot(aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = Treatment3)) +
  stat_ellipse(aes(color = Treatment3))

meta %>% 
  ggplot(aes(x = NMDS1, y = NMDS2, color = Treatment3, shape = Treatment3)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  labs(x = "Axis 1",
       y = "Axis 2",
       title = "Fungi NMDS",
       color = "Treatment",
       shape = "Treatment") +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  geom_text(aes(x = 0.7, y = -0.7, label = "PERMANOVA, p = 0.008"),
            size = 3, color = "gray30") +
  geom_text(aes(x = 0.78, y = -0.8, label = "Stress = 0.237"),
            size = 3, color = "gray30") # only 2% explained by Treatment3 lol


# Beta dispersion 
# By Channel
fungi.betadisper.c <- betadisper(fungi.dist, 
                              group = meta$Channel, 
                              type = "centroid")

anova(fungi.betadisper.c) # p = 0.0002724 

meta$betadisper.channel <- fungi.betadisper.c$distances

betadisper.c.hsd <- HSD.test(aov(betadisper.channel ~ Channel, data = meta), trt = "Channel")
betadisper.hsd
# Channel 19  0.5667671      a
# Channel 21  0.5361576     ab
# Channel 13  0.5076694     bc
# Channel 12  0.4803405      c

meta %>% 
  ggplot(aes(Channel, betadisper.channel), color = Channel) +
  geom_jitter(aes(color = Channel), 
              alpha = 0.8, 
              size = 4) +
  geom_boxplot(aes(fill = Channel), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("Beta dispersion") 


# Beta dispersion by Treatment3
fungi.betadisper.t3 <- betadisper(fungi.dist, 
                                 group = meta$Treatment3, 
                                 type = "centroid")

anova(fungi.betadisper.t3) # NS 

meta$betadisper.treatment3 <- fungi.betadisper.t3$distances

meta %>% 
  ggplot(aes(Treatment3, betadisper.treatment3)) +
  geom_jitter(aes(color = Treatment3), 
              alpha = 0.8, 
              size = 4) +
  geom_boxplot(aes(fill = Treatment3), 
               alpha = 0.3, 
               outlier.shape = NA) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  xlab(NULL) +
  ylab("Beta dispersion") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 


# Write table with beta dispersion distances
write.table(meta, 
            file = "data/cleaned/sequencing/fungi_diversity.txt", 
            quote = FALSE, 
            sep = "\t", 
            row.names = FALSE)



# Shannon diversity -------------------------------------------------------

# By Channel
# Explore distribution
boxplot(Shannon ~ Channel, data = meta)
qqPlot(meta$Shannon) # not normal?
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
  ylab("Fungi Shannon diversity")


# By Treatment3
qqplot(meta$Shannon)
shapiro.test(meta$Shannon) # p-value = 0.0004526

kruskal.test(Shannon ~ Treatment3, data = meta) # NS

# Plot Shannon diversity
meta %>% 
  ggplot(aes(Treatment3, Shannon), color = Treatment3) +
  geom_jitter(aes(color = Treatment3), 
              alpha = 0.8, 
              size = 4) +
  geom_boxplot(aes(fill = Treatment3), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 



# Richness ----------------------------------------------------------------

# By Channel
# Explore distribution
qqPlot(meta$Richness) # looks normal
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
  ylab("Fungi richness")


# By Treatment3
# Explore distribution
boxplot(Richness ~ Treatment3, data = meta)
qqPlot(meta$Richness)

t.test(filter(meta, Treatment3 == "Control")$Richness,
       filter(meta, Treatment3 == "Treated")$Richness) # NS

# Plot 
meta %>% 
  ggplot(aes(Treatment3, Richness), color = Treatment3) +
  geom_jitter(aes(color = Treatment3), 
              alpha = 0.8, 
              size = 3) +
  geom_boxplot(aes(fill = Treatment3), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("No. of ASVs") +
  ggtitle("Fungi richness") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "#000000"))

# Extract values
summary(filter(meta, Treatment3 == "Control")$Richness)
summary(filter(meta, Treatment3 == "Treated")$Richness)



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
            file = "data/cleaned/sequencing/fungi_phylum_avg-ch.txt",
            quote = F,
            sep ="\t",
            col.names = NA)

# Average proportions by Treatment3 (all phyla)
fungi.phylum <- read.table("data/cleaned/sequencing/fungi_phylum.txt", 
                           sep = "\t", header = T, row.names = 1)
fungi.phylum <- as.data.frame(t(fungi.phylum))
colnames(fungi.phylum) <- meta$Treatment3
fungi.phylum <- fungi.phylum[ , order(names(fungi.phylum))]
fungi.phylum$Control_avg <- rowMeans(fungi.phylum[ , 1:31])
fungi.phylum$Treated_avg <- rowMeans(fungi.phylum[ , 32:62])
fungi.phylum <- fungi.phylum[ , -c(1:62)]
colnames(fungi.phylum) <- c("Control", "Treated")
fungi.phylum <- as.data.frame(t(fungi.phylum))

write.table(fungi.phylum,
            file = "data/cleaned/sequencing/fungi_phylum_avg-t3.txt",
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
            file = "data/cleaned/sequencing/fungi_phylum-dominant_sample.txt",
            quote = F,
            sep ="\t",
            col.names = NA) # > 1% abundance

rm(fungi.phylum.nd,
   fungi.phylum.nd.sum,
   fungi.phylum.un,
   unclassified.row)


# By Channel
# Average proportions by Channel (dominant phyla)
fungi.phylum.dc <- read.table("data/cleaned/sequencing/fungi_phylum-dominant_sample.txt", 
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
            file = "data/cleaned/sequencing/fungi_phylum-dominant_avg-ch.txt",
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

write.table(fungi.phylum.bar,
            file = "data/cleaned/sequencing/fungi_phylum-dominant_avg-ch_barplot.txt",
            quote = FALSE,
            sep ="\t",
            col.names = NA) # > 1% abundance

fungi.phylum.bar %>% 
  ggplot(aes(x = Channel, y = proportion, fill = phylum)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  ylab("Relative abundance (%)") +
  theme_bw(base_size = 15)+
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c(brewer.pal(n = 4, "Set1"), 
                               "#999999"))

# By Treatment3
# Average proportions by Treatment3 (dominant phyla)
fungi.phylum.dt3 <- read.table("data/cleaned/sequencing/fungi_phylum-dominant_sample.txt", 
                              sep = "\t", header = T, row.names = 1)
fungi.phylum.dt3 <- as.data.frame(t(fungi.phylum.dt3))
colnames(fungi.phylum.dt3) <- meta$Channel
fungi.phylum.dt3 <- fungi.phylum.dt3[ , order(names(fungi.phylum.dt3))]
fungi.phylum.dt3$Control_avg <- rowMeans(fungi.phylum.dt3[ , 1:31])
fungi.phylum.dt3$Treated_avg <- rowMeans(fungi.phylum.dt3[ , 32:62])
fungi.phylum.dt3 <- fungi.phylum.dt3[ , -c(1:62)]
colnames(fungi.phylum.dt3) <- c("Control", "Treated")
fungi.phylum.dt3 <- as.data.frame(t(fungi.phylum.dt3))

write.table(fungi.phylum.dt3,
            file = "data/cleaned/sequencing/fungi_phylum-dominant_avg-t3.txt",
            quote = F,
            sep ="\t",
            col.names = NA) # > 1% abundance

# Reshape data for stacked bar plot
fungi.phylum.bar.t3 <- melt(fungi.phylum.dt3)
names(fungi.phylum.bar.t3) <- c("phylum", "proportion")

# Change rep() parameters based on number of cols
dim(fungi.phylum.dt3) # times = col number
fungi.phylum.bar.t3$Treatment3 <- rep(c("Control",
                                  "Treated"), 
                                times = 5)

write.table(fungi.phylum.bar.t3,
            file = "data/cleaned/sequencing/fungi_phylum-dominant_avg-t3_barplot.txt",
            quote = FALSE,
            sep ="\t",
            col.names = NA) # > 1% abundance

fungi.phylum.bar.t3 %>% 
  ggplot(aes(x = Treatment3, y = proportion, fill = phylum)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  ylab("Relative abundance (%)") +
  theme_bw(base_size = 15)+
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c(brewer.pal(n = 4, "Set1"), 
                               "#999999"))
                               


save.image("RData/ITS_prelim-stats.RData")
