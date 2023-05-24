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
            quote = FALSE, 
            sep = "\t", 
            col.names = NA)
write.table(barc.tax, 
            file = "data/cleaned/sequencing/bac_arc_clean_tax.txt", 
            quote = FALSE, 
            sep = "\t", 
            col.names = NA)
write.table(barc.rep, 
            file = "data/cleaned/sequencing/bac_arc_clean_rep.txt", 
            quote = FALSE, 
            sep = "\t", 
            row.names = FALSE)

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
            quote = FALSE, 
            sep = "\t", 
            row.names = FALSE)


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


# NMDS & beta dispersion --------------------------------------------------

# NMDS ordination
barc.dist <- vegdist(barc.norm, method = "bray")
barc.nmds <- metaMDS(barc.dist, k = 2)
barc.nmds$stress # 0.1680364 (varies)

meta$NMDS1 <- barc.nmds$points[ , 1]
meta$NMDS2 <- barc.nmds$points[ , 2]


# Test community similarity differences
adonis2(barc.dist ~ meta$Channel) # p < 0.001, 15% of variability explained by Channel
adonis2(barc.dist ~ meta$Treatment1) # p < 0.001, 13% of variability explained by Treatment
adonis2(barc.dist ~ meta$Treatment2) # p < 0.001, 11% of variability explained by Treatment2 (BAF and ORD combined)
adonis2(barc.dist ~ meta$Treatment3) # p = 0.026, 3% of variability explained by Treatment3

# Plot NMDS
# By channel
meta %>% 
ggplot(aes(x = NMDS1, y = NMDS2, color = Channel, shape = Channel)) +
  geom_point(size = 3) +
  stat_ellipse() 

meta %>% 
  ggplot(aes(x = NMDS1, y = NMDS2, color = Channel, shape = Channel)) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(15:18)) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_minimal() +
  theme(legend.title = element_blank())

# By treatment1
meta %>% 
  ggplot(aes(x = NMDS1, y = NMDS2, color = Treatment, shape = Treatment)) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(15:18)) +
  scale_color_manual(values = c("#33A02C","#33A02C", "#1F78B4", "red")) +
  theme_minimal()

# By treatment2
meta %>% 
  ggplot(aes(x = NMDS1, y = NMDS2, color = Treatment2, shape = Treatment2)) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(15, 17, 18)) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_minimal() 

# By treatment3
meta %>% 
  ggplot(aes(x = NMDS1, y = NMDS2, color = Treatment3, shape = Treatment3)) +
  geom_point(size = 4) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_minimal() 

# By channel and treatment
meta %>% 
  ggplot(aes(x = NMDS1, y = NMDS2, color = Channel, shape = Treatment)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c(15:18)) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) 



# Beta dispersion by channel
barc.betadisper.c <- betadisper(barc.dist, 
                              group = meta$Channel, 
                              type = "centroid")

anova(barc.betadisper.c) # p = 0.0001425 

meta$betadisper.channel <- barc.betadisper.c$distances

betadisper.c.hsd <- HSD.test(aov(betadisper.channel ~ Channel, data = meta), trt = "Channel")
betadisper.c.hsd

meta %>% 
  ggplot(aes(Channel, betadisper.channel)) +
  geom_jitter(aes(color = Channel), 
              alpha = 0.8, 
              size = 4) +
  geom_boxplot(aes(fill = Channel), 
               alpha = 0.3, 
               outlier.shape = NA) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  xlab(NULL) +
  ylab("Beta dispersion") 


# Beta dispersion by Treatment1
barc.betadisper.t1 <- betadisper(barc.dist, 
                                group = meta$Treatment1, 
                                type = "centroid")

anova(barc.betadisper.t) # p = 5.534e-05 

meta$betadisper.treatment1 <- barc.betadisper.t1$distances

betadisper.t1.hsd <- HSD.test(aov(betadisper.treatment1 ~ Treatment1, data = meta), 
                             trt = "Treatment1")
betadisper.t1.hsd
betadis.letters.t1 <- betadisper.t1.hsd$groups
betadis.letters.t1 <- betadis.letters.t1[c("Baffle", "One rock dam", 
                                         "Upland treatment", "Control"), ]

letters <- data.frame(label = betadis.letters.t1$groups,
                      x = 1:4,
                      y = c(rep(0.55, 4)))

meta %>% 
  ggplot(aes(Treatment1, betadisper.treatment1)) +
  geom_jitter(aes(color = Treatment1), 
              alpha = 0.8, 
              size = 4) +
  geom_boxplot(aes(fill = Treatment1), 
               alpha = 0.3, 
               outlier.shape = NA) +
  scale_color_manual(values = c("#33A02C","#33A02C", "#1F78B4", "red")) +
  scale_fill_manual(values = c("#33A02C","#33A02C", "#1F78B4", "red")) +
  xlab(NULL) +
  ylab("Beta dispersion") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") 
  

# Beta dispersion by treatment 2
barc.betadisper.t2 <- betadisper(barc.dist, 
                                group = meta$Treatment2, 
                                type = "centroid")

anova(barc.betadisper.t2) # p = 5.683e-05 

meta$betadisper.treatment2 <- barc.betadisper.t2$distances

betadisper.t2.hsd <- HSD.test(aov(betadisper.treatment2 ~ Treatment2, data = meta), 
                             trt = "Treatment2")
betadisper.t2.hsd
betadis.letters.t2 <- betadisper.t2.hsd$groups
betadis.letters.t2 <- betadis.letters.t2[c("In-channel treatment", 
                                         "Upland treatment", "Control"), ]
letters <- data.frame(label = betadis.letters.t2$groups,
                      x = 1:3,
                      y = c(rep(0.55, 3)))

meta %>% 
  ggplot(aes(Treatment2, betadisper.treatment2)) +
  geom_jitter(aes(color = Treatment2), 
              alpha = 0.8, 
              size = 4) +
  geom_boxplot(aes(fill = Treatment2), 
               alpha = 0.3, 
               outlier.shape = NA) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  xlab(NULL) +
  ylab("Beta dispersion") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") 

# Beta dispersion by treatment 2
barc.betadisper.t3 <- betadisper(barc.dist, 
                                 group = meta$Treatment3, 
                                 type = "centroid")

anova(barc.betadisper.t3) # NS

meta$betadisper.treatment3 <- barc.betadisper.t3$distances

write.table(meta, 
            file = "data/cleaned/sequencing/bac_arc_diversity.txt", 
            quote = FALSE, 
            sep = "\t", 
            row.names = FALSE)



# Shannon diversity -------------------------------------------------------

# By channel
# Explore distribution
boxplot(Shannon ~ Channel, data = meta)

plot(tapply(meta$Shannon,
            meta$Channel, var),
     tapply(meta$Shannon,
            meta$Channel, mean))

plot(aov(Shannon ~ Channel, data = meta)) # QQ plot is pretty off; likely not normal

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


# By Treatment2
# Explore distribution
boxplot(Shannon ~ Treatment2, data = meta)

plot(tapply(meta$Shannon,
            meta$Treatment2, var),
     tapply(meta$Shannon,
            meta$Treatment2, mean))

plot(aov(Shannon ~ Treatment2, data = meta)) # QQ plot is pretty off; likely not normal

shapiro.test(meta$Shannon) # p-value = 9.452e-05
kruskal.test(Shannon ~ Treatment2, data = meta) # p-value = 0.1412

# Plot Shannon diversity
meta %>% 
  ggplot(aes(Treatment2, Shannon), color = Treatment2) +
  geom_jitter(aes(color = Treatment2), 
              alpha = 0.8, 
              size = 4) +
  geom_boxplot(aes(fill = Treatment2), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 


# By Treatment3
  # Explore distribution
boxplot(Shannon ~ Treatment3, data = meta)
qqPlot(meta$Shannon)

t.test(filter(meta, Treatment3 == "Control")$Shannon,
       filter(meta, Treatment3 == "Treated")$Shannon) # NS

# Plot Shannon diversity
meta |> 
  ggplot(aes(Treatment3, Shannon)) +
  geom_boxplot() +
  geom_jitter()



# Richness ----------------------------------------------------------------

# Explore distribution
boxplot(Richness ~ Channel, data = meta)

plot(tapply(meta$Richness,
            meta$Channel, var),
     tapply(meta$Richness,
            meta$Channel, mean))

plot(aov(Richness ~ Channel, data = meta))

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
  ylab("Richness")


# By Treatment2
# Explore distribution
boxplot(Richness ~ Treatment2, data = meta)

plot(tapply(meta$Richness,
            meta$Treatment2, var),
     tapply(meta$Richness,
            meta$Treatment2, mean))

plot(aov(Richness ~ Treatment2, data = meta)) # QQ plot seems normal

shapiro.test(meta$Richness) # p-value = 0.9817
summary(aov(meta$Richness ~ meta$Treatment2)) # p-value = 0.288

# Plot Richness diversity
meta %>% 
  ggplot(aes(Treatment2, Richness), color = Treatment2) +
  geom_jitter(aes(color = Treatment2), 
              alpha = 0.8, 
              size = 4) +
  geom_boxplot(aes(fill = Treatment2), 
               alpha = 0.3, 
               outlier.shape = NA) +
  xlab(NULL) +
  ylab("Richness") +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 


# By Treatment3
# Explore distribution
boxplot(Richness ~ Treatment3, data = meta)
qqPlot(meta$Richness)

t.test(filter(meta, Treatment3 == "Control")$Richness,
       filter(meta, Treatment3 == "Treated")$Richness) # NS

# Plot 
meta |> 
  ggplot(aes(Treatment3, Richness)) +
  geom_boxplot() +
  geom_jitter()



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
            quote = FALSE,
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
            quote = FALSE,
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
            quote = FALSE,
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
            quote = FALSE,
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
            quote = FALSE,
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
            quote = FALSE,
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
            quote = FALSE,
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
            quote = FALSE,
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
