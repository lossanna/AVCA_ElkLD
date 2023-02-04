library(tidyverse)
library(car)

# Load data ---------------------------------------------------------------

fapro.raw <- read.table("data/cleaned/sequencing/faprotax-output.tsv",
                        header = T)

meta <- read.table("data/cleaned/sequencing/sequencing_metadata.txt",
                   sep = "\t", header = TRUE, row.names = 1)

barc.asv <- read.table("data/cleaned/sequencing/bac_arc_clean_asv.txt",
                       sep = "\t", header = TRUE, row.names = 1)


# Data wrangling ----------------------------------------------------------

# Remove ASV col
fapro0 <- fapro.raw %>% 
  select(-asv)  

# Transform
fapro0 <- as.data.frame(t(fapro0))

# Name cols
colnames(fapro0) <- fapro0[1, ]
fapro0 <- fapro0[-1, ]

# Add Sample col and remove row names
fapro0$Sample <- 1:nrow(fapro0)
fapro0 <- fapro0[ , c(93, 1:92)] # check that sample numbers match name
row.names(fapro0) <- c()

# Convert all to numeric
str(fapro0)
fapro0[] <- lapply(fapro0, as.numeric)
str(fapro0)

# Remove empty cols
fapro <- fapro0 %>% 
  select(-Sample)
fapro <- fapro[ , colSums(fapro !=0) > 0]
fapro <- fapro %>% 
  mutate(Sample = c(1:62))

# Add metadata
fapro <- left_join(meta, fapro) 
fapro <- fapro %>% 
  mutate(Channel = factor(fapro.all$Channel),
         Treatment = factor(fapro.all$Treatment,
                            levels = c("Baffle", "One rock dam", 
                                       "Upland treatment", "No treatment")),
         Treatment2 = factor(fapro.all$Treatment2,
                             levels = c("In-channel treatment", "Upland treatment",
                                        "No treatment")))

# Write table of categories
write.csv(colnames(fapro[ , -c(1:5)]),
          file = "data/cleaned/sequencing/faprotax-categories.csv",
          row.names = FALSE)

# Row sums of FAPROTAX-assigned reads
sum.fapro <- fapro0 %>% 
  select(-Sample) 
sum.fapro <- sum.fapro %>% 
  mutate(sum.fapro = rowSums(sum.fapro)) %>% 
  select(sum.fapro) %>% 
  mutate(Sample = 1:62)

# Row sums of reads from all ASVs
sum.asv <- data.frame(Sample = 1:62,
                      sum_asv = rowSums(barc.asv))
  






# Cellulolysis ------------------------------------------------------------

# By Treatment2
qqPlot(fapro$cellulolysis) # not normal



plot(aov(cellulolysis ~ Treatment2, data = fapro))
