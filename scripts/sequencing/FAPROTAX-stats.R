library(tidyverse)
library(car)

# Load data ---------------------------------------------------------------

fapro.raw <- read.table("data/cleaned/sequencing/faprotax-output.tsv",
                        header = T, row.names = 1)

meta <- read.table("data/cleaned/sequencing/sequencing_metadata.txt",
                   sep = "\t", header = TRUE, row.names = 1)

barc.asv <- read.table("data/cleaned/sequencing/bac_arc_clean_asv.txt",
                       sep = "\t", header = TRUE, row.names = 1)


# Data wrangling ----------------------------------------------------------

# Remove ASV col
fapro0 <- fapro.raw %>% 
  select(-asv)  

# Convert reads to proportions
fapro0 <- (sweep(as.matrix(fapro0), 2, rowSums(barc.asv), "/")) * 100
fapro0 <- as.data.frame(t(fapro0))

# Add metadata
fapro <- fapro0 %>% 
  mutate(Sample = c(1:62)) 
rownames(fapro) <- c()
fapro <- left_join(meta, fapro) 
fapro <- fapro %>% 
  mutate(Channel = factor(fapro$Channel),
         Treatment = factor(fapro$Treatment,
                            levels = c("Baffle", "One rock dam", 
                                       "Upland treatment", "No treatment")),
         Treatment2 = factor(fapro$Treatment2,
                             levels = c("In-channel treatment", "Upland treatment",
                                        "No treatment")))

# Write table of categories
write.csv(colnames(fapro[ , -c(1:5)]),
          file = "data/cleaned/sequencing/faprotax-categories.csv",
          row.names = FALSE)






# Cellulolysis ------------------------------------------------------------

# By Treatment2
qqPlot(fapro$cellulolysis) # not normal



plot(aov(cellulolysis ~ Treatment2, data = fapro))
