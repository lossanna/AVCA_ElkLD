library(tidyverse)
library(car)

# Load data ---------------------------------------------------------------

fapro.raw <- read.table("data/cleaned/sequencing/faprotax-output.tsv",
                        header = T)
meta <- read.table("data/cleaned/sequencing/sequencing_metadata.txt",
                   sep = "\t", header = TRUE)


# List of categories ------------------------------------------------------

write.csv(fapro.raw$group,
          file = "data/cleaned/sequencing/faprotax-categories.csv",
          row.names = FALSE)

# Wrangling ---------------------------------------------------------------

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

# Add metadata
meta <- meta[-63, ] # remove unsigned
fapro <- left_join(meta, fapro0) 
fapro <- fapro %>% 
  mutate(Channel = factor(fapro$Channel),
         Treatment = factor(fapro$Treatment,
                            levels = c("Baffle", "One rock dam", 
                                       "Upland treatment", "No treatment")),
         Treatment2 = factor(fapro$Treatment2,
                             levels = c("In-channel treatment", "Upland treatment",
                                        "No treatment")))



# Cellulolysis ------------------------------------------------------------

# By Treatment2
qqPlot(fapro$cellulolysis) # not normal



plot(aov(cellulolysis ~ Treatment2, data = fapro))
