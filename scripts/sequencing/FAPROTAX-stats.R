library(tidyverse)


# Load data ---------------------------------------------------------------

fapro.raw <- read.table("data/cleaned/sequencing/faprotax-output.tsv")


# Wrangling ---------------------------------------------------------------

fapro <- fapro.raw %>% 
  select(-asv)  
fapro <- as.data.frame(t(fapro))  

test <- fapro[-1, ]
sum(test$methanotrophy)
