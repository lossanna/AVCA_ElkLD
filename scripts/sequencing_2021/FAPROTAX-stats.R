# Purpose:
# Created: 2023-01-19

library(tidyverse)
library(car)

# Load data ---------------------------------------------------------------

fapro.raw <- read.table("data/cleaned/sequencing/faprotax-output.tsv",
                        header = T, row.names = 1)

meta <- read.table("data/cleaned/sequencing/sequencing_metadata.txt",
                   sep = "\t", header = TRUE)

barc.asv <- read.table("data/cleaned/sequencing/bac-arc_clean_asv.txt",
                       sep = "\t", header = TRUE, row.names = 1)


# Data wrangling ----------------------------------------------------------

# Remove ASV col
fapro0 <- fapro.raw %>% 
  select(-asv)  

# Convert reads to proportions
fapro0 <- (sweep(as.matrix(fapro0), 2, rowSums(barc.asv), "/")) * 100
fapro0 <- as.data.frame(t(fapro0))

# Write table of categories
write.csv(colnames(fapro0),
          file = "data/cleaned/sequencing/faprotax-categories.csv",
          row.names = FALSE)

# Remove cols that sum to less than 1%
fapro <- fapro0[ , colSums(abs(fapro0)) > 1]

# Remove other irrelevant cols
fapro <- fapro |> 
  select(-human_pathogens_all, -human_associated, -animal_parasites_or_symbionts,
         -intracellular_parasites, -predatory_or_exoparasitic)

# Add metadata
fapro <- fapro %>% 
  mutate(Sample = c(1:62)) 
rownames(fapro) <- c()
fapro <- left_join(meta, fapro) 

# Sum all involved in N-cycling
fapro <- fapro |> 
  mutate(n_cyclers = aerobic_ammonia_oxidation + nitrification + nitrate_denitrification + 
         nitrite_denitrification + nitrous_oxide_denitrification + denitrification +
           nitrogen_fixation + nitrite_respiration + nitrate_respiration +
           nitrate_reduction + nitrogen_respiration + ureolysis)

# Write to CSV
write.csv(fapro,
          file = "data/cleaned/sequencing/faprotax-proportions_clean.csv",
          row.names = FALSE)



# Cellulolysis ------------------------------------------------------------

qqPlot(fapro$cellulolysis) # mostly normal except for 2?



# Nitrification -----------------------------------------------------------

qqPlot(fapro$nitrification) # mostly normal except for one?

t.test(filter(fapro, Treatment3 == "Control")$nitrification,
       filter(fapro, Treatment3 == "Treated")$nitrification) # NS

# Plot
fapro |> 
  ggplot(aes(x = Treatment3, y = nitrification, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Proportion of nitrifying bac & arc",
       x = NULL,
       y = "Relative abundance (%)") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")


# Chemoheterotrophy -------------------------------------------------------

qqPlot(fapro$chemoheterotrophy) # normal

t.test(filter(fapro, Treatment3 == "Control")$chemoheterotrophy,
       filter(fapro, Treatment3 == "Treated")$chemoheterotrophy) # NS

# Plot
fapro |> 
  ggplot(aes(x = Treatment3, y = chemoheterotrophy, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Proportion of chemoheterotrophs",
       x = NULL,
       y = "Relative abundance (%)") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")


# N-cyclers ---------------------------------------------------------------

qqPlot(fapro$n_cyclers) # normal

t.test(filter(fapro, Treatment3 == "Control")$n_cyclers,
       filter(fapro, Treatment3 == "Treated")$n_cyclers) # NS

# Plot
fapro |> 
  ggplot(aes(x = Treatment3, y = n_cyclers, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Proportion of N-cyclers",
       x = NULL,
       y = "Relative abundance (%)") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")


# Methanotrophy -----------------------------------------------------------

qqPlot(fapro$methanotrophy) # normal

t.test(filter(fapro, Treatment3 == "Control")$methanotrophy,
       filter(fapro, Treatment3 == "Treated")$methanotrophy) # NS

# Plot
fapro |> 
  ggplot(aes(x = Treatment3, y = methanotrophy, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Proportion of methanotrophs",
       x = NULL,
       y = "Relative abundance (%)") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
