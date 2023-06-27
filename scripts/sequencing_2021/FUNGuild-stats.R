# Purpose: Write clean table translating ASV reads into relative proportions for FUNGuild categories, 
#   and explore groups of potential interest.
# Created: 2023-06-26
# Updated: 2023-06-26

library(tidyverse)
library(car)

# Load data ---------------------------------------------------------------

funguild.raw <- read.table("data/cleaned/sequencing/FUNGuild-output.txt",
                           sep = "\t", header = T, row.names = 1)

meta <- read.table("data/cleaned/sequencing/sequencing_metadata.txt",
                   sep = "\t", header = TRUE)

fungi.asv <- read.table("data/cleaned/sequencing/fungi_clean_asv.txt",
                       sep = "\t", header = TRUE, row.names = 1)



# Confidence --------------------------------------------------------------

# Keep only those listed as Highly Probable and Probable
unique(funguild.raw$confidenceRanking)
funguild.prob <- funguild.raw |> 
  filter(confidenceRanking %in% c("Probable", "Highly Probable")) 
funguild.prob$ASV <- rownames(funguild.prob)
nrow(funguild.prob) / nrow(funguild.raw) # 41% of ASVs


# By trophic level --------------------------------------------------------

# Keep only those listed as Highly Probable and Probable
funguild.prob.trophic <- funguild.prob |> 
  select(trophicMode, ASV)

# Assign trophic
fungi.trophic <- as.data.frame(t(fungi.asv))
fungi.trophic$ASV <- rownames(fungi.trophic)
fungi.trophic <- left_join(fungi.trophic, funguild.prob.trophic) # add guild information
fungi.trophic <- fungi.trophic[ , c(63:64, 1:62)]
fungi.trophic$trophicMode[is.na(fungi.trophic$trophicMode)] <- "Unknown"
fungi.trophic <- fungi.trophic |> 
  select(-ASV)
fungi.trophic <- aggregate(. ~ trophicMode, data = fungi.trophic, sum) # group ASVs of same guild and sum reads
rownames(fungi.trophic) <- fungi.trophic$trophicMode
fungi.trophic <- fungi.trophic[ , -1]
fungi.trophic <- (sweep(as.matrix(fungi.trophic), 2, rowSums(fungi.asv), "/")) * 100 # change reads to relative proportions
fungi.trophic <- as.data.frame(t(fungi.trophic))

# Add metadata
fungi.trophic <- bind_cols(meta, fungi.trophic)
rownames(fungi.trophic) <- c()

write.csv(fungi.trophic,
          file = "data/cleaned/sequencing/FUNGuild-proportions-trophic_clean.csv",
          row.names = F)


# By guild ----------------------------------------------------------------

# Keep only those listed as Highly Probable and Probable
funguild.prob.guild <- funguild.prob |> 
  select(guild, ASV)

# Assign guilds
fungi.guilds <- as.data.frame(t(fungi.asv))
fungi.guilds$ASV <- rownames(fungi.guilds)
fungi.guilds <- left_join(fungi.guilds, funguild.prob.guild) # add guild information
fungi.guilds <- fungi.guilds[ , c(63:64, 1:62)]
fungi.guilds$guild[is.na(fungi.guilds$guild)] <- "Unknown"
fungi.guilds <- fungi.guilds |> 
  select(-ASV)
fungi.guilds <- aggregate(. ~ guild, data = fungi.guilds, sum) # group ASVs of same guild and sum reads
rownames(fungi.guilds) <- fungi.guilds$guild
fungi.guilds <- fungi.guilds[ , -1]
fungi.guilds <- (sweep(as.matrix(fungi.guilds), 2, rowSums(fungi.asv), "/")) * 100 # change reads to relative proportions
fungi.guilds <- as.data.frame(t(fungi.guilds))

# Add metadata
fungi.guilds <- bind_cols(meta, fungi.guilds)
rownames(fungi.guilds) <- c()

write.csv(fungi.guilds,
          file = "data/cleaned/sequencing/FUNGuild-proportions-guild_clean.csv",
          row.names = F)

# Write list of guilds
guilds <- funguild.prob |> 
  group_by(guild, trophicMode) |> 
  summarise(count = n(),
            .groups = "keep")
write.csv(guilds,
          file = "data/cleaned/sequencing/FUNGuild-guild-categories.csv",
          row.names = F)


# Saprotrophs -------------------------------------------------------------

qqPlot(fungi.trophic$Saprotroph) # normal

t.test(filter(fungi.trophic, Treatment3 == "Control")$Saprotroph,
       filter(fungi.trophic, Treatment3 == "Treated")$Saprotroph) # NS

# Plot
fungi.trophic |> 
  ggplot(aes(x = Treatment3, y = Saprotroph, fill = Treatment3, color = Treatment3)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA) +
  geom_jitter(size = 2) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Proportion of saprotrophic fungi",
       x = NULL,
       y = "Relative abundance (%)") +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")

