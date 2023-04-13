# Purpose: Data screening for 2021 plant and soil data, and write cleaned data table.

# Checked for normality, visualized distribution, checked collinearity
# Determined all were normally distributed except TN_perc & TC_perc, which were log-transformed,
#   and found that OM & TC were collinear
# Produced a clean data sheet for 2021 data (Data-2021_clean.csv).
#   Previously had created data for SEM input, but then later decided SEM was not the right tool.


library(tidyverse)
library(readxl)
library(caret)
library(performance)
library(ggpubr)
library(car)

# Load data ---------------------------------------------------------------

total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
herb.all <- read.csv("data/cleaned/Summarised-all_herb-cover.csv")
notree.all <- read.csv("data/cleaned/Summarised-all_notree-cover.csv")
per.div <- read.csv("data/cleaned/Summarised-all_perennial-diversity.csv")

barc.div <- read.table("data/cleaned/sequencing/bac_arc_diversity.txt",
                     sep = "\t", header = TRUE)
soil.chem <- read_xlsx("data/Excel_LO_edited/Blankinship-soil-chemistry_TN-TC-OM_LO.xlsx", 
                       sheet = "R_LO") |> 
  select(-Year, -Treatment)
elevation <- read_xlsx("data/Excel_LO_edited/Vegetation monitoring point elevation change data_LO.xlsx",
                       sheet = "R_LO")
elevation$Elev_Diff[elevation$Elev_Diff == 9999] <- NA # values of 9999 indicate measurement was not possible



# Compile 2021 data -------------------------------------------------------

# Filter out to 2021 only
total.2021 <- total.all %>% 
  filter(Year == "2021")
herb.2021 <- herb.all %>% 
  filter(Year == "2021") |> 
  rename(herb = Cover)
notree.2021 <- notree.all |> 
  filter(Year == "2021") |> 
  rename(notree = Cover)
rich.2021 <- per.div %>% 
  filter(Year == "2021") 
shan.2021 <- per.div %>% 
  filter(Year == "2021")

# Remove some sequencing cols
barc.div <- barc.div |> 
  select(-Name, -Treatment1, -Treatment2)

# Compile variables
dat.2021 <- total.2021 %>% 
  left_join(herb.2021) %>% 
  left_join(notree.2021) |> 
  left_join(soil.chem) %>% 
  left_join(elevation) %>% 
  left_join(rich.2021) %>% 
  left_join(shan.2021) |> 
  left_join(barc.div)
dat.2021 <- dat.2021 |> 
  mutate(CN_ratio = TC_perc / TN_perc) |> 
  rename(Richness.barc = Richness,
         Shannon.barc = Shannon)


# Visualize distribution --------------------------------------------------

# Histogram
hist(dat.2021$Cover, breaks = 10)
hist(dat.2021$herb, breaks = 10)
hist(dat.2021$notree, breaks = 10)
hist(dat.2021$TN_perc, breaks = 10)
hist(dat.2021$TC_perc, breaks = 10)
hist(dat.2021$OM_perc, breaks = 10)
hist(dat.2021$Elev_Diff)
hist(dat.2021$rich)
hist(dat.2021$shan, breaks = 10)
hist(dat.2021$Richness.barc)
hist(dat.2021$Shannon.barc)

# Boxplot
dat.2021$Channel <- factor(dat.2021$Channel)

vis.boxplot <- function(dat, y, ylab) {
  ggplot(dat,
         aes(x = Channel,
             y = y,
             fill = Channel)) +
    geom_boxplot(outlier.shape = NA) +
    
    geom_jitter() +
    theme_bw() +
    xlab(NULL) +
    ylab(ylab) +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Dark2")
}

vis.boxplot(dat.2021, dat.2021$Cover, "Total plant cover (%)")
vis.boxplot(dat.2021, dat.2021$herb, "Herbaceous cover (%)")
vis.boxplot(dat.2021, dat.2021$notree, "Grass, forb & shrub cover (%)")
vis.boxplot(dat.2021, dat.2021$TN_perc, "Soil N (%)") # Appears to be outlier
vis.boxplot(dat.2021, dat.2021$TC_perc, "Soil C (%)") # Appears to be outlier
vis.boxplot(dat.2021, dat.2021$OM_perc, "Organic matter (%)")
vis.boxplot(dat.2021, dat.2021$Elev_Diff, "Elevation difference, 2011-2019 (m)")
vis.boxplot(dat.2021, dat.2021$rich, "Perennial plant richness")
vis.boxplot(dat.2021, dat.2021$shan, "Perennial plant Shannon diversity")
vis.boxplot(dat.2021, dat.2021$Richness.barc, "Bacteria & archea richness")
vis.boxplot(dat.2021, dat.2021$Shannon.barc, "Bacteria & archea Shannon diversity")

# Quantile-quantile plots
qqPlot(dat.2021$Cover)
qqPlot(dat.2021$herb)
qqPlot(dat.2021$notree)
qqPlot(dat.2021$TN_perc) # not normal?
qqPlot(dat.2021$TC_perc) # not normal?
qqPlot(dat.2021$CN_ratio)
qqPlot(dat.2021$OM_perc)
qqPlot(dat.2021$Elev_Diff)
qqPlot(dat.2021$rich)
qqPlot(dat.2021$shan)
qqPlot(dat.2021$Richness.barc)
qqPlot(dat.2021$Shannon.barc)


# Log transformation ------------------------------------------------------

dat.2021 <- dat.2021 %>% 
  mutate(TN_log = log(TN_perc * 1000),
         TC_log = log(TC_perc * 1000))

hist(dat.2021$TN_log, breaks = 10)
hist(dat.2021$TC_log, breaks = 10)

qqPlot(dat.2021$TN_log)
qqPlot(dat.2021$TC_log)

# Add TN & TC as ppt
dat.2021 <- dat.2021 |> 
  mutate(TN_ppt = TN_perc * 10,
         TC_ppt = TC_perc * 10)


# Bivariate scatterplot matrix --------------------------------------------

pairs(~ Cover + TN_log + TC_log + OM_perc + Elev_Diff, data = dat.2021)
pairs(~ herb + TN_log + TC_log + OM_perc + Elev_Diff, data = dat.2021)
pairs(~ notree + TN_log + TC_log + OM_perc + Elev_Diff, data = dat.2021)
pairs(~ rich + TN_log + TC_log + OM_perc + Elev_Diff, data = dat.2021)
pairs(~ shan + TN_log + TC_log + OM_perc + Elev_Diff, data = dat.2021)


# Reorder cols ------------------------------------------------------------

dat.2021 <- dat.2021 |> 
  select(Sample, Channel, Station, Treatment1, Treatment2, Treatment3,
         Cover, herb, notree, rich, shan,
         TN_log, TN_perc, TN_ppt, TC_log, TC_perc, TC_ppt, CN_ratio, OM_perc,
         Elev_Diff, Richness.barc, Shannon.barc, NMDS1, NMDS2, betadisper.channel,
         betadisper.treatment1, betadisper.treatment2, betadisper.treatment3)


# Save --------------------------------------------------------------------

write.csv(dat.2021,
          file = "data/cleaned/Data-2021_clean.csv",
          row.names = FALSE)

save.image("RData/Data-screening_2021.RData")
