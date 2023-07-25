# Purpose: Data screening for 2021 plant and soil data, and write cleaned data table.

# Checked for normality, visualized distribution, checked collinearity:
# Determined all were normally distributed except TN_perc, TC_perc, OM_perc,
#   chemoheterotrophy, n-cyclers. All of these were were log-transformed.
# Determined that TN, TC, OM and C:N ratio are collinear (especially TN & TC).
#   As a result, will only use TN & OM in analysis.
# Also found that bacteria/archaea richness & shannon are kind of correlated,
#   so will just use richness for both barc and fungi (Shannon is kind of non-normal).

# Produced a clean data sheet for 2021 data (Data-2021_clean.csv).
#  This is also to be used for SEM 2.0 input.
# Created: 2022-04-18
# Last updated: 2023-07-25

library(tidyverse)
library(readxl)
library(caret)
library(performance)
library(ggpubr)
library(car)
library(Hmisc)

# Load data ---------------------------------------------------------------

total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
herb.all <- read.csv("data/cleaned/Summarised-all_herb-cover.csv")
notree.all <- read.csv("data/cleaned/Summarised-all_notree-cover.csv")
tree.all <- read.csv("data/cleaned/Summarised-all_tree-cover.csv")
per.div <- read.csv("data/cleaned/Summarised-all_perennial-diversity.csv")

barc.div <- read.table("data/cleaned/sequencing/bac-arc_diversity.txt",
                     sep = "\t", header = TRUE)
fungi.div <- read.table("data/cleaned/sequencing/fungi_diversity.txt",
                        sep = "\t", header = TRUE)
soil.chem <- read_xlsx("data/Excel_LO_edited/Blankinship-soil-chemistry_TN-TC-OM_LO.xlsx", 
                       sheet = "R_LO")
elev <- read_csv("data/cleaned/Cross-section-elevation_clean.csv")
fapro <- read.csv("data/cleaned/sequencing/faprotax-proportions_clean.csv")
funguild.trophic <- read.csv("data/cleaned/sequencing/FUNGuild-proportions-trophic_clean.csv")


# Compile 2021 data -------------------------------------------------------

# Filter out to 2021 only from temporal veg data
total.2021 <- total.all %>% 
  filter(Year == "2021") |> 
  rename(total = Cover)
herb.2021 <- herb.all %>% 
  filter(Year == "2021") |> 
  rename(herb = Cover)
notree.2021 <- notree.all |> 
  filter(Year == "2021") |> 
  rename(notree = Cover)
perdiv.2021 <- per.div %>% 
  filter(Year == "2021") |> 
  rename(perveg.richness = rich,
         perveg.shannon = shan)

# Filter out other variables for SEM input
notree.2018 <- notree.all |> 
  filter(Year == "2018") |> 
  rename(notree.18 = Cover)

tree.avg1214 <- tree.all |> 
  filter(Year %in% c("2012", "2013", "2014")) |> 
  group_by(Sample, Channel, Station, Treatment3) |> 
  summarise(tree = mean(Cover),
            .groups = "keep")

herb.2018 <- herb.all |> 
  filter(Year == "2018") |> 
  rename(herb.18 = Cover)

# Compile variables
dat.2021 <- total.2021 %>% 
  left_join(herb.2021) %>% 
  select(-PlotTimeID, -Year, -year.xaxis) |> # remove year cols; left_join() will not work in joining 2018 values
  left_join(herb.2018) |> 
  select(-PlotTimeID, -Year, -year.xaxis) |> 
  left_join(notree.2021) |> 
  select(-PlotTimeID, -Year, -year.xaxis) |> 
  left_join(notree.2018) |> 
  select(-PlotTimeID, -Year, -year.xaxis) |> 
  left_join(tree.avg1214) |> 
  left_join(perdiv.2021) |> 
  select(-PlotTimeID, -Year, -year.xaxis, -station.trt, -channel.trt, -Treatment1, -Treatment2) |> 
  left_join(soil.chem) %>% 
  mutate(
    CN_ratio = TC_perc / TN_perc) |> 
  left_join(barc.div) |> 
  rename(barc.richness = Richness,
         barc.shannon = Shannon,
         barc.betadisp.chan = betadisper.channel,
         barc.betadisp.3 = betadisper.treatment3,
         barc.NMDS1 = NMDS1,
         barc.NMDS2 = NMDS2) |> 
  left_join(fungi.div) |> 
  rename(fungi.richness = Richness,
         fungi.shannon = Shannon,
         fungi.betadisp.chan = betadisper.channel,
         fungi.betadisp.3 = betadisper.treatment3,
         fungi.NMDS1 = NMDS1,
         fungi.NMDS2 = NMDS2) |> 
  arrange(Sample) |> 
  mutate(chemoheterotrophy = fapro$chemoheterotrophy,
         n.cycler = fapro$n_cyclers,
         saprotroph = funguild.trophic$Saprotroph) |> 
  left_join(elev) |> 
  select(Sample, Name, Channel, Station, Treatment3,
         total, herb, herb.18, notree, notree.18, tree, perveg.richness, perveg.shannon,
         TN_perc, TC_perc, CN_ratio, OM_perc,
         barc.richness, barc.shannon, barc.NMDS1, barc.NMDS2, barc.betadisp.3, 
         fungi.richness, fungi.shannon, fungi.NMDS1, fungi.NMDS2, fungi.betadisp.3, 
         chemoheterotrophy, n.cycler, saprotroph, dElev, dElev_corrected)



# Visualize distribution --------------------------------------------------

# Histogram
hist(dat.2021$total, breaks = 10)
hist(dat.2021$herb, breaks = 10)
hist(dat.2021$herb.18, breaks = 10)
hist(dat.2021$notree, breaks = 15)
hist(dat.2021$notree.18, breaks = 15)
hist(dat.2021$tree, breaks = 15) # not normal
hist(dat.2021$perveg.richness)
hist(dat.2021$perveg.shannon, breaks = 10)
hist(dat.2021$TN_perc, breaks = 10) # not normal
hist(dat.2021$TC_perc, breaks = 10) # not normal
hist(dat.2021$CN_ratio)
hist(dat.2021$OM_perc, breaks = 10)
hist(dat.2021$barc.richness)
hist(dat.2021$barc.shannon)
hist(dat.2021$fungi.richness, breaks = 10)
hist(dat.2021$fungi.shannon, breaks = 10)
hist(dat.2021$chemoheterotrophy, breaks = 15)
hist(dat.2021$n.cycler, breaks = 10)
hist(dat.2021$saprotroph)
hist(elev$dElev, breaks = 10) # not normal - right tail skew?
hist(elev$dElev_corrected, breaks = 10)

# Boxplot
vis.boxplot <- function(dat, y, ylab) {
  ggplot(dat,
         aes(x = Treatment3,
             y = y)) +
    geom_boxplot(outlier.shape = NA,
                 alpha = 0.3,
                 aes(fill = Treatment3)) +
    geom_jitter(alpha = 0.8,
                size = 3,
                aes(color = Treatment3)) +
    theme_bw() +
    xlab(NULL) +
    ylab(ylab) +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("red", "#1F78B4")) +
    scale_color_manual(values = c("red", "#1F78B4"))
}

vis.boxplot(dat.2021, dat.2021$total, "Total plant cover (%)")
vis.boxplot(dat.2021, dat.2021$herb, "Herbaceous cover (%)")
vis.boxplot(dat.2021, dat.2021$herb.18, "Herbaceous cover, 2018 (%)")
vis.boxplot(dat.2021, dat.2021$notree, "Grass, forb & shrub cover (%)")
vis.boxplot(dat.2021, dat.2021$notree.18, "Grass, forb & shrub cover, 2018 (%)")
vis.boxplot(dat.2021, dat.2021$tree, "Tree cover, 2012-2014 (%)")
vis.boxplot(dat.2021, dat.2021$perveg.richness, "Perennial plant richness")
vis.boxplot(dat.2021, dat.2021$perveg.shannon, "Perennial plant Shannon diversity")
vis.boxplot(dat.2021, dat.2021$TN_perc, "Soil N (%)") # Appears to be outlier, but I checked it and nothing is obviously wrong
vis.boxplot(dat.2021, dat.2021$TC_perc, "Soil C (%)") # Appears to be outlier, but I checked it and nothing is obviously wrong
vis.boxplot(dat.2021, dat.2021$CN_ratio, "C:N ratio")
vis.boxplot(dat.2021, dat.2021$OM_perc, "Organic matter (%)")
vis.boxplot(dat.2021, dat.2021$barc.richness, "Bacteria & archea richness")
vis.boxplot(dat.2021, dat.2021$barc.shannon, "Bacteria & archea Shannon diversity") # appears to be low outlier
vis.boxplot(dat.2021, dat.2021$fungi.richness, "Fungi richness") 
vis.boxplot(dat.2021, dat.2021$fungi.shannon, "Fungi Shannon diversity") # appears to be low outlier
vis.boxplot(dat.2021, dat.2021$chemoheterotrophy, "Chemoheterotrophs (%)")
vis.boxplot(dat.2021, dat.2021$n.cycler, "N-cycling bacteria & archea (%)") # appears to be an outlier
vis.boxplot(dat.2021, dat.2021$saprotroph, "Fungi saprotrophs (%)")
vis.boxplot(dat.2021, dat.2021$dElev_corrected, "Elevation difference, 2011-2019 (m)")

# Quantile-quantile plots
qqPlot(dat.2021$total)
qqPlot(dat.2021$herb)
qqPlot(dat.2021$herb.18)
qqPlot(dat.2021$notree)
qqPlot(dat.2021$notree.18)
qqPlot(dat.2021$tree) # almost normal?
qqPlot(dat.2021$perveg.richness)
qqPlot(dat.2021$perveg.shannon)
qqPlot(dat.2021$TN_perc) # not normal
qqPlot(dat.2021$TC_perc) # not normal
qqPlot(dat.2021$CN_ratio)
qqPlot(dat.2021$OM_perc) # outlier?
qqPlot(dat.2021$barc.richness)
qqPlot(dat.2021$barc.shannon) # not normal?
qqPlot(dat.2021$fungi.richness)
qqPlot(dat.2021$fungi.shannon) # not normal?
qqPlot(dat.2021$barc.richness)
qqPlot(dat.2021$chemoheterotrophy) # possibly not normal?
qqPlot(dat.2021$n.cycler) # outlier?
qqPlot(dat.2021$saprotroph)
qqPlot(dat.2021$dElev) # outliers
qqPlot(dat.2021$dElev_corrected) # cannot be log-transformed because of 0 values


# Log transformation ------------------------------------------------------

# TN & TC
dat.2021 <- dat.2021 %>% 
  mutate(TN_log = log(TN_perc),
         TC_log = log(TC_perc))

hist(dat.2021$TN_log, breaks = 10)
hist(dat.2021$TC_log, breaks = 10)

qqPlot(dat.2021$TN_log) # log-transformation has fixed normality issue
qqPlot(dat.2021$TC_log) # log-transformation has fixed normality issue

# Add TN & TC as ppt
dat.2021 <- dat.2021 |> 
  mutate(TN_ppt = TN_perc * 10,
         TC_ppt = TC_perc * 10)


# OM
qqPlot(log(dat.2021$OM_perc)) # log-transformation has fixed outlier
dat.2021 <- dat.2021 |> 
  mutate(OM_log = log(OM_perc))

# Barc & fungi Shannon
qqPlot(log(dat.2021$barc.shannon)) # log-transformation does not help
qqPlot(log(dat.2021$fungi.shannon)) # log-transformation does not help

# Chemoheterotrophs & N-cyclers
qqPlot(log(dat.2021$chemoheterotrophy)) # log-transformation helps a little
qqPlot(log(dat.2021$n.cycler)) # log-transformation has fixed outlier
dat.2021 <- dat.2021 |> 
  mutate(chemoheterotrophy_log = log(chemoheterotrophy),
         n.cycler_log = log(n.cycler)) |> 
  rename(chemoheterotrophy_perc = chemoheterotrophy,
         n.cycler_perc = n.cycler)

# dElev
qqPlot(log(dat.2021$dElev)) # log-transformation does not really help


# Variables that have been log-transformed: 
#   TN, TC, OM, chemoheterotrophs, n-cyclers



# Bivariate scatterplot matrix --------------------------------------------

# All, notree
pairs(~ notree + notree.18 + tree + perveg.richness + perveg.shannon + TN_log + 
        TC_log + OM_log + CN_ratio + barc.richness + barc.shannon +
        fungi.richness + fungi.shannon + chemoheterotrophy_log + n.cycler_log, data = dat.2021)

# All, herb
pairs(~ herb + herb.18 + tree + perveg.richness + perveg.shannon + TN_log + 
        TC_log + OM_log + CN_ratio + barc.richness + barc.shannon +
        fungi.richness + fungi.shannon + chemoheterotrophy_log + n.cycler_log, data = dat.2021)


# TN, TC, OM
pairs(~ TN_log + TC_log + OM_log + CN_ratio, data = dat.2021) 
#   TN & TC highly correlated; TC & OM kind of correlated; CN & TC/TN kind of correlated
#   conclusion: drop TC from analysis, or just use CN_ratio
#     but TN better relates to N-cycling

# TN, N-cyclers
pairs(~ TN_log + n.cycler_log, data = dat.2021) 

# Bacteria, archaea & fungi richness and Shannon
pairs(~ barc.richness + barc.shannon +
        fungi.richness + fungi.shannon, data = dat.2021)
#   barc richness & shannon pretty correlated
#   conclusion: just use barc richness, because barc shannon was possibly not normal
#     to be even, probably drop fungi shannon as well



# Correlation table -------------------------------------------------------

# All data
corr.dat <- dat.2021 |> 
  select(notree, notree.18, tree, perveg.richness, perveg.shannon, TN_log, TC_log, CN_ratio, OM_log,
         barc.richness, fungi.richness, chemoheterotrophy_log, n.cycler_log, saprotroph)
corr.all <- rcorr(as.matrix(corr.dat))[["r"]]

# Treated
corr.dat.trt <- dat.2021 |> 
  filter(Treatment3 == "Treated") |> 
  select(notree, notree.18, tree, perveg.richness, perveg.shannon, TN_log, TC_log, CN_ratio, OM_log,
         barc.richness, fungi.richness, chemoheterotrophy_log, n.cycler_log, saprotroph)
corr.trt <- rcorr(as.matrix(corr.dat.trt))[["r"]]

# Control
corr.dat.ctrl <- dat.2021 |> 
  filter(Treatment3 == "Control") |> 
  select(notree, notree.18, tree, perveg.richness, perveg.shannon, TN_log, TC_log, CN_ratio, OM_log,
         barc.richness, fungi.richness, chemoheterotrophy_log, n.cycler_log, saprotroph)
corr.ctrl <- rcorr(as.matrix(corr.dat.ctrl))[["r"]]


# Reorder cols ------------------------------------------------------------

dat.2021 <- dat.2021 |> 
  select(Sample, Name, Channel, Station, Treatment3,
         total, herb, herb.18, notree, notree.18, tree, perveg.richness, perveg.shannon,
         TN_perc, TN_ppt, TN_log, TC_perc, TC_ppt, TC_log, CN_ratio, OM_perc, OM_log,
         barc.richness, barc.shannon, barc.NMDS1, barc.NMDS2, barc.betadisp.3, 
         fungi.richness, fungi.shannon, fungi.NMDS1, fungi.NMDS2, fungi.betadisp.3, 
         chemoheterotrophy_perc, chemoheterotrophy_log, n.cycler_perc, n.cycler_log, 
         saprotroph, dElev, dElev_corrected)



# Save --------------------------------------------------------------------

write.csv(dat.2021,
          file = "data/cleaned/Data-2021_clean.csv",
          row.names = FALSE)

save.image("RData/Data-screening_2021.RData")
