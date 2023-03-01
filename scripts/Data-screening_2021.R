library(tidyverse)
library(readxl)
library(caret)
library(performance)
library(ggpubr)
library(car)

# Load data ---------------------------------------------------------------

total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
woody.all <- read.csv("data/cleaned/Summarised-all_woody-herb-cover.csv")

per.diversity <- read.csv("data/cleaned/Summarised-all_perennial-diversity.csv")

meta <- read.table("data/cleaned/sequencing/bac_arc_diversity.txt",
                   sep = "\t", header = TRUE)

soil.chem <- read_xlsx("data/Excel_LO_edited/Blankinship-soil-chemistry_TN-TC-OM_LO.xlsx", 
                       sheet = "R_LO") |> 
  select(-Year, -Treatment)

elevation <- read_xlsx("data/Excel_LO_edited/Vegetation monitoring point elevation change data_LO.xlsx",
                       sheet = "R_LO")
elevation$Elev_Diff[elevation$Elev_Diff == 9999] <- NA # values of 9999 indicate measurement was not possible

width <- read_xlsx("data/Excel_raw/Channel-width_LiDAR-GIS.xlsx")
width <- width %>% 
  select(Channel, Station, Average) %>% 
  rename(Width = Average)

diff.12.21 <- read.csv("data/cleaned/Difference_2012-2021.csv")
diff.12.21$station.trt <- gsub("One station dam", "One rock dam", diff.12.21$station.trt)


# Compile 2021 data -------------------------------------------------------

# Filter out to 2021 only
total.2021 <- total.all %>% 
  filter(Year == "2021-11-01")
herb.2021 <- woody.all %>% 
  filter(Year == "2021-11-01",
         woody == "Herbaceous") %>% 
  rename(Herbaceous = Cover) %>% 
  select(-woody)
wood.2021 <- woody.all %>% 
  filter(Year == "2021-11-01",
         woody == "Woody") %>% 
  rename(Woody = Cover) %>% 
  select(-woody)
richness.2021 <- per.diversity %>% 
  filter(year.date == "2021-11-01") %>% 
  select(Channel, Station, rich)
shannon.2021 <- per.diversity %>% 
  filter(year.date == "2021-11-01") %>% 
  select(Channel, Station, shan)

# Compile variables
dat.2021 <- total.2021 %>% 
  left_join(herb.2021) %>% 
  left_join(wood.2021) %>% 
  mutate(Woody = replace_na(Woody, 0)) %>% # NA signals woody species were not measured at these stations; hence, cover is 0
  left_join(soil.chem) %>% 
  left_join(elevation) %>% 
  left_join(richness.2021) %>% 
  left_join(shannon.2021) %>% 
  left_join(width) %>% 
  left_join(diff.12.21) # C19 2+4 data sheet from Nov 2012 is missing, so differences are all NA
dat.2021 <- dat.2021 |> 
  mutate(CN_ratio = TC_perc / TN_perc)

# Visualize distribution --------------------------------------------------

# Histogram
hist(dat.2021$Cover, breaks = 10)
hist(dat.2021$Herbaceous)
hist(dat.2021$Woody, breaks = 15)
hist(dat.2021$TN_perc, breaks = 10)
hist(dat.2021$TC_perc, breaks = 10)
hist(dat.2021$OM_perc, breaks = 10)
hist(dat.2021$Elev_Diff)
hist(dat.2021$rich)
hist(dat.2021$shan, breaks = 10)
hist(dat.2021$Width, breaks = 15)


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
vis.boxplot(dat.2021, dat.2021$Herbaceous, "Herbaceous cover (%)")
vis.boxplot(dat.2021, dat.2021$Woody, "Woody cover (%)")
vis.boxplot(dat.2021, dat.2021$TN_perc, "Soil N (%)") # Appears to be outlier
vis.boxplot(dat.2021, dat.2021$TC_perc, "Soil C (%)") # Appears to be outlier
vis.boxplot(dat.2021, dat.2021$OM_perc, "Organic matter (%)")
vis.boxplot(dat.2021, dat.2021$Elev_Diff, "Elevation difference, 2011-2019 (m)")
vis.boxplot(dat.2021, dat.2021$rich, "Perennial plant richness")
vis.boxplot(dat.2021, dat.2021$shan, "Perennial plant Shannon diversity")
vis.boxplot(dat.2021, dat.2021$Width, "Channel width (m)")
vis.boxplot(dat.2021, dat.2021$total.diff, "Difference in total cover (%)")

# Quantile-quantile plots
qqPlot(dat.2021$Cover)
qqPlot(dat.2021$Herbaceous)
qqPlot(dat.2021$Woody)
qqPlot(dat.2021$TN_perc) # not normal?
qqPlot(dat.2021$TC_perc) # not normal?
qqPlot(dat.2021$CN_ratio)
qqPlot(dat.2021$OM_perc)
qqPlot(dat.2021$Elev_Diff)
qqPlot(dat.2021$rich)
qqPlot(dat.2021$shan)
qqPlot(dat.2021$Width) # maybe problematic
qqPlot(dat.2021$total.diff)


# Log transformation ------------------------------------------------------

dat.2021 <- dat.2021 %>% 
  mutate(TN_log = log(TN_perc * 1000),
         TC_log = log(TC_perc * 1000))

hist(dat.2021$TN_log, breaks = 10)
hist(dat.2021$TC_log, breaks = 10)

qqPlot(dat.2021$TN_log)
qqPlot(dat.2021$TC_log)

dat.2021.sem <- dat.2021


# Bivariate scatterplot matrix --------------------------------------------

pairs(~ Cover + TN_log + TC_log + OM_perc + Elev_Diff + Width, data = dat.2021)
pairs(~ Herbaceous + TN_log + TC_log + OM_perc + Elev_Diff + Width, data = dat.2021)
pairs(~ Woody + TN_log + TC_log + OM_perc + Elev_Diff + Width, data = dat.2021)
pairs(~ rich + TN_log + TC_log + OM_perc + Elev_Diff + Width, data = dat.2021)
pairs(~ shan + TN_log + TC_log + OM_perc + Elev_Diff + Width, data = dat.2021)


# Add treatments and rename cols ------------------------------------------

dat.2021$Name <- paste0(dat.2021$Channel, ", ", dat.2021$Station)
dat.2021 <- left_join(dat.2021, meta)

dat.2021 <- dat.2021 %>% 
  rename(Richness.barc = Richness,
         Shannon.barc = Shannon) %>% 
  mutate(TN_ppt = TN_perc * 10,
         TC_ppt = TC_perc * 10) |> 
  mutate(Treatment3 = case_when(
    Treatment2 == "No treatment" ~ "Control",
    Treatment2 == "Upland treatment" ~ "Control",
    Treatment2 == "In-channel treatment" ~ "Treated"))

dat.2021 <- dat.2021 |> 
  select(PlotTimeID, Sample, Name, Channel, Station, Treatment1, Treatment2, Treatment3,
         Cover, Herbaceous, Woody, rich, shan,
         TN_perc, TN_log, TN_ppt, TC_perc, TC_log, TC_ppt, CN_ratio, OM_perc,
         Elev_Diff, Width,
         Richness.barc, Shannon.barc, NMDS1, NMDS2, betadisper.channel,
         betadisper.treatment1, betadisper.treatment2, betadisper.treatment3)


# Save --------------------------------------------------------------------

write.csv(dat.2021,
          file = "data/cleaned/Data-2021_clean.csv",
          row.names = FALSE)

save.image("RData/Data-screening_2021.RData")
 



# Old analysis ------------------------------------------------------------

dat.2021.sem <- dat.2021.sem |> 
  select(-CN_ratio)

# Multiple linear regression 
totcover.lm <- lm(Cover ~ TN_log + TC_log + OM_perc + Elev_Diff + Width, 
                  data = dat.2021)
car::vif(totcover.lm) # TN and TC are collinear
check_model(totcover.lm) # maximize Plots window if get "Error in grid.call"

# Drop TC because it is collinear and similar biologically to OM
totcover.lm <- lm(Cover ~ TN_log + OM_perc + Elev_Diff + Width, 
                  data = dat.2021)

check_model(totcover.lm)

summary(totcover.lm)

# write.csv(dat.2021,
#           file = "data/cleaned/SEM-input.csv",
#           row.names = FALSE) 
  # do not overwrite CSV because I don't think I can currently recreate it because of problems with the Difference_2012-2021 script and data