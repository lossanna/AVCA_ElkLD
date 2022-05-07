library(tidyverse)
library(readxl)
library(caret)
library(performance)
library(ggpubr)

# Load data ---------------------------------------------------------------

total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
woody.all <- read.csv("data/cleaned/Summarised-all_woody-herb-cover.csv")

richness <- read.csv("data/cleaned/All-Nov_perennial-richness.csv")
shannon <- read.csv("data/cleaned/All-Nov_perennial-shannon.csv")

soil.chem <- read_xlsx("data/Excel_LO_edited/Blankinship-soil-chemistry_TN-TC-OM_LO.xlsx", 
                       sheet = "R_LO")

elevation <- read_xlsx("data/Excel_LO_edited/Vegetation monitoring point elevation change data_LO.xlsx",
                       sheet = "R_LO")
elevation$Elev_Diff[elevation$Elev_Diff == 9999] <- NA # values of 9999 indicate measurement was not possible

width <- read_xlsx("data/Excel_raw/Channel-width_LiDAR-GIS.xlsx")
width <- width %>% 
  select(Channel, Station, Average) %>% 
  rename(Width = Average)


# Compile 2021 data -------------------------------------------------------

# Filter out 2021
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
richness.2021 <- richness %>% 
  filter(Year == "2021-11-01")
shannon.2021 <- shannon %>% 
  filter(Year == "2021-11-01")

# Compile variables
soil.chem$Year <- as.character(soil.chem$Year)
dat.2021 <- total.2021 %>% 
  left_join(herb.2021) %>% 
  left_join(wood.2021) %>% 
  left_join(soil.chem) %>% 
  left_join(elevation) %>% 
  left_join(richness.2021) %>% 
  left_join(shannon.2021) %>% 
  left_join(width)


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
    geom_boxplot() +
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
vis.boxplot(dat.2021, dat.2021$TN_perc, "Soil N (%)") # Appear to be outliers
vis.boxplot(dat.2021, dat.2021$TC_perc, "Soil C (%)") # Appear to be outliers
vis.boxplot(dat.2021, dat.2021$OM_perc, "Organic matter (%)")
vis.boxplot(dat.2021, dat.2021$Elev_Diff, "Elevation difference, 2011-2019 (m)")
vis.boxplot(dat.2021, dat.2021$rich, "Perennial plant richness")
vis.boxplot(dat.2021, dat.2021$shan, "Perennial plant Shannon diversity")
vis.boxplot(dat.2021, dat.2021$Width, "Channel width (m)")

# Quantile-quantile plots
ggqqplot(dat.2021$Cover)
ggqqplot(dat.2021$Herbaceous)
ggqqplot(dat.2021$Woody)
ggqqplot(dat.2021$TN_perc) # not normal?
ggqqplot(dat.2021$TC_perc) # not normal?
ggqqplot(dat.2021$OM_perc)
ggqqplot(dat.2021$Elev_Diff)
ggqqplot(dat.2021$rich)
ggqqplot(dat.2021$shan)
ggqqplot(dat.2021$Width) # maybe problematic


# Log transformation ------------------------------------------------------

dat.2021 <- dat.2021 %>% 
  mutate(TN_log = log(TN_perc * 1000),
         TC_log = log(TC_perc * 1000))

hist(dat.2021$TN_log, breaks = 10)
hist(dat.2021$TC_log, breaks = 10)

ggqqplot(dat.2021$TN_log)
ggqqplot(dat.2021$TC_log)


# Bivariate scatterplot matrix --------------------------------------------

pairs(~ Cover + TN_log + TC_log + OM_perc + Elev_Diff + Width, data = dat.2021)
pairs(~ Herbaceous + TN_log + TC_log + OM_perc + Elev_Diff + Width, data = dat.2021)
pairs(~ Woody + TN_log + TC_log + OM_perc + Elev_Diff + Width, data = dat.2021)
pairs(~ rich + TN_log + TC_log + OM_perc + Elev_Diff + Width, data = dat.2021)
pairs(~ shan + TN_log + TC_log + OM_perc + Elev_Diff + Width, data = dat.2021)


# Multiple linear regression ----------------------------------------------

totcover.lm <- lm(Cover ~ TN_log + TC_log + OM_perc + Elev_Diff + Width, 
                  data = dat.2021)
car::vif(totcover.lm) # TN and TC are collinear
check_model(totcover.lm) # maximize Plots window if get "Error in grid.call"

# Drop TC because it is collinear and similar biologically to OM
totcover.lm <- lm(Cover ~ TN_log + OM_perc + Elev_Diff + Width, 
                  data = dat.2021)

check_model(totcover.lm)

summary(totcover.lm)



# Save --------------------------------------------------------------------

save.image("RData-RMarkdown/Data-screening_2021.RData")
