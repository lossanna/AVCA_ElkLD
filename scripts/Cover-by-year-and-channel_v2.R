library(tidyverse)
library(agricolae)
library(plotrix)

# Load data ---------------------------------------------------------------

plant.all <- read.csv("data/cleaned/Summarised-all_plant-species-cover.csv")
ground.all <- read.csv("data/cleaned/Summarised-all_ground-cover.csv")
total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
fungr.all <- read.csv("data/cleaned/Summarised-all_functional-group-cover.csv")
gfst.all <- read.csv("data/cleaned/Summarised-all_grass-forb-shrub-tree-cover.csv")
woody.all <- read.csv("data/cleaned/Summarised-all_woody-herb-cover.csv")
inwood.all <- read.csv("data/cleaned/Summarised-all_invasive-woody-cover.csv")
ingfst.all <- read.csv("data/cleaned/Summarised-all_invasive-grassforbshrubtree-cover.csv")
innat.all <- read.csv("data/cleaned/Summarised-all_invasive-native-cover.csv")


# Functions ---------------------------------------------------------------

# Add year as date and character, and retain Nov samples only
year <- function(x) {
  x <- x %>% 
    mutate(year.date = as.Date(x$Year))
  
  x[ , "year.xaxis"] <- NA
  for(i in 1:nrow(x)) {
    if(x$Year[i] == "2012-11-01") {
      x$year.xaxis[i] <- "2012-01-01"
    } else if(x$Year[i] == "2013-11-01") {
      x$year.xaxis[i] <- "2013-01-01"
    } else if(x$Year[i] == "2014-11-01") {
      x$year.xaxis[i] <- "2014-01-01"
    } else if(x$Year[i] == "2015-11-01") {
      x$year.xaxis[i] <- "2015-01-01"
    } else if(x$Year[i] == "2018-11-01") {
      x$year.xaxis[i] <- "2018-01-01"
    } else if(x$Year[i] == "2021-11-01") {
      x$year.xaxis[i] <- "2021-01-01"
    } else {
      x$year.xaxis[i] <- "2012-03-01"
    }
  }
  x$year.xaxis <- as.Date(x$year.xaxis)
  
  x <- x %>% 
    filter(year.xaxis != "2012-03-01") 
  x$Year <- as.factor(gsub("-.*", "", x$Year))
    
  return(x)
}


# Data wrangling ----------------------------------------------------------

plant.all <- year(plant.all)
ground.all <- year(ground.all)
total.all <- year(total.all)
fungr.all <- year(fungr.all)
gfst.all <- year(gfst.all)
woody.all <- year(woody.all)
inwood.all <- year(inwood.all)
ingfst.all <- year(ingfst.all)
innat.all <- year(innat.all)


# Total plant cover -------------------------------------------------------

# Find channel averages by year
total.channel <- total.all %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# Plot
total.plot <- ggplot(total.channel, aes(x = year.xaxis, y = mean, 
                                                group = channel.trt, 
                                                color = channel.trt)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
total.plot


# ANOVA C12
summary(aov(Cover ~ Year, data = filter(total.all, Channel == "Channel 12"))) # NS

# ANOVA C13
summary(aov(Cover ~ Year, data = filter(total.all, Channel == "Channel 13")))
total13 <- total.all %>% 
  filter(Channel == "Channel 13")
anova.total13 <- aov(total13$Cover ~ total13$Year)
hsd.total13 <- HSD.test(anova.total13, trt = "total13$Year")
hsd.total13
total13.letters <- hsd.total13$groups
total13.letters <- total13.letters %>% 
  mutate(Year = rownames(total13.letters)) %>% 
  arrange(Year)

# ANOVA C19
summary(aov(Cover ~ Year, data = filter(total.all, Channel == "Channel 19")))
total19 <- total.all %>% 
  filter(Channel == "Channel 19")
anova.total19 <- aov(total19$Cover ~ total19$Year)
hsd.total19 <- HSD.test(anova.total19, trt = "total19$Year")
hsd.total19
total19.letters <- hsd.total19$groups
total19.letters <- total19.letters %>% 
  mutate(Year = rownames(total19.letters)) %>% 
  arrange(Year)

# ANOVA C21
summary(aov(Cover ~ Year, data = filter(total.all, Channel == "Channel 21"))) # NS


# Plot with letters added
letters <- data.frame(label = c(total13.letters$groups,
                                total19.letters$groups),
                      channel.trt = c(rep("Channel 13: In-channel treatment", 6),
                                      rep("Channel 19: Upland treatment", 6)),
                      x = rep(total.channel$year.xaxis[1:6], 2),
                      y = c(70, 60, 70, 65, 60, 55,
                            50, 45, 48, 43, 55, 60))

anova.lab <- data.frame(label = rep("ANOVA", 2),
                           channel.trt = c("Channel 13: In-channel treatment",
                                           "Channel 19: Upland treatment"),
                           x = c(rep(as.Date("2020-01-01"), 2)),
                           y = c(23, 23))

total.plot.letters <- ggplot(total.channel, aes(x = year.xaxis, y = mean, 
                                                    group = channel.trt, 
                                                    color = channel.trt)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  geom_text(data = anova.lab,
            mapping = aes(x = x, y = y, label = label),
            size = 3, color = "black")
total.plot.letters

pdf("output_figs/Total-cover.pdf", width = 7, height = 5)
total.plot.letters
dev.off()



# Ground cover ------------------------------------------------------------

# Find channel averages by year
ground.channel <- ground.all %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, Common) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# Plot only rock, gravel, and soil
ground.channel.grs <- ground.channel %>% 
  filter(Common %in% c("Gravel", "Rock", "Soil"))

ground.grs.plot <- ggplot(ground.channel.grs, 
                              aes(x = year.xaxis, y = mean,
                                  group = Common, color = Common, shape = Common)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Gravel, rock, and soil cover") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ground.grs.plot

# Plot soil only
ground.channel.soil <- ground.channel.grs %>% 
  filter(Common == "Soil")

ground.soil.plot <- ggplot(ground.channel.soil, 
                           aes(x = year.xaxis, y = mean, 
                           group = channel.trt, color = channel.trt)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Soil cover") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
ground.soil.plot

# Soil for ANOVA
soil <- ground.all %>% 
  filter(Common == "Soil")

# ANOVA soil C12
summary(aov(Cover ~ Year, data = filter(soil, Channel == "Channel 12")))
soil12 <- soil %>% 
  filter(Channel == "Channel 12")
anova.soil12 <- aov(soil12$Cover ~ soil12$Year)
hsd.soil12 <- HSD.test(anova.soil12, trt = "soil12$Year")
hsd.soil12
soil12.letters <- hsd.soil12$groups
soil12.letters <- soil12.letters %>% 
  mutate(Year = rownames(soil12.letters)) %>% 
  arrange(Year)

# ANOVA soil C13
summary(aov(Cover ~ Year, data = filter(soil, Channel == "Channel 13")))
soil13 <- soil %>% 
  filter(Channel == "Channel 13")
anova.soil13 <- aov(soil13$Cover ~ soil13$Year)
hsd.soil13 <- HSD.test(anova.soil13, trt = "soil13$Year")
hsd.soil13
soil13.letters <- hsd.soil13$groups
soil13.letters <- soil13.letters %>% 
  mutate(Year = rownames(soil13.letters)) %>% 
  arrange(Year)

# ANOVA soil C19
summary(aov(Cover ~ Year, data = filter(soil, Channel == "Channel 19")))
soil19 <- soil %>% 
  filter(Channel == "Channel 19")
anova.soil19 <- aov(soil19$Cover ~ soil19$Year)
hsd.soil19 <- HSD.test(anova.soil19, trt = "soil19$Year")
hsd.soil19
soil19.letters <- hsd.soil19$groups
soil19.letters <- soil19.letters %>% 
  mutate(Year = rownames(soil19.letters)) %>% 
  arrange(Year)

# ANOVA soil C21
summary(aov(Cover ~ Year, data = filter(soil, Channel == "Channel 21")))
soil21 <- soil %>% 
  filter(Channel == "Channel 21")
anova.soil21 <- aov(soil21$Cover ~ soil21$Year)
hsd.soil21 <- HSD.test(anova.soil21, trt = "soil21$Year")
hsd.soil21
soil21.letters <- hsd.soil21$groups
soil21.letters <- soil21.letters %>% 
  mutate(Year = rownames(soil21.letters)) %>% 
  arrange(Year)

# Plot soil only with letters added
letters <- data.frame(label = c(soil12.letters$groups, 
                                soil13.letters$groups,
                                soil19.letters$groups,
                                soil21.letters$groups),
                      channel.trt = c(rep("Channel 12: No treatment", 6),
                                      rep("Channel 13: In-channel treatment", 6),
                                      rep("Channel 19: Upland treatment", 6),
                                      rep("Channel 21: In-channel treatment", 6)),
                      x = rep(ground.channel.soil$year.xaxis[1:6], 4),
                      y = c(40, 52, 42, 60, 40, 53,
                            65, 61, 57, 40, 32, 35,
                            50, 55, 40, 63, 50, 40,
                            53, 59, 50, 47, 53, 25))

anova.lab <- data.frame(label = c(rep("ANOVA", 4)),
                        channel.trt = c("Channel 12: No treatment",
                                        "Channel 13: In-channel treatment",
                                        "Channel 19: Upland treatment",
                                         "Channel 21: In-channel treatment"),
                        x = c(rep(as.Date("2020-01-01"), 4)),
                        y = c(65, 65, 65, 65))

ground.soil.plot.letters <- ggplot(ground.channel.soil, 
                                       aes(x = year.xaxis, y = mean, 
                                           group = channel.trt, color = channel.trt)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Soil cover") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  geom_text(data = anova.lab,
            mapping = aes(x = x, y = y, label = label),
            size = 3, color = "black")
ground.soil.plot.letters

pdf("output_figs/Soil-cover.pdf", width = 7, height = 5)
ground.soil.plot.letters
dev.off()



# Functional group (gfst) -------------------------------------------------

# Find channel averages by year
gfst.channel <- gfst.all %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, gfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# Plot
gfst.channel$gfst <- factor(gfst.channel$gfst, 
                                levels = c("Grass", "Forb", "Shrub", "Tree"))

gfst.plot <- ggplot(gfst.channel, aes(x = year.xaxis, y = mean, 
                                              group = gfst, color = gfst, shape = gfst)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE),
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Grass, forb, shrub, and tree cover") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
gfst.plot



# Functional group (gfs) --------------------------------------------------

# Find channel averages by year
gfs.channel <- gfst.all %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, gfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(gfst != "Tree")

# Plot by accurate date
gfs.channel$gfst <- factor(gfs.channel$gfst, 
                               levels = c("Grass", "Forb", "Shrub"))

gfs.plot <- ggplot(gfs.channel, aes(x = year.xaxis, y = mean, 
                                            group = gfst, color = gfst, shape = gfst)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Grass, forb, and shrub cover") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
gfs.plot



# Woody/herbaceous --------------------------------------------------------

# Find channel averages by year
woody.channel <- woody.all %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, woody) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# Plot
woody.plot <- ggplot(woody.channel, aes(x = year.xaxis, y = mean, 
                                                group = woody, color = woody, shape = woody)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Woody and herbaceous cover") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
woody.plot

# Herbaceous for ANOVA
herb <- woody.all %>% 
  filter(woody == "Herbaceous")

# ANOVA herbaceous C12
summary(aov(Cover ~ Year, data = filter(herb, Channel == "Channel 12")))
herb12 <- herb %>% 
  filter(Channel == "Channel 12")
anova.herb12 <- aov(herb12$Cover ~ herb12$Year)
hsd.herb12 <- HSD.test(anova.herb12, trt = "herb12$Year")
hsd.herb12
herb12.letters <- hsd.herb12$groups
herb12.letters <- herb12.letters %>% 
  mutate(Year = rownames(herb12.letters)) %>% 
  arrange(Year)

# ANOVA herbaceous C13
summary(aov(Cover ~ Year, data = filter(herb, Channel == "Channel 13")))
herb13 <- herb %>% 
  filter(Channel == "Channel 13")
anova.herb13 <- aov(herb13$Cover ~ herb13$Year)
hsd.herb13 <- HSD.test(anova.herb13, trt = "herb13$Year")
hsd.herb13
herb13.letters <- hsd.herb13$groups
herb13.letters <- herb13.letters %>% 
  mutate(Year = rownames(herb13.letters)) %>% 
  arrange(Year)

# ANOVA herbaceous C19
summary(aov(Cover ~ Year, data = filter(herb, Channel == "Channel 19")))
herb19 <- herb %>% 
  filter(Channel == "Channel 19")
anova.herb19 <- aov(herb19$Cover ~ herb19$Year)
hsd.herb19 <- HSD.test(anova.herb19, trt = "herb19$Year")
hsd.herb19
herb19.letters <- hsd.herb19$groups
herb19.letters <- herb19.letters %>% 
  mutate(Year = rownames(herb19.letters)) %>% 
  arrange(Year)

# ANOVA herbaceous C21
summary(aov(Cover ~ Year, data = filter(herb, Channel == "Channel 21")))
herb21 <- herb %>% 
  filter(Channel == "Channel 21")
anova.herb21 <- aov(herb21$Cover ~ herb21$Year)
hsd.herb21 <- HSD.test(anova.herb21, trt = "herb21$Year")
hsd.herb21
herb21.letters <- hsd.herb21$groups
herb21.letters <- herb21.letters %>% 
  mutate(Year = rownames(herb21.letters)) %>% 
  arrange(Year)


# Herbaceous (plots) ------------------------------------------------------

# Find channel averages by year
herb.channel <- woody.all %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, woody) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(woody == "Herbaceous") 

# Plot
herb.plot <- ggplot(herb.channel, aes(x = year.xaxis, y = mean, 
                                              group = channel.trt, color = channel.trt)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous cover") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~channel.trt)
herb.plot

# Plot with letters added
letters <- data.frame(label = c(herb12.letters$groups, 
                                herb13.letters$groups,
                                herb19.letters$groups,
                                herb21.letters$groups),
                      channel.trt = c(rep("Channel 12: No treatment", 6),
                                      rep("Channel 13: In-channel treatment", 6),
                                      rep("Channel 19: Upland treatment", 6),
                                      rep("Channel 21: In-channel treatment", 6)),
                      x = rep(ground.channel.soil$year.xaxis[1:6], 4),
                      y = c(20, 25, 15, 25, 30, 23,
                            20, 15, 22, 20, 38, 33,
                            20, 12, 25, 8, 13, 28,
                            15, 13, 23, 18, 25, 25))

anova.lab <- data.frame(label = c(rep("ANOVA", 4)),
                           channel.trt = c("Channel 12: No treatment",
                                           "Channel 13: In-channel treatment",
                                           "Channel 19: Upland treatment",
                                           "Channel 21: In-channel treatment"),
                           x = c(rep(as.Date("2020-01-01"), 4)),
                           y = c(8, 8, 8, 8))

herb.plot.letters <- ggplot(herb.channel, aes(x = year.xaxis, y = mean, 
                                              group = channel.trt, color = channel.trt)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous cover") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~channel.trt) +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  geom_text(data = anova.lab,
            mapping = aes(x = x, y = y, label = label),
            size = 3, color = "black")
herb.plot.letters

pdf("output_figs/Herbaceous-cover.pdf", width = 7, height = 5)
herb.plot.letters
dev.off()



# Invasive/native ---------------------------------------------------------

# Find channel averages by year
innat.channel <- innat.all %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, Native) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# Plot 
innat.plot <- ggplot(innat.channel, aes(x = year.date, y = mean, 
                                        group = Native, color = Native, shape = Native)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive/native") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
innat.plot



# Invasive/native and woody/herbaceous ------------------------------------

# Find channel averages by year
inwood.channel <- inwood.all %>%  
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, inwood) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# Plot
inwood.plot <- ggplot(inwood.channel, aes(x = year.date, y = mean, 
                                          group = inwood, color = inwood, shape = inwood)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive and woody status") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
inwood.plot

# Plot only known native status
inwood.channel.known <- inwood.channel %>% 
  filter(inwood %in% c("Invasive herb", "Native herb", "Native woody"))

inwood.known.plot <- ggplot(inwood.channel.known, aes(x = year.date, y = mean, 
                                                          group = inwood, color = inwood, shape = inwood)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive and woody status") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
inwood.known.plot


# Native/invasive herb for ANOVA
ivherb <- inwood.all %>% 
  filter(inwood == "Invasive herb")
ntherb <- inwood.all %>% 
  filter(inwood == "Native herb")

# ANOVA invasive herb C12
summary(aov(Cover ~ Year, data = filter(ivherb, Channel == "Channel 12"))) # NS

# ANOVA invasive herb C13
summary(aov(Cover ~ Year, data = filter(ivherb, Channel == "Channel 13"))) # NS

# ANOVA invasive herb C19
summary(aov(Cover ~ Year, data = filter(ivherb, Channel == "Channel 19")))
ivherb19 <- ivherb %>% 
  filter(Channel == "Channel 19")
anova.ivherb19 <- aov(ivherb19$Cover ~ ivherb19$Year)
hsd.ivherb19 <- HSD.test(anova.ivherb19, trt = "ivherb19$Year")
hsd.ivherb19
ivherb19.letters <- hsd.ivherb19$groups
ivherb19.letters <- ivherb19.letters %>% 
  mutate(Year = rownames(ivherb19.letters)) %>% 
  arrange(Year)

# ANOVA invasive herb C21
summary(aov(Cover ~ Year, data = filter(ivherb, Channel == "Channel 21"))) # NS


# ANOVA native herb C12
summary(aov(Cover ~ Year, data = filter(ntherb, Channel == "Channel 12"))) # NS

# ANOVA native herb C13
summary(aov(Cover ~ Year, data = filter(ntherb, Channel == "Channel 13"))) # NS
ntherb13 <- ntherb %>% 
  filter(Channel == "Channel 13")
anova.ntherb13 <- aov(ntherb13$Cover ~ ntherb13$Year)
hsd.ntherb13 <- HSD.test(anova.ntherb13, trt = "ntherb13$Year")
hsd.ntherb13
ntherb13.letters <- hsd.ntherb13$groups
ntherb13.letters <- ntherb13.letters %>% 
  mutate(Year = rownames(ntherb13.letters)) %>% 
  arrange(Year)

# ANOVA native herb C19
summary(aov(Cover ~ Year, data = filter(ntherb, Channel == "Channel 19")))
ntherb19 <- ntherb %>% 
  filter(Channel == "Channel 19")
anova.ntherb19 <- aov(ntherb19$Cover ~ ntherb19$Year)
hsd.ntherb19 <- HSD.test(anova.ntherb19, trt = "ntherb19$Year")
hsd.ntherb19
ntherb19.letters <- hsd.ntherb19$groups
ntherb19.letters <- ntherb19.letters %>% 
  mutate(Year = rownames(ntherb19.letters)) %>% 
  arrange(Year)

# ANOVA native herb C21
summary(aov(Cover ~ Year, data = filter(ntherb, Channel == "Channel 21"))) # NS

# Native woody is the same as all woody (there are no invasive woody)



# Invasive/native and gfst ------------------------------------------------

# Find channel averages by year
ingfst.channel <- ingfst.all %>%  
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, ingfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# Plot
ingfst.plot <- ggplot(ingfst.channel, aes(x = year.xaxis, y = mean, 
                                          group = ingfst, color = ingfst)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive and functional group") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ingfst.plot

# Plot by known native status
ingfst.channel.known <- ingfst.channel %>% 
  filter(ingfst %in% c("Native tree", "Invasive forb", "Invasive grass", "Native forb",
                       "Native grass", "Native shrub"))

ingfst.known.plot <- ggplot(ingfst.channel.known, aes(x = year.xaxis, y = mean, 
                                                      group = ingfst, color = ingfst, shape = ingfst)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive and functional group") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ingfst.known.plot



# Invasive/native and gfs -------------------------------------------------

# Find channel averages by year
ingfs.channel <- ingfst.all %>%  
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, ingfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(ingfst != "Native tree")

# Plot
ingfs.plot <- ggplot(ingfs.channel, aes(x = year.xaxis, y = mean, 
                                        group = ingfst, color = ingfst)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive and functional group") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ingfst.plot

# Plot known native status
ingfs.channel.known <- ingfs.channel %>% 
  filter(ingfst %in% c("Invasive forb", "Invasive grass", "Native forb",
                       "Native grass", "Native shrub"))

ingfs.channel.known$ingfst <- factor(ingfs.channel.known$ingfst,
                                         levels = c("Native grass", "Invasive grass",
                                                    "Native forb", "Invasive forb",
                                                    "Native shrub"))

ingfs.known.plot <- ggplot(ingfs.channel.known, aes(x = year.xaxis, y = mean, 
                                                    group = ingfst, color = ingfst, shape = ingfst)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Grass, forb, and shrub cover") +
  scale_color_manual(values = c("#1B9E77", "#66C2A5", "#D95F02", "#FC8D62", "#7570B3")) +
  scale_shape_manual(values = c(16, 17, 16, 17, 16)) +
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ingfs.known.plot



# Invasive/native and grass/forb ------------------------------------------

# Find channel averages by year
ingf.channel <- ingfst.all %>%  
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, ingfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(!ingfst %in% c("Native tree", "Native shrub"))

# Plot November only, invasive/native herbs, align year on x-axis
ingfs.channel.known<- ingf.channel %>% 
  filter(ingfst %in% c("Invasive forb", "Invasive grass", "Native forb",
                       "Native grass"))

ingfs.channel.known$ingfst <- factor(ingfs.channel.known$ingfst,
                                         levels = c("Native grass", "Invasive grass",
                                                    "Native forb", "Invasive forb",
                                                    "Native shrub"))

ingf.known.plot <- ggplot(ingfs.channel.known, aes(x = year.xaxis, y = mean, 
                                                   group = ingfst, color = ingfst, shape = ingfst)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Grass and forb cover") +
  scale_color_manual(values = c("#1B9E77", "#66C2A5", "#D95F02", "#FC8D62")) +
  scale_shape_manual(values = c(16, 17, 16, 17)) +
  theme_bw(base_size = 14) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ingf.known.plot


# Save data ---------------------------------------------------------------

save.image("RData/Cover-by-year-and-channel_v2.RData")

