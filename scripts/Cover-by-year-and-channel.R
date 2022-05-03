library(tidyverse)
library(agricolae)
library(plotrix)
library(FSA)
library(rcompanion)

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

# Add year as date and character
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
    
  return(x)
}

plant.all <- year(plant.all)
ground.all <- year(ground.all)
total.all <- year(total.all)
fungr.all <- year(fungr.all)
gfst.all <- year(gfst.all)
woody.all <- year(woody.all)
inwood.all <- year(inwood.all)
ingfst.all <- year(ingfst.all)
innat.all <- year(innat.all)

# Functions ---------------------------------------------------------------

normality <- function(x) {
  ifelse(x > 0.05, "yes", "no")
}

sw <- function(dat) {
  dat.2012 <- dat %>% 
    filter(Year == "2012-11-01") %>% 
    select(-Station, -Year, -station.trt, -channel.trt, -year.date, -year.xaxis)
  dat.2012 <- lapply(dat.2012, shapiro.test)
  dat.2012 <- sapply(dat.2012, `[`, "p.value")
  dat.2012 <- as.data.frame(dat.2012)
  
  dat.2013 <- dat %>% 
    filter(Year == "2013-11-01") %>% 
    select(-Station, -Year, -station.trt, -channel.trt, -year.date, -year.xaxis)
  dat.2013 <- lapply(dat.2013, shapiro.test)
  dat.2013 <- sapply(dat.2013, `[`, "p.value")
  dat.2013 <- as.data.frame(dat.2013)
  
  dat.2014 <- dat %>% 
    filter(Year == "2014-11-01") %>% 
    select(-Station, -Year, -station.trt, -channel.trt, -year.date, -year.xaxis)
  dat.2014 <- lapply(dat.2014, shapiro.test)
  dat.2014 <- sapply(dat.2014, `[`, "p.value")
  dat.2014 <- as.data.frame(dat.2014)
  
  dat.2015 <- dat %>% 
    filter(Year == "2015-11-01") %>% 
    select(-Station, -Year, -station.trt, -channel.trt, -year.date, -year.xaxis)
  dat.2015 <- lapply(dat.2015, shapiro.test)
  dat.2015 <- sapply(dat.2015, `[`, "p.value")
  dat.2015 <- as.data.frame(dat.2015)
  
  dat.2018 <- dat %>% 
    filter(Year == "2018-11-01") %>% 
    select(-Station, -Year, -station.trt, -channel.trt, -year.date, -year.xaxis)
  dat.2018 <- lapply(dat.2018, shapiro.test)
  dat.2018 <- sapply(dat.2018, `[`, "p.value")
  dat.2018 <- as.data.frame(dat.2018)
  
  dat.2021 <- dat %>% 
    filter(Year == "2021-11-01") %>% 
    select(-Station, -Year, -station.trt, -channel.trt, -year.date, -year.xaxis)
  dat.2021 <- lapply(dat.2021, shapiro.test)
  dat.2021 <- sapply(dat.2021, `[`, "p.value")
  dat.2021 <- as.data.frame(dat.2021)
  
  dat.sum <- rbind(dat.2012, dat.2013, dat.2014, dat.2015, dat.2018, dat.2021)
  rownames(dat.sum) <- c("2012", "2013", "2014", "2015", "2018", "2021")
  return(dat.sum)
}


# Total plant cover -------------------------------------------------------

# Find channel averages by year
total.channel <- total.all %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# Plot by accurate date
total.plot <- ggplot(total.channel, aes(x = year.date, y = mean, 
                                        group = Channel, color = Channel, 
                                        shape = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
total.plot

# Plot November only and align year on x-axis
total.channel.nov <- total.channel %>% 
  filter(Year != "2012-03-01") 

total.plot.nov <- ggplot(total.channel.nov, aes(x = year.xaxis, y = mean, 
                                                group = channel.trt, 
                                                color = channel.trt)) +
  geom_line(size = 1) +
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
total.plot.nov


# Shapiro-Wilk
total.all.nov <- total.all %>% 
  filter(Year != "2012-03-01") %>% 
  ungroup() 

total.wide <- total.all.nov %>% 
  pivot_wider(names_from = Channel, values_from = Cover)

total.sw <- sw(total.wide)
noquote(apply(total.sw, 2, normality))

total.all.nov$Year <- gsub("-.*", "", total.all.nov$Year)
total.all.nov$Year <- as.factor(total.all.nov$Year)


# ANOVA
summary(aov(Cover ~ Year, data = filter(total.all.nov, Channel == "Channel 21"))) # NS

# Kruskal-Wallis
total12 <- total.all.nov %>% 
  filter(Channel == "Channel 12")
kruskal.test(Cover ~ Year, data = total12)
dt.total12 <- dunnTest(total12$Cover ~ total12$Year, method = "bh")
dt.total12 <- dt.total12$res
cldList(comparison = dt.total12$Comparison,
        p.value    = dt.total12$P.adj,
        threshold  = 0.05)
total12.letters <- total12 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            .groups = "keep") %>% 
  arrange(desc(mean))
total12.letters$groups <- c("a", "ab", "ab", "ab", "ab", "b")

total13 <- total.all.nov %>% 
  filter(Channel == "Channel 13")
kruskal.test(Cover ~ Year, data = total13)
dt.total13 <- dunnTest(total13$Cover ~ total13$Year, method = "bh")
dt.total13 <- dt.total13$res
cldList(comparison = dt.total13$Comparison,
        p.value    = dt.total13$P.adj,
        threshold  = 0.05) # largest is b
total13.letters <- total13 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            .groups = "keep") %>% 
  arrange(desc(mean))
total13.letters$groups <- c("a", "ab", "ab", "ab", "b", "b")

total19 <- total.all.nov %>% 
  filter(Channel == "Channel 19")
kruskal.test(Cover ~ Year, data = total19)
dt.total19 <- dunnTest(total19$Cover ~ total19$Year, method = "bh")
dt.total19 <- dt.total19$res
cldList(comparison = dt.total19$Comparison,
        p.value    = dt.total19$P.adj,
        threshold  = 0.05)
total19.letters <- total19 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            .groups = "keep") %>% 
  arrange(desc(mean))
total19.letters$groups <- c("a", "a", "ab", "ab", "bc", "c")


# Plot Nov only with year aligned on x-axis and ANOVA/KW letters added
letters <- data.frame(label = c(total12.letters$groups, total13.letters$groups,
                                total19.letters$groups),
                      channel.trt = c(rep("Channel 12: No treatment", 6),
                                      rep("Channel 13: In-channel treatment", 6),
                                      rep("Channel 19: Upland treatment", 6)),
                      x = rep(total.channel.nov$year.xaxis[1:6], 3),
                      y = c(40, 45, 30, 40, 40, 40,
                            70, 60, 70, 65, 60, 55,
                            50, 45, 48, 43, 55, 60))

anova.kw.lab <- data.frame(label = rep("Kruskal-Wallis", 3),
                           channel.trt = c("Channel 12: No treatment",
                                           "Channel 13: In-channel treatment",
                                           "Channel 19: Upland treatment"),
                           x = c(rep(as.Date("2020-01-01"), 3)),
                           y = c(23, 23, 23))

total.plot.nov.letters <- ggplot(total.channel.nov, aes(x = year.xaxis, y = mean, 
                                                        group = channel.trt, 
                                                        color = channel.trt)) +
  geom_line(size = 1) +
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
  geom_text(data = anova.kw.lab,
            mapping = aes(x = x, y = y, label = label),
            size = 3, color = "black")
total.plot.nov.letters

pdf("output_figs/Total-cover.pdf", width = 7, height = 5)
total.plot.nov.letters
dev.off()



# Ground cover ------------------------------------------------------------

# Find channel averages by year
ground.channel <- ground.all %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, Common) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# Plot by accurate date
ground.plot <- ggplot(ground.channel, aes(x = year.date, y = mean, 
                                          group = Common, color = Common, 
                                          shape = Common)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Ground cover") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ground.plot

# Plot just gravel, rock, and soil by accurate date 
ground.channel.grs <- ground.channel %>% 
  filter(Common %in% c("Gravel", "Rock", "Soil"))

ground.grs.plot <- ggplot(ground.channel.grs, 
                          aes(x = year.date, y = mean, 
                              group = Common, color = Common, shape = Common)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Ground cover") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ground.grs.plot

# Plot November only and only rock, gravel, and soil; align year on x-axis
ground.channel.grs.nov <- ground.channel.grs %>% 
  filter(Year != "2012-03-01")

ground.grs.plot.nov <- ggplot(ground.channel.grs.nov, 
                              aes(x = year.xaxis, y = mean,
                                  group = Common, color = Common, shape = Common)) +
  geom_line(size = 1) +
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
ground.grs.plot.nov

# Plot November only, soil only, and align year on x-axis
ground.channel.soil.nov <- ground.channel.grs.nov %>% 
  filter(Common == "Soil")

ground.soil.plot.nov <- ggplot(ground.channel.soil.nov, 
                               aes(x = year.xaxis, y = mean, 
                                   group = channel.trt, color = channel.trt)) +
  geom_line(size = 1) +
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
ground.soil.plot.nov


# Shapiro-Wilk for gravel, rock, soil
ground.all.nov <- ground.all %>% 
  filter(Year != "2012-03-01") %>% 
  ungroup()

ground.grs.wide <- ground.all.nov %>% 
  filter(Common %in% c("Gravel", "Rock", "Soil")) %>% 
  pivot_wider(names_from = c(Channel, Common), values_from = Cover)

ground.sw <- sw(ground.grs.wide)
noquote(apply(ground.sw, 2, normality))

ground.all.nov$Year <- gsub("-.*", "", ground.all.nov$Year)
ground.all.nov$Year <- as.factor(ground.all.nov$Year)

soil <- ground.all.nov %>% 
  filter(Common == "Soil")


# Soil: ANOVA
summary(aov(Cover ~ Year, data = filter(soil, Channel == "Channel 13")))
soil13 <- soil %>% 
  filter(Channel == "Channel 13")
anova.soil13 <- aov(soil13$Cover ~ soil13$Year)
hsd.soil13 <- HSD.test(anova.soil13, trt = "soil13$Year")
hsd.soil13

summary(aov(Cover ~ Year, data = filter(soil, Channel == "Channel 19")))
soil19 <- soil %>% 
  filter(Channel == "Channel 19")
anova.soil19 <- aov(soil19$Cover ~ soil19$Year)
hsd.soil19 <- HSD.test(anova.soil19, trt = "soil19$Year")
hsd.soil19

# Soil: Kruskal-Wallis
soil12 <- soil %>% 
  filter(Channel == "Channel 12")
kruskal.test(Cover ~ Year, data = soil12)
dt.soil12 <- dunnTest(soil12$Cover ~ soil12$Year, method = "bh")
dt.soil12 <- dt.soil12$res
cldList(comparison = dt.soil12$Comparison,
        p.value    = dt.soil12$P.adj,
        threshold  = 0.05) # biggest is c
soil12.letters <- soil12 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            .groups = "keep") %>% 
  arrange(desc(mean))
soil12.letters$groups.cldList <- c("a", "ac", "ac", "bc", "b", "b") # I don't know why they are so weird
soil12.letters$possible.groups <- c("a", "ab", "ab", "bc", "c", "c") # groups that make sense to me and also fit the p-values

soil21 <- soil %>% 
  filter(Channel == "Channel 21")
kruskal.test(Cover ~ Year, data = soil21)
dt.soil21 <- dunnTest(soil21$Cover ~ soil21$Year, method = "bh")
dt.soil21 <- dt.soil21$res
cldList(comparison = dt.soil21$Comparison,
        p.value    = dt.soil21$P.adj,
        threshold  = 0.05) 
soil21.letters <- soil21 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            .groups = "keep") %>% 
  arrange(desc(mean))
soil21.letters$groups <- c("a", "ab", "ab", "ab", "b", "c")



# Functional group (as collected) -----------------------------------------

# Find channel averages by year
fungr.channel <- fungr.all %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, Functional) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# Plot by accurate date
fungr.plot <- ggplot(fungr.channel, aes(x = year.date, y = mean, 
                                            group = Functional, color = Functional, shape = Functional)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by functional group") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
fungr.plot



# Functional group (gfst) -------------------------------------------------

# Find channel averages by year
gfst.channel <- gfst.all %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, gfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# Plot by accurate date
gfst.plot <- ggplot(gfst.channel, aes(x = year.date, y = mean, 
                                      group = gfst, color = gfst, shape = gfst)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by functional group") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
gfst.plot

# Plot November only and align year on x-axis
gfst.channel.nov <- gfst.channel %>% 
  filter(Year != "2012-03-01")

gfst.channel.nov$gfst <- factor(gfst.channel.nov$gfst, 
                                levels = c("Grass", "Forb", "Shrub", "Tree"))

gfst.plot.nov <- ggplot(gfst.channel.nov, aes(x = year.xaxis, y = mean, 
                                              group = gfst, color = gfst, shape = gfst)) +
  geom_line(size = 1) +
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
gfst.plot.nov



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
gfs.plot <- ggplot(gfs.channel, aes(x = year.date, y = mean, 
                                        group = gfst, color = gfst, shape = gfst)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by functional group") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
gfs.plot

# Plot November only and align year on x-axis
gfs.channel.nov <- gfs.channel %>% 
  filter(Year != "2012-03-01")

gfs.channel.nov$gfst <- factor(gfs.channel.nov$gfst, 
                               levels = c("Grass", "Forb", "Shrub"))

gfs.plot.nov <- ggplot(gfs.channel.nov, aes(x = year.xaxis, y = mean, 
                                            group = gfst, color = gfst, shape = gfst)) +
  geom_line(size = 1) +
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
gfs.plot.nov



# Woody/herbaceous --------------------------------------------------------

# Find channel averages by year
woody.channel <- woody.all %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, woody) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# Plot by accurate date
woody.plot <- ggplot(woody.channel, aes(x = year.date, y = mean, 
                                        group = woody, color = woody, shape = woody)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by woody/herbaceous") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
woody.plot

# Plot November only and align year on x-axis
woody.channel.nov <- woody.channel %>% 
  filter(Year != "2012-03-01")

woody.plot.nov <- ggplot(woody.channel.nov, aes(x = year.xaxis, y = mean, 
                                                group = woody, color = woody, shape = woody)) +
  geom_line(size = 1) +
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
woody.plot.nov


# Shapiro-Wilk
woody.all.nov <- woody.all %>% 
  filter(Year != "2012-03-01") %>% 
  ungroup()

woody.grs.wide <- woody.all.nov %>% 
  pivot_wider(names_from = c(Channel, woody), values_from = Cover)

woody.sw <- sw(woody.grs.wide)
noquote(apply(woody.sw, 2, normality))

woody.all.nov$Year <- gsub("-.*", "", woody.all.nov$Year)
woody.all.nov$Year <- as.factor(woody.all.nov$Year)

herb <- woody.all.nov %>% 
  filter(woody == "Herbaceous")
woody <- woody.all.nov %>% 
  filter(woody == "Woody")


# Herbaceous: ANOVA
summary(aov(Cover ~ Year, data = filter(herb, Channel == "Channel 21")))
herb21 <- herb %>% 
  filter(Channel == "Channel 21")
anova.herb21 <- aov(herb21$Cover ~ herb21$Year)
hsd.herb21 <- HSD.test(anova.herb21, trt = "herb21$Year")
hsd.herb21

# Herbaceous: Kruskal-Wallis
herb12 <- herb %>% 
  filter(Channel == "Channel 12")
kruskal.test(Cover ~ Year, data = herb12)
dt.herb12 <- dunnTest(herb12$Cover ~ herb12$Year, method = "bh")
dt.herb12 <- dt.herb12$res
cldList(comparison = dt.herb12$Comparison,
        p.value    = dt.herb12$P.adj,
        threshold  = 0.05)
herb12.letters <- herb12 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            .groups = "keep") %>% 
  arrange(desc(mean))
herb12.letters$groups <- c("a", "a", "a", "ab", "ab", "b")

herb13 <- herb %>% 
  filter(Channel == "Channel 13")
kruskal.test(Cover ~ Year, data = herb13)
dt.herb13 <- dunnTest(herb13$Cover ~ herb13$Year, method = "bh")
dt.herb13 <- dt.herb13$res
cldList(comparison = dt.herb13$Comparison,
        p.value    = dt.herb13$P.adj,
        threshold  = 0.05) # biggest is c
herb13.letters <- herb13 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            .groups = "keep") %>% 
  arrange(desc(mean))
herb13.letters$groups <- c("a", "ab", "abc", "abc", "bc", "c")

herb19 <- herb %>% 
  filter(Channel == "Channel 19")
kruskal.test(Cover ~ Year, data = herb19)
dt.herb19 <- dunnTest(herb19$Cover ~ herb19$Year, method = "bh")
dt.herb19 <- dt.herb19$res
cldList(comparison = dt.herb19$Comparison,
        p.value    = dt.herb19$P.adj,
        threshold  = 0.05) # b is biggest??
herb19.letters <- herb19 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            .groups = "keep") %>% 
  arrange(desc(mean))
herb19.letters$groups.cldList <- c("b", "b", "ab", "ac", "ac", "c") # as is from cldList
herb19.letters$groups <- c("a", "a", "ab", "bc", "bc", "c") # what I think makes sense


# Woody: ANOVA
  # None

# Woody: Kruskal-Wallis
kruskal.test(Cover ~ Year, data = filter(woody, Channel == "Channel 12")) # NS
kruskal.test(Cover ~ Year, data = filter(woody, Channel == "Channel 13")) # NS
kruskal.test(Cover ~ Year, data = filter(woody, Channel == "Channel 21")) # NS

woody19 <- woody %>% 
  filter(Channel == "Channel 19")
kruskal.test(Cover ~ Year, data = woody19)
dt.woody19 <- dunnTest(woody19$Cover ~ woody19$Year, method = "bh")
dt.woody19 <- dt.woody19$res
cldList(comparison = dt.woody19$Comparison,
        p.value    = dt.woody19$P.adj,
        threshold  = 0.05) 
woody19.letters <- woody19 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            .groups = "keep") %>% 
  arrange(desc(mean))
woody19.letters$groups <- c("a", "a", "a", "ab", "ab", "b")


# Herbaceous (plots) ------------------------------------------------------

# Find channel averages by year
herb.channel.nov <- woody.all %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, woody) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(woody == "Herbaceous") %>% 
  filter(Year != "2012-03-01")

# Plot November only, soil only, and align year on x-axis
herb.plot.nov <- ggplot(herb.channel.nov, aes(x = year.xaxis, y = mean, 
                                              group = channel.trt, color = channel.trt)) +
  geom_line(size = 1) +
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
herb.plot.nov



# Invasive/native ---------------------------------------------------------

# Find channel averages by year
innat.channel <- innat.all %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, Native) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# Plot by accurate date
innat.plot <- ggplot(innat.channel, aes(x = year.date, y = mean, 
                                        group = Native, color = Native, shape = Native)) +
  geom_line(size = 1) +
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

# Plot by accurate date
inwood.plot <- ggplot(inwood.channel, aes(x = year.date, y = mean, 
                                          group = inwood, color = inwood, shape = inwood)) +
  geom_line(size = 1) +
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

# Plot only known native status by accurate date
inwood.channel.known <- inwood.channel %>% 
  filter(inwood %in% c("Invasive herb", "Native herb", "Native woody"))

inwood.known.plot <- ggplot(inwood.channel.known, aes(x = year.date, y = mean, 
                                                          group = inwood, color = inwood, shape = inwood)) +
  geom_line(size = 1) +
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

# Plot November only, known native status only, and align year on x-axis
inwood.channel.known.nov <- inwood.channel.known %>% 
  filter(Year != "2012-03-01")

inwood.channel.known.nov$inwood <- factor(inwood.channel.known.nov$inwood,
                                          levels = c("Native herb", "Invasive herb", "Native woody"))

inwood.known.plot.nov <- ggplot(inwood.channel.known.nov, aes(x = year.xaxis, y = mean, 
                                                          group = inwood, color = inwood, shape = inwood)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Woody and herbaceous cover") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
inwood.known.plot.nov


# Shapiro-Wilk for invasive herb, native herb, and native woody
inwood.known.nov <- inwood.all %>% 
  filter(inwood %in% c("Invasive herb", "Native herb", "Native woody")) %>% 
  filter(Year != "2012-03-01") %>% 
  ungroup()

inwood.known.wide <- inwood.known.nov %>% 
  pivot_wider(names_from = c(Channel, inwood), values_from = Cover)

inwood.sw <- sw(inwood.known.wide)
noquote(apply(inwood.sw, 2, normality))

inwood.known.nov$Year <- gsub("-.*", "", inwood.known.nov$Year)
inwood.known.nov$Year <- as.factor(inwood.known.nov$Year)

ivherb <- inwood.known.nov %>% 
  filter(inwood == "Invasive herb")
ntherb <- inwood.known.nov %>% 
  filter(inwood == "Native herb")


# Invasive herb: Kruskal-Wallis
kruskal.test(Cover ~ Year, data = filter(ivherb, Channel == "Channel 12"))
kruskal.test(Cover ~ Year, data = filter(ivherb, Channel == "Channel 13"))
kruskal.test(Cover ~ Year, data = filter(ivherb, Channel == "Channel 21"))

ivherb19 <- ivherb %>% 
  filter(Channel == "Channel 19")
kruskal.test(Cover ~ Year, data = ivherb19)
dt.ivherb19 <- dunnTest(ivherb19$Cover ~ ivherb19$Year, method = "bh")
dt.ivherb19 <- dt.ivherb19$res
cldList(comparison = dt.ivherb19$Comparison,
        p.value    = dt.ivherb19$P.adj,
        threshold  = 0.05) # b is biggest
ivherb19.letters <- ivherb19 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            .groups = "keep") %>% 
  arrange(desc(mean))
ivherb19.letters$groups <- c("a", "b", "b", "b", "b", "b")


# Native herb: ANOVA
summary(aov(Cover ~ Year, data = filter(ntherb, Channel == "Channel 12")))
ntherb12 <- ntherb %>% 
  filter(Channel == "Channel 12")
anova.ntherb12 <- aov(ntherb12$Cover ~ ntherb12$Year)
hsd.ntherb12 <- HSD.test(anova.ntherb12, trt = "ntherb12$Year")
hsd.ntherb12

# Native herb: Kruskal-Wallis
ntherb13 <- ntherb %>% 
  filter(Channel == "Channel 13")
kruskal.test(Cover ~ Year, data = ntherb13)
dt.ntherb13 <- dunnTest(ntherb13$Cover ~ ntherb13$Year, method = "bh")
dt.ntherb13 <- dt.ntherb13$res
cldList(comparison = dt.ntherb13$Comparison,
        p.value    = dt.ntherb13$P.adj,
        threshold  = 0.05) # b is biggest
ntherb13.letters <- ntherb13 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            .groups = "keep") %>% 
  arrange(desc(mean))
ntherb13.letters$groups <- c("a", "ab", "ab", "b", "b", "b")

ntherb19 <- ntherb %>% 
  filter(Channel == "Channel 19")
kruskal.test(Cover ~ Year, data = ntherb19)
dt.ntherb19 <- dunnTest(ntherb19$Cover ~ ntherb19$Year, method = "bh")
dt.ntherb19 <- dt.ntherb19$res
cldList(comparison = dt.ntherb19$Comparison,
        p.value    = dt.ntherb19$P.adj,
        threshold  = 0.05) # b is biggest?? (switch a & b)
ntherb19.letters <- ntherb19 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            .groups = "keep") %>% 
  arrange(desc(mean))
ntherb19.letters$groups.cldList <- c("b", "ab", "abc", "ac", "ac", "c")
ntherb19.letters$groups <- c("a", "ab", "abc", "bc", "bc", "c")

kruskal.test(Cover ~ Year, data = filter(ntherb, Channel == "Channel 21")) # NS


# Native woody is the same as all woody (there are no invasive woody)



# Invasive/native and gfst ------------------------------------------------

# Find channel averages by year
ingfst.channel <- ingfst.all %>%  
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, ingfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# Plot by accurate date
ingfst.plot <- ggplot(ingfst.channel, aes(x = year.date, y = mean, 
                                          group = ingfst, color = ingfst)) +
  geom_line(size = 1) +
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

# Plot by known native status and accurate date
ingfst.channel.known <- ingfst.channel %>% 
  filter(ingfst %in% c("Native tree", "Invasive forb", "Invasive grass", "Native forb",
                       "Native grass", "Native shrub"))

ingfst.known.plot <- ggplot(ingfst.channel.known, aes(x = year.date, y = mean, 
                                                      group = ingfst, color = ingfst, shape = ingfst)) +
  geom_line(size = 1) +
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

# Plot by accurate date
ingfs.plot <- ggplot(ingfs.channel, aes(x = year.date, y = mean, 
                                        group = ingfst, color = ingfst)) +
  geom_line(size = 1) +
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

# Plot known native status by accurate date
ingfs.channel.known <- ingfs.channel %>% 
  filter(ingfst %in% c("Invasive forb", "Invasive grass", "Native forb",
                       "Native grass", "Native shrub"))

ingfs.known.plot <- ggplot(ingfs.channel.known, aes(x = year.date, y = mean, 
                                                    group = ingfst, color = ingfst, shape = ingfst)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~channel.trt) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive and woody status") +
  scale_color_manual(values = c("#1B9E77", "#66C2A5", "#D95F02", "#FC8D62", "#7570B3")) +
  scale_shape_manual(values = c(16, 17, 16, 17, 16)) +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ingfs.known.plot

# Plot November and known native status only, align year on x-axis
ingfs.channel.known.nov <- ingfs.channel.known %>% 
  filter(Year != "2012-03-01")

ingfs.channel.known.nov$ingfst <- factor(ingfs.channel.known.nov$ingfst,
                                         levels = c("Native grass", "Invasive grass",
                                                    "Native forb", "Invasive forb",
                                                    "Native shrub"))

ingfs.known.plot.nov <- ggplot(ingfs.channel.known.nov, aes(x = year.xaxis, y = mean, 
                                                            group = ingfst, color = ingfst, shape = ingfst)) +
  geom_line(size = 1) +
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
ingfs.known.plot.nov



# Invasive/native and grass/forb ------------------------------------------

# Find channel averages by year, November only
ingf.channel.nov <- ingfst.all %>%  
  group_by(Channel, Year, year.date, year.xaxis, channel.trt, ingfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(!ingfst %in% c("Native tree", "Native shrub")) %>% 
  filter(Year != "2012-03-01")

# Plot November only, invasive/native herbs, align year on x-axis
ingfs.channel.known.nov <- ingf.channel.nov %>% 
  filter(ingfst %in% c("Invasive forb", "Invasive grass", "Native forb",
                       "Native grass"))

ingfs.channel.known.nov$ingfst <- factor(ingfs.channel.known.nov$ingfst,
                                         levels = c("Native grass", "Invasive grass",
                                                    "Native forb", "Invasive forb",
                                                    "Native shrub"))

ingf.known.plot.nov <- ggplot(ingfs.channel.known.nov, aes(x = year.xaxis, y = mean, 
                                                           group = ingfst, color = ingfst, shape = ingfst)) +
  geom_line(size = 1) +
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
ingf.known.plot.nov


# Save data ---------------------------------------------------------------

save.image("RData-RMarkdown/Cover-by-year-and-channel.RData")

