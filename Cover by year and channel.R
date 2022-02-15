library(tidyverse)
library(agricolae)
library(plotrix)
library(FSA)
library(rcompanion)

# Load data ---------------------------------------------------------------

load("Summarised plant and ground cover.RData")

# Functions ---------------------------------------------------------------

normality <- function(x) {
  ifelse(x > 0.05, "yes", "no")
}

sw <- function(dat) {
  dat.2012 <- dat %>% 
    filter(Year == "2012-11-01") %>% 
    select(-Station, -Year, -Treatment)
  dat.2012 <- lapply(dat.2012, shapiro.test)
  dat.2012 <- sapply(dat.2012, `[`, "p.value")
  dat.2012 <- as.data.frame(dat.2012)
  
  dat.2013 <- dat %>% 
    filter(Year == "2013-11-01") %>% 
    select(-Station, -Year, -Treatment)
  dat.2013 <- lapply(dat.2013, shapiro.test)
  dat.2013 <- sapply(dat.2013, `[`, "p.value")
  dat.2013 <- as.data.frame(dat.2013)
  
  dat.2014 <- dat %>% 
    filter(Year == "2014-11-01") %>% 
    select(-Station, -Year, -Treatment)
  dat.2014 <- lapply(dat.2014, shapiro.test)
  dat.2014 <- sapply(dat.2014, `[`, "p.value")
  dat.2014 <- as.data.frame(dat.2014)
  
  dat.2015 <- dat %>% 
    filter(Year == "2015-11-01") %>% 
    select(-Station, -Year, -Treatment)
  dat.2015 <- lapply(dat.2015, shapiro.test)
  dat.2015 <- sapply(dat.2015, `[`, "p.value")
  dat.2015 <- as.data.frame(dat.2015)
  
  dat.2018 <- dat %>% 
    filter(Year == "2018-11-01") %>% 
    select(-Station, -Year, -Treatment)
  dat.2018 <- lapply(dat.2018, shapiro.test)
  dat.2018 <- sapply(dat.2018, `[`, "p.value")
  dat.2018 <- as.data.frame(dat.2018)
  
  dat.2021 <- dat %>% 
    filter(Year == "2021-11-01") %>% 
    select(-Station, -Year, -Treatment)
  dat.2021 <- lapply(dat.2021, shapiro.test)
  dat.2021 <- sapply(dat.2021, `[`, "p.value")
  dat.2021 <- as.data.frame(dat.2021)
  
  dat.sum <- rbind(dat.2012, dat.2013, dat.2014, dat.2015, dat.2018, dat.2021)
  rownames(dat.sum) <- c("2012", "2013", "2014", "2015", "2018", "2021")
  return(dat.sum)
}


###### Entire channel averages ############################################

# Total plant cover -------------------------------------------------------

# Plot
total.channel <- total.all %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

total.all.plot <- ggplot(total.channel, aes(x = Year, y = mean, 
                                            group = Channel, color = Channel, shape = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover (all)") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
total.all.plot


total.channel.nov <- total.channel %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(total.channel.nov)) {
  if(total.channel.nov$Year[i] == "2012-11-01") {
    total.channel.nov$Year[i] <- "2012-01-01"
  } else if(total.channel.nov$Year[i] == "2013-11-01") {
    total.channel.nov$Year[i] <- "2013-01-01"
  } else if(total.channel.nov$Year[i] == "2014-11-01") {
    total.channel.nov$Year[i] <- "2014-01-01"
  } else if(total.channel.nov$Year[i] == "2015-11-01") {
    total.channel.nov$Year[i] <- "2015-01-01"
  } else if(total.channel.nov$Year[i] == "2018-11-01") {
    total.channel.nov$Year[i] <- "2018-01-01"
  } else {
    total.channel.nov$Year[i] <- "2021-01-01"
  }
}

total.channel.nov[ , "channel.trt"] <- NA
for(i in 1:nrow(total.channel.nov)) {
  if(total.channel.nov$Channel[i] == "Channel 12") {
    total.channel.nov$channel.trt[i] <- "Channel 12: No treatment"
  } else if(total.channel.nov$Channel[i] == "Channel 13") {
    total.channel.nov$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(total.channel.nov$Channel[i] == "Channel 19") {
    total.channel.nov$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    total.channel.nov$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

total.plot.nov <- ggplot(total.channel.nov, aes(x = Year, y = mean, 
                                                group = channel.trt, color = channel.trt)) +
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
total.yearchar <- total.all %>% 
  filter(Year != "2012-03-01") %>% 
  ungroup()
total.yearchar$Year <- as.character(total.yearchar$Year)
total.yearchar.wide <- total.yearchar %>% 
  pivot_wider(names_from = Channel, values_from = Cover)

total.sw <- sw(total.yearchar.wide)
noquote(apply(total.sw, 2, normality))

total.yearchar$Year <- gsub("-.*", "", total.yearchar$Year)

# ANOVA
  # none

# Kruskal-Wallis
total12 <- total.yearchar %>% 
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

total13 <- total.yearchar %>% 
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

total19 <- total.yearchar %>% 
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

kruskal.test(Cover ~ Year, data = filter(total.yearchar, Channel == "Channel 21")) # NS



# Ground cover ------------------------------------------------------------

ground.channel <- ground.all %>% 
  group_by(Channel, Year, Common) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

ground.all.plot <- ggplot(ground.channel, aes(x = Year, y = mean, 
                                              group = Common, color = Common, shape = Common)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Ground cover (all)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ground.all.plot


ground.channel.grs <- ground.channel %>% 
  filter(Common %in% c("Gravel", "Rock", "Soil"))

ground.grs.plot <- ggplot(ground.channel.grs, aes(x = Year, y = mean, 
                                                  group = Common, color = Common, shape = Common)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Ground cover (all)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ground.grs.plot


ground.channel.grs.nov <- ground.channel.grs %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(ground.channel.grs.nov)) {
  if(ground.channel.grs.nov$Year[i] == "2012-11-01") {
    ground.channel.grs.nov$Year[i] <- "2012-01-01"
  } else if(ground.channel.grs.nov$Year[i] == "2013-11-01") {
    ground.channel.grs.nov$Year[i] <- "2013-01-01"
  } else if(ground.channel.grs.nov$Year[i] == "2014-11-01") {
    ground.channel.grs.nov$Year[i] <- "2014-01-01"
  } else if(ground.channel.grs.nov$Year[i] == "2015-11-01") {
    ground.channel.grs.nov$Year[i] <- "2015-01-01"
  } else if(ground.channel.grs.nov$Year[i] == "2018-11-01") {
    ground.channel.grs.nov$Year[i] <- "2018-01-01"
  } else {
    ground.channel.grs.nov$Year[i] <- "2021-01-01"
  }
}

ground.channel.grs.nov[ , "channel.trt"] <- NA
for(i in 1:nrow(ground.channel.grs.nov)) {
  if(ground.channel.grs.nov$Channel[i] == "Channel 12") {
    ground.channel.grs.nov$channel.trt[i] <- "Channel 12: No treatment"
  } else if(ground.channel.grs.nov$Channel[i] == "Channel 13") {
    ground.channel.grs.nov$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(ground.channel.grs.nov$Channel[i] == "Channel 19") {
    ground.channel.grs.nov$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    ground.channel.grs.nov$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

ground.grs.plot.nov <- ggplot(ground.channel.grs.nov, aes(x = Year, y = mean, 
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


ground.channel.nov.soil <- ground.channel.grs.nov %>% 
  filter(Common == "Soil")

ground.soil.plot.nov <- ggplot(ground.channel.nov.soil, aes(x = Year, y = mean, 
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
ground.yearchar <- ground.all %>% 
  filter(Year != "2012-03-01") %>% 
  ungroup()
ground.yearchar$Year <- as.character(ground.yearchar$Year)
ground.yearchar <- ground.yearchar %>% 
  filter(Common %in% c("Gravel", "Rock", "Soil"))
ground.yearchar.wide <- ground.yearchar %>% 
  pivot_wider(names_from = c(Channel, Common), values_from = Cover)

ground.sw <- sw(ground.yearchar.wide)
noquote(apply(ground.sw, 2, normality))

soil <- ground.yearchar %>% 
  filter(Common == "Soil")
soil$Year <- gsub("-.*", "", soil$Year)

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
soil12.letters$possible.groups <- c("a", "ab", "ab", "bc", "c", "c") # groups that make sense to me

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
soil21.letters$groups <- c("a", "a", "a", "a", "a", "b")




# Functional group (as collected) -----------------------------------------

fungr.channel <- fungr.all %>% 
  group_by(Channel, Year, Functional) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

fungr.all.plot <- ggplot(fungr.channel, aes(x = Year, y = mean, 
                                            group = Functional, color = Functional, shape = Functional)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by functional group (all)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
fungr.all.plot



# Functional group (gfst) -------------------------------------------------

gfst.channel <- gfst.all %>% 
  group_by(Channel, Year, gfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

gfst.all.plot <- ggplot(gfst.channel, aes(x = Year, y = mean, 
                                          group = gfst, color = gfst, shape = gfst)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by functional group (all)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
gfst.all.plot


gfst.channel.nov <- gfst.channel %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(gfst.channel.nov)) {
  if(gfst.channel.nov$Year[i] == "2012-11-01") {
    gfst.channel.nov$Year[i] <- "2012-01-01"
  } else if(gfst.channel.nov$Year[i] == "2013-11-01") {
    gfst.channel.nov$Year[i] <- "2013-01-01"
  } else if(gfst.channel.nov$Year[i] == "2014-11-01") {
    gfst.channel.nov$Year[i] <- "2014-01-01"
  } else if(gfst.channel.nov$Year[i] == "2015-11-01") {
    gfst.channel.nov$Year[i] <- "2015-01-01"
  } else if(gfst.channel.nov$Year[i] == "2018-11-01") {
    gfst.channel.nov$Year[i] <- "2018-01-01"
  } else {
    gfst.channel.nov$Year[i] <- "2021-01-01"
  }
}

gfst.channel.nov[ , "channel.trt"] <- NA
for(i in 1:nrow(gfst.channel.nov)) {
  if(gfst.channel.nov$Channel[i] == "Channel 12") {
    gfst.channel.nov$channel.trt[i] <- "Channel 12: No treatment"
  } else if(gfst.channel.nov$Channel[i] == "Channel 13") {
    gfst.channel.nov$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(gfst.channel.nov$Channel[i] == "Channel 19") {
    gfst.channel.nov$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    gfst.channel.nov$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

gfst.channel.nov$gfst <- factor(gfst.channel.nov$gfst, 
                                levels = c("Grass", "Forb", "Shrub", "Tree"))

gfst.plot.nov <- ggplot(gfst.channel.nov, aes(x = Year, y = mean, 
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

gfs.channel <- gfst.all %>% 
  group_by(Channel, Year, gfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(gfst != "Tree")

gfs.all.plot <- ggplot(gfs.channel, aes(x = Year, y = mean, 
                                        group = gfst, color = gfst, shape = gfst)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by functional group (all)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
gfs.all.plot


gfs.channel.nov <- gfs.channel %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(gfs.channel.nov)) {
  if(gfs.channel.nov$Year[i] == "2012-11-01") {
    gfs.channel.nov$Year[i] <- "2012-01-01"
  } else if(gfs.channel.nov$Year[i] == "2013-11-01") {
    gfs.channel.nov$Year[i] <- "2013-01-01"
  } else if(gfs.channel.nov$Year[i] == "2014-11-01") {
    gfs.channel.nov$Year[i] <- "2014-01-01"
  } else if(gfs.channel.nov$Year[i] == "2015-11-01") {
    gfs.channel.nov$Year[i] <- "2015-01-01"
  } else if(gfs.channel.nov$Year[i] == "2018-11-01") {
    gfs.channel.nov$Year[i] <- "2018-01-01"
  } else {
    gfs.channel.nov$Year[i] <- "2021-01-01"
  }
}

gfs.channel.nov[ , "channel.trt"] <- NA
for(i in 1:nrow(gfs.channel.nov)) {
  if(gfs.channel.nov$Channel[i] == "Channel 12") {
    gfs.channel.nov$channel.trt[i] <- "Channel 12: No treatment"
  } else if(gfs.channel.nov$Channel[i] == "Channel 13") {
    gfs.channel.nov$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(gfs.channel.nov$Channel[i] == "Channel 19") {
    gfs.channel.nov$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    gfs.channel.nov$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

gfs.channel.nov$gfst <- factor(gfs.channel.nov$gfst, 
                               levels = c("Grass", "Forb", "Shrub"))

gfs.plot.nov <- ggplot(gfs.channel.nov, aes(x = Year, y = mean, 
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

woody.channel <- woody.all %>% 
  group_by(Channel, Year, woody) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

woody.all.plot <- ggplot(woody.channel, aes(x = Year, y = mean, 
                                            group = woody, color = woody, shape = woody)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by woody/herbaceous (all)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
woody.all.plot


woody.channel.nov <- woody.channel %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(woody.channel.nov)) {
  if(woody.channel.nov$Year[i] == "2012-11-01") {
    woody.channel.nov$Year[i] <- "2012-01-01"
  } else if(woody.channel.nov$Year[i] == "2013-11-01") {
    woody.channel.nov$Year[i] <- "2013-01-01"
  } else if(woody.channel.nov$Year[i] == "2014-11-01") {
    woody.channel.nov$Year[i] <- "2014-01-01"
  } else if(woody.channel.nov$Year[i] == "2015-11-01") {
    woody.channel.nov$Year[i] <- "2015-01-01"
  } else if(woody.channel.nov$Year[i] == "2018-11-01") {
    woody.channel.nov$Year[i] <- "2018-01-01"
  } else {
    woody.channel.nov$Year[i] <- "2021-01-01"
  }
}

woody.channel.nov[ , "channel.trt"] <- NA
for(i in 1:nrow(woody.channel.nov)) {
  if(woody.channel.nov$Channel[i] == "Channel 12") {
    woody.channel.nov$channel.trt[i] <- "Channel 12: No treatment"
  } else if(woody.channel.nov$Channel[i] == "Channel 13") {
    woody.channel.nov$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(woody.channel.nov$Channel[i] == "Channel 19") {
    woody.channel.nov$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    woody.channel.nov$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

woody.plot.nov <- ggplot(woody.channel.nov, aes(x = Year, y = mean, 
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
woody.yearchar <- woody.all %>% 
  filter(Year != "2012-03-01") %>% 
  ungroup()
woody.yearchar$Year <- as.character(woody.yearchar$Year)
woody.yearchar.wide <- woody.yearchar %>% 
  pivot_wider(names_from = c(Channel, woody), values_from = Cover)

woody.sw <- sw(woody.yearchar.wide)
noquote(apply(woody.sw, 2, normality))

herb <- woody.yearchar %>% 
  filter(woody == "Herbaceous")
herb$Year <- gsub("-.*", "", herb$Year)
woody <- woody.yearchar %>% 
  filter(woody == "Woody")
woody$Year <- gsub("-.*", "", woody$Year)

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

herb.channel.nov <- woody.all %>% 
  group_by(Channel, Year, woody) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(woody == "Herbaceous") %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(herb.channel.nov)) {
  if(herb.channel.nov$Year[i] == "2012-11-01") {
    herb.channel.nov$Year[i] <- "2012-01-01"
  } else if(herb.channel.nov$Year[i] == "2013-11-01") {
    herb.channel.nov$Year[i] <- "2013-01-01"
  } else if(herb.channel.nov$Year[i] == "2014-11-01") {
    herb.channel.nov$Year[i] <- "2014-01-01"
  } else if(herb.channel.nov$Year[i] == "2015-11-01") {
    herb.channel.nov$Year[i] <- "2015-01-01"
  } else if(herb.channel.nov$Year[i] == "2018-11-01") {
    herb.channel.nov$Year[i] <- "2018-01-01"
  } else {
    herb.channel.nov$Year[i] <- "2021-01-01"
  }
}

herb.channel.nov[ , "channel.trt"] <- NA
for(i in 1:nrow(herb.channel.nov)) {
  if(herb.channel.nov$Channel[i] == "Channel 12") {
    herb.channel.nov$channel.trt[i] <- "Channel 12: No treatment"
  } else if(herb.channel.nov$Channel[i] == "Channel 13") {
    herb.channel.nov$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(herb.channel.nov$Channel[i] == "Channel 19") {
    herb.channel.nov$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    herb.channel.nov$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

herb.plot.nov <- ggplot(herb.channel.nov, aes(x = Year, y = mean, 
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

innat.channel <- innat.all %>% 
  group_by(Channel, Year, Native) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

innat.all.plot <- ggplot(innat.channel, aes(x = Year, y = mean, 
                                            group = Native, color = Native, shape = Native)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive/native (all)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
innat.all.plot



# Invasive/native and woody/herbaceous ------------------------------------

inwood.channel <- inwood.all %>%  
  group_by(Channel, Year, inwood) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")


inwood.all.plot <- ggplot(inwood.channel, aes(x = Year, y = mean, 
                                              group = inwood, color = inwood, shape = inwood)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive and woody status (all)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
inwood.all.plot


inwood.channel.known <- inwood.channel %>% 
  filter(inwood %in% c("Invasive herb", "Native herb", "Native woody"))

inwood.known.all.plot <- ggplot(inwood.channel.known, aes(x = Year, y = mean, 
                                                          group = inwood, color = inwood, shape = inwood)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive and woody status (all)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
inwood.known.all.plot

inwood.channel.known.nov <- inwood.channel.known %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(inwood.channel.known.nov)) {
  if(inwood.channel.known.nov$Year[i] == "2012-11-01") {
    inwood.channel.known.nov$Year[i] <- "2012-01-01"
  } else if(inwood.channel.known.nov$Year[i] == "2013-11-01") {
    inwood.channel.known.nov$Year[i] <- "2013-01-01"
  } else if(inwood.channel.known.nov$Year[i] == "2014-11-01") {
    inwood.channel.known.nov$Year[i] <- "2014-01-01"
  } else if(inwood.channel.known.nov$Year[i] == "2015-11-01") {
    inwood.channel.known.nov$Year[i] <- "2015-01-01"
  } else if(inwood.channel.known.nov$Year[i] == "2018-11-01") {
    inwood.channel.known.nov$Year[i] <- "2018-01-01"
  } else {
    inwood.channel.known.nov$Year[i] <- "2021-01-01"
  }
}

inwood.channel.known.nov[ , "channel.trt"] <- NA
for(i in 1:nrow(inwood.channel.known.nov)) {
  if(inwood.channel.known.nov$Channel[i] == "Channel 12") {
    inwood.channel.known.nov$channel.trt[i] <- "Channel 12: No treatment"
  } else if(inwood.channel.known.nov$Channel[i] == "Channel 13") {
    inwood.channel.known.nov$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(inwood.channel.known.nov$Channel[i] == "Channel 19") {
    inwood.channel.known.nov$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    inwood.channel.known.nov$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}


inwood.channel.known.nov$inwood <- factor(inwood.channel.known.nov$inwood,
                                          levels = c("Native herb", "Invasive herb", "Native woody"))

inwood.known.plot.nov <- ggplot(inwood.channel.known.nov, aes(x = Year, y = mean, 
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
inwood.known.yearchar <- inwood.all %>% 
  filter(Year != "2012-03-01") %>% 
  filter(inwood %in% c("Invasive herb", "Native herb", "Native woody")) %>% 
  ungroup()
inwood.known.yearchar$Year <- as.character(inwood.known.yearchar$Year)
inwood.known.yearchar.wide <- inwood.known.yearchar %>% 
  pivot_wider(names_from = c(Channel, inwood), values_from = Cover)

inwood.known.sw <- sw(inwood.known.yearchar.wide)
noquote(apply(inwood.known.sw, 2, normality))

ivherb <- inwood.known.yearchar %>% 
  filter(inwood == "Invasive herb")
ivherb$Year <- gsub("-.*", "", ivherb$Year)
ntherb <- inwood.known.yearchar %>% 
  filter(inwood == "Native herb")
ntherb$Year <- gsub("-.*", "", ntherb$Year)

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



# Invasive/native and gfst ------------------------------------------------

ingfst.channel <- ingfst.all %>%  
  group_by(Channel, Year, ingfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")


ingfst.all.plot <- ggplot(ingfst.channel, aes(x = Year, y = mean, 
                                              group = ingfst, color = ingfst)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive and functional group (all)") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ingfst.all.plot


ingfst.channel.known <- ingfst.channel %>% 
  filter(ingfst %in% c("Native tree", "Invasive forb", "Invasive grass", "Native forb",
                       "Native grass", "Native shrub"))

ingfst.known.all.plot <- ggplot(ingfst.channel.known, aes(x = Year, y = mean, 
                                                          group = ingfst, color = ingfst, shape = ingfst)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive and woody status (all)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ingfst.known.all.plot


# Invasive/native and gfs -------------------------------------------------

ingfs.channel <- ingfst.all %>%  
  group_by(Channel, Year, ingfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(ingfst != "Native tree")


ingfs.all.plot <- ggplot(ingfs.channel, aes(x = Year, y = mean, 
                                            group = ingfst, color = ingfst)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive and functional group (all)") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ingfst.all.plot


ingfs.channel.known <- ingfs.channel %>% 
  filter(ingfst %in% c("Invasive forb", "Invasive grass", "Native forb",
                       "Native grass", "Native shrub"))

ingfs.known.all.plot <- ggplot(ingfs.channel.known, aes(x = Year, y = mean, 
                                                        group = ingfst, color = ingfst, shape = ingfst)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive and woody status (all)") +
  scale_color_brewer(palette = "Paired") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ingfs.known.all.plot


ingfs.channel.known.nov <- ingfs.channel.known %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(ingfs.channel.known.nov)) {
  if(ingfs.channel.known.nov$Year[i] == "2012-11-01") {
    ingfs.channel.known.nov$Year[i] <- "2012-01-01"
  } else if(ingfs.channel.known.nov$Year[i] == "2013-11-01") {
    ingfs.channel.known.nov$Year[i] <- "2013-01-01"
  } else if(ingfs.channel.known.nov$Year[i] == "2014-11-01") {
    ingfs.channel.known.nov$Year[i] <- "2014-01-01"
  } else if(ingfs.channel.known.nov$Year[i] == "2015-11-01") {
    ingfs.channel.known.nov$Year[i] <- "2015-01-01"
  } else if(ingfs.channel.known.nov$Year[i] == "2018-11-01") {
    ingfs.channel.known.nov$Year[i] <- "2018-01-01"
  } else {
    ingfs.channel.known.nov$Year[i] <- "2021-01-01"
  }
}

ingfs.channel.known.nov[ , "channel.trt"] <- NA
for(i in 1:nrow(ingfs.channel.known.nov)) {
  if(ingfs.channel.known.nov$Channel[i] == "Channel 12") {
    ingfs.channel.known.nov$channel.trt[i] <- "Channel 12: No treatment"
  } else if(ingfs.channel.known.nov$Channel[i] == "Channel 13") {
    ingfs.channel.known.nov$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(ingfs.channel.known.nov$Channel[i] == "Channel 19") {
    ingfs.channel.known.nov$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    ingfs.channel.known.nov$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

ingfs.channel.known.nov$ingfst <- factor(ingfs.channel.known.nov$ingfst,
                                         levels = c("Native grass", "Invasive grass",
                                                    "Native forb", "Invasive forb",
                                                    "Native shrub"))

ingfs.known.plot.nov <- ggplot(ingfs.channel.known.nov, aes(x = Year, y = mean, 
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

ingf.channel.nov <- ingfst.all %>%  
  group_by(Channel, Year, ingfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep") %>% 
  filter(!ingfst %in% c("Native tree", "Native shrub")) %>% 
  filter(Year != "2012-03-01")

for(i in 1:nrow(ingf.channel.nov)) {
  if(ingf.channel.nov$Year[i] == "2012-11-01") {
    ingf.channel.nov$Year[i] <- "2012-01-01"
  } else if(ingf.channel.nov$Year[i] == "2013-11-01") {
    ingf.channel.nov$Year[i] <- "2013-01-01"
  } else if(ingf.channel.nov$Year[i] == "2014-11-01") {
    ingf.channel.nov$Year[i] <- "2014-01-01"
  } else if(ingf.channel.nov$Year[i] == "2015-11-01") {
    ingf.channel.nov$Year[i] <- "2015-01-01"
  } else if(ingf.channel.nov$Year[i] == "2018-11-01") {
    ingf.channel.nov$Year[i] <- "2018-01-01"
  } else {
    ingf.channel.nov$Year[i] <- "2021-01-01"
  }
}

ingf.channel.nov[ , "channel.trt"] <- NA
for(i in 1:nrow(ingf.channel.nov)) {
  if(ingf.channel.nov$Channel[i] == "Channel 12") {
    ingf.channel.nov$channel.trt[i] <- "Channel 12: No treatment"
  } else if(ingf.channel.nov$Channel[i] == "Channel 13") {
    ingf.channel.nov$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(ingf.channel.nov$Channel[i] == "Channel 19") {
    ingf.channel.nov$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    ingf.channel.nov$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

ingfs.channel.known.nov <- ingf.channel.nov %>% 
  filter(ingfst %in% c("Invasive forb", "Invasive grass", "Native forb",
                       "Native grass"))

ingfs.channel.known.nov$ingfst <- factor(ingfs.channel.known.nov$ingfst,
                                         levels = c("Native grass", "Invasive grass",
                                                    "Native forb", "Invasive forb",
                                                    "Native shrub"))

ingf.known.plot.nov <- ggplot(ingfs.channel.known.nov, aes(x = Year, y = mean, 
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



###### ORD only for Ch 13 and 21 ##########################################

# Total plant cover -------------------------------------------------------

# ORD only for Channels 13 and 21
ord.13 <- total.all %>% 
  filter(Channel == "Channel 13" & str_detect(Station, "ORD"))
ord.21 <- total.all %>% 
  filter(Channel == "Channel 21" & str_detect(Station, "ORD"))
ch.12.19 <- total.all %>% 
  filter(Channel %in% c("Channel 12", "Channel 19"))
total.ord <- rbind(ch.12.19, ord.13, ord.21)

# Plot
total.ord.channel <- total.ord %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

total.ord.plot <- ggplot(total.ord.channel, aes(x = Year, y = mean, 
                                                group = Channel, color = Channel, shape = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover (ORD only)") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
total.ord.plot

# Shapiro-Wilk
total.ord.yearchar <- total.ord %>% 
  filter(Year != "2012-03-01") %>% 
  ungroup() %>% 
  filter(Channel %in% c("Channel 13", "Channel 21"))
total.ord.yearchar$Year <- as.character(total.ord.yearchar$Year)
total.ord.yearchar.wide <- total.ord.yearchar %>% 
  pivot_wider(names_from = Channel, values_from = Cover)

total.ord.sw <- sw(total.ord.yearchar.wide)
noquote(apply(total.ord.sw, 2, normality))

# ANOVA
summary(aov(Cover ~ Year, data = filter(total.ord.yearchar, Channel == "Channel 13"))) # NS
summary(aov(Cover ~ Year, data = filter(total.ord.yearchar, Channel == "Channel 21"))) # NS



# Ground cover ------------------------------------------------------------

# ORD only for Channels 13 and 21
ord.13 <- ground.all %>% 
  filter(Channel == "Channel 13" & str_detect(Station, "ORD"))
ord.21 <- ground.all %>% 
  filter(Channel == "Channel 21" & str_detect(Station, "ORD"))
ch.12.19 <- ground.all %>% 
  filter(Channel %in% c("Channel 12", "Channel 19"))
ground.ord <- rbind(ch.12.19, ord.13, ord.21)

ground.ord.channel <- ground.ord %>% 
  group_by(Channel, Year, Common) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

ground.ord.plot <- ggplot(ground.ord.channel, 
                          aes(x = Year, y = mean, 
                              group = Common, color = Common, shape = Common)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Ground cover (ORD only)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ground.ord.plot


ground.ord.channel.grs <- ground.ord.channel %>% 
  filter(Common %in% c("Gravel", "Rock", "Soil"))

ground.ord.grs.plot <- ggplot(ground.ord.channel.grs, aes(x = Year, y = mean, 
                                                          group = Common, color = Common, shape = Common)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Ground cover (ORD only)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ground.ord.grs.plot

# Shapiro-Wilk for gravel, rock, soil
ground.ord.yearchar <- ground.ord %>% 
  filter(Year != "2012-03-01") %>% 
  ungroup() %>% 
  filter(Channel %in% c("Channel 13", "Channel 21"))
ground.ord.yearchar$Year <- as.character(ground.ord.yearchar$Year)
ground.ord.yearchar <- ground.ord.yearchar %>% 
  filter(Common %in% c("Gravel", "Rock", "Soil"))
ground.ord.yearchar.wide <- ground.ord.yearchar %>% 
  pivot_wider(names_from = c(Channel, Common), values_from = Cover)

ground.ord.sw <- sw(ground.ord.yearchar.wide)
noquote(apply(ground.ord.sw, 2, normality))

soil.ord <- ground.ord.yearchar %>% 
  filter(Common == "Soil")

# Soil: ANOVA
summary(aov(Cover ~ Year, data = filter(soil.ord, Channel == "Channel 13"))) 
soil.ord13 <- soil.ord %>% 
  filter(Channel == "Channel 13")
anova.soil.ord13 <- aov(soil.ord13$Cover ~ soil.ord13$Year)
hsd.soil.ord13 <- HSD.test(anova.soil.ord13, trt = "soil.ord13$Year")
hsd.soil.ord13

summary(aov(Cover ~ Year, data = filter(soil.ord, Channel == "Channel 21")))
soil.ord21 <- soil.ord %>% 
  filter(Channel == "Channel 21")
anova.soil.ord21 <- aov(soil.ord21$Cover ~ soil.ord21$Year)
hsd.soil.ord21 <- HSD.test(anova.soil.ord21, trt = "soil.ord21$Year")
hsd.soil.ord21



# Functional group (as collected) -----------------------------------------

# ORD only for Channels 13 and 21
ord.13 <- fungr.all %>% 
  filter(Channel == "Channel 13" & str_detect(Station, "ORD"))
ord.21 <- fungr.all %>% 
  filter(Channel == "Channel 21" & str_detect(Station, "ORD"))
ch.12.19 <- fungr.all %>% 
  filter(Channel %in% c("Channel 12", "Channel 19"))
fungr.ord <- rbind(ch.12.19, ord.13, ord.21)

# Plot
fungr.ord.channel <- fungr.ord %>% 
  group_by(Channel, Year, Functional) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

fungr.ord.plot <- ggplot(fungr.ord.channel, aes(x = Year, y = mean, 
                                                group = Functional, color = Functional, shape = Functional)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by functional group (ORD only)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
fungr.ord.plot



# Functional group (gfst) -------------------------------------------------

# ORD only for Channels 13 and 21
ord.13 <- gfst.all %>% 
  filter(Channel == "Channel 13" & str_detect(Station, "ORD"))
ord.21 <- gfst.all %>% 
  filter(Channel == "Channel 21" & str_detect(Station, "ORD"))
ch.12.19 <- gfst.all %>% 
  filter(Channel %in% c("Channel 12", "Channel 19"))
gfst.ord <- rbind(ch.12.19, ord.13, ord.21)

# Plot
gfst.ord.channel <- gfst.ord %>% 
  group_by(Channel, Year, gfst) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

gfst.ord.plot <- ggplot(gfst.ord.channel, 
                        aes(x = Year, y = mean,
                            group = gfst, color = gfst, shape = gfst)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by functional group (ORD only)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
gfst.ord.plot


# Woody/herbaceous --------------------------------------------------------

# ORD only for Channels 13 and 21
ord.13 <- woody.all %>% 
  filter(Channel == "Channel 13" & str_detect(Station, "ORD"))
ord.21 <- woody.all %>% 
  filter(Channel == "Channel 21" & str_detect(Station, "ORD"))
ch.12.19 <- woody.all %>% 
  filter(Channel %in% c("Channel 12", "Channel 19"))
woody.ord <- rbind(ch.12.19, ord.13, ord.21)

# Plot
woody.ord.channel <- woody.ord %>% 
group_by(Channel, Year, woody) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

woody.ord.plot <- ggplot(woody.ord.channel, aes(x = Year, y = mean, 
                                        group = woody, color = woody, shape = woody)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by woody/herbaceous (ORD only)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
woody.ord.plot

# Shapiro-Wilk
woody.ord.yearchar <- woody.ord %>% 
  filter(Year != "2012-03-01") %>% 
  ungroup() %>% 
  filter(Channel %in% c("Channel 13", "Channel 21"))
woody.ord.yearchar$Year <- as.character(woody.ord.yearchar$Year)
woody.ord.yearchar.wide <- woody.ord.yearchar %>% 
  pivot_wider(names_from = c(Channel, woody), values_from = Cover)

woody.ord.sw <- sw(woody.ord.yearchar.wide)
noquote(apply(woody.ord.sw, 2, normality))

herb.ord <- woody.ord.yearchar %>% 
  filter(woody == "Herbaceous")
herb.ord$Year <- gsub("-.*", "", herb.ord$Year)
wood.ord <- woody.ord.yearchar %>% 
  filter(woody == "Woody")
woody.ord$Year <- gsub("-.*", "", woody.ord$Year)

# Herbaceous: ANOVA
summary(aov(Cover ~ Year, data = filter(herb.ord, Channel == "Channel 21")))
herb.ord21 <- herb.ord %>% 
  filter(Channel == "Channel 21")
anova.herb.ord21 <- aov(herb.ord21$Cover ~ herb.ord21$Year)
hsd.herb.ord21 <- HSD.test(anova.herb.ord21, trt = "herb.ord21$Year")
hsd.herb.ord21 # why are all the groups a?

# Herbaceous: Kruskal-Wallis
herb.ord13 <- herb.ord %>% 
  filter(Channel == "Channel 13")
kruskal.test(Cover ~ Year, data = herb.ord13)
dt.herb.ord13 <- dunnTest(herb.ord13$Cover ~ herb.ord13$Year, method = "bh")
dt.herb.ord13 <- dt.herb.ord13$res
cldList(comparison = dt.herb.ord13$Comparison,
        p.value    = dt.herb.ord13$P.adj,
        threshold  = 0.05) # b is biggest
herb.ord13.letters <- herb.ord13 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(Cover),
            .groups = "keep") %>% 
  arrange(desc(mean))
herb.ord13.letters$groups <- c("a", "a", "ab", "ab", "ab", "b")


# Invasive/native ---------------------------------------------------------

# ORD only for Channels 13 and 21
ord.13 <- innat.all %>% 
  filter(Channel == "Channel 13" & str_detect(Station, "ORD"))
ord.21 <- innat.all %>% 
  filter(Channel == "Channel 21" & str_detect(Station, "ORD"))
ch.12.19 <- innat.all %>% 
  filter(Channel %in% c("Channel 12", "Channel 19"))
innat.ord <- rbind(ch.12.19, ord.13, ord.21)

# Plot
innat.ord.channel <- innat.ord %>% 
  group_by(Channel, Year, Native) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

innat.ord.plot <- ggplot(innat.ord.channel, aes(x = Year, y = mean, 
                                                group = Native, color = Native, shape = Native)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive/native (ORD only)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
innat.ord.plot


# Invasive/native and woody/herbaceous ------------------------------------

# ORD only for Channels 13 and 21
ord.13 <- inwood.all %>% 
  filter(Channel == "Channel 13" & str_detect(Station, "ORD"))
ord.21 <- inwood.all %>% 
  filter(Channel == "Channel 21" & str_detect(Station, "ORD"))
ch.12.19 <- inwood.all %>% 
  filter(Channel %in% c("Channel 12", "Channel 19"))
inwood.ord <- rbind(ch.12.19, ord.13, ord.21)

# Plot
inwood.ord.channel <- inwood.ord %>% 
  group_by(Channel, Year, inwood) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

inwood.ord.plot <- ggplot(inwood.ord.channel, 
                          aes(x = Year, y = mean, 
                              group = inwood, color = inwood, shape = inwood)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive and woody status (ORD only)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
inwood.ord.plot


inwood.ord.channel.known <- inwood.ord.channel %>% 
  filter(inwood %in% c("Invasive herb", "Native herb", "Native woody"))

inwood.ord.known.plot <- ggplot(inwood.ord.channel.known, 
                                aes(x = Year, y = mean, 
                                    group = inwood, color = inwood, shape = inwood)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by invasive and woody status (ORD only)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
inwood.ord.known.plot


# Save data ---------------------------------------------------------------

save.image("Cover by year and channel.RData")

