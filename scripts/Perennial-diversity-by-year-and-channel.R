library(tidyverse)
library(vegan)
library(plotrix)
library(agricolae)
library(FSA)
library(rcompanion)

# Load data ---------------------------------------------------------------

load("plant.all.RData")

# Exclude March sampling with missing data sheets, and annual plants
  # annuals were not always sampled by species every year
plant.all.nov <- plant.all %>% 
  filter(Year != "2012-03-01") %>% 
  filter(!str_detect(Functional, "Annual"))



# Functions ---------------------------------------------------------------

normality <- function(x) {
  ifelse(x > 0.05, "yes", "no")
}

sw <- function(dat) {
  dat.2012 <- dat %>% 
    filter(Year == "2012-11-01") %>% 
    select(-Station, -Station, -Year)
  dat.2012 <- lapply(dat.2012, shapiro.test)
  dat.2012 <- sapply(dat.2012, `[`, "p.value")
  dat.2012 <- as.data.frame(dat.2012)
  
  dat.2013 <- dat %>% 
    filter(Year == "2013-11-01") %>% 
    select(-Station, -Year)
  dat.2013 <- lapply(dat.2013, shapiro.test)
  dat.2013 <- sapply(dat.2013, `[`, "p.value")
  dat.2013 <- as.data.frame(dat.2013)
  
  dat.2014 <- dat %>% 
    filter(Year == "2014-11-01") %>% 
    select(-Station, -Year)
  dat.2014 <- lapply(dat.2014, shapiro.test)
  dat.2014 <- sapply(dat.2014, `[`, "p.value")
  dat.2014 <- as.data.frame(dat.2014)
  
  dat.2015 <- dat %>% 
    filter(Year == "2015-11-01") %>% 
    select(-Station, -Year)
  dat.2015 <- lapply(dat.2015, shapiro.test)
  dat.2015 <- sapply(dat.2015, `[`, "p.value")
  dat.2015 <- as.data.frame(dat.2015)
  
  dat.2018 <- dat %>% 
    filter(Year == "2018-11-01") %>% 
    select(-Station, -Year)
  dat.2018 <- lapply(dat.2018, shapiro.test)
  dat.2018 <- sapply(dat.2018, `[`, "p.value")
  dat.2018 <- as.data.frame(dat.2018)
  
  dat.2021 <- dat %>% 
    filter(Year == "2021-11-01") %>% 
    select(-Station, -Year)
  dat.2021 <- lapply(dat.2021, shapiro.test)
  dat.2021 <- sapply(dat.2021, `[`, "p.value")
  dat.2021 <- as.data.frame(dat.2021)
  
  dat.sum <- rbind(dat.2012, dat.2013, dat.2014, dat.2015, dat.2018, dat.2021)
  rownames(dat.sum) <- c("2012", "2013", "2014", "2015", "2018", "2021")
  return(dat.sum)
}


###### Entire channel averages ############################################

# Richness ----------------------------------------------------------------

# By channel and station
richness <- plant.all.nov %>%  
  group_by(Year, Station, Channel) %>% 
  summarise(rich = n_distinct(Common),
            .groups = "keep")

# Channel summarised (stations averaged)
richness.channel <- richness %>% 
  group_by(Year, Channel) %>% 
  summarise(mean = mean(rich),
            SD = sd(rich),
            SE = std.error(rich),
            .groups = "keep") 

# Plot
richness.plot <- ggplot(richness.channel, aes(x = Year, y = mean, 
                                        group = Channel, color = Channel, shape = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Species no.") +
  ggtitle("Perennial species richness (all)") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
richness.plot

richness.channel.year <- richness.channel
for(i in 1:nrow(richness.channel.year)) {
  if(richness.channel.year$Year[i] == "2012-11-01") {
    richness.channel.year$Year[i] <- "2012-01-01"
  } else if(richness.channel.year$Year[i] == "2013-11-01") {
    richness.channel.year$Year[i] <- "2013-01-01"
  } else if(richness.channel.year$Year[i] == "2014-11-01") {
    richness.channel.year$Year[i] <- "2014-01-01"
  } else if(richness.channel.year$Year[i] == "2015-11-01") {
    richness.channel.year$Year[i] <- "2015-01-01"
  } else if(richness.channel.year$Year[i] == "2018-11-01") {
    richness.channel.year$Year[i] <- "2018-01-01"
  } else {
    richness.channel.year$Year[i] <- "2021-01-01"
  }
}

richness.channel.year[ , "channel.trt"] <- NA
for(i in 1:nrow(richness.channel.year)) {
  if(richness.channel.year$Channel[i] == "Channel 12") {
    richness.channel.year$channel.trt[i] <- "Channel 12: No treatment"
  } else if(richness.channel.year$Channel[i] == "Channel 13") {
    richness.channel.year$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(richness.channel.year$Channel[i] == "Channel 19") {
    richness.channel.year$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    richness.channel.year$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

richness.plot.fw <- ggplot(richness.channel.year, aes(x = Year, y = mean, 
                                                    group = channel.trt, color = channel.trt)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Number of species") +
  ggtitle("Perennial species richness") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~channel.trt)
richness.plot.fw

# Shapiro-Wilk
richness.yearchar <- richness %>% 
  ungroup()
richness.yearchar$Year <- as.character(richness.yearchar$Year)
richness.yearchar.wide <- richness.yearchar %>% 
  pivot_wider(names_from = Channel, values_from = rich)

richness.sw <- sw(richness.yearchar.wide)
noquote(apply(richness.sw, 2, normality))

richness.yearchar$Year <- gsub("-.*", "", richness.yearchar$Year)

# ANOVA
summary(aov(rich ~ Year, data = filter(richness.yearchar, Channel == "Channel 12")))
richness12.yearchar <- richness.yearchar %>% 
  filter(Channel == "Channel 12")
anova.richness12 <- aov(richness12.yearchar$rich ~ richness12.yearchar$Year)
hsd.richness12 <- HSD.test(anova.richness12, trt = "richness12.yearchar$Year")
hsd.richness12 # why are they all a?

summary(aov(rich ~ Year, data = filter(richness.yearchar, Channel == "Channel 13")))
richness13.yearchar <- richness.yearchar %>% 
  filter(Channel == "Channel 13")
anova.richness13 <- aov(richness13.yearchar$rich ~ richness13.yearchar$Year)
hsd.richness13 <- HSD.test(anova.richness13, trt = "richness13.yearchar$Year")
hsd.richness13

# Kruskal-Wallis
richness19 <- richness.yearchar %>% 
  filter(Channel == "Channel 19")
kruskal.test(rich ~ Year, data = richness19)
dt.richness19 <- dunnTest(richness19$rich ~ richness19$Year, method = "bh")
dt.richness19 <- dt.richness19$res
cldList(comparison = dt.richness19$Comparison,
        p.value    = dt.richness19$P.adj,
        threshold  = 0.05)
richness19.letters <- richness19 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(rich),
            .groups = "keep") %>% 
  arrange(desc(mean))
richness19.letters$groups <- c("a", "ab", "ab", "ab", "b", "b")

richness21 <- richness.yearchar %>% 
  filter(Channel == "Channel 21")
kruskal.test(rich ~ Year, data = richness21)
dt.richness21 <- dunnTest(richness21$rich ~ richness21$Year, method = "bh")
dt.richness21 <- dt.richness21$res
cldList(comparison = dt.richness21$Comparison,
        p.value    = dt.richness21$P.adj,
        threshold  = 0.05)
richness21.letters <- richness21 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(rich),
            .groups = "keep") %>% 
  arrange(desc(mean))
richness21.letters$groups <- c("a", "ab", "ab", "ab", "ab", "b")



# Shannon diversity -------------------------------------------------------

# By channel and station
shannon <- plant.all.nov %>%  
  group_by(Year, Station, Channel) %>% 
  summarise(shan = diversity(Cover),
            .groups = "keep")

# Channel summarised (stations averaged)
shannon.channel <- shannon %>% 
  group_by(Year, Channel) %>% 
  summarise(mean = mean(shan),
            SD = sd(shan),
            SE = std.error(shan),
            .groups = "keep") 

# Plot
shannon.plot <- ggplot(shannon.channel, aes(x = Year, y = mean, 
                                      group = Channel, color = Channel, shape = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial diversity (all)") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
shannon.plot

shannon.channel.year <- shannon.channel
for(i in 1:nrow(shannon.channel.year)) {
  if(shannon.channel.year$Year[i] == "2012-11-01") {
    shannon.channel.year$Year[i] <- "2012-01-01"
  } else if(shannon.channel.year$Year[i] == "2013-11-01") {
    shannon.channel.year$Year[i] <- "2013-01-01"
  } else if(shannon.channel.year$Year[i] == "2014-11-01") {
    shannon.channel.year$Year[i] <- "2014-01-01"
  } else if(shannon.channel.year$Year[i] == "2015-11-01") {
    shannon.channel.year$Year[i] <- "2015-01-01"
  } else if(shannon.channel.year$Year[i] == "2018-11-01") {
    shannon.channel.year$Year[i] <- "2018-01-01"
  } else {
    shannon.channel.year$Year[i] <- "2021-01-01"
  }
}

shannon.channel.year[ , "channel.trt"] <- NA
for(i in 1:nrow(shannon.channel.year)) {
  if(shannon.channel.year$Channel[i] == "Channel 12") {
    shannon.channel.year$channel.trt[i] <- "Channel 12: No treatment"
  } else if(shannon.channel.year$Channel[i] == "Channel 13") {
    shannon.channel.year$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(shannon.channel.year$Channel[i] == "Channel 19") {
    shannon.channel.year$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    shannon.channel.year$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

shannon.plot.fw <- ggplot(shannon.channel.year, aes(x = Year, y = mean, 
                                                  group = channel.trt, color = channel.trt)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial plant diversity") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~channel.trt)
shannon.plot.fw

# Shapiro-Wilk
shannon.yearchar <- shannon %>% 
  ungroup()
shannon.yearchar$Year <- as.character(shannon.yearchar$Year)
shannon.yearchar.wide <- shannon.yearchar %>% 
  pivot_wider(names_from = Channel, values_from = shan)

shannon.sw <- sw(shannon.yearchar.wide)
noquote(apply(shannon.sw, 2, normality))

shannon.yearchar$Year <- gsub("-.*", "", shannon.yearchar$Year)

# ANOVA
summary(aov(shan ~ Year, data = filter(shannon.yearchar, Channel == "Channel 12")))
summary(aov(shan ~ Year, data = filter(shannon.yearchar, Channel == "Channel 13")))
summary(aov(shan ~ Year, data = filter(shannon.yearchar, Channel == "Channel 21")))

# Kruskal-Wallis
kruskal.test(shan ~ Year, data = filter(shannon.yearchar, Channel == "Channel 19"))



###### ORD only for Ch 13 and 21 ##########################################


# Richness ----------------------------------------------------------------

# ORD only for Channels 13 and 21
ord.13 <- plant.all.nov %>% 
  filter(Channel == "Channel 13" & str_detect(Station, "ORD"))
ord.21 <- plant.all.nov %>% 
  filter(Channel == "Channel 21" & str_detect(Station, "ORD"))
ch.12.19 <- plant.all.nov %>% 
  filter(Channel %in% c("Channel 12", "Channel 19"))
richness.ord <- rbind(ch.12.19, ord.13, ord.21)

# By channel and station
richness.ord <- richness.ord %>%  
  group_by(Year, Station, Channel) %>% 
  summarise(rich = n_distinct(Common),
            .groups = "keep")

# Channel summarised (stations averaged)
richness.ord.channel <- richness.ord %>% 
  group_by(Year, Channel) %>% 
  summarise(mean = mean(rich),
            SD = sd(rich),
            SE = std.error(rich),
            .groups = "keep") 

# Plot
richness.ord.plot <- ggplot(richness.ord.channel, aes(x = Year, y = mean, 
                                                      group = Channel, color = Channel, shape = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Species no.") +
  ggtitle("Perennial species richness (ORD only)") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
richness.ord.plot

# Shapiro-Wilk
richness.ord.yearchar <- richness.ord %>% 
  ungroup()
richness.ord.yearchar$Year <- as.character(richness.ord.yearchar$Year)
richness.ord.yearchar.wide <- richness.ord.yearchar %>% 
  pivot_wider(names_from = Channel, values_from = rich)

richness.ord.sw <- sw(richness.ord.yearchar.wide)
noquote(apply(richness.ord.sw, 2, normality))

richness.ord.yearchar$Year <- gsub("-.*", "", richness.ord.yearchar$Year)

# ANOVA
summary(aov(rich ~ Year, data = filter(richness.ord.yearchar, Channel == "Channel 12")))
richness.ord12.yearchar <- richness.ord.yearchar %>% 
  filter(Channel == "Channel 12")
anova.richness.ord12 <- aov(richness.ord12.yearchar$rich ~ richness.ord12.yearchar$Year)
hsd.richness.ord12 <- HSD.test(anova.richness.ord12, trt = "richness.ord12.yearchar$Year")
hsd.richness.ord12 # why are all the groups a?

summary(aov(rich ~ Year, data = filter(richness.ord.yearchar, Channel == "Channel 13")))
richness.ord13.yearchar <- richness.ord.yearchar %>% 
  filter(Channel == "Channel 13")
anova.richness.ord13 <- aov(richness.ord13.yearchar$rich ~ richness.ord13.yearchar$Year)
hsd.richness.ord13 <- HSD.test(anova.richness.ord13, trt = "richness.ord13.yearchar$Year")
hsd.richness.ord13


# Kruskal-Wallis
richness.ord19 <- richness.ord.yearchar %>% 
  filter(Channel == "Channel 19")
kruskal.test(rich ~ Year, data = richness.ord19)
dt.richness.ord19 <- dunnTest(richness.ord19$rich ~ richness.ord19$Year, method = "bh")
dt.richness.ord19 <- dt.richness.ord19$res
cldList(comparison = dt.richness.ord19$Comparison,
        p.value    = dt.richness.ord19$P.adj,
        threshold  = 0.05)
richness.ord19.letters <- richness.ord19 %>% 
  group_by(Channel, Year) %>% 
  summarise(mean = mean(rich),
            .groups = "keep") %>% 
  arrange(desc(mean))
richness.ord19.letters$groups <- c("a", "ab", "ab", "ab", "b", "b")

kruskal.test(rich ~ Year, data = filter(richness.ord.yearchar, Channel == "Channel 21"))



# Shannon diversity -------------------------------------------------------

# By channel and station
shannon.ord <- plant.all.nov %>%  
  group_by(Year, Station, Channel) %>% 
  summarise(shan = diversity(Cover),
            .groups = "keep")

# Channel summarised (stations averaged)
shannon.ord.channel <- shannon.ord %>% 
  group_by(Year, Channel) %>% 
  summarise(mean = mean(shan),
            SD = sd(shan),
            SE = std.error(shan),
            .groups = "keep") 

# Plot
shannon.ord.plot <- ggplot(shannon.ord.channel, aes(x = Year, y = mean, 
                                                    group = Channel, color = Channel, shape = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial plant diversity (ORD only)") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
shannon.ord.plot

# Shapiro-Wilk
shannon.ord.yearchar <- shannon.ord %>% 
  ungroup()
shannon.ord.yearchar$Year <- as.character(shannon.ord.yearchar$Year)
shannon.ord.yearchar.wide <- shannon.ord.yearchar %>% 
  pivot_wider(names_from = Channel, values_from = shan)

shannon.ord.sw <- sw(shannon.ord.yearchar.wide)
noquote(apply(shannon.ord.sw, 2, normality))

shannon.ord.yearchar$Year <- gsub("-.*", "", shannon.ord.yearchar$Year)

# ANOVA
summary(aov(shan ~ Year, data = filter(shannon.ord.yearchar, Channel == "Channel 12")))
summary(aov(shan ~ Year, data = filter(shannon.ord.yearchar, Channel == "Channel 13")))
summary(aov(shan ~ Year, data = filter(shannon.ord.yearchar, Channel == "Channel 21")))

# Kruskal-Wallis
kruskal.test(shan ~ Year, data = shannon.ord19)



save.image(("Perennial diversity by year and channel.RData"))
