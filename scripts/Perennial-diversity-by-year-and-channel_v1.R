library(tidyverse)
library(vegan)
library(plotrix)
library(agricolae)
library(FSA)
library(rcompanion)

# Load data ---------------------------------------------------------------

plant.all <- read.csv("data/cleaned/Summarised-all_plant-species-cover.csv")

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

# Exclude March sampling with missing data sheets, and annual plants
  # annuals were not always sampled by species every year
plant.nov.per <- plant.all %>% 
  filter(Year != "2012-03-01") %>% 
  filter(!str_detect(Functional, "Annual"))



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


# Richness ----------------------------------------------------------------

# By channel and station
richness <- plant.nov.per %>%  
  group_by(Channel, Station, Year, year.date, year.xaxis, 
           channel.trt, station.trt) %>% 
  summarise(rich = n_distinct(Common),
            .groups = "keep")

# Channel summarised (stations averaged)
richness.channel <- richness %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt) %>% 
  summarise(mean = mean(rich),
            SD = sd(rich),
            SE = std.error(rich),
            .groups = "keep") 

# Plot by accurate date
richness.plot <- ggplot(richness.channel, aes(x = year.date, y = mean, 
                                              group = Channel, color = Channel, 
                                              shape = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Species no.") +
  ggtitle("Perennial species richness") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
richness.plot

# Plot with year aligned on x-axis
richness.plot.fw <- ggplot(richness.channel, aes(x = year.xaxis, y = mean, 
                                                 group = channel.trt, 
                                                 color = channel.trt)) +
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
richness.wide <- richness %>% 
  ungroup() %>% 
  pivot_wider(names_from = Channel, values_from = rich)

richness.sw <- sw(richness.wide)
noquote(apply(richness.sw, 2, normality))

richness$Year <- gsub("-.*", "", richness$Year)
richness$Year <- as.factor(richness$Year)

# ANOVA
summary(aov(rich ~ Year, data = filter(richness, Channel == "Channel 12"))) 
      # not sure why this is significant when none of the groups are different 
        # from each other
richness12 <- richness %>% 
  filter(Channel == "Channel 12")
anova.richness12 <- aov(richness12$rich ~ richness12$Year)
hsd.richness12 <- HSD.test(anova.richness12, trt = "richness12$Year")
hsd.richness12 # why are they all a?
TukeyHSD(anova.richness12) # there really are no differences between groups

summary(aov(rich ~ Year, data = filter(richness, Channel == "Channel 13")))
richness13 <- richness %>% 
  filter(Channel == "Channel 13")
anova.richness13 <- aov(richness13$rich ~ richness13$Year)
hsd.richness13 <- HSD.test(anova.richness13, trt = "richness13$Year")
hsd.richness13

# Kruskal-Wallis
richness19 <- richness %>% 
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

kruskal.test(rich ~ Year, data = filter(richness, Channel == "Channel 21")) # NS


# Plot with year aligned on x-axis and ANOVA/KW letters added
letters <- data.frame(label = c(hsd.richness13$groups[ , "groups"],
                                richness19.letters$groups),
                      channel.trt = c(rep("Channel 13: In-channel treatment", 6),
                                      rep("Channel 19: Upland treatment", 6)),
                      x = rep(richness.channel$year.xaxis[1:6], 2),
                      y = c(9.3, 8.2, 8.2, 9, 10.6, 9,
                            10.9, 9.5, 9.5, 10.1, 8.2, 8.1))

anova.kw.lab <- data.frame(label = c("ANOVA", "Kruskal-Wallis"),
                           channel.trt = c("Channel 13: In-channel treatment", 
                                           "Channel 19: Upland treatment"),
                           x = c(rep(as.Date("2020-01-01"), 2)),
                           y = c(10.5, 10.5))

richness.plot.letters <- ggplot(richness.channel, aes(x = year.xaxis, y = mean, 
                                                      group = channel.trt, 
                                                      color = channel.trt)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Number of species") +
  ggtitle("Perennial species richness") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~channel.trt) +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  geom_text(data = anova.kw.lab,
            mapping = aes(x = x, y = y, label = label),
            size = 3, color = "black")
richness.plot.letters

pdf("output_figs/Perennial-richness.pdf", width = 7, height = 5)
richness.plot.letters
dev.off()



# Shannon diversity -------------------------------------------------------

# By channel and station
shannon <- plant.nov.per %>%  
  group_by(Channel, Station, Year, year.date, year.xaxis, 
           channel.trt, station.trt) %>% 
  summarise(shan = diversity(Cover),
            .groups = "keep")

# Channel summarised (stations averaged)
shannon.channel <- shannon %>% 
  group_by(Channel, Year, year.date, year.xaxis, channel.trt) %>% 
  summarise(mean = mean(shan),
            SD = sd(shan),
            SE = std.error(shan),
            .groups = "keep") 

# Plot by accurate date
shannon.plot <- ggplot(shannon.channel, aes(x = year.date, y = mean, 
                                            group = Channel, color = Channel, 
                                            shape = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial diversity") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
shannon.plot

# Plot with year aligned on x-axis
shannon.plot.fw <- ggplot(shannon.channel, aes(x = year.xaxis, y = mean, 
                                               group = channel.trt, 
                                               color = channel.trt)) +
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
shannon.wide <- shannon %>% 
  ungroup() %>% 
  pivot_wider(names_from = Channel, values_from = shan)

shannon.sw <- sw(shannon.wide)
noquote(apply(shannon.sw, 2, normality))

shannon$Year <- gsub("-.*", "", shannon$Year)
shannon$Year <- as.factor(shannon$Year)

# ANOVA
summary(aov(shan ~ Year, data = filter(shannon, Channel == "Channel 12"))) # NS
summary(aov(shan ~ Year, data = filter(shannon, Channel == "Channel 13"))) # NS
summary(aov(shan ~ Year, data = filter(shannon, Channel == "Channel 21"))) # NS

# Kruskal-Wallis
kruskal.test(shan ~ Year, data = filter(shannon, Channel == "Channel 19")) # NS

# No statistical letters to add; write plot to pdf
pdf("output_figs/Perennial-Shannon.pdf", width = 7, height = 5)
shannon.plot.fw
dev.off()


# Save data ---------------------------------------------------------------

# Revert Year to YYYY-MM-DD
richness <- richness %>% 
  ungroup() %>% 
  mutate(Year = year.date) %>% 
  select(-year.date, -year.xaxis)
shannon <- shannon %>% 
  ungroup() %>% 
  mutate(Year = year.date) %>% 
  select(-year.date, -year.xaxis)
per.diversity <- richness %>% 
  left_join(shannon)

# Add back upland treatment and in-channel treatment dummy variables
per.diversity[ , "up.trt"] <- NA
for(i in 1:nrow(per.diversity)) {
  if(per.diversity$Channel[i] == "Channel 19") {
    per.diversity$up.trt[i] <- 1
  } else {
    per.diversity$up.trt[i] <- 0
  }
}

per.diversity[ , "inch.trt"] <- NA
for(i in 1:nrow(per.diversity)) {
  if(per.diversity$Channel[i] == "Channel 21") {
    per.diversity$inch.trt[i] <- 1
  } else if(per.diversity$Channel[i] == "Channel 13") {
    per.diversity$inch.trt[i] <- 1
  } else {
    per.diversity$inch.trt[i] <- 0
  }
}

write.csv(per.diversity,
          file = "data/cleaned/All-Nov_perennial-diversity.csv",
          row.names = FALSE)


save.image("RData/Perennial-diversity-by-year-and-channel.RData")
