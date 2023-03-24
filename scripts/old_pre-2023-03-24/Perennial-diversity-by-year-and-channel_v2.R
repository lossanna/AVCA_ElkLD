library(tidyverse)
library(vegan)
library(plotrix)
library(agricolae)

# Load data ---------------------------------------------------------------

plant.all <- read.csv("data/cleaned/old-summarised/Summarised-all_plant-species-cover.csv")


# Data wrangling ----------------------------------------------------------

# Add year as date and character, and retain Nov samples for perennial species only
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
    filter(year.xaxis != "2012-03-01") %>% 
    filter(!str_detect(Functional, "Annual"))
  x$Year <- as.factor(gsub("-.*", "", x$Year))
  
  return(x)
}

plant.per <- year(plant.all)


# Richness ----------------------------------------------------------------

# By channel and station
richness <- plant.per %>%  
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

write.csv(richness.channel,
          file = "data/cleaned/old-summarised/Channel-average_richness.csv",
          row.names = FALSE)

# Plot
richness.plot <- ggplot(richness.channel, aes(x = year.xaxis, y = mean, 
                                                 group = channel.trt, 
                                                 color = channel.trt)) +
  geom_line(linewidth = 1) +
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
richness.plot

# ANOVA C12
summary(aov(rich ~ Year, data = filter(richness, Channel == "Channel 12"))) 
      # not sure why this is significant when none of the groups are different 
        # from each other
richness12 <- richness %>% 
  filter(Channel == "Channel 12")
anova.richness12 <- aov(richness12$rich ~ richness12$Year)
hsd.richness12 <- HSD.test(anova.richness12, trt = "richness12$Year")
hsd.richness12 # why are they all a?
TukeyHSD(anova.richness12) # there really are no differences between groups

# ANOVA C13
summary(aov(rich ~ Year, data = filter(richness, Channel == "Channel 13")))
richness13 <- richness %>% 
  filter(Channel == "Channel 13")
anova.richness13 <- aov(richness13$rich ~ richness13$Year)
hsd.richness13 <- HSD.test(anova.richness13, trt = "richness13$Year")
hsd.richness13
richness13.letters <- hsd.richness13$groups
richness13.letters <- richness13.letters %>% 
  mutate(Year = rownames(richness13.letters)) %>% 
  arrange(Year)

# ANOVA C19
summary(aov(rich ~ Year, data = filter(richness, Channel == "Channel 19")))
richness19 <- richness %>% 
  filter(Channel == "Channel 19")
anova.richness19 <- aov(richness19$rich ~ richness19$Year)
hsd.richness19 <- HSD.test(anova.richness19, trt = "richness19$Year")
hsd.richness19
richness19.letters <- hsd.richness19$groups
richness19.letters <- richness19.letters %>% 
  mutate(Year = rownames(richness19.letters)) %>% 
  arrange(Year)

# ANOVA C21
summary(aov(rich ~ Year, data = filter(richness, Channel == "Channel 21")))
richness21 <- richness %>% 
  filter(Channel == "Channel 21")
anova.richness21 <- aov(richness21$rich ~ richness21$Year)
hsd.richness21 <- HSD.test(anova.richness21, trt = "richness21$Year")
hsd.richness21
richness21.letters <- hsd.richness21$groups
richness21.letters <- richness21.letters %>% 
  mutate(Year = rownames(richness21.letters)) %>% 
  arrange(Year)

# Plot with letters added
letters <- data.frame(label = c(richness13.letters$groups,
                                richness19.letters$groups,
                                richness21.letters$groups),
                      channel.trt = c(rep("Channel 13: In-channel treatment", 6),
                                      rep("Channel 19: Upland treatment", 6),
                                      rep("Channel 21: In-channel treatment", 6)),
                      x = rep(richness.channel$year.xaxis[1:6], 3),
                      y = c(9.3, 8.2, 8.2, 9, 10.6, 9,
                            10.9, 9.5, 9.5, 10.1, 8.2, 8.1,
                            7.6, 7.9, 9.8, 8.5, 9.7, 6.9))

anova.lab <- data.frame(label = c(rep("ANOVA", 3)),
                           channel.trt = c("Channel 13: In-channel treatment", 
                                           "Channel 19: Upland treatment",
                                           "Channel 21: In-channel treatment"),
                           x = c(rep(as.Date("2020-01-01"), 3)),
                           y = c(10.5, 10.5, 10.5))

richness.plot.letters <- ggplot(richness.channel, aes(x = year.xaxis, y = mean, 
                                                      group = channel.trt, 
                                                      color = channel.trt)) +
  geom_line(linewidth = 1) +
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
  geom_text(data = anova.lab,
            mapping = aes(x = x, y = y, label = label),
            size = 3, color = "black")
richness.plot.letters




# Shannon diversity -------------------------------------------------------

# By channel and station
shannon <- plant.per %>%  
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

write.csv(shannon.channel,
          file = "data/cleaned/old-summarised/Channel-average_Shannon.csv",
          row.names = FALSE)

# Plot 
shannon.plot <- ggplot(shannon.channel, aes(x = year.xaxis, y = mean, 
                                               group = channel.trt, 
                                               color = channel.trt)) +
  geom_line(linewidth = 1) +
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
shannon.plot

# ANOVA
summary(aov(shan ~ Year, data = filter(shannon, Channel == "Channel 12"))) # NS
summary(aov(shan ~ Year, data = filter(shannon, Channel == "Channel 13"))) # NS
summary(aov(shan ~ Year, data = filter(shannon, Channel == "Channel 19"))) # NS
summary(aov(shan ~ Year, data = filter(shannon, Channel == "Channel 21"))) # NS



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
          file = "data/cleaned/old-summarised/All-Nov_perennial-diversity.csv",
          row.names = FALSE)


save.image("RData/old_pre-2023-03-24/Perennial-diversity-by-year-and-channel_v2.RData")
