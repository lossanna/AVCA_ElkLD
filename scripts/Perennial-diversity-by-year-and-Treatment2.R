library(tidyverse)
library(vegan)
library(plotrix)
library(agricolae)

# Load data ---------------------------------------------------------------

plant.all <- read.csv("data/cleaned/Summarised-all_plant-species-cover.csv")


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
  
  x$Treatment2 <- gsub("^.*?: ", "", x$channel.trt)
  
  x <- x %>% 
    mutate(Treatment2 = case_when(
      Treatment2 == "No treatment" ~ "Control",
      TRUE ~ Treatment2
    ))
  
  return(x)
}

plant.per <- year(plant.all)


# Richness ----------------------------------------------------------------

# By treatment and station
richness <- plant.per %>%  
  group_by(Channel, Station, Treatment2, Year, year.date, year.xaxis, 
           channel.trt, station.trt) %>% 
  summarise(rich = n_distinct(Common),
            .groups = "keep")

# Channel summarised (stations averaged)
richness.avg <- richness %>% 
  group_by(Treatment2, Year, year.date, year.xaxis) %>% 
  summarise(mean = mean(rich),
            SD = sd(rich),
            SE = std.error(rich),
            .groups = "keep") 

write.csv(richness.avg,
          file = "data/cleaned/Treatment2-average_richness.csv",
          row.names = FALSE)

# Plot
richness.plot <- ggplot(richness.avg, aes(x = year.xaxis, y = mean, 
                                              group = Treatment2, 
                                              color = Treatment2)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Number of species") +
  ggtitle("Perennial species richness") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~Treatment2)
richness.plot

# ANOVA 
summary(aov(rich ~ Treatment2 * Year, data = richness))
anova.rich <- aov(rich ~ Treatment2 * Year, data = richness)
Anova(anova.rich, type = "III")
TukeyHSD(anova.rich, which = "Treatment2")
TukeyHSD(anova.rich, which = "Year")
TukeyHSD(anova.rich, which = "Treatment2:Year")



# Shannon diversity -------------------------------------------------------

# By channel and station
shannon <- plant.per %>%  
  group_by(Channel, Station, Treatment2, Year, year.date, year.xaxis, 
           channel.trt, station.trt) %>% 
  summarise(shan = diversity(Cover),
            .groups = "keep")

# Channel summarised (stations averaged)
shannon.avg <- shannon %>% 
  group_by(Treatment2, Year, year.date, year.xaxis, channel.trt) %>% 
  summarise(mean = mean(shan),
            SD = sd(shan),
            SE = std.error(shan),
            .groups = "keep") 

write.csv(shannon.avg,
          file = "data/cleaned/Treatment2-average_Shannon.csv",
          row.names = FALSE)

# Plot 
shannon.plot <- ggplot(shannon.avg, aes(x = year.xaxis, y = mean, 
                                            group = Treatment2, 
                                            color = Treatment2)) +
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
  facet_wrap(~Treatment2)
shannon.plot

# ANOVA
summary(aov(shan ~ Treatment2 * Year, data = shannon))
anova.shan <- aov(shan ~ Treatment2 * Year, data = shannon)
Anova(anova.shan, type = "III") # NS



