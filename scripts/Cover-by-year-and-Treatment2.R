library(tidyverse)
library(agricolae)
library(plotrix)
library(car)

# Load data ---------------------------------------------------------------

precip <- read.table("data/PimaCounty_precip/PimaCounty_precip_2012-2021.txt",
                     sep = "\t", header = TRUE)
precip$year.xaxis <- as.Date(precip$year.xaxis)
precip_join <- precip %>% 
  select(year.xaxis, Precip_cum)

total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")




# Functions ---------------------------------------------------------------

# Add year as date and character, retain Nov samples only, add Treatment2 col
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
  
  x$Treatment2 <- gsub("^.*?: ", "", x$channel.trt)
  
  x <- x %>% 
    mutate(Treatment2 = case_when(
      Treatment2 == "No treatment" ~ "Control",
      TRUE ~ Treatment2))
  
  x <- left_join(x, precip_join)
  
  return(x)
}


# Data wrangling ----------------------------------------------------------

total.all <- year(total.all)


# Total plant cover -------------------------------------------------------

# Find averages by year
total.avg <- total.all %>% 
  group_by(Treatment2, Year, year.date, year.xaxis) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

write.csv(total.avg,
          file = "data/cleaned/Treatment2-average_total-cover.csv",
          row.names = FALSE)

# Plot
total.plot <- ggplot(total.avg, aes(x = year.xaxis, y = mean, 
                                        group = Treatment2, 
                                        color = Treatment2)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Treatment2) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
total.plot

# ANOVA
summary(aov(Cover ~ Treatment2 * Year, data = total.all))
anova.total <- aov(Cover ~ Treatment2 * Year, data = total.all)
Anova(anova.total, type = "III")
TukeyHSD(anova.total, which = "Treatment2")

# Correlation with precipitation
plot(Cover ~ Precip_cum, data = total.all)





