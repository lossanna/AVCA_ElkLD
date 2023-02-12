library(tidyverse)
library(agricolae)
library(plotrix)
library(car)

# Load data ---------------------------------------------------------------

precip <- read.table("data/PimaCounty_precip/PimaCounty_precip_2012-2021.txt",
                     sep = "\t", header = TRUE)
precip$year.xaxis <- as.Date(precip$year.xaxis)
precip_join <- precip[1:6, ] %>% 
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

# Two-factor ANOVA
summary(aov(Cover ~ Treatment2 * Year, data = total.all))
anova.total <- aov(Cover ~ Treatment2 * Year, data = total.all)
Anova(anova.total, type = "III")
# Treatment2        5734   2  4.6298   0.01037 *  
# Year              5468   5  1.7662   0.11911    
# Treatment2:Year  24527  10  3.9608 3.891e-05 ***
TukeyHSD(anova.total, which = "Treatment2")
#                                        p adj
# In-channel treatment-Control           0.0154913
# Upland treatment-Control               0.9979668
# Upland treatment-In-channel treatment  0.0072213
TukeyHSD(anova.total, which = "Year")


# One-way ANOVA for In-channel
summary(aov(Cover ~ Year, data = filter(total.all, Treatment2 == "In-channel treatment"))) # NS

# Correlation with precipitation
total.all <- left_join(total.all, precip_join)
plot(Cover ~ Precip_cum, data = total.all)
summary(lm(Cover ~ Precip_cum, data = total.all))


# 2012-2015 precipitation
(6.46 - 10.98) / 10.98 # 41% decrease

# 2-factor ANOVA, 2012-2015
total.12.15 <- total.all %>% 
  filter(Year %in% c("2012", "2013", "2014", "2015"))
summary(aov(Cover ~ Treatment2 * Year, data = total.12.15))
anova.total.12.15 <- aov(Cover ~ Treatment2 * Year, data = total.12.15)
Anova(anova.total.12.15, type = "III")
# Treatment2        5734   2  5.3900  0.005156 ** 
# Year              5261   3  3.2967  0.021254 *  
# Treatment2:Year  17188   6  5.3858 3.162e-05 ***


save.image("RData/Cover-by-year-and-Treatment2.RData")
