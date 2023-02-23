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

# Add year as date and character, retain Nov samples only, add Treatment3 col
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
  
  x$Treatment3 <- gsub("^.*?: ", "", x$channel.trt)
  
  x <- x %>% 
    mutate(Treatment3 = case_when(
      Treatment3 == "No treatment" ~ "Control",
      Treatment3 == "Upland treatment" ~ "Control",
      TRUE ~ Treatment3))
  
  return(x)
}


# Data wrangling ----------------------------------------------------------

total.all <- year(total.all)


# Total plant cover -------------------------------------------------------

# Find averages by year
total.avg <- total.all %>% 
  group_by(Treatment3, Year, year.date, year.xaxis) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

write.csv(total.avg,
          file = "data/cleaned/Treatment3-average_total-cover.csv",
          row.names = FALSE)

# Plot
total.plot <- ggplot(total.avg, aes(x = year.xaxis, y = mean, 
                                    group = Treatment3, 
                                    color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  scale_color_manual(values = c("red", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
total.plot

# Two-factor ANOVA
summary(aov(Cover ~ Treatment3 * Year, data = total.all))
anova.total <- aov(Cover ~ Treatment3 * Year, data = total.all)
# Treatment3        1   7910    7910  12.156 0.000551 ***
# Year              5   9274    1855   2.850 0.015416 *  
# Treatment3:Year   5   9656    1931   2.968 0.012231 *  
TukeyHSD(anova.total, which = "Treatment3")
#                                        p adj
# In-channel treatment-Control           0.000551
TukeyHSD(anova.total, which = "Year")


# One-way ANOVA for In-channel
summary(aov(Cover ~ Year, data = filter(total.all, Treatment3 == "In-channel treatment"))) # NS

# One-way ANOVA for Control
summary(aov(Cover ~ Year, data = filter(total.all, Treatment3 == "Control")))
total.ctrl <- total.all |> 
  filter(Treatment3 == "Control")
anova.total.ctrl <- aov(total.ctrl$Cover ~ total.ctrl$Year)
hsd.total.ctrl <- HSD.test(anova.total.ctrl, trt = "total.ctrl$Year")
hsd.total.ctrl

# Correlation with precipitation
total.all <- left_join(total.all, precip_join)
plot(Cover ~ Precip_cum, data = total.all)
summary(lm(Cover ~ Precip_cum, data = total.all))


# 2012-2015 precipitation
(6.46 - 10.98) / 10.98 # 41% decrease

# 2-factor ANOVA, 2012-2015
total.12.15 <- total.all %>% 
  filter(Year %in% c("2012", "2013", "2014", "2015"))
summary(aov(Cover ~ Treatment3 * Year, data = total.12.15))
anova.total.12.15 <- aov(Cover ~ Treatment3 * Year, data = total.12.15)
TukeyHSD(anova.total.12.15, which = "Treatment3") # p = 0.0001197
TukeyHSD(anova.total.12.15, which = "Year")

# One-way ANOVA, 2012-2015
summary(aov(Cover ~ Year, data = filter(total.12.15, Treatment3 == "In-channel treatment"))) # NS

summary(aov(Cover ~ Year, data = filter(total.12.15, Treatment3 == "Control"))) # NS
total.12.15.ctrl <- total.12.15 |> 
  filter(Treatment3 == "Control")
anova.total.12.15.ctrl <- aov(total.12.15.ctrl$Cover ~ total.12.15.ctrl$Year)
hsd.total.12.15.ctrl <- HSD.test(anova.total.12.15.ctrl, trt = "total.12.15.ctrl$Year")
hsd.total.12.15.ctrl
# 2013               61.16528      a
# 2012               59.25694      a
# 2014               51.68333     ab
# 2015               39.35282      b


save.image("RData/Cover-by-year-and-Treatment3.RData")
