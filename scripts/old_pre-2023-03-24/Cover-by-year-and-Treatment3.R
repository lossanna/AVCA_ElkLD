# Purpose: Plot and analysis for ANOVA comparisons by Treatment3 (Treated/Control)
#   for total cover and herbaceous cover
# Old analysis, includes: 
#   one-way ANOVA for Control and Treated separately
#   two-factor ANOVA
#   comparisons of just 2012-2015 (to relate to precipitation)
# Analysis discontinued because one-way ANOVA violates independence of repeated measures,
#   and two-factor ANOVA has Year as a fixed factor


library(tidyverse)
library(agricolae)
library(plotrix)
library(car)
library(lme4)
library(nlme)
library(rstatix)

# Load data ---------------------------------------------------------------

precip <- read.table("data/PimaCounty_precip/PimaCounty_precip_2012-2021.txt",
                     sep = "\t", header = TRUE)
precip$year.xaxis <- as.Date(precip$year.xaxis)
precip_join <- precip[1:6, ] %>% 
  select(year.xaxis, Precip_cum)

total.all <- read.csv("data/cleaned/old-summarised/Summarised-all_total-plant-cover.csv")
herb.all <- read.csv("data/cleaned/old-summarised/Summarised-all_woody-herb-cover.csv") |> 
  filter(woody == "Herbaceous")


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
      Treatment3 == "In-channel treatment" ~ "Treated",
      Treatment3 == "No treatment" ~ "Control",
      Treatment3 == "Upland treatment" ~ "Control",
      TRUE ~ Treatment3)) |> 
    mutate(Channel = factor(Channel),
           Station = factor(Station),
           Treatment3 = factor(Treatment3))
  
  return(x)
}


# Data wrangling ----------------------------------------------------------

total.all <- year(total.all)
herb.all <- year(herb.all)


# Total plant cover -------------------------------------------------------

# Find averages by year
total.avg <- total.all %>% 
  group_by(Treatment3, Year, year.date, year.xaxis) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

write.csv(total.avg,
          file = "data/cleaned/old-summarised/Treatment3-average_total-cover.csv",
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
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
total.plot


# Two-factor ANOVA
summary(aov(Cover ~ Treatment3 * Year, data = total.all))
# Treatment3        1   7910    7910  12.156 0.000551 ***
# Year              5   9274    1855   2.850 0.015416 *  
# Treatment3:Year   5   9656    1931   2.968 0.012231 *  
anova.total <- aov(Cover ~ Treatment3 * Year, data = total.all)
TukeyHSD(anova.total, which = "Treatment3") # p = 0.000551
TukeyHSD(anova.total, which = "Year")


# One-way ANOVA for Treated
summary(aov(Cover ~ Year, data = filter(total.all, Treatment3 == "Treated"))) # NS

# One-way ANOVA for Control
summary(aov(Cover ~ Year, data = filter(total.all, Treatment3 == "Control")))
total.ctrl <- total.all |> 
  filter(Treatment3 == "Control")
anova.total.ctrl <- aov(total.ctrl$Cover ~ total.ctrl$Year)
hsd.total.ctrl <- HSD.test(anova.total.ctrl, trt = "total.ctrl$Year")
hsd.total.ctrl
# 2021         62.87903      a
# 2013         61.16528      a
# 2012         59.25694      a
# 2014         51.68333     ab
# 2018         46.09274     ab
# 2015         39.35282      b

# Plot with one-way ANOVA letters
total.ctrl.letters <- hsd.total.ctrl$groups
total.ctrl.letters <- total.ctrl.letters |> 
  mutate(Year = rownames(total.ctrl.letters)) |> 
  arrange(Year)
letters <- data.frame(x = total.avg$year.xaxis[1:6],
                      y = rep(67, 6),
                      label = total.ctrl.letters$groups,
                      Treatment3 = c(rep("Control", 6)))

total.plot.letters <- ggplot(total.avg, aes(x = year.xaxis, y = mean, 
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
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black")
total.plot.letters


# Correlation with precipitation
total.all <- left_join(total.all, precip_join)
plot(Cover ~ Precip_cum, data = total.all)
summary(lm(Cover ~ Precip_cum + Treatment3, data = total.all))


# 2012-2015 precipitation
(6.46 - 10.98) / 10.98 # 41% decrease

# 2-factor ANOVA, 2012-2015
total.12.15 <- total.all %>% 
  filter(Year %in% c("2012", "2013", "2014", "2015"))
summary(aov(Cover ~ Treatment3 * Year, data = total.12.15))
anova.total.12.15 <- aov(Cover ~ Treatment3 * Year, data = total.12.15)
TukeyHSD(anova.total.12.15, which = "Treatment3") # p = 0.0001197
TukeyHSD(anova.total.12.15, which = "Year")

# One-way ANOVA, 2012-2015 (shows the same thing as all years)
summary(aov(Cover ~ Year, data = filter(total.12.15, Treatment3 == "Treated"))) # NS

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





# Herbaceous cover --------------------------------------------------------

# Find averages by year
herb.avg <- herb.all %>% 
  group_by(Treatment3, Year, year.date, year.xaxis) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

write.csv(herb.avg,
          file = "data/cleaned/old-summarised/Treatment3-average_herb-cover.csv",
          row.names = FALSE)

# Plot
herb.plot <- ggplot(herb.avg, aes(x = year.xaxis, y = mean, 
                                  group = Treatment3, 
                                  color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous cover") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
herb.plot

# Two-factor ANOVA
summary(aov(Cover ~ Treatment3 * Year, data = herb.all))
# Treatment3        1   1917  1917.4  14.104 0.000202 ***
# Year              5   7091  1418.1  10.432 2.37e-09 ***
# Treatment3:Year   5   2319   463.8   3.412 0.005042 ** 
anova.herb <- aov(Cover ~ Treatment3 * Year, data = herb.all)
TukeyHSD(anova.herb, which = "Treatment3") # p = 0.0002022

# One-way ANOVA for Treated
summary(aov(Cover ~ Year, data = filter(herb.all, Treatment3 == "Treated"))) 
herb.trt <- herb.all |> 
  filter(Treatment3 == "Treated")
anova.herb.trt <- aov(herb.trt$Cover ~ herb.trt$Year)
hsd.herb.trt <- HSD.test(anova.herb.trt, trt = "herb.trt$Year")
hsd.herb.trt
# 2018      24.489919      a
# 2021      21.761290     ab
# 2014      15.139113     bc
# 2015      12.781250     cd
# 2012      11.436828     cd
# 2013       6.929598      d

# One-way ANOVA for Control
summary(aov(Cover ~ Year, data = filter(herb.all, Treatment3 == "Control")))
herb.ctrl <- herb.all |> 
  filter(Treatment3 == "Control")
anova.herb.ctrl <- aov(herb.ctrl$Cover ~ herb.ctrl$Year)
hsd.herb.ctrl <- HSD.test(anova.herb.ctrl, trt = "herb.ctrl$Year")
hsd.herb.ctrl
# 2021        26.78629      a
# 2014        22.28333     ab
# 2012        20.03472     ab
# 2018        19.89718     ab
# 2013        17.41528      b
# 2015        14.21169      b


# Plot with one-way ANOVA letters
herb.ctrl.letters <- hsd.herb.ctrl$groups
herb.ctrl.letters <- herb.ctrl.letters |> 
  mutate(Year = rownames(herb.ctrl.letters)) |> 
  arrange(Year)
herb.trt.letters <- hsd.herb.trt$groups
herb.trt.letters <-herb.trt.letters |> 
  mutate(Year = rownames(herb.trt.letters)) |> 
  arrange(Year)
letters <- data.frame(x = rep(herb.avg$year.xaxis[1:6], 2),
                      y = rep(28, 12),
                      label = c(herb.ctrl.letters$groups,
                                herb.trt.letters$groups),
                      Treatment3 = c(rep("Control", 6),
                                     rep("Treated", 6)))
herb.plot.letters <- ggplot(herb.avg, aes(x = year.xaxis, y = mean, 
                                  group = Treatment3, 
                                  color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous cover") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black")
herb.plot.letters


save.image("RData/old_pre-2023-03-24/Cover-by-year-and-Treatment3.RData")
