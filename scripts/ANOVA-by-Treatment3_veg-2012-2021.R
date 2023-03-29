# Purpose: Run one-way repeated measures ANOVA to compare Treated vs. Control for response variables
#   (total cover, herb cover, richness, Shannon).
# Includes plots and ANOVA tests.

library(tidyverse)
library(agricolae)
library(plotrix)
library(car)
library(lme4)
library(nlme)
library(rstatix)
library(performance)
library(emmeans)

# Load data ---------------------------------------------------------------

total.all <- read_csv("data/cleaned/Summarised-all_total-plant-cover.csv")
herb.all <- read_csv("data/cleaned/Summarised-all_herb-cover.csv") 
per.div <- read_csv("data/cleaned/Summarised-all_perennial-diversity.csv")


# Functions ---------------------------------------------------------------

# Convert columns to factor or date as needed
convert.cols <- function(x) {
  x$year.xaxis <- as.Date(x$year.xaxis)
  
  group.cols <- c("Sample", "Year", "Channel", "Station", "Treatment1", "Treatment2", "Treatment3")
  
  x[group.cols] <- lapply(x[group.cols], factor)
  
  return(x)
}


# Data wrangling ----------------------------------------------------------

total.all <- convert.cols(total.all)
herb.all <- convert.cols(herb.all)
per.div <- convert.cols(per.div)


# Total plant cover -------------------------------------------------------

# Find averages by year
total.avg <- total.all %>% 
  group_by(Treatment3, Year, year.xaxis) %>% 
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
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
total.plot

# Repeat measures ANOVA
anova.total <- aov(Cover ~ Treatment3 + Error(Year), data = total.all)
summary(anova.total)
summary(aov(Cover ~ Treatment3, data = total.all)) # without repeat measures to compare

# Year as random factor
lm.total <- lmer(Cover ~ Treatment3 + (1|Year), data = total.all)
check_model(lm.total)
Anova(lm.total)

lm2.total <- lme(Cover ~ Treatment3, random = ~1|Year, data = total.all)
emmeans(lm2.total, specs = "Treatment3") # Control > Treated

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



# Herbaceous cover --------------------------------------------------------

# Find averages by year
herb.avg <- herb.all %>% 
  group_by(Treatment3, Year, year.xaxis) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

write.csv(herb.avg,
          file = "data/cleaned/Treatment3-average_herb-cover.csv",
          row.names = FALSE)

# Plot
herb.plot <- ggplot(herb.avg, aes(x = year.xaxis, y = mean, 
                                    group = Treatment3, 
                                    color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE),  linewidth = 0.8) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous cover") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
herb.plot

# Repeat measures ANOVA
anova.herb <- aov(Cover ~ Treatment3 + Error(Year), data = herb.all)
summary(anova.herb)
summary(aov(Cover ~ Treatment3, data = herb.all)) # without repeat measures to compare

# Year as random factor
lm.herb <- lmer(Cover ~ Treatment3 + (1|Year), data = herb.all)
check_model(lm.herb)
Anova(lm.herb)

lm2.herb <- lme(Cover ~ Treatment3, random = ~1|Year, data = herb.all)
emmeans(lm2.herb, specs = "Treatment3") # Control > Treated


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



# Richness ----------------------------------------------------------------

# Find averages by year
rich.avg <- per.div %>% 
  group_by(Treatment3, Year, year.xaxis) %>% 
  summarise(mean = mean(rich),
            SD = sd(rich),
            SE = std.error(rich),
            .groups = "keep")

write.csv(rich.avg,
          file = "data/cleaned/Treatment3-average_richness.csv",
          row.names = FALSE)

# Plot
rich.plot <- ggplot(rich.avg, aes(x = year.xaxis, y = mean, 
                                  group = Treatment3, 
                                  color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("No. of species") +
  ggtitle("Perennial richness") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
rich.plot

# Repeat measures ANOVA
anova.rich <- aov(rich ~ Treatment3 + Error(Year), data = per.div)
summary(anova.rich) # p = 7.65e-06
summary(aov(rich ~ Treatment3, data = per.div)) # without repeat measures to compare

# Year as random factor
lm.rich <- lmer(rich ~ Treatment3 + (1|Year), data = per.div)
check_model(lm.rich)
Anova(lm.rich)

lm2.rich <- lme(rich ~ Treatment3, random = ~1|Year, data = per.div)
emmeans(lm2.rich, specs = "Treatment3") # Control > Treated

# One-way ANOVA for Treated
summary(aov(rich ~ Year, data = filter(per.div, Treatment3 == "Treated"))) 
rich.trt <- per.div |> 
  filter(Treatment3 == "Treated")
anova.rich.trt <- aov(rich.trt$rich ~ rich.trt$Year)
hsd.rich.trt <- HSD.test(anova.rich.trt, trt = "rich.trt$Year")
hsd.rich.trt
# 2018      8.900000      a
# 2014      7.741935     ab
# 2015      7.466667     ab
# 2012      7.322581     ab
# 2013      6.827586      b
# 2021      6.677419      b

# One-way ANOVA for Control
summary(aov(rich ~ Year, data = filter(per.div, Treatment3 == "Control")))
rich.ctrl <- per.div |> 
  filter(Treatment3 == "Control")
anova.rich.ctrl <- aov(rich.ctrl$rich ~ rich.ctrl$Year)
hsd.rich.ctrl <- HSD.test(anova.rich.ctrl, trt = "rich.ctrl$Year")
hsd.rich.ctrl
# 2012       9.866667      a
# 2013       9.133333     ab
# 2018       8.645161     ab
# 2015       8.612903     ab
# 2014       8.200000     ab
# 2021       7.580645      b




# Shannon -----------------------------------------------------------------

# Find averages by year
shan.avg <- per.div %>% 
  group_by(Treatment3, Year, year.xaxis) %>% 
  summarise(mean = mean(shan),
            SD = sd(shan),
            SE = std.error(shan),
            .groups = "keep")

write.csv(shan.avg,
          file = "data/cleaned/Treatment3-average_shannon.csv",
          row.names = FALSE)

# Plot
shan.plot <- ggplot(shan.avg, aes(x = year.xaxis, y = mean, 
                                  group = Treatment3, 
                                  color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial diversity") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 
shan.plot

# Repeat measures ANOVA
anova.shan <- aov(shan ~ Treatment3 + Error(Year), data = per.div)
summary(anova.shan)
summary(aov(shan ~ Treatment3, data = per.div)) # without repeat measures to compare

# Year as random factor
lm.shan <- lmer(shan ~ Treatment3 + (1|Year), data = per.div) # something weird is happening here
check_model(lm.shan) # something is def not right
summary(lm.shan) # why is there no variance? idk
Anova(lm.shan)

lm2.shan <- lme(shan ~ Treatment3, random = ~1|Year, data = per.div)
emmeans(lm2.shan, specs = "Treatment3")


# One-way ANOVA for Treated
summary(aov(shan ~ Year, data = filter(per.div, Treatment3 == "Treated"))) # NS

# One-way ANOVA for Control
summary(aov(shan ~ Year, data = filter(per.div, Treatment3 == "Control"))) # NS


save.image("RData/ANOVA-by-Treatment3_veg-2012-2021.RData")
