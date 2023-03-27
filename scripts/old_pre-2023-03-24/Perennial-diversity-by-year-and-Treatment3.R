# Purpose: Plot and analysis for ANOVA comparisons by Treatment3 (Treated/Control)
#   for perennial richness and Shannon diversity
# Old analysis, includes: 
#   one-way ANOVA for Control and Treated separately
#   two-factor ANOVA
# Analysis discontinued because one-way ANOVA violates independence of repeated measures,
#   and two-factor ANOVA has Year as a fixed factor


library(tidyverse)
library(vegan)
library(plotrix)
library(agricolae)

# Load data ---------------------------------------------------------------

per.div <- read.csv("data/cleaned/old-summarised/Summarised-all_perennial-diversity.csv") |> 
  mutate(year.xaxis = as.Date(year.xaxis),
         Year = factor(Year),
         Treatment3 = case_when(
           station.trt == "Baffle" ~ "Treated",
           station.trt == "One rock dam" ~ "Treated",
           station.trt == "No treatment" ~ "Control"))



# Richness ----------------------------------------------------------------

# Channel summarised (stations averaged)
richness.avg <- per.div %>% 
  group_by(Treatment3, Year, year.date, year.xaxis) %>% 
  summarise(mean = mean(rich),
            SD = sd(rich),
            SE = std.error(rich),
            .groups = "keep") 

write.csv(richness.avg,
          file = "data/cleaned/old-summarised/Treatment3-average_richness.csv",
          row.names = FALSE)

# Plot
richness.plot <- ggplot(richness.avg, aes(x = year.xaxis, y = mean, 
                                          group = Treatment3, 
                                          color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Number of species") +
  ggtitle("Perennial species richness") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~Treatment3)
richness.plot

# Two-factor ANOVA 
summary(aov(rich ~ Treatment3 * Year, data = per.div))
# Treatment3        1  126.6  126.55  21.217 5.73e-06 ***
# Year              5  101.0   20.21   3.388  0.00529 ** 
# Treatment3:Year   5   87.6   17.52   2.937  0.01300 * 
anova.rich <- aov(rich ~ Treatment3 * Year, data = per.div)
TukeyHSD(anova.rich, which = "Treatment3") # p = 5.7e-06

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


# Shannon diversity -------------------------------------------------------

# Channel summarised (stations averaged)
shannon.avg <- per.div %>% 
  group_by(Treatment3, Year, year.date, year.xaxis) %>% 
  summarise(mean = mean(shan),
            SD = sd(shan),
            SE = std.error(shan),
            .groups = "keep") 

write.csv(shannon.avg,
          file = "data/cleaned/old-summarised/Treatment3-average_Shannon.csv",
          row.names = FALSE)

# Plot 
shannon.plot <- ggplot(shannon.avg, aes(x = year.xaxis, y = mean, 
                                        group = Treatment3, 
                                        color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial plant diversity") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~Treatment3)
shannon.plot

# Two-factor ANOVA 
summary(aov(shan ~ Treatment3 * Year, data = per.div))
# Treatment3        1   0.97  0.9683   5.740 0.0171 *
# Year              5   0.31  0.0616   0.365 0.8722  
# Treatment3:Year   5   0.81  0.1617   0.959 0.4431 

# One-way ANOVA for Treated
summary(aov(shan ~ Year, data = filter(per.div, Treatment3 == "Treated"))) # NS

# One-way ANOVA for Control
summary(aov(shan ~ Year, data = filter(per.div, Treatment3 == "Control"))) # NS


save.image("RData/old_pre-2023-03-24/Perennial-diversity-by-year-and-Treatment3.RData")


