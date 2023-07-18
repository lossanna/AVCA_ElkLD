# Purpose: Run for all response variables (total cover, herb cover, notree cover, richness, Shannon):
#   (1) one-way repeated measures ANOVA to compare Treated vs. Control,
#   (2) two-factor ANOVA with a Treatment3*Year interaction term to compare overall change over time,
#   (3) one-way ANOVA for Control and Treated separately to compare year-to-year change.

# Includes plots and ANOVA tests.
#   Repeat-measures ANOVA compares Control vs. Treated directly, and accounts for repeat measures
#     on the same plots over multiple years. This is more methodologically correct, but doesn't quite
#     answer as interesting of a question (it doesn't matter so much inherently how the channels started
#     out, but rather how they react to change over time).
#   Two-way ANOVA with Treatment*Year interaction compares how Treated vs. Control changed over time,
#     and does not account for repeat measures on the same plots. Cannot run check_model() on repeat ANOVA,
#     but can for linear models with Year as random factor. Ran linear models with Year as random factor
#     with two packages, because lme4 allows you to use check_model(), and nlme allows you to use emmeans().
#   One-way ANOVA for Control & Treated separately shows change over time, and does not them directly
#     or account for repeat measures on same plots. This is probably the most intuitive interpretation of line graphs.

# Will probably go with two-way ANOVA because it answers a more interesting/relevant question, even though
#   it doesn't account for repeat measures.
# check_model() for all two-way ANOVAs look good.

# Created: 2023-03-27
# Last updated: 2023-07-14

library(tidyverse)
library(agricolae)
library(plotrix)
library(car)
library(lme4)
library(nlme)
library(performance)
library(emmeans)
library(ggpubr)

# Load data ---------------------------------------------------------------

total.all <- read_csv("data/cleaned/Summarised-all_total-plant-cover.csv")
herb.all <- read_csv("data/cleaned/Summarised-all_herb-cover.csv") 
notree.all <- read_csv("data/cleaned/Summarised-all_notree-cover.csv")
annual.all <- read_csv("data/cleaned/Summarised-all_annual-cover.csv")
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
notree.all <- convert.cols(notree.all)
annual.all <- convert.cols(annual.all)
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
ggplot(total.avg, aes(x = year.xaxis, y = mean, 
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


# Repeat measures ANOVA
summary(aov(Cover ~ Treatment3 + Error(Year), data = total.all))
summary(aov(Cover ~ Treatment3, data = total.all)) # without repeat measures to compare

# Year as random factor
lm.total <- lmer(Cover ~ Treatment3 + (1|Year), data = total.all)
check_model(lm.total)
Anova(lm.total)

lm2.total <- lme(Cover ~ Treatment3, random = ~1|Year, data = total.all)
emmeans(lm2.total, specs = "Treatment3") # Control > Treated


# Two-factor ANOVA
summary(aov(Cover ~ Treatment3 * Year, data = total.all))
#                  Df Sum Sq Mean Sq F value   Pr(>F) 
# Treatment3:Year   5   9193    1839   2.817 0.016465 *  
# Based on visual estimate, Control > Treated for change over time (Treatment3:Year)
check_model(aov(Cover ~ Treatment3 * Year, data = total.all))


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

letters.total <- data.frame(x = total.avg$year.xaxis[1:6],
                      y = rep(67, 6),
                      label = total.ctrl.letters$groups,
                      Treatment3 = c(rep("Control", 6)))
total.plot <- ggplot(total.avg, aes(x = year.xaxis, y = mean, 
                     group = Treatment3, 
                     color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_text(data = letters.total,
            mapping = aes(x = x, y = y, label = label),
            color = "black")
total.plot



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
ggplot(herb.avg, aes(x = year.xaxis, y = mean, 
                     group = Treatment3, 
                     color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE),  linewidth = 0.8) +
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous cover") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 


# Repeat measures ANOVA
summary(aov(Cover ~ Treatment3 + Error(Year), data = herb.all)) # p = 0.000221
summary(aov(Cover ~ Treatment3, data = herb.all)) # without repeat measures to compare

# Year as random factor
lm.herb <- lmer(Cover ~ Treatment3 + (1|Year), data = herb.all)
check_model(lm.herb)
Anova(lm.herb)
summary(lm.herb)

lm2.herb <- lme(Cover ~ Treatment3, random = ~1|Year, data = herb.all)
emmeans(lm2.herb, specs = "Treatment3") # Control > Treated


# Two-factor ANOVA
summary(aov(Cover ~ Treatment3 * Year, data = herb.all))
# Treatment3:Year   5   2281   456.2   3.366 0.005529 ** 
# Based on visual estimate, Treated probably changed more over time than Control?
check_model(aov(Cover ~ Treatment3 * Year, data = herb.all))


# One-way ANOVA for Treated
summary(aov(Cover ~ Year, data = filter(herb.all, Treatment3 == "Treated"))) 
herb.trt <- herb.all |> 
  filter(Treatment3 == "Treated")
anova.herb.trt <- aov(herb.trt$Cover ~ herb.trt$Year) # p = 9.86e-10 ***
hsd.herb.trt <- HSD.test(anova.herb.trt, trt = "herb.trt$Year")
hsd.herb.trt
# 2018      24.489919      a
# 2021      22.201613     ab
# 2014      15.139113     bc
# 2015      12.368952     cd
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

letters.herb <- data.frame(x = rep(herb.avg$year.xaxis[1:6], 2),
                      y = rep(28, 12),
                      label = c(herb.ctrl.letters$groups,
                                herb.trt.letters$groups),
                      Treatment3 = c(rep("Control", 6),
                                     rep("Treated", 6)))
herb.plot <- ggplot(herb.avg, aes(x = year.xaxis, y = mean, 
                     group = Treatment3, 
                     color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous cover") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_text(data = letters.herb,
            mapping = aes(x = x, y = y, label = label),
            color = "black")
herb.plot



# Notree cover (total cover minus trees) ----------------------------------

# Find averages by year
notree.avg <- notree.all %>% 
  group_by(Treatment3, Year, year.xaxis) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

write.csv(notree.avg,
          file = "data/cleaned/Treatment3-average_notree-cover.csv",
          row.names = FALSE)

# Plot
ggplot(notree.avg, aes(x = year.xaxis, y = mean,
                       group = Treatment3, 
                       color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Grass, forb & shrub cover") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 


# Repeat measures ANOVA
summary(aov(Cover ~ Treatment3 + Error(Year), data = notree.all)) # NS
summary(aov(Cover ~ Treatment3, data = notree.all)) # without repeat measures to compare

# Year as random factor
lm.notree <- lmer(Cover ~ Treatment3 + (1|Year), data = notree.all)
check_model(lm.notree)
Anova(lm.notree) # NS

lm2.notree <- lme(Cover ~ Treatment3, random = ~1|Year, data = notree.all)
emmeans(lm2.total, specs = "Treatment3") 


# Two-factor ANOVA
summary(aov(Cover ~ Treatment3 * Year, data = notree.all))
# Treatment3:Year   5   5532  1106.4   3.471  0.00447 ** 
# Based on visual estimate, Control probably changed more over time than Treated?
check_model(aov(Cover ~ Treatment3 * Year, data = notree.all))


# One-way ANOVA for Treated
summary(aov(Cover ~ Year, data = filter(notree.all, Treatment3 == "Treated"))) # p = 0.00304
notree.trt <- notree.all |> 
  filter(Treatment3 == "Treated")
anova.notree.trt <- aov(notree.trt$Cover ~ notree.trt$Year)
hsd.notree.trt <- HSD.test(anova.notree.trt, trt = "notree.trt$Year")
hsd.notree.trt
# 2018         41.69960      a
# 2021         37.47581     ab
# 2014         31.40927    abc
# 2015         30.33669    abc
# 2012         26.72446     bc
# 2013         21.80029      c

# One-way ANOVA for Control
summary(aov(Cover ~ Year, data = filter(notree.all, Treatment3 == "Control")))
notree.ctrl <- notree.all |> 
  filter(Treatment3 == "Control")
anova.notree.ctrl <- aov(notree.ctrl$Cover ~ notree.ctrl$Year)
hsd.notree.ctrl <- HSD.test(anova.notree.ctrl, trt = "notree.ctrl$Year")
hsd.notree.ctrl
# 2021          42.41935      a
# 2012          31.73194     ab
# 2014          31.58750     ab
# 2018          27.71774      b
# 2013          27.71250      b
# 2015          20.89315      b

# Plot with one-way ANOVA letters
notree.ctrl.letters <- hsd.notree.ctrl$groups
notree.ctrl.letters <- notree.ctrl.letters |> 
  mutate(Year = rownames(notree.ctrl.letters)) |> 
  arrange(Year)
notree.trt.letters <- hsd.notree.trt$groups
notree.trt.letters <- notree.trt.letters |> 
  mutate(Year = rownames(notree.trt.letters)) |> 
  arrange(Year)

letters.notree <- data.frame(x = rep(notree.avg$year.xaxis[1:6], 2),
                      y = rep(47, 12),
                      label = c(notree.ctrl.letters$groups,
                                notree.trt.letters$groups),
                      Treatment3 = c(rep("Control", 6),
                                     rep("Treated", 6)))
notree.plot <- ggplot(notree.avg, aes(x = year.xaxis, y = mean, 
                       group = Treatment3, 
                       color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Grass, forb & shrub cover, 2012-2021") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_text(data = letters.notree,
            mapping = aes(x = x, y = y, label = label),
            color = "black")  +
  theme(axis.text.x = element_text(color = "black")) +
  theme(plot.margin = margin(0.1, 0, 0.25, 0, "in")) 
notree.plot

tiff("figures/2023-07_draft-figures/temporal-ANOVA_notree-cover.tiff", width = 8, height = 4, units = "in", res = 150)
notree.plot
dev.off()



# Annual cover ------------------------------------------------------------

# Find averages by year
annual.avg <- annual.all %>% 
  group_by(Treatment3, Year, year.xaxis) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

write.csv(annual.avg,
          file = "data/cleaned/Treatment3-average_annual-cover.csv",
          row.names = FALSE)

# Plot
ggplot(annual.avg, aes(x = year.xaxis, y = mean,
                       group = Treatment3, 
                       color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Annual cover") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 



# Compare veg cover change ------------------------------------------------

ggarrange(total.plot, herb.plot, notree.plot,
          ncol = 2, nrow = 2)


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
ggplot(rich.avg, aes(x = year.xaxis, y = mean, 
                     group = Treatment3, 
                     color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("No. of species") +
  ggtitle("Perennial richness") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") 


# Repeat measures ANOVA
summary(aov(rich ~ Treatment3 + Error(Year), data = per.div)) # p = 7.65e-06
summary(aov(rich ~ Treatment3, data = per.div)) # without repeat measures to compare

# Year as random factor
lm.rich <- lmer(rich ~ Treatment3 + (1|Year), data = per.div)
check_model(lm.rich)
Anova(lm.rich)

lm2.rich <- lme(rich ~ Treatment3, random = ~1|Year, data = per.div)
emmeans(lm2.rich, specs = "Treatment3") # Control > Treated


# Two-factor ANOVA
summary(aov(rich ~ Treatment3 * Year, data = per.div))
# Treatment3:Year   5   76.7   15.33   2.434   0.0346 *   
# Based on visual estimate, Control probably changed more over time than Treated?
check_model(aov(rich ~ Treatment3 * Year, data = per.div))


# One-way ANOVA for Treated
summary(aov(rich ~ Year, data = filter(per.div, Treatment3 == "Treated"))) # p = 0.0516

# One-way ANOVA for Control
summary(aov(rich ~ Year, data = filter(per.div, Treatment3 == "Control"))) # 0.00881
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

# Plot with one-way ANOVA letters
rich.ctrl.letters <- hsd.rich.ctrl$groups
rich.ctrl.letters <- rich.ctrl.letters |> 
  mutate(Year = rownames(rich.ctrl.letters)) |> 
  arrange(Year)

letters.rich <- data.frame(x = rich.avg$year.xaxis[1:6],
                      y = rep(10.4, 6),
                      label = rich.ctrl.letters$groups,
                      Treatment3 = rep("Control", 6))
rich.plot <- ggplot(rich.avg, aes(x = year.xaxis, y = mean, 
                     group = Treatment3, 
                     color = Treatment3)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("No. of species") +
  ggtitle("Perennial plant species richness, 2012-2021") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  geom_text(data = letters.rich,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "black")) +
  theme(plot.margin = margin(0, 0, 0.25, 0, "in")) 
rich.plot

tiff("figures/2023-07_draft-figures/temporal-ANOVA_richness.tiff", width = 8, height = 4, units = "in", res = 150)
rich.plot
dev.off()



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
  facet_wrap(~Treatment3) +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial plant diversity, 2012-2021") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "black"))
shan.plot


# Repeat measures ANOVA
summary(aov(shan ~ Treatment3 + Error(Year), data = per.div))
summary(aov(shan ~ Treatment3, data = per.div)) # without repeat measures to compare

# Year as random factor
lm.shan <- lmer(shan ~ Treatment3 + (1|Year), data = per.div) # something weird is happening here
check_model(lm.shan) # something is def not right
summary(lm.shan) # why is there no variance? idk
Anova(lm.shan)

lm2.shan <- lme(shan ~ Treatment3, random = ~1|Year, data = per.div)
emmeans(lm2.shan, specs = "Treatment3")


# Two-factor ANOVA
summary(aov(shan ~ Treatment3 * Year, data = per.div))
# Treatment3:Year   5   0.61  0.1214   0.690 0.63137 
# No significant difference
check_model(aov(shan ~ Treatment3 * Year, data = per.div))


# One-way ANOVA for Treated
summary(aov(shan ~ Year, data = filter(per.div, Treatment3 == "Treated"))) # NS

# One-way ANOVA for Control
summary(aov(shan ~ Year, data = filter(per.div, Treatment3 == "Control"))) # NS


# No letters needed, write out previous plot
tiff("figures/2023-07_draft-figures/temporal-ANOVA_Shannon.tiff", width = 8, height = 4, units = "in", res = 150)
shan.plot
dev.off()



# Combine notree, richness & Shannon plots --------------------------------

tiff("figures/2023-07_draft-figures/temporal-ANOVA_notree-richness-Shannon.tiff", units = "in", height = 12.6, width = 8, res = 150)
ggarrange(notree.plot, rich.plot, shan.plot,
          ncol = 1, nrow = 3,
          labels = c("(A)", "(B)", "(C)")) 

dev.off()



save.image("RData/ANOVA-by-Treatment3_veg-2012-2021.RData")
