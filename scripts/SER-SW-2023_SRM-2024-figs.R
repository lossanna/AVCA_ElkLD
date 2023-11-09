# Purpose: Create figures for SER-SW 2023 and SRM 2024 conferences. Need temporal
#   veg graph of just herb and shrub cover (not total).

# Created: 2023-11-09
# Last updated: 2023-11-09

library(tidyverse)
library(agricolae)
library(plotrix)
library(ggpubr)
library(rstatix)

# Load data ---------------------------------------------------------------

herb.all <- read.csv("data/publish/Herb-cover_2012-2021.csv") 
shrub.all <- read.csv("data/publish/Shrub-cover_2012-2021.csv")


# Functions ---------------------------------------------------------------

# Convert columns to factor or date as needed
convert.cols <- function(x) {
  x$year.xaxis <- as.Date(x$year.xaxis)
  
  group.cols <- c("Sample", "Year", "Treatment")
  
  x[group.cols] <- lapply(x[group.cols], factor)
  
  return(x)
}


# Data wrangling ----------------------------------------------------------

herb.all <- convert.cols(herb.all)
shrub.all <- convert.cols(shrub.all)


# Herbaceous cover --------------------------------------------------------

# Find averages by year
herb.avg <- herb.all %>% 
  group_by(Treatment, Year, year.xaxis) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# One-way ANOVA for Control
summary(aov(Cover ~ Year, data = filter(herb.all, Treatment == "Control"))) # 0.00434
herb.ctrl <- herb.all |> 
  filter(Treatment == "Control")
anova.herb.ctrl <- aov(herb.ctrl$Cover ~ herb.ctrl$Year)
hsd.herb.ctrl <- HSD.test(anova.herb.ctrl, trt = "herb.ctrl$Year")
hsd.herb.ctrl$groups

# One-way ANOVA for Treated
summary(aov(Cover ~ Year, data = filter(herb.all, Treatment == "Treated"))) # p = 3.77e-10
herb.trt <- herb.all |> 
  filter(Treatment == "Treated")
anova.herb.trt <- aov(herb.trt$Cover ~ herb.trt$Year)
hsd.herb.trt <- HSD.test(anova.herb.trt, trt = "herb.trt$Year")
hsd.herb.trt$groups

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
                           y = rep(32, 12),
                           label = c(herb.ctrl.letters$groups,
                                     herb.trt.letters$groups),
                           Treatment = c(rep("Control", 6),
                                         rep("Treated", 6)))
ptext.herb <- data.frame(x = rep(as.Date("2019-09-01"), 2),
                         y = c(8, 8),
                         label = c("ANOVA, p = 0.004", "ANOVA, p < 0.001"),
                         Treatment = c("Control", "Treated"))

herb.plot <- ggplot(herb.avg, aes(x = year.xaxis, y = mean, 
                                  group = Treatment, 
                                  color = Treatment)) +
  geom_line() +
  geom_point() +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  facet_wrap(~Treatment) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous cover") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(data = letters.herb,
            mapping = aes(x = x, y = y, label = label),
            color = "black",
            size = 3.5)  +
  geom_text(data = ptext.herb,
            aes(x = x, y = y, label = label),
            color = "gray30",
            size = 2.5) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.2, l = 0.1, "in")) 
herb.plot


letters.herb2t <- data.frame(x = herb.avg$year.xaxis[1:6],
                            y = c(9, 5.5, 12.5, 10, 17, 18),
                            label = herb.trt.letters$groups)
letters.herb2c <- data.frame(x = herb.avg$year.xaxis[1:6],
                            y = c(23, 21, 26.5, 17, 28.5, 31),
                            label = herb.ctrl.letters$groups)

ptext.herb2 <- data.frame(x = as.Date("2019-09-01"),
                          y = 8,
                          label = "ANOVA, p < 0.001")
herb.plot2 <- ggplot(herb.avg, aes(x = year.xaxis, y = mean, 
                                   color = Treatment)) +
  geom_line() +
  geom_point() +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous cover") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_text(data = letters.herb2t,
            mapping = aes(x = x, y = y, label = label),
            color = "black",
            size = 3) +
  geom_text(data = letters.herb2c,
            mapping = aes(x = x, y = y, label = label),
            color = "gray30",
            size = 3,
            fontface = "italic") +
  geom_text(data = ptext.herb2,
            aes(x = x, y = y, label = label),
            color = "gray30",
            size = 2.5) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "in")) +
  theme(legend.title = element_blank())
herb.plot2





# Shrub cover -------------------------------------------------------------

# Find averages by year
shrub.avg <- shrub.all %>% 
  group_by(Treatment, Year, year.xaxis) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# One-way ANOVA for Control
summary(aov(Cover ~ Year, data = filter(shrub.all, Treatment == "Control"))) # p = 0.0112
shrub.ctrl <- shrub.all |> 
  filter(Treatment == "Control")
anova.shrub.ctrl <- aov(shrub.ctrl$Cover ~ shrub.ctrl$Year)
hsd.shrub.ctrl <- HSD.test(anova.shrub.ctrl, trt = "shrub.ctrl$Year")
hsd.shrub.ctrl$groups

# One-way ANOVA for Treated
summary(aov(Cover ~ Year, data = filter(shrub.all, Treatment == "Treated"))) # NS, p = 0.982


# Plot with one-way ANOVA letters
shrub.ctrl.letters <- hsd.shrub.ctrl$groups
shrub.ctrl.letters <- shrub.ctrl.letters |> 
  mutate(Year = rownames(shrub.ctrl.letters)) |> 
  arrange(Year)

letters.shrub <- data.frame(x = shrub.avg$year.xaxis[1:6],
                            y = rep(21, 6),
                            label = shrub.ctrl.letters$groups,
                            Treatment = rep("Control", 6))
ptext.shrub <- data.frame(x = rep(as.Date("2019-09-01"), 2),
                          y = c(6.5, 6.5),
                          label = c("ANOVA, p = 0.011", "ANOVA, p = 0.982"),
                          Treatment = c("Control", "Treated"))

shrub.plot <- ggplot(shrub.avg, aes(x = year.xaxis, y = mean, 
                                    group = Treatment, 
                                    color = Treatment)) +
  geom_line() +
  geom_point() +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  facet_wrap(~Treatment) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Shrub cover") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(data = letters.shrub,
            mapping = aes(x = x, y = y, label = label),
            color = "black",
            size = 3.5)  +
  geom_text(data = ptext.shrub,
            aes(x = x, y = y, label = label),
            color = "gray30",
            size = 2.5) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.2, l = 0.1, "in")) 
shrub.plot


letters.shrub2 <- data.frame(x = shrub.avg$year.xaxis[1:6],
                             y = c(9, 8, 7.4, 4.5, 5.5, 12),
                             label = shrub.ctrl.letters$groups)

ptext.shrub2 <- data.frame(x = as.Date("2019-09-01"),
                          y = 6,
                          label = "ANOVA, p = 0.982 \nANOVA, p = 0.011")
shrub.plot2 <- ggplot(shrub.avg, aes(x = year.xaxis, y = mean, 
                                   color = Treatment)) +
  geom_line() +
  geom_point() +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Shrub cover") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  geom_text(data = letters.shrub2,
            mapping = aes(x = x, y = y, label = label),
            color = "black",
            size = 3) +
  geom_text(data = ptext.shrub2,
            aes(x = x, y = y, label = label),
            color = "gray30",
            size = 2.5) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "in")) +
  theme(legend.title = element_blank())
shrub.plot2


# Combine herb & shrub ----------------------------------------------------

tiff("figures/2023-11_SER-SW-2023-and-SRM-2024/temporal-ANOVA-herb-shrub.tiff", units = "in", height = 8, width = 5, res = 600)
ggarrange(herb.plot2, shrub.plot2,
          ncol = 1, nrow = 2,
          labels = c("(A)", "(B)"))

dev.off()

