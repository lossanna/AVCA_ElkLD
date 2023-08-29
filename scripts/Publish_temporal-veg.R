# Purpose: Create temporal veg figures for publication, and code for published R Markdown.
#   Main figures: combined temporal cover 
#   Supp figures: temporal diversity
#   Code: ANOVA and plots for notree, herb, shrub, rich, shan; average table for invasive
#     and list of most common invasive

# Created: 2023-08-28
# Last updated: 2023-08-29

library(tidyverse)
library(agricolae)
library(plotrix)
library(ggpubr)
library(rstatix)

# Load data ---------------------------------------------------------------

notree.all <- read_csv("data/publish/Herb-and-shrub-cover_2012-2021.csv")
herb.all <- read_csv("data/publish/Herb-cover_2012-2021.csv") 
shrub.all <- read_csv("data/publish/Shrub-cover_2012-2021.csv")
invasive.all <- read_csv("data/publish/Invasive-cover_2012-2021.csv")
plant.all <- read_csv("data/publish/Species-cover_2012-2021.csv")
per.div <- read_csv("data/publish/Perennial-plant-diversity_2012-2021.csv")


# Functions ---------------------------------------------------------------

# Convert columns to factor or date as needed
convert.cols <- function(x) {
  x$year.xaxis <- as.Date(x$year.xaxis)
  
  group.cols <- c("Sample", "Year", "Treatment")
  
  x[group.cols] <- lapply(x[group.cols], factor)
  
  return(x)
}


# Data wrangling ----------------------------------------------------------

notree.all <- convert.cols(notree.all)
herb.all <- convert.cols(herb.all)
shrub.all <- convert.cols(shrub.all)
invasive.all <- convert.cols(invasive.all)
per.div <- convert.cols(per.div)


# Grass, forb, and shrub cover (notree) -----------------------------------

# Find averages by year
notree.avg <- notree.all %>% 
  group_by(Treatment, Year, year.xaxis) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")

# One-way ANOVA for Control
summary(aov(Cover ~ Year, data = filter(notree.all, Treatment == "Control"))) # p = 4.3e-06
notree.ctrl <- notree.all |> 
  filter(Treatment == "Control")
anova.notree.ctrl <- aov(notree.ctrl$Cover ~ notree.ctrl$Year)
hsd.notree.ctrl <- HSD.test(anova.notree.ctrl, trt = "notree.ctrl$Year")
hsd.notree.ctrl

# One-way ANOVA for Treated
summary(aov(Cover ~ Year, data = filter(notree.all, Treatment == "Treated"))) # p = 0.00304
notree.trt <- notree.all |> 
  filter(Treatment == "Treated")
anova.notree.trt <- aov(notree.trt$Cover ~ notree.trt$Year)
hsd.notree.trt <- HSD.test(anova.notree.trt, trt = "notree.trt$Year")
hsd.notree.trt

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
                             y = rep(49, 12),
                             label = c(notree.ctrl.letters$groups,
                                       notree.trt.letters$groups),
                             Treatment = c(rep("Control", 6),
                                           rep("Treated", 6)))
ptext.notree <- data.frame(x = rep(as.Date("2019-09-01"), 2),
                           y = c(22, 22),
                           label = c("ANOVA, p < 0.001", "ANOVA, p = 0.003"),
                           Treatment = c("Control", "Treated"))

notree.plot <- ggplot(notree.avg, aes(x = year.xaxis, y = mean, 
                                      group = Treatment, 
                                      color = Treatment)) +
  geom_line() +
  geom_point() +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  facet_wrap(~Treatment) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Vegetation cover, 2012-2021") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(data = letters.notree,
            mapping = aes(x = x, y = y, label = label),
            color = "black",
            size = 3.5)  +
  geom_text(data = ptext.notree,
            aes(x = x, y = y, label = label),
            color = "gray30",
            size = 2.5) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(plot.margin = margin(t = 0.1, r = 0.1, b = 0.2, l = 0.1, "in")) 
notree.plot



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
hsd.herb.ctrl

# One-way ANOVA for Treated
summary(aov(Cover ~ Year, data = filter(herb.all, Treatment == "Treated"))) # p = 3.77e-10
herb.trt <- herb.all |> 
  filter(Treatment == "Treated")
anova.herb.trt <- aov(herb.trt$Cover ~ herb.trt$Year)
hsd.herb.trt <- HSD.test(anova.herb.trt, trt = "herb.trt$Year")
hsd.herb.trt

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
hsd.shrub.ctrl

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


# Combine notree, herb & shrub --------------------------------------------

tiff("figures/2023-09_publish-figures/temporal-ANOVA_notree-herb-shrub.tiff", units = "in", height = 8, width = 6, res = 1000)
ggarrange(notree.plot, herb.plot, shrub.plot,
          ncol = 1, nrow = 3,
          labels = c("(A)", "(B)", "(C)")) 

dev.off()




# Invasive cover ----------------------------------------------------------

# Most common species
most.invasive.ctrl <- plant.all |> 
  filter(Native == "Invasive",
         Treatment == "Control") |> 
  group_by(Common) |> 
  summarise(mean = mean(Cover)) |> 
  arrange(desc(mean))

most.invasive.trt <- plant.all |> 
  filter(Native == "Invasive",
         Treatment == "Treated") |> 
  group_by(Common) |> 
  summarise(mean = mean(Cover)) |> 
  arrange(desc(mean)) 

# Find averages by year
invasive.avg <- invasive.all %>% 
  group_by(Treatment, Year, year.xaxis) %>% 
  summarise(mean = mean(Cover),
            SD = sd(Cover),
            SE = std.error(Cover),
            .groups = "keep")



# Perennial plant richness ------------------------------------------------

# Find averages by year
rich.avg <- per.div %>% 
  group_by(Treatment, Year, year.xaxis) %>% 
  summarise(mean = mean(rich),
            SD = sd(rich),
            SE = std.error(rich),
            .groups = "keep")

# One-way ANOVA for Control
summary(aov(rich ~ Year, data = filter(per.div, Treatment == "Control"))) # 0.00881
rich.ctrl <- per.div |> 
  filter(Treatment == "Control")
anova.rich.ctrl <- aov(rich.ctrl$rich ~ rich.ctrl$Year)
hsd.rich.ctrl <- HSD.test(anova.rich.ctrl, trt = "rich.ctrl$Year")
hsd.rich.ctrl

# One-way ANOVA for Treated
summary(aov(rich ~ Year, data = filter(per.div, Treatment == "Treated"))) # p = 0.0516

# Plot with one-way ANOVA letters
rich.ctrl.letters <- hsd.rich.ctrl$groups
rich.ctrl.letters <- rich.ctrl.letters |> 
  mutate(Year = rownames(rich.ctrl.letters)) |> 
  arrange(Year)

letters.rich <- data.frame(x = rich.avg$year.xaxis[1:6],
                           y = rep(10.4, 6),
                           label = rich.ctrl.letters$groups,
                           Treatment = rep("Control", 6))
ptext.rich <- data.frame(x = rep(as.Date("2019-09-01"), 2),
                         y = c(6.7, 10.3),
                         label = c("ANOVA, p = 0.009", "ANOVA, p = 0.052"),
                         Treatment = c("Control", "Treated"))

rich.plot <- ggplot(rich.avg, aes(x = year.xaxis, y = mean, 
                                  group = Treatment, 
                                  color = Treatment)) +
  geom_line() +
  geom_point() +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  facet_wrap(~Treatment) +
  xlab(NULL) +
  ylab("No. of species") +
  ggtitle("Perennial plant species richness, 2012-2021") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_text(data = letters.rich,
            mapping = aes(x = x, y = y, label = label),
            color = "black",
            size = 3.5) +
  geom_text(data = ptext.rich,
            aes(x = x, y = y, label = label),
            color = "gray30",
            size = 2.5) +
  theme(axis.text.x = element_text(color = "black")) +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
rich.plot



# Perennial plant diversity (Shannon) -------------------------------------

# Find averages by year
shan.avg <- per.div %>% 
  group_by(Treatment, Year, year.xaxis) %>% 
  summarise(mean = mean(shan),
            SD = sd(shan),
            SE = std.error(shan),
            .groups = "keep")

# One-way ANOVA for Control
summary(aov(shan ~ Year, data = filter(per.div, Treatment == "Control"))) # p = 0.934

# One-way ANOVA for Treated
summary(aov(shan ~ Year, data = filter(per.div, Treatment == "Treated"))) # p = 0.725


# Plot with one-way ANOVA
ptext.shan <- data.frame(x = rep(as.Date("2019-09-01"), 2),
                         y = c(1.33, 1.33),
                         label = c("ANOVA, p = 0.934", "ANOVA, p = 0.725"),
                         Treatment = c("Control", "Treated"))

shan.plot <- ggplot(shan.avg, aes(x = year.xaxis, y = mean, 
                                  group = Treatment, 
                                  color = Treatment)) +
  geom_line() +
  geom_point() +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  facet_wrap(~Treatment) +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial plant diversity") +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(color = "black")) +
  geom_text(data = ptext.shan,
            aes(x = x, y = y, label = label),
            color = "gray30",
            size = 2.5) +
  theme(plot.margin = margin(0.1, 0.1, 0.2, 0.1, "in")) 
shan.plot


# Combine richness & Shannon ----------------------------------------------

tiff("figures/2023-09_publish-figures/temporal-ANOVA_richness-Shannon.tiff", units = "in", height = 5.5, width = 6, res = 300)
ggarrange( rich.plot, shan.plot,
           ncol = 1, nrow = 2,
           labels = c("(A)", "(B)")) 

dev.off()


save.image("RData/Publish_temporal-veg.RData")
