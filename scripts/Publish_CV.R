# Purpose: Create veg CV figures for publication, and code for published R Markdown.
#   Main figures: combined notree, herb, shrub
#   Supp figures: combined richness & Shannon
#   Code: calculate CV, t-test/Mann-Whitney, plot

# Will use Mann-Whitney rather than log transformation for ease of interpretation.
# Am including distribution and qq-plots here, but will not in R Markdown.

# Created: 2023-08-28
# Last updated: 2023-08-29

library(tidyverse)
library(car) # version 3.1-2
library(scales)
library(ggpubr)

# Load data ---------------------------------------------------------------

notree.all <- read_csv("data/publish/Herb-and-shrub-cover_2012-2021.csv")
herb.all <- read_csv("data/publish/Herb-cover_2012-2021.csv") 
shrub.all <- read_csv("data/publish/Shrub-cover_2012-2021.csv")
per.div <- read_csv("data/publish/Perennial-plant-diversity_2012-2021.csv")


# Notree cover ------------------------------------------------------------

# Find CV for each sample over time
notree.sample <- notree.all |> 
  group_by(Sample, Treatment) |> 
  summarise(CV = sd(Cover) / mean(Cover),
            .groups = "keep")

# Explore distribution
qqPlot(filter(notree.sample, Treatment == "Treated")$CV) # normal
qqPlot(filter(notree.sample, Treatment == "Control")$CV) # normal

# Compare means
t.test(filter(notree.sample, Treatment == "Treated")$CV,
       filter(notree.sample, Treatment == "Control")$CV) # NS, p = 0.882

# Plot
notree.plot.cv <- notree.sample |> 
  ggplot(aes(x = Treatment, y = CV)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment)) +
  geom_jitter(size = 1,
              alpha = 0.8,
              aes(color = Treatment)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Vegetation cover",
       x = NULL,
       y = NULL) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(color = "black")) +
  geom_text(aes(x = 0.9, y = 0.95, label = "t-test, p = 0.882"),
            color = "gray30",
            size = 2.5) +
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.2, "in")) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = 0.75, linetype = "dashed") +
  theme(plot.title = element_text(size = 12))
notree.plot.cv



# Herb cover --------------------------------------------------------------

# Find CV for each sample over time
herb.sample <- herb.all |> 
  group_by(Sample, Treatment) |> 
  summarise(CV = sd(Cover) / mean(Cover),
            .groups = "keep")

# Explore distribution
qqPlot(filter(herb.sample, Treatment == "Treated")$CV) # normal
qqPlot(filter(herb.sample, Treatment == "Control")$CV) # not quite normal?

# Compare means
wilcox.test(filter(herb.sample, Treatment == "Treated")$CV,
            filter(herb.sample, Treatment == "Control")$CV) # NS, p = 0.148

# Plot
herb.plot.cv <- herb.sample |> 
  ggplot(aes(x = Treatment, y = CV)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment)) +
  geom_jitter(size = 1,
              alpha = 0.8,
              aes(color = Treatment)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Herbaceous cover",
       x = NULL, 
       y = NULL) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent)  +
  theme(axis.text.x = element_text(color = "black")) +
  geom_text(aes(x = 0.95, y = 1.18, label = "Mann-Whitney, \np = 0.122"),
            color = "gray30",
            size = 2.5) +
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.2, "in")) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = 0.75, linetype = "dashed") +
  theme(plot.title = element_text(size = 12))
herb.plot.cv


# Shrub cover -------------------------------------------------------------

# Find CV for each sample over time
shrub.sample <- shrub.all |> 
  group_by(Sample, Treatment) |> 
  summarise(CV = sd(Cover) / mean(Cover),
            .groups = "keep") # NaNs produced because some have 0 cover and can't divide by 0

# Replace NaNs with 0
shrub.sample[1, 3] <- 0
shrub.sample[5, 3] <- 0
shrub.sample[8, 3] <- 0

# Explore distribution
qqPlot(filter(shrub.sample, Treatment == "Control")$CV) # not normal
qqPlot(filter(shrub.sample, Treatment == "Treated")$CV) # not normal

# Compare means
wilcox.test(filter(shrub.sample, Treatment == "Treated")$CV,
            filter(shrub.sample, Treatment == "Control")$CV,
            exact = FALSE) # p = 0.01429


# Plot
letters.shrub <- data.frame(x = c(1, 2),
                            y = c(2.5, 2.5),
                            label = c("a", "b"))

shrub.plot.cv <- shrub.sample |> 
  ggplot(aes(x = Treatment, y = CV)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment)) +
  geom_jitter(size = 1,
              alpha = 0.8,
              aes(color = Treatment)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Shrub cover",
       x = NULL,
       y = "Coefficient of variation") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent)  +
  theme(axis.text.x = element_text(color = "black")) +
  geom_text(aes(x = 0.95, y = 2.75, label = "Mann-Whitney, \np = 0.014"),
            color = "gray30",
            size = 2.5) +
  geom_text(data = letters.shrub,
            aes(x = x, y = y, label = label),
            color = "black") +
  theme(plot.margin = margin(0.1, 0, 0.1, 0.1, "in")) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = 0.75, linetype = "dashed") +
  theme(plot.title = element_text(size = 12))
shrub.plot.cv


# Combine notree, herb, shrub ---------------------------------------------

tiff("figures/2023-09_publish-figures/Fig3_CV_shrub-herb-notree.tiff", units = "in", height = 4, width = 7, res = 1000)
ggarrange(shrub.plot.cv, herb.plot.cv, notree.plot.cv,
          ncol = 3, nrow = 1,
          labels = c("(A)", "(B)", "(C)")) 

dev.off()


# Richness ----------------------------------------------------------------

# Find CV for each sample over time
rich.sample <- per.div |> 
  group_by(Sample, Treatment) |> 
  summarise(CV = sd(rich) / mean(rich),
            .groups = "keep")

# Explore distribution
qqPlot(filter(rich.sample, Treatment == "Treated")$CV) # not quite normal
qqPlot(filter(rich.sample, Treatment == "Control")$CV) # normal

# Compare means
wilcox.test(filter(rich.sample, Treatment == "Treated")$CV,
            filter(rich.sample, Treatment == "Control")$CV) # NS, p = 0.093

# Plot
rich.plot.cv <- rich.sample |> 
  ggplot(aes(x = Treatment, y = CV)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment)) +
  geom_jitter(size = 1,
              alpha = 0.8,
              aes(color = Treatment)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Perennial plant species \nrichness",
       x = NULL,
       y = "Coefficient of variation") +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(color = "black")) +
  geom_text(aes(x = 0.95, y = 0.65, label = "Mann-Whitney, \np = 0.062"),
            color = "gray30",
            size = 2.5) +
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.1, "in")) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = 0.75, linetype = "dashed") +
  theme(plot.title = element_text(size = 12))
rich.plot.cv


# Shannon -----------------------------------------------------------------

# Find CV for each sample over time
shan.sample <- per.div |> 
  group_by(Sample, Treatment) |> 
  summarise(CV = sd(shan) / mean(shan),
            .groups = "keep")

# Explore distribution
qqPlot(filter(shan.sample, Treatment == "Treated")$CV) # normal
qqPlot(filter(shan.sample, Treatment == "Control")$CV) # almost normal

# Compare
t.test(filter(shan.sample, Treatment == "Treated")$CV,
       filter(shan.sample, Treatment == "Control")$CV) # NS, p = 0.075

# Plot
shan.plot.cv <- shan.sample |> 
  ggplot(aes(x = Treatment, y = CV)) +
  geom_boxplot(alpha = 0.3,
               outlier.shape = NA,
               aes(fill = Treatment)) +
  geom_jitter(size = 1,
              alpha = 0.8,
              aes(color = Treatment)) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  labs(title = "Perennial plant diversity",
       x = NULL,
       y = NULL) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_y_continuous(labels = percent) +
  theme(axis.text.x = element_text(color = "black")) +
  geom_text(aes(x = 0.95, y = 0.83, label = "t-test, p = 0.075"),
            color = "gray30",
            size = 2.5) +
  theme(plot.margin = margin(0.1, 0.1, 0.1, 0.15, "in")) +
  stat_summary(fun = mean, geom = "errorbar", aes(ymax = after_stat(y), ymin = after_stat(y)),
               width = 0.75, linetype = "dashed") +
  theme(plot.title = element_text(size = 12))
shan.plot.cv



# Combine richness & Shannon ----------------------------------------------

# Supplemental figure
tiff("figures/2023-09_publish-figures/FigS5_CV_rich-shan.tiff", units = "in", height = 4, width = 5.5, res = 300)
ggarrange(rich.plot.cv, shan.plot.cv,
          ncol = 2, nrow = 1,
          labels = c("(A)", "(B)")) 

dev.off()



save.image("RData/Publish_CV.RData")
