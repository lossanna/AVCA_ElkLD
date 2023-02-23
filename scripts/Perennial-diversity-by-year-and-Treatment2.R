library(tidyverse)
library(vegan)
library(plotrix)
library(agricolae)

# Load data ---------------------------------------------------------------

per.div <- read.csv("data/cleaned/Summarised-all_perennial-diversity.csv")
per.div$year.xaxis <- as.Date(per.div$year.xaxis)
per.div$Year <- factor(per.div$Year)


# Richness ----------------------------------------------------------------

# Channel summarised (stations averaged)
richness.avg <- per.div %>% 
  group_by(Treatment2, Year, year.date, year.xaxis) %>% 
  summarise(mean = mean(rich),
            SD = sd(rich),
            SE = std.error(rich),
            .groups = "keep") 

write.csv(richness.avg,
          file = "data/cleaned/Treatment2-average_richness.csv",
          row.names = FALSE)

# Plot
richness.plot <- ggplot(richness.avg, aes(x = year.xaxis, y = mean, 
                                              group = Treatment2, 
                                              color = Treatment2)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Number of species") +
  ggtitle("Perennial species richness") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~Treatment2)
richness.plot

# ANOVA 
summary(aov(rich ~ Treatment2 * Year, data = per.div))
anova.rich <- aov(rich ~ Treatment2 * Year, data = per.div)
Anova(anova.rich, type = "III")
# Treatment2        98.85   2   8.7389 0.0001983 ***
# Year              76.57   5   2.7078 0.0204050 *  
# Treatment2:Year  174.48  10   3.0851 0.0008918 ***
TukeyHSD(anova.rich, which = "Treatment2")
#                                       p adj
# In-channel treatment-Control          0.0000001
# Upland treatment-Control              0.0048189
# Upland treatment-In-channel treatment 0.0619346
TukeyHSD(anova.rich, which = "Year")
TukeyHSD(anova.rich, which = "Treatment2:Year")



# Shannon diversity -------------------------------------------------------

# Channel summarised (stations averaged)
shannon.avg <- per.div %>% 
  group_by(Treatment2, Year, year.date, year.xaxis) %>% 
  summarise(mean = mean(shan),
            SD = sd(shan),
            SE = std.error(shan),
            .groups = "keep") 

write.csv(shannon.avg,
          file = "data/cleaned/Treatment2-average_Shannon.csv",
          row.names = FALSE)

# Plot 
shannon.plot <- ggplot(shannon.avg, aes(x = year.xaxis, y = mean, 
                                            group = Treatment2, 
                                            color = Treatment2)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = mean - SE, ymax = mean + SE)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial plant diversity") +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  facet_wrap(~Treatment2)
shannon.plot

# ANOVA
summary(aov(shan ~ Treatment2 * Year, data = per.div))
anova.shan <- aov(shan ~ Treatment2 * Year, data = per.div)
Anova(anova.shan, type = "III") # NS


save.image("RData/Perennial-diversity-by-year-and-Treatment2.RData")


