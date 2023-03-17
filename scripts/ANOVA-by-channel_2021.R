# Purpose: Run one-way ANOVAs to compare between channels for 2021 plant and soil data.

# AVCA & partners were more interested in comparing channels, but later I decided I wanted to group
#   samples differently, and that the channel distinctions were less meaningful to those who
#   had never worked on the project.
# Analysis grouping by channel was never formally presented anywhere.


library(tidyverse)
library(ggpubr)
library(agricolae)

# Load data ---------------------------------------------------------------

dat.2021.raw <- read.csv("data/cleaned/SEM-input.csv")
meta <- read.table("data/cleaned/sequencing/bac_arc_diversity.txt",
                   sep = "\t", header = TRUE)

# Data wrangling ----------------------------------------------------------

meta$Station <- gsub("^.*?, ", "", meta$Name)
dat.2021 <- left_join(dat.2021.raw, meta)

dat.2021 <- dat.2021 %>% 
  rename(Richness.barc = Richness,
         Shannon.barc = Shannon) %>% 
  mutate(TN_ppt = TN_perc * 10,
         TC_ppt = TC_perc * 10)


# Normal distribution -----------------------------------------------------

# Quantile-quantile plots
ggqqplot(dat.2021$Cover)
ggqqplot(dat.2021$Herbaceous)
ggqqplot(dat.2021$Woody)
ggqqplot(dat.2021$TN_perc) # not normal - needs log transformation
ggqqplot(dat.2021$TN_log)
ggqqplot(dat.2021$TC_perc) # not normal - needs log transformation
ggqqplot(dat.2021$TC_log)
ggqqplot(dat.2021$OM_perc)
ggqqplot(dat.2021$rich)
ggqqplot(dat.2021$shan)
ggqqplot(dat.2021$Richness.barc)
ggqqplot(dat.2021$Shannon.barc)


# Total N -----------------------------------------------------------------

anova.tn <- aov(dat.2021$TN_log ~ dat.2021$Channel, data = dat.2021)
summary(anova.tn) # p = 0.00548 
hsd.tn <- HSD.test(anova.tn, trt = "dat.2021$Channel")
hsd.tn
tn.letters <- hsd.tn$groups
tn.letters <- tn.letters %>% 
  mutate(Channel = rownames(tn.letters)) %>% 
  arrange(Channel)

# Boxplot
letters <- data.frame(label = tn.letters$groups,
                      x = 1:4,
                      y = rep(0.64, 4))

tn2021.plot <- ggplot(dat.2021, aes(x = Channel, y = TN_ppt)) +
  geom_boxplot(aes(fill = Channel),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Channel),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Total N (mg/g soil)") +
  ggtitle("Soil nitrogen") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "#000000"))
tn2021.plot


# Total C -----------------------------------------------------------------

anova.tc <- aov(dat.2021$TC_log ~ dat.2021$Channel, data = dat.2021)
summary(anova.tc) # p = 0.0139 
hsd.tc <- HSD.test(anova.tc, trt = "dat.2021$Channel")
hsd.tc
tc.letters <- hsd.tc$groups
tc.letters <- tc.letters %>% 
  mutate(Channel = rownames(tc.letters)) %>% 
  arrange(Channel)

# Boxplot
letters <- data.frame(label = tc.letters$groups,
                      x = 1:4,
                      y = rep(6.5, 4))

tc2021.plot <- ggplot(dat.2021, aes(x = Channel, y = TC_ppt)) +
  geom_boxplot(aes(fill = Channel),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Channel),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Total C (mg/g soil)") +
  ggtitle("Soil carbon") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "#000000"))
tc2021.plot


# Organic matter ----------------------------------------------------------

anova.om <- aov(dat.2021$OM_perc ~ dat.2021$Channel, data = dat.2021)
summary(anova.om)
hsd.om <- HSD.test(anova.om, trt = "dat.2021$Channel")
hsd.om
om.letters <- hsd.om$groups
om.letters <- om.letters %>% 
  mutate(Channel = rownames(om.letters)) %>% 
  arrange(Channel)

# Boxplot
letters <- data.frame(label = om.letters$groups,
                      x = 1:4,
                      y = rep(2.1, 4))

om2021.plot <- ggplot(dat.2021, aes(x = Channel, y = OM_perc)) +
  geom_boxplot(aes(fill = Channel),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Channel),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Organic matter (%)") +
  ggtitle("Soil organic matter") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "#000000"))
om2021.plot



# Soil bac arc richness ---------------------------------------------------

# ANOVA
summary(aov(dat.2021$Richness.barc ~ dat.2021$Channel)) # NS

# Boxplot
barc.rich2021.plot <- ggplot(dat.2021, aes(x = Channel, y = Richness.barc)) +
  geom_boxplot(aes(fill = Channel),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Channel),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("No. of species (ASVs)") +
  ggtitle("Soil bacterial & archaeal richness") +
  theme(axis.text.x = element_text(color = "#000000"))
barc.rich2021.plot


# Soil bac arc diversity --------------------------------------------------

# ANOVA
summary(aov(dat.2021$Shannon.barc ~ dat.2021$Channel)) # NS

# Boxplot
barc.shan2021.plot <- ggplot(dat.2021, aes(x = Channel, y = Shannon.barc)) +
  geom_boxplot(aes(fill = Channel),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Channel),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Shannon diversity index") + 
  ggtitle("Soil bacterial & archaeal diversity") +
  theme(axis.text.x = element_text(color = "#000000"))
barc.shan2021.plot



# Combine soil graphs -----------------------------------------------------

ggarrange(tn2021.plot, tc2021.plot, om2021.plot, barc.rich2021.plot,
          ncol = 2, nrow = 2)



# Total plant cover -------------------------------------------------------

# ANOVA
summary(aov(dat.2021$Cover ~ dat.2021$Channel))
anova.total <- aov(dat.2021$Cover ~ dat.2021$Channel)
hsd.total <- HSD.test(anova.total, trt = "dat.2021$Channel")
hsd.total
total.letters <- hsd.total$groups
total.letters <- total.letters %>% 
  mutate(Channel = rownames(total.letters)) %>% 
  arrange(Channel)

# Boxplot
letters <- data.frame(label = total.letters$groups,
                      x = 1:4,
                      y = rep(140, 4))

total2021.plot <- ggplot(dat.2021, aes(x = Channel, y = Cover)) +
  geom_boxplot(aes(fill = Channel),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Channel),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "#000000"))
total2021.plot


# Herbaceous cover --------------------------------------------------------

# ANOVA
summary(aov(dat.2021$Herbaceous ~ dat.2021$Channel))
anova.herb <- aov(dat.2021$Herbaceous ~ dat.2021$Channel)
hsd.herb <- HSD.test(anova.herb, trt = "dat.2021$Channel")
hsd.herb
herb.letters <- hsd.herb$groups
herb.letters <- herb.letters %>% 
  mutate(Channel = rownames(herb.letters)) %>% 
  arrange(Channel)

# Boxplot
letters <- data.frame(label = herb.letters$groups,
                      x = 1:4,
                      y = rep(75, 4))

herb2021.plot <- ggplot(dat.2021, aes(x = Channel, y = Herbaceous)) +
  geom_boxplot(aes(fill = Channel),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Channel),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous plant cover") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "#000000"))
herb2021.plot


# Perennial plant richness ------------------------------------------------

# ANOVA
summary(aov(dat.2021$rich ~ dat.2021$Channel))
anova.rich <- aov(dat.2021$rich ~ dat.2021$Channel)
hsd.rich <- HSD.test(anova.rich, trt = "dat.2021$Channel")
hsd.rich
rich.letters <- hsd.rich$groups
rich.letters <- rich.letters %>% 
  mutate(Channel = rownames(rich.letters)) %>% 
  arrange(Channel)

# Boxplot
letters <- data.frame(label = rich.letters$groups,
                      x = 1:4,
                      y = rep(13.3, 4))

rich2021.plot <- ggplot(dat.2021, aes(x = Channel, y = rich)) +
  geom_boxplot(aes(fill = Channel),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Channel),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("No. of species") +
  ggtitle("Perennial plant richness") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black") +
  theme(axis.text.x = element_text(color = "#000000"))
rich2021.plot



# Perennial plant diversity -----------------------------------------------

# ANOVA
summary(aov(dat.2021$shan ~ dat.2021$Channel)) # NS

# Boxplot
shan2021.plot <- ggplot(dat.2021, aes(x = Channel, y = Herbaceous)) +
  geom_boxplot(aes(fill = Channel),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Channel),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Shannon diversity index") +
  ggtitle("Perennial plant diversity") +
  theme(axis.text.x = element_text(color = "#000000"))
shan2021.plot


# Combine plant graphs ----------------------------------------------------

ggarrange(total2021.plot, herb2021.plot, rich2021.plot, shan2021.plot,
          ncol = 2, nrow = 2)


# Save --------------------------------------------------------------------

save.image("RData/ANOVA-by-channel_2021.RData")
