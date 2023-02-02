library(tidyverse)
library(ggpubr)
library(agricolae)

# Load data ---------------------------------------------------------------

dat.2021 <- read.csv("data/cleaned/SEM-input.csv")
meta <- read.table("data/cleaned/sequencing/bac_arc_diversity.txt",
                   sep = "\t", header = TRUE)

# Data wrangling ----------------------------------------------------------

meta$Station <- gsub("^.*?, ", "", meta$Name)
dat.2021 <- left_join(dat.2021, meta)

dat.2021 <- dat.2021 %>% 
  rename(Richness.barc = Richness,
         Shannon.barc = Shannon)


# Normal distribution -----------------------------------------------------

# Quantile-quantile plots
ggqqplot(dat.2021$Cover)
ggqqplot(dat.2021$Herbaceous)
ggqqplot(dat.2021$Woody)
ggqqplot(dat.2021$TN_perc) # not normal - needs log transformation
ggqqplot(dat.2021$TN_log)
ggqqplot(dat.2021$TC_perc) # not normal?
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
# Channel 19        2.892133      a
# Channel 13        2.800003      a
# Channel 21        2.696254     ab
# Channel 12        2.421492      b


# Total C -----------------------------------------------------------------

anova.tc <- aov(dat.2021$TC_log ~ dat.2021$Channel, data = dat.2021)
summary(anova.tc) # p = 0.0139 
hsd.tc <- HSD.test(anova.tc, trt = "dat.2021$Channel")
hsd.tc
# Channel 19        4.893734      a
# Channel 13        4.805516      a
# Channel 21        4.606063     ab
# Channel 12        4.256848      b


# Organic matter ----------------------------------------------------------

anova.om <- aov(dat.2021$OM_perc ~ dat.2021$Channel, data = dat.2021)
summary(anova.om)
hsd.om <- HSD.test(anova.om, trt = "dat.2021$Channel")
hsd.om
# Channel 19        1.3677887      a
# Channel 13        1.2597340     ab
# Channel 21        1.1264686     bc
# Channel 12        0.9121873      c


# Soil bac arc richness ---------------------------------------------------

# ANOVA
summary(aov(dat.2021$Richness.barc ~ dat.2021$Channel)) # NS


# Soil bac arc diversity --------------------------------------------------

# ANOVA
summary(aov(dat.2021$Shannon.barc ~ dat.2021$Channel)) # NS



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
              size = 3) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black")
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
              size = 3) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Herbaceous plant cover") +
  geom_text(data = letters,
            mapping = aes(x = x, y = y, label = label),
            color = "black")
herb2021.plot


# Perennial plant richness ------------------------------------------------

# ANOVA
summary(aov(dat.2021$rich ~ dat.2021$Channel))
anova.rich <- aov(dat.2021$rich ~ dat.2021$Channel)
hsd.rich <- HSD.test(anova.rich, trt = "dat.2021$Channel")
hsd.rich
# Channel 12      8.357143      a
# Channel 13      7.562500     ab
# Channel 19      6.941176     ab
# Channel 21      5.733333      b

# Boxplot
rich2021.plot <- ggplot(dat.2021, aes(x = Channel, y = rich)) +
  geom_boxplot(aes(fill = Channel),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Channel),
              alpha = 0.9,
              size = 3) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("No. of species") +
  ggtitle("Perennial plant richness")
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
              size = 3) +
  scale_fill_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  scale_color_manual(values = c("red", "#33A02C", "#1F78B4", "#33A02C")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Herbaceous plant cover (%)")
shan2021.plot


# Save --------------------------------------------------------------------

save.image("RData/ANOVA-between-channels_2021.RData")
