library(tidyverse)
library(car)
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
dat.2021$Treatment2 <- factor(dat.2021$Treatment2,
                              levels = c("In-channel treatment", 
                                         "Upland treatment", "No treatment"))

# Add C:N ratio col
dat.2021 <- dat.2021 %>% 
  mutate(CN_ratio = TC_perc / TN_perc)



# Normal distribution -----------------------------------------------------

# Quantile-quantile plots
qqPlot(dat.2021$Cover)
qqPlot(dat.2021$Herbaceous)
qqPlot(dat.2021$Woody)
qqPlot(dat.2021$TN_perc) # not normal - needs log transformation
qqPlot(dat.2021$TN_log)
qqPlot(dat.2021$TC_perc) # not normal - needs log transformation
qqPlot(dat.2021$TC_log)
qqPlot(dat.2021$OM_perc)
qqPlot(dat.2021$CN_ratio)
qqPlot(dat.2021$rich)
qqPlot(dat.2021$shan)
qqPlot(dat.2021$Richness.barc)
qqPlot(dat.2021$Shannon.barc)


# Total N -----------------------------------------------------------------

anova.tn <- aov(dat.2021$TN_log ~ dat.2021$Treatment2, data = dat.2021)
summary(anova.tn) # p = 0.00234 
hsd.tn <- HSD.test(anova.tn, trt = "dat.2021$Treatment2")
hsd.tn
tn.letters <- hsd.tn$groups
tn.letters <- tn.letters[c("In-channel treatment", 
                           "Upland treatment", "No treatment"), ]

# Boxplot
letters <- data.frame(label = tn.letters$groups,
                      x = 1:3,
                      y = rep(0.64, 3))

tn2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = TN_ppt)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
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

anova.tc <- aov(dat.2021$TC_log ~ dat.2021$Treatment2, data = dat.2021)
summary(anova.tc) # p = 0.00774  
hsd.tc <- HSD.test(anova.tc, trt = "dat.2021$Treatment2")
hsd.tc
tc.letters <- hsd.tc$groups
tc.letters <- tc.letters[c("In-channel treatment", 
                           "Upland treatment", "No treatment"), ]

# Boxplot
letters <- data.frame(label = tc.letters$groups,
                      x = 1:3,
                      y = rep(6.5, 3))

tc2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = TC_ppt)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
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


# C:N ratio ---------------------------------------------------------------

anova.cn <- aov(dat.2021$CN_ratio ~ dat.2021$Treatment2, data = dat.2021)
summary(anova.cn) # p = 0.142  

# Boxplot
tc2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = CN_ratio)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Proportion") +
  ggtitle("Carbon-to-nitrogen ratio") +
  theme(axis.text.x = element_text(color = "#000000"))
tc2021.plot


# Organic matter ----------------------------------------------------------

anova.om <- aov(dat.2021$OM_perc ~ dat.2021$Treatment2, data = dat.2021)
summary(anova.om) # p = 2.49e-06
hsd.om <- HSD.test(anova.om, trt = "dat.2021$Treatment2")
hsd.om
om.letters <- hsd.om$groups
om.letters <- om.letters[c("In-channel treatment", 
                           "Upland treatment", "No treatment"), ]

# Boxplot
letters <- data.frame(label = om.letters$groups,
                      x = 1:3,
                      y = rep(2.1, 3))

om2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = OM_perc)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
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
summary(aov(dat.2021$Richness.barc ~ dat.2021$Treatment2)) # NS

# Boxplot
barc.rich2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = Richness.barc)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("No. of species (ASVs)") +
  ggtitle("Soil bacterial & archaeal richness") +
  theme(axis.text.x = element_text(color = "#000000"))
barc.rich2021.plot


# Soil bac arc diversity --------------------------------------------------

# ANOVA
summary(aov(dat.2021$Shannon.barc ~ dat.2021$Treatment2)) # NS

# Boxplot
barc.shan2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = Shannon.barc)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
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
summary(aov(dat.2021$Cover ~ dat.2021$Treatment2)) # NS

# Boxplot
total2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = Cover)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover") +
  theme(axis.text.x = element_text(color = "#000000"))
total2021.plot


# Herbaceous cover --------------------------------------------------------

# ANOVA
summary(aov(dat.2021$Herbaceous ~ dat.2021$Treatment2))
anova.herb <- aov(dat.2021$Herbaceous ~ dat.2021$Treatment2)
hsd.herb <- HSD.test(anova.herb, trt = "dat.2021$Treatment2")
hsd.herb
herb.letters <- hsd.herb$groups
herb.letters <- herb.letters[c("In-channel treatment", 
                             "Upland treatment", "No treatment"), ]

# Boxplot
letters <- data.frame(label = herb.letters$groups,
                      x = 1:3,
                      y = rep(75, 3))

herb2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = Herbaceous)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
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
summary(aov(dat.2021$rich ~ dat.2021$Treatment2)) # NS

# Boxplot
rich2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = rich)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("No. of species") +
  ggtitle("Perennial plant richness") +
  theme(axis.text.x = element_text(color = "#000000"))
rich2021.plot



# Perennial plant diversity -----------------------------------------------

# ANOVA
summary(aov(dat.2021$shan ~ dat.2021$Treatment2)) # NS

# Boxplot
shan2021.plot <- ggplot(dat.2021, aes(x = Treatment2, y = shan)) +
  geom_boxplot(aes(fill = Treatment2),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment2),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("#33A02C", "#1F78B4", "red")) +
  scale_color_manual(values = c("#33A02C", "#1F78B4", "red")) +
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


# Elevation difference ----------------------------------------------------

summary(aov(dat.2021$Elev_Diff ~ dat.2021$Treatment2))
anova.elev <- aov(dat.2021$Elev_Diff ~ dat.2021$Treatment2)
hsd.elev <- HSD.test(anova.elev, trt = "dat.2021$Treatment2")
hsd.elev


# Save --------------------------------------------------------------------

save.image("RData/ANOVA-by-treatment2_2021.RData")
