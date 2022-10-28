library(tidyverse)
library(ggpubr)
library(agricolae)

# Load data ---------------------------------------------------------------

dat.2021 <- read.csv("data/cleaned/SEM-input.csv")


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
ggqqplot(dat.2021$Elev_Diff)
ggqqplot(dat.2021$rich)
ggqqplot(dat.2021$shan)
ggqqplot(dat.2021$total.diff)


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


# Save --------------------------------------------------------------------

save.image("RData/ANOVA-between-channels.RData")
