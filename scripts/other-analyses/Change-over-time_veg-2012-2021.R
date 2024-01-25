# Purpose: Compare the change over time (calculated by the log response ratio per unit time,
#   as described in Munson 2013, Ecology) to see if rock structures facilitate more or less change
#   because the value of the response variables themselves are not as interesting as how they change
#   in comparison to one another (rather than comparing the absolute values).
# Includes plots of Treated vs. Control with all years combined, and by year interval,
#   and t-test/Wilcox test for comparison.

# Calculated the change over time from year to year (5 time intervals) for each sample.
# Had to remove a few 0 values for log transformation.
# Found no difference in percent difference for Treated vs Control for any response variable
#   (total cover, herb cover, richness, Shannon).
# Created: 2023-02-23
# Last updated: 2023-07-20


library(tidyverse)
library(car)

# Load data ---------------------------------------------------------------

total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
herb.all <- read.csv("data/cleaned/Summarised-all_herb-cover.csv") 
notree.all <- read.csv("data/cleaned/Summarised-all_notree-cover.csv")
per.div <- read.csv("data/cleaned/Summarised-all_perennial-diversity.csv")
station.meta <- read.csv("data/station-metadata.csv")

# Convert 0 values to 0.000001 to be able to find log
total.all$Cover[total.all$Cover == 0] <- NA
herb.all$Cover[herb.all$Cover == 0] <- NA
notree.all$Cover[notree.all$Cover == 0] <- NA
per.div$rich[per.div$rich == 0] <- NA
per.div$shan[per.div$shan == 0] <- NA

# Create general metadata without year information for each sample
grouping.cols <- total.all |> 
  select(Sample, Channel, Station, station.trt, channel.trt, Treatment1, 
         Treatment2, Treatment3) |> 
  distinct(.keep_all = TRUE) 


# Total cover -------------------------------------------------------------

# Select cols to pivot
total.long <- total.all |> 
  select(Sample, Year, Cover) 

# Pivot wider so every column is a sample and every row is a year
  # and separate 2012-2015 and 2018-2021
total.wide <- total.long |> 
  pivot_wider(names_from = Sample, values_from = Cover)
total.change1 <- total.wide[1:4, -c(1)]
total.change2 <- total.wide[4:6, -c(1)]

# Convert to time series object
total.change1 <- as.matrix(total.change1)
totalts1 <- ts(total.change1, 1, 4, frequency = 1)
total.change2 <- as.matrix(total.change2)
totalts2 <- ts(total.change2, 1, 3, frequency = 1)

# Calculate log change 
total.lc1 <- log(totalts1) - log(stats::lag(totalts1)) # 1-year interval for 2012-2015
total.lc2 <- (log(totalts2) - log(stats::lag(totalts2))) / 3 # 3-year interval for 2015-2021

# Reformat as dataframe, add years, names & Treatment3
total.lc <- rbind(total.lc1, total.lc2)
total.lc <- as.data.frame(total.lc)
total.lc$Year <- c("2012-2013", "2013-2014", "2014-2015", "2015-2018", "2018-2021")
total.lc <- total.lc |> 
  pivot_longer(!Year, names_to = "Sample", values_to = "dCover")
total.lc$Sample <- gsub("^.*?\\.", "", total.lc$Sample)
total.lc$Sample <- as.numeric(total.lc$Sample)
total.lc <- left_join(grouping.cols, total.lc) |> 
  arrange(Sample) |> 
  select(Sample, Year, Channel, Station, station.trt, channel.trt, Treatment1,
         Treatment2, Treatment3, dCover)

write_csv(total.lc,
          file = "data/cleaned/Log-change_total-cover.csv")


# Plot by Treatment3
# All years
ggplot(total.lc, aes(x = Treatment3, y = dCover)) +
  geom_boxplot(aes(fill = Treatment3),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment3),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Log ratio per unit time") +
  ggtitle("Change in total cover")

# By year
ggplot(total.lc, aes(x = Year, y = dCover)) +
  geom_boxplot(aes(fill = Treatment3),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment3),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Log ratio per unit time") +
  ggtitle("Change in total cover") +
  facet_wrap(~Treatment3)

# Comparison
qqPlot(filter(total.lc, Treatment3 == "Treated")$dCover) # almost normal?
qqPlot(filter(total.lc, Treatment3 == "Control")$dCover) # not really normal?

wilcox.test(filter(total.lc, Treatment3 == "Treated")$dCover, 
            filter(total.lc, Treatment3 == "Control")$dCover) # NS
  
t.test(filter(total.lc, Treatment3 == "Treated")$dCover, 
       filter(total.lc, Treatment3 == "Control")$dCover) # NS


# Herbaceous cover --------------------------------------------------------

# Select cols to pivot
herb.long <- herb.all |> 
  select(Sample, Year, Cover) 

# Pivot wider so every column is a sample and every row is a year
#   and separate 2012-2015 and 2018-2021
herb.wide <- herb.long |> 
  pivot_wider(names_from = Sample, values_from = Cover)
herb.change1 <- herb.wide[1:4, -c(1)]
herb.change2 <- herb.wide[4:6, -c(1)]

# Convert to time series object
herb.change1 <- as.matrix(herb.change1)
herbts1 <- ts(herb.change1, 1, 4, frequency = 1)
herb.change2 <- as.matrix(herb.change2)
herbts2 <- ts(herb.change2, 1, 3, frequency = 1)

# Calculate log change 
herb.lc1 <- log(herbts1) - log(stats::lag(herbts1)) # 1-year interval for 2012-2015
herb.lc2 <- (log(herbts2) - log(stats::lag(herbts2))) / 3 # 3-year interval for 2015-2021

# Reformat as dataframe, add years, names & Treatment3
herb.lc <- rbind(herb.lc1, herb.lc2)
herb.lc <- as.data.frame(herb.lc)
herb.lc$Year <- c("2012-2013", "2013-2014", "2014-2015", "2015-2018", "2018-2021")
herb.lc <- herb.lc |> 
  pivot_longer(!Year, names_to = "Sample", values_to = "dCover")
herb.lc$Sample <- gsub("^.*?\\.", "", herb.lc$Sample)
herb.lc$Sample <- as.numeric(herb.lc$Sample)
herb.lc <- left_join(grouping.cols, herb.lc) |> 
  arrange(Sample) |> 
  select(Sample, Year, Channel, Station, station.trt, channel.trt, Treatment1,
         Treatment2, Treatment3, dCover)

write_csv(herb.lc,
          file = "data/cleaned/Log-change_herb-cover.csv")


# Plot by Treatment3
# All years
ggplot(herb.lc, aes(x = Treatment3, y = dCover)) +
  geom_boxplot(aes(fill = Treatment3),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment3),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Log ratio per unit time") +
  ggtitle("Change in herbaceous cover") 

# By year
ggplot(herb.lc, aes(x = Year, y = dCover)) +
  geom_boxplot(aes(fill = Treatment3),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment3),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Log ratio per unit time") +
  ggtitle("Change in herbaceous cover") +
  facet_wrap(~Treatment3)

# Comparison
qqPlot(filter(herb.lc, Treatment3 == "Treated")$dCover) # not really normal?
qqPlot(filter(herb.lc, Treatment3 == "Control")$dCover) # almost normal?

wilcox.test(filter(herb.lc, Treatment3 == "Treated")$dCover, 
            filter(herb.lc, Treatment3 == "Control")$dCover) # NS

t.test(filter(herb.lc, Treatment3 == "Treated")$dCover, 
       filter(herb.lc, Treatment3 == "Control")$dCover) # NS


# Notree cover -------------------------------------------------------------

# Select cols to pivot
notree.long <- notree.all |> 
  select(Sample, Year, Cover) 

# Pivot wider so every column is a sample and every row is a year
# and separate 2012-2015 and 2018-2021
notree.wide <- notree.long |> 
  pivot_wider(names_from = Sample, values_from = Cover)
notree.change1 <- notree.wide[1:4, -c(1)]
notree.change2 <- notree.wide[4:6, -c(1)]

# Convert to time series object
notree.change1 <- as.matrix(notree.change1)
notreets1 <- ts(notree.change1, 1, 4, frequency = 1)
notree.change2 <- as.matrix(notree.change2)
notreets2 <- ts(notree.change2, 1, 3, frequency = 1)

# Calculate log change 
notree.lc1 <- log(notreets1) - log(stats::lag(notreets1)) # 1-year interval for 2012-2015
notree.lc2 <- (log(notreets2) - log(stats::lag(notreets2))) / 3 # 3-year interval for 2015-2021

# Reformat as dataframe, add years, names & Treatment3
notree.lc <- rbind(notree.lc1, notree.lc2)
notree.lc <- as.data.frame(notree.lc)
notree.lc$Year <- c("2012-2013", "2013-2014", "2014-2015", "2015-2018", "2018-2021")
notree.lc <- notree.lc |> 
  pivot_longer(!Year, names_to = "Sample", values_to = "dCover")
notree.lc$Sample <- gsub("^.*?\\.", "", notree.lc$Sample)
notree.lc$Sample <- as.numeric(notree.lc$Sample)
notree.lc <- left_join(grouping.cols, notree.lc) |> 
  arrange(Sample) |> 
  select(Sample, Year, Channel, Station, station.trt, channel.trt, Treatment1,
         Treatment2, Treatment3, dCover)

write_csv(notree.lc,
          file = "data/cleaned/Log-change_notree-cover.csv")


# Plot by Treatment3
# All years
notree.plot.lc1 <- ggplot(notree.lc, aes(x = Treatment3, y = dCover)) +
  geom_boxplot(aes(fill = Treatment3),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment3),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Log ratio per unit time") +
  ggtitle("Change in grass/forb/shrub cover")
notree.plot.lc1

# By year
notree.plot.lc2 <- ggplot(notree.lc, aes(x = Year, y = dCover)) +
  geom_boxplot(aes(fill = Treatment3),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment3),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Log ratio per unit time") +
  ggtitle("Change in grass, forb & shrub cover") +
  facet_wrap(~Treatment3)
notree.plot.lc2

# Comparison
qqPlot(filter(notree.lc, Treatment3 == "Treated")$dCover) # not normal
qqPlot(filter(notree.lc, Treatment3 == "Control")$dCover) # not normal?

wilcox.test(filter(notree.lc, Treatment3 == "Treated")$dCover, 
            filter(notree.lc, Treatment3 == "Control")$dCover) # NS



# Perennial richness ------------------------------------------------------

# Select cols to pivot
rich.long <- per.div |> 
  select(Sample, Year, rich) 

# Pivot wider so every column is a sample and every row is a year
#   and separate 2012-2015 and 2018-2021
rich.wide <- rich.long |> 
  pivot_wider(names_from = Sample, values_from = rich)
rich.change1 <- rich.wide[1:4, -c(1)]
rich.change2 <- rich.wide[4:6, -c(1)]

# Convert to time series object
rich.change1 <- as.matrix(rich.change1)
richts1 <- ts(rich.change1, 1, 4, frequency = 1)
rich.change2 <- as.matrix(rich.change2)
richts2 <- ts(rich.change2, 1, 3, frequency = 1)

# Calculate log change 
rich.lc1 <- log(richts1) - log(stats::lag(richts1)) # 1-year interval for 2012-2015
rich.lc2 <- (log(richts2) - log(stats::lag(richts2))) / 3 # 3-year interval for 2015-2021

# Reformat as dataframe, add years, names & Treatment3
rich.lc <- rbind(rich.lc1, rich.lc2)
rich.lc <- as.data.frame(rich.lc)
rich.lc$Year <- c("2012-2013", "2013-2014", "2014-2015", "2015-2018", "2018-2021")
rich.lc <- rich.lc |> 
  pivot_longer(!Year, names_to = "Sample", values_to = "dRichness")
rich.lc$Sample <- gsub("^.*?\\.", "", rich.lc$Sample)
rich.lc$Sample <- as.numeric(rich.lc$Sample)
rich.lc <- left_join(grouping.cols, rich.lc) |> 
  arrange(Sample) |> 
  select(Sample, Year, Channel, Station, station.trt, channel.trt, Treatment1,
         Treatment2, Treatment3, dRichness)

write_csv(rich.lc,
          file = "data/cleaned/Log-change_rich.csv")


# Plot by Treatment3
# All years
rich.plot.lc1 <- ggplot(rich.lc, aes(x = Treatment3, y = dRichness)) +
  geom_boxplot(aes(fill = Treatment3),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment3),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Log ratio per unit time") +
  ggtitle("Change in perennial richness")
rich.plot.lc1

# By year
rich.plot.lc2 <- ggplot(rich.lc, aes(x = Year, y = dRichness)) +
  geom_boxplot(aes(fill = Treatment3),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment3),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Log ratio per unit time") +
  ggtitle("Change in perennial richness") +
  facet_wrap(~Treatment3)
rich.plot.lc2


# Comparison
qqPlot(filter(rich.lc, Treatment3 == "Treated")$dRichness) # not really normal?
qqPlot(filter(rich.lc, Treatment3 == "Control")$dRichness) # not really normal?

wilcox.test(filter(rich.lc, Treatment3 == "Treated")$dRichness, 
            filter(rich.lc, Treatment3 == "Control")$dRichness) # NS

t.test(filter(rich.lc, Treatment3 == "Treated")$dRichness, 
       filter(rich.lc, Treatment3 == "Control")$dRichness) # NS



# Perennial Shannon -------------------------------------------------------

# Select cols to pivot
shan.long <- per.div |> 
  select(Sample, Year, shan) 

# Pivot wider so every column is a sample and every row is a year
#   and separate 2012-2015 and 2018-2021
shan.wide <- shan.long |> 
  pivot_wider(names_from = Sample, values_from = shan)
shan.change1 <- shan.wide[1:4, -c(1)]
shan.change2 <- shan.wide[4:6, -c(1)]

# Convert to time series object
shan.change1 <- as.matrix(shan.change1)
shants1 <- ts(shan.change1, 1, 4, frequency = 1)
shan.change2 <- as.matrix(shan.change2)
shants2 <- ts(shan.change2, 1, 3, frequency = 1)

# Calculate log change 
shan.lc1 <- log(shants1) - log(stats::lag(shants1)) # 1-year interval for 2012-2015
shan.lc2 <- (log(shants2) - log(stats::lag(shants2))) / 3 # 3-year interval for 2015-2021

# Reformat as dataframe, add years, names & Treatment3
shan.lc <- rbind(shan.lc1, shan.lc2)
shan.lc <- as.data.frame(shan.lc)
shan.lc$Year <- c("2012-2013", "2013-2014", "2014-2015", "2015-2018", "2018-2021")
shan.lc <- shan.lc |> 
  pivot_longer(!Year, names_to = "Sample", values_to = "dShannon")
shan.lc$Sample <- gsub("^.*?\\.", "", shan.lc$Sample)
shan.lc$Sample <- as.numeric(shan.lc$Sample)
shan.lc <- left_join(grouping.cols, shan.lc) |> 
  arrange(Sample) |> 
  select(Sample, Year, Channel, Station, station.trt, channel.trt, Treatment1,
         Treatment2, Treatment3, dShannon)

write_csv(shan.lc,
          file = "data/cleaned/Log-change_shan.csv")


# Plot by Treatment3
# All years
shan.plot.lc1 <- ggplot(shan.lc, aes(x = Treatment3, y = dShannon)) +
  geom_boxplot(aes(fill = Treatment3),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment3),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Log ratio per unit time") +
  ggtitle("Change in perennial diversity") 
shan.plot.lc1

# By year
shan.plot.lc2 <- ggplot(shan.lc, aes(x = Year, y = dShannon)) +
  geom_boxplot(aes(fill = Treatment3),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment3),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Log ratio per unit time") +
  ggtitle("Change in perennial diversity") +
  facet_wrap(~Treatment3)
shan.plot.lc2


# Comparison
qqPlot(filter(shan.lc, Treatment3 == "Treated")$dShannon) # not normal?
qqPlot(filter(shan.lc, Treatment3 == "Control")$dShannon) # not normal?

wilcox.test(filter(shan.lc, Treatment3 == "Treated")$dShannon, 
            filter(shan.lc, Treatment3 == "Control")$dShannon) # NS

t.test(filter(shan.lc, Treatment3 == "Treated")$dShannon, 
       filter(shan.lc, Treatment3 == "Control")$dShannon) # NS



# First/last comparison (2012, 2021), all res vars ------------------------

# Extract out 2012 and 2021, set up cols for finding difference (subtraction)
# Total cover
totalfirst <- total.all |> 
  filter(Year == 2012) |> 
  select(Sample, Cover) |> 
  rename(first_total = Cover) |> 
  rbind(c(Sample = 17, first_total = NA)) # add missing sample
totallast <- total.all |> 
  filter(Year == 2021) |> 
  select(Sample, Cover) |> 
  rename(last_total = Cover)

# Herb cover
herbfirst <- herb.all |> 
  filter(Year == 2012) |> 
  select(Sample, Cover) |> 
  rename(first_herb = Cover) |> 
  rbind(c(Sample = 17, first_herb = NA)) # add missing sample
herblast <- herb.all |> 
  filter(Year == 2021) |> 
  select(Sample, Cover) |> 
  rename(last_herb = Cover)

# Notree cover
notreefirst <- notree.all |> 
  filter(Year == 2012) |> 
  select(Sample, Cover) |> 
  rename(first_notree = Cover) |> 
  rbind(c(Sample = 17, first_notree = NA)) # add missing sample
notreelast <- notree.all |> 
  filter(Year == 2021) |> 
  select(Sample, Cover) |> 
  rename(last_notree = Cover)

# Perennial richness
richfirst <- per.div |> 
  filter(Year == 2012) |> 
  select(Sample, rich) |> 
  rename(first_rich = rich) |> 
  rbind(c(Sample = 17, first_rich = NA)) # add missing sample
richlast <- per.div |> 
  filter(Year == 2021) |> 
  select(Sample, rich) |> 
  rename(last_rich = rich)

# Perennial Shannon
shanfirst <- per.div |> 
  filter(Year == 2012) |> 
  select(Sample, shan) |> 
  rename(first_shan = shan) |> 
  rbind(c(Sample = 17, first_shan = NA)) # add missing sample
shanlast <- per.div |> 
  filter(Year == 2021) |> 
  select(Sample, shan) |> 
  rename(last_shan = shan)

# Combine all cols
firstlast <- left_join(totalfirst, totallast) |> 
  left_join(herbfirst) |> 
  left_join(herblast) |> 
  left_join(notreefirst) |> 
  left_join(notreelast) |> 
  left_join(richfirst) |> 
  left_join(richlast) |> 
  left_join(shanfirst) |> 
  left_join(shanlast)

# Find difference (subtract 2021 - 2012)
firstlast <- firstlast |> 
  mutate(total = last_total / first_total,
         notree = last_notree / first_notree,
         herb = last_herb / first_herb,
         rich = last_rich / first_rich,
         shan = last_shan / first_shan)

# Calculate change in cover according to Munson 2013
firstlast <- firstlast |> 
  mutate(total.lc = log(total) / (2021 - 2012),
         herb.lc = log(herb) / (2021 - 2012),
         notree.lc = log(notree) / (2021 - 2012),
         rich.lc = log(rich) / (2021 - 2012),
         shan.lc = log(shan) / (2021 - 2012)) |> 
  select(Sample, total.lc, herb.lc, notree.lc, rich.lc, shan.lc) |> 
  arrange(Sample)

firstlast <- left_join(firstlast, station.meta) |> 
  select(Sample, Name, Channel, Station, Treatment3, total.lc,
         herb.lc, notree.lc, rich.lc, shan.lc)
  

# Write to csv
write.csv(firstlast,
          file = "data/cleaned/Log-change_first-last.csv",
          row.names = FALSE)

# Plot
notree.plot.lc3 <- ggplot(firstlast, aes(x = Treatment3, y = notree.lc)) +
  geom_boxplot(aes(fill = Treatment3),
               alpha = 0.4,
               outlier.shape = NA) +
  geom_jitter(aes(color = Treatment3),
              alpha = 0.9,
              size = 2) +
  scale_fill_manual(values = c("red", "#1F78B4")) +
  scale_color_manual(values = c("red", "#1F78B4")) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab(NULL) +
  ylab("Log ratio per unit time") +
  ggtitle("Change in grass, forb & shrub cover from 2012 to 2021") 
notree.plot.lc3

save.image("RData/Change-over-time_veg-2012-2021.RData")

