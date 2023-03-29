# Purpose: Compare the percent difference over time (calculated by the log response ratio per unit time,
#   as described in Munson 2013, Ecology) to see if rock structures facilitate more or less change
#   because the value of the response variables themselves are not as interesting as how they change
#   in comparison to one another (rather than comparing the absolute values).
# Includes plots of Treated vs. Control with all years combined, and by year interval,
#   and t-test/Wilcox test for comparison.

# Calculated the percent difference from year to year (5 time intervals) for each sample.
# Found no difference in percent difference for Treated vs Control for any response variable
#   (total cover, herb cover, richness, Shannon).


library(tidyverse)
library(car)

# Load data ---------------------------------------------------------------

total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
herb.all <- read.csv("data/cleaned/Summarised-all_herb-cover.csv") 
per.div <- read.csv("data/cleaned/Summarised-all_perennial-diversity.csv")

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

# Calculate percent change 
total.pd1 <- log(totalts1) - log(stats::lag(totalts1)) # 1-year interval for 2012-2015
total.pd2 <- (log(totalts2) - log(stats::lag(totalts2))) / 3 # 3-year interval for 2015-2021

# Reformat as dataframe, add years, names & Treatment3
total.pd <- rbind(total.pd1, total.pd2)
total.pd <- as.data.frame(total.pd)
total.pd$Year <- c("2012-2013", "2013-2014", "2014-2015", "2015-2018", "2018-2021")
total.pd <- total.pd |> 
  pivot_longer(!Year, names_to = "Sample", values_to = "dCover")
total.pd$Sample <- gsub("^.*?\\.", "", total.pd$Sample)
total.pd$Sample <- as.numeric(total.pd$Sample)
total.pd <- left_join(grouping.cols, total.pd) |> 
  arrange(Sample) |> 
  select(Sample, Year, Channel, Station, station.trt, channel.trt, Treatment1,
         Treatment2, Treatment3, dCover)

write_csv(total.pd,
          file = "data/cleaned/Percent-difference_total-cover.csv")


# Plot by Treatment3
# All years
ggplot(total.pd, aes(x = Treatment3, y = dCover)) +
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
ggplot(total.pd, aes(x = Year, y = dCover)) +
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
qqPlot(filter(total.pd, Treatment3 == "Treated")$dCover) # almost normal?
qqPlot(filter(total.pd, Treatment3 == "Control")$dCover) # not really normal?

wilcox.test(filter(total.pd, Treatment3 == "Treated")$dCover, 
            filter(total.pd, Treatment3 == "Control")$dCover) # NS
  
t.test(filter(total.pd, Treatment3 == "Treated")$dCover, 
       filter(total.pd, Treatment3 == "Control")$dCover) # NS



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

# Calculate percent change 
herb.pd1 <- log(herbts1) - log(stats::lag(herbts1)) # 1-year interval for 2012-2015
herb.pd2 <- (log(herbts2) - log(stats::lag(herbts2))) / 3 # 3-year interval for 2015-2021

# Reformat as dataframe, add years, names & Treatment3
herb.pd <- rbind(herb.pd1, herb.pd2)
herb.pd <- as.data.frame(herb.pd)
herb.pd$Year <- c("2012-2013", "2013-2014", "2014-2015", "2015-2018", "2018-2021")
herb.pd <- herb.pd |> 
  pivot_longer(!Year, names_to = "Sample", values_to = "dCover")
herb.pd$Sample <- gsub("^.*?\\.", "", herb.pd$Sample)
herb.pd$Sample <- as.numeric(herb.pd$Sample)
herb.pd <- left_join(grouping.cols, herb.pd) |> 
  arrange(Sample) |> 
  select(Sample, Year, Channel, Station, station.trt, channel.trt, Treatment1,
         Treatment2, Treatment3, dCover)

write_csv(herb.pd,
          file = "data/cleaned/Percent-difference_herb-cover.csv")


# Plot by Treatment3
# All years
ggplot(herb.pd, aes(x = Treatment3, y = dCover)) +
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
ggplot(herb.pd, aes(x = Year, y = dCover)) +
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
qqPlot(filter(herb.pd, Treatment3 == "Treated")$dCover) # not really normal?
qqPlot(filter(herb.pd, Treatment3 == "Control")$dCover) # almost normal?

wilcox.test(filter(herb.pd, Treatment3 == "Treated")$dCover, 
            filter(herb.pd, Treatment3 == "Control")$dCover) # NS

t.test(filter(herb.pd, Treatment3 == "Treated")$dCover, 
       filter(herb.pd, Treatment3 == "Control")$dCover) # NS



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

# Calculate percent change 
rich.pd1 <- log(richts1) - log(stats::lag(richts1)) # 1-year interval for 2012-2015
rich.pd2 <- (log(richts2) - log(stats::lag(richts2))) / 3 # 3-year interval for 2015-2021

# Reformat as dataframe, add years, names & Treatment3
rich.pd <- rbind(rich.pd1, rich.pd2)
rich.pd <- as.data.frame(rich.pd)
rich.pd$Year <- c("2012-2013", "2013-2014", "2014-2015", "2015-2018", "2018-2021")
rich.pd <- rich.pd |> 
  pivot_longer(!Year, names_to = "Sample", values_to = "dRichness")
rich.pd$Sample <- gsub("^.*?\\.", "", rich.pd$Sample)
rich.pd$Sample <- as.numeric(rich.pd$Sample)
rich.pd <- left_join(grouping.cols, rich.pd) |> 
  arrange(Sample) |> 
  select(Sample, Year, Channel, Station, station.trt, channel.trt, Treatment1,
         Treatment2, Treatment3, dRichness)

write_csv(rich.pd,
          file = "data/cleaned/Percent-difference_rich.csv")


# Plot by Treatment3
# All years
ggplot(rich.pd, aes(x = Treatment3, y = dRichness)) +
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

# By year
ggplot(rich.pd, aes(x = Year, y = dRichness)) +
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

# Comparison
qqPlot(filter(rich.pd, Treatment3 == "Treated")$dRichness) # not really normal?
qqPlot(filter(rich.pd, Treatment3 == "Control")$dRichness) # not really normal?

wilcox.test(filter(rich.pd, Treatment3 == "Treated")$dRichness, 
            filter(rich.pd, Treatment3 == "Control")$dRichness) # NS

t.test(filter(rich.pd, Treatment3 == "Treated")$dRichness, 
       filter(rich.pd, Treatment3 == "Control")$dRichness) # NS



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

# Calculate percent change 
shan.pd1 <- log(shants1) - log(stats::lag(shants1)) # 1-year interval for 2012-2015
shan.pd2 <- (log(shants2) - log(stats::lag(shants2))) / 3 # 3-year interval for 2015-2021

# Reformat as dataframe, add years, names & Treatment3
shan.pd <- rbind(shan.pd1, shan.pd2)
shan.pd <- as.data.frame(shan.pd)
shan.pd$Year <- c("2012-2013", "2013-2014", "2014-2015", "2015-2018", "2018-2021")
shan.pd <- shan.pd |> 
  pivot_longer(!Year, names_to = "Sample", values_to = "dShannon")
shan.pd$Sample <- gsub("^.*?\\.", "", shan.pd$Sample)
shan.pd$Sample <- as.numeric(shan.pd$Sample)
shan.pd <- left_join(grouping.cols, shan.pd) |> 
  arrange(Sample) |> 
  select(Sample, Year, Channel, Station, station.trt, channel.trt, Treatment1,
         Treatment2, Treatment3, dShannon)

write_csv(shan.pd,
          file = "data/cleaned/Percent-difference_shan.csv")


# Plot by Treatment3
# All years
ggplot(shan.pd, aes(x = Treatment3, y = dShannon)) +
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

# By year
ggplot(shan.pd, aes(x = Year, y = dShannon)) +
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


# Comparison
qqPlot(filter(shan.pd, Treatment3 == "Treated")$dShannon) # not normal?
qqPlot(filter(shan.pd, Treatment3 == "Control")$dShannon) # not normal?

wilcox.test(filter(shan.pd, Treatment3 == "Treated")$dShannon, 
            filter(shan.pd, Treatment3 == "Control")$dShannon) # NS

t.test(filter(shan.pd, Treatment3 == "Treated")$dShannon, 
       filter(shan.pd, Treatment3 == "Control")$dShannon) # NS



save.image("RData/Percent-change-over-time.RData")

