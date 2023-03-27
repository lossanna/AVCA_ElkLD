library(tidyverse)

# Load data ---------------------------------------------------------------

total.all <- read.csv("data/cleaned/Summarised-all_total-plant-cover.csv")
herb.all <- read.csv("data/cleaned/Summarised-all_herb-cover.csv") 
per.div <- read.csv("data/cleaned/Summarised-all_perennial-diversity.csv")
meta.raw <- read.table("data/cleaned/sequencing/sequencing_metadata.txt",
                   sep = "\t", header = TRUE)

# Data wrangling ----------------------------------------------------------

# Format meta to connect Names with Sample no.
meta <- meta.raw |> 
  select(Sample, Name)


# Total cover -------------------------------------------------------------

# Format to join with names from meta
total.change <- total.all |> 
  select(Year, Channel, Station, Cover) 
total.change$Name <- paste0(total.change$Channel, ", ", total.change$Station)
total.change <- total.change |> 
  select(-Channel, -Station) |> 
  mutate(Year = as.numeric(Year))

# Add Sample no. from meta
total.change <- left_join(total.change, meta)
total.change <- total.change |> 
  select(-Name)
total.change.long <- total.change

# Pivot wider so every column is a sample and every row is a year
  # separate 2012-2015 and 2018-2021
total.change.wide <- total.change.long |> 
  pivot_wider(names_from = Sample, values_from = Cover)
total.change1 <- total.change.wide[1:4, -c(1)]
total.change2 <- total.change.wide[4:6, -c(1)]

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
total.pd <- left_join(total.pd, meta)
total.pd <- total.pd |> 
  mutate(Treatment3 = case_when(
    str_detect(total.pd$Name, "Channel 12|Channel 19") ~ "Control",
    str_detect(total.pd$Name, "Channel 13|Channel 21") ~ "Treated")) |> 
  arrange(Sample)

# Back-transform from log
total.pd$backtrans.dCover <- exp(total.pd$dCover)

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
  
t.test(filter(total.pd, Treatment3 == "Treated")$dCover, 
       filter(total.pd, Treatment3 == "Control")$dCover) # NS



# Herbaceous cover --------------------------------------------------------

# Format to join with names from meta
herb.change <- herb.all |> 
  select(Year, Channel, Station, Cover) 
herb.change$Name <- paste0(herb.change$Channel, ", ", herb.change$Station)
herb.change <- herb.change |> 
  select(-Channel, -Station) |> 
  mutate(Year = as.numeric(Year))

# Add Sample no. from meta
herb.change <- left_join(herb.change, meta)
herb.change <- herb.change |> 
  select(-Name)
herb.change.long <- herb.change

# Pivot wider so every column is a sample and every row is a year
  # separate 2012-2015 and 2018-2021
herb.change.wide <- herb.change.long |> 
  pivot_wider(names_from = Sample, values_from = Cover)
herb.change1 <- herb.change.wide[1:4, -c(1)]
herb.change2 <- herb.change.wide[4:6, -c(1)]

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
herb.pd <- left_join(herb.pd, meta)
herb.pd <- herb.pd |> 
  mutate(Treatment3 = case_when(
    str_detect(herb.pd$Name, "Channel 12|Channel 19") ~ "Control",
    str_detect(herb.pd$Name, "Channel 13|Channel 21") ~ "Treated")) |> 
  arrange(Sample)

# Back-transform from log
herb.pd$backtrans.dCover <- exp(herb.pd$dCover)

write_csv(herb.pd,
          file = "data/cleaned/Percent-difference_herb-cover.csv")


# Plot by Treatment3
# All years
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

# T-test
t.test(filter(herb.pd, Treatment3 == "Treated")$dCover, 
       filter(herb.pd, Treatment3 == "Control")$dCover) # NS



# Perennial richness ------------------------------------------------------

# Format to join with names from meta
rich.change <- per.div |> 
  select(Year, Channel, Station, rich) 
rich.change$Name <- paste0(rich.change$Channel, ", ", rich.change$Station)
rich.change <- rich.change |> 
  select(-Channel, -Station) |> 
  mutate(Year = as.numeric(Year))

# Add Sample no. from meta
rich.change <- left_join(rich.change, meta)
rich.change <- rich.change |> 
  select(-Name)
rich.change.long <- rich.change

# Pivot wider so every column is a sample and every row is a year
# separate 2012-2015 and 2018-2021
rich.change.wide <- rich.change.long |> 
  pivot_wider(names_from = Sample, values_from = rich)
rich.change1 <- rich.change.wide[1:4, -c(1)]
rich.change2 <- rich.change.wide[4:6, -c(1)]

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
rich.pd <- left_join(rich.pd, meta)
rich.pd <- rich.pd |> 
  mutate(Treatment3 = case_when(
    str_detect(rich.pd$Name, "Channel 12|Channel 19") ~ "Control",
    str_detect(rich.pd$Name, "Channel 13|Channel 21") ~ "Treated")) |> 
  arrange(Sample)

# Back-transform from log
rich.pd$backtrans.dRichness <- exp(rich.pd$dRichness)

write_csv(rich.pd,
          file = "data/cleaned/Percent-difference_rich.csv")


# Plot by Treatment3
# All years
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

t.test(filter(rich.pd, Treatment3 == "Treated")$dRichness, 
       filter(rich.pd, Treatment3 == "Control")$dRichness) # NS



# Perennial Shannon -------------------------------------------------------

# Format to join with names from meta
shan.change <- per.div |> 
  select(Year, Channel, Station, shan) 
shan.change$Name <- paste0(shan.change$Channel, ", ", shan.change$Station)
shan.change <- shan.change |> 
  select(-Channel, -Station) |> 
  mutate(Year = as.numeric(Year))

# Add Sample no. from meta
shan.change <- left_join(shan.change, meta)
shan.change <- shan.change |> 
  select(-Name)
shan.change.long <- shan.change

# Pivot wider so every column is a sample and every row is a year
  # separate 2012-2015 and 2018-2021
shan.change.wide <- shan.change.long |> 
  pivot_wider(names_from = Sample, values_from = shan)
shan.change1 <- shan.change.wide[1:4, -c(1)]
shan.change2 <- shan.change.wide[4:6, -c(1)]

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
shan.pd <- left_join(shan.pd, meta)
shan.pd <- shan.pd |> 
  mutate(Treatment3 = case_when(
    str_detect(shan.pd$Name, "Channel 12|Channel 19") ~ "Control",
    str_detect(shan.pd$Name, "Channel 13|Channel 21") ~ "Treated")) |> 
  arrange(Sample)

# Back-transform from log
shan.pd$backtrans.dShannon <- exp(shan.pd$dShannon)

write_csv(shan.pd,
          file = "data/cleaned/Percent-difference_shan.csv")


# Plot by Treatment3
# All years
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

# T-test
t.test(filter(shan.pd, Treatment3 == "Treated")$dShannon, 
       filter(shan.pd, Treatment3 == "Control")$dShannon) # NS



save.image("RData/Percent-change-over-time.RData")

