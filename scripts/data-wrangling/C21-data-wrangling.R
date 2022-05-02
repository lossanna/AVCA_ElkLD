library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

plant <- read_xlsx("scripts/data-wrangling/Plant-functional-groups-by-common-name.xlsx")

raw.c21.01_07.5 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                            sheet = "1 + 7.5 BAF")
raw.c21.01_49 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "1 + 49 ORD")
raw.c21.02_15 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "CHECK 2+15BAF")
raw.c21.03_80 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "3 + 80 BAF")
raw.c21.04_90 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "4 + 90 ORD")
raw.c21.06_36 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "6 + 36 BAF")
raw.c21.07_49.5 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "7 + 49.5 ORD")
raw.c21.07_66 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "7 + 66 BAF")
raw.c21.08_62 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "8 + 62 BAF")
raw.c21.09_99 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "9 + 99 ORD")
raw.c21.10_13 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "10 + 13 BAF")
raw.c21.11_12 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                            sheet = "11 + 12 ORD")
raw.c21.11_24 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                            sheet = "11 + 24 BAF")
raw.c21.11_63 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "11 + 63 ORD")
raw.c21.11_73 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 21 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "11 + 73 BAF")

names.raw <- c("Common", "Scientific", "Nov21_2L", "Nov21_1L", "Nov21_1R", "Nov21_2R",
               "Nov18_2L", "Nov18_1L", "Nov18_1R", "Nov18_2R",
               "Mar12_3L", "Mar12_2L", "Mar12_1L", "Mar12_1R", "Mar12_2R", "Mar12_3R",
               "Nov12_3L", "Nov12_2L", "Nov12_1L", "Nov12_1R", "Nov12_2R", "Nov12_3R",
               "Nov13_3L", "Nov13_2L", "Nov13_1L", "Nov13_1R", "Nov13_2R", "Nov13_3R",
               "Nov14_2L", "Nov14_1L", "Nov14_1R", "Nov14_2R",
               "Nov15_2L", "Nov15_1L", "Nov15_1R", "Nov15_2R") 



# C21 1 + 7.5 -------------------------------------------------------------

# Remove headers, rename columns, and remove unnecessary rows
c21.01_07.5 <- raw.c21.01_07.5[-c(1:7), -c(37:43)]
colnames(c21.01_07.5) <- names.raw

c21.01_07.5 <- c21.01_07.5 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c21.01_07.5$Common[c21.01_07.5$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c21.01_07.5$Common[c21.01_07.5$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.01_07.5$Common[c21.01_07.5$Common == "Cup grass"] <- "Cupgrass"
c21.01_07.5$Common[c21.01_07.5$Common == "Pepper weed"] <- "Pepperweed"

# Assign missing scientific names (when known) and correct spelling
c21.01_07.5$Scientific[c21.01_07.5$Common == "Pepperweed"] <- "Lepidium"
c21.01_07.5$Scientific[c21.01_07.5$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c21.01_07.5$Common, "Unk") == TRUE)
which(str_detect(c21.01_07.5$Common, "2021") == TRUE)
c21.01_07.5[19, 1] <- "Unknown annual grass, C21"
c21.01_07.5[28, 1] <- "Unknown annual forb, C21"

# Assign functional groups
c21.01_07.5 <- c21.01_07.5 %>% 
  mutate(Functional = rep(NA, nrow(c21.01_07.5)))
for(i in 1:nrow(c21.01_07.5)) {
  if(c21.01_07.5$Common[i] %in% plant$per.grass == TRUE) {
    c21.01_07.5$Functional[i] <- "Perennial grass"
  } else if(c21.01_07.5$Common[i] %in% plant$an.grass == TRUE) {
    c21.01_07.5$Functional[i] <- "Annual grass"
  } else if(c21.01_07.5$Common[i] %in% plant$per.forb == TRUE) {
    c21.01_07.5$Functional[i] <- "Perennial forb"
  } else if(c21.01_07.5$Common[i] %in% plant$an.forb == TRUE) {
    c21.01_07.5$Functional[i] <- "Annual forb"
  } else if(c21.01_07.5$Common[i] %in% plant$shrub == TRUE) {
    c21.01_07.5$Functional[i] <- "Shrub"
  } else if(c21.01_07.5$Common[i] %in% plant$tree == TRUE) {
    c21.01_07.5$Functional[i] <- "Tree"
  } else if(c21.01_07.5$Common[i] %in% plant$ground == TRUE) {
    c21.01_07.5$Functional[i] <- "Ground cover"
  } else {
    c21.01_07.5$Functional[i] <- "assign unknown"
  }
}

c21.01_07.5[19, "Functional"] <- "Annual grass"
c21.01_07.5[28, "Functional"] <- "Annual forb"
count(c21.01_07.5, Functional)

# Assign native status
c21.01_07.5 <- c21.01_07.5 %>% 
  mutate(Native = rep(NA, nrow(c21.01_07.5)))
for(i in 1:nrow(c21.01_07.5)) {
  if(c21.01_07.5$Common[i] %in% plant$invasive == TRUE) {
    c21.01_07.5$Native[i] <- "Invasive"
  } else if(c21.01_07.5$Common[i] %in% plant$native == TRUE) {
    c21.01_07.5$Native[i] <- "Native"
  } else if(c21.01_07.5$Common[i] %in% plant$ground == TRUE) {
    c21.01_07.5$Native[i] <- "Ground cover"
  } else {
    c21.01_07.5$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.01_07.5 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.01_07.5 <- c21.01_07.5 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.01_07.5 <- cbind(c21.01_07.5, cover)

wide.c21.01_07.5 <- c21.01_07.5

# Pivot data from wide to long
c21.01_07.5 <- wide.c21.01_07.5
c21.01_07.5 <- c21.01_07.5 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.01_07.5 <- c21.01_07.5 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.01_07.5)) {
  if(c21.01_07.5$Year[i] == "Mar12") {
    c21.01_07.5$Year[i] <- "2012-03-01"
  } else if(c21.01_07.5$Year[i] == "Nov12") {
    c21.01_07.5$Year[i] <- "2012-11-01"
  } else if(c21.01_07.5$Year[i] == "Nov13") {
    c21.01_07.5$Year[i] <- "2013-11-01"
  } else if(c21.01_07.5$Year[i] == "Nov14") {
    c21.01_07.5$Year[i] <- "2014-11-01"
  } else if(c21.01_07.5$Year[i] == "Nov15") {
    c21.01_07.5$Year[i] <- "2015-11-01"
  } else if(c21.01_07.5$Year[i] == "Nov18") {
    c21.01_07.5$Year[i] <- "2018-11-01"
  } else {
    c21.01_07.5$Year[i] <- "2021-11-01"
  }
}
c21.01_07.5$Year <- as.Date(c21.01_07.5$Year, format = "%Y-%m-%d")

# Add channel and station
c21.01_07.5$Station <- rep("Channel 21_Station 01 + 07.5 BAF", nrow(c21.01_07.5))



# C21 1 + 49 --------------------------------------------------------------

# Remove headers, rename columns, and remove unnecessary rows
c21.01_49 <- raw.c21.01_49[-c(1:7), -c(37:43)]
colnames(c21.01_49) <- names.raw

c21.01_49 <- c21.01_49 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row 
  # AZ wrightwort is in 2 rows, with data in both rows
  # Am manually moving/re-entering data to condense into 1 row
c21.01_49[37, 7] <- "0.5"
c21.01_49 <- c21.01_49[-26, ]

# Reassign CHs/Xs with values for Nov 2013 annual grasses
which(c21.01_49 == "CH", arr.ind = TRUE)

c21.01_49[14, 23] <- "0"
c21.01_49[16, 23] <- "2.5"

c21.01_49[14, 24] <- "0"
c21.01_49[16, 24] <- "1.25"
c21.01_49[20, 24] <- "1.25"

# Standardize common names
c21.01_49$Common[c21.01_49$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c21.01_49$Common[c21.01_49$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.01_49$Common[c21.01_49$Common == "Cup grass"] <- "Cupgrass"
c21.01_49$Common[c21.01_49$Common == "spiderling"] <- "Coulter's spiderling"

# Assign missing scientific names (when known) and correct spelling
c21.01_49$Scientific[c21.01_49$Common == "Pepperweed"] <- "Lepidium"
c21.01_49$Scientific[c21.01_49$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c21.01_49$Scientific[c21.01_49$Scientific == "Eragrostis echinocloidea"] <- "Eragrostis echinochloidea"

# Rename unknowns
which(str_detect(c21.01_49$Common, "Unk") == TRUE)
which(str_detect(c21.01_49$Common, "2021") == TRUE)
c21.01_49[19, 1] <- "Unknown annual grass, C21"

# Assign functional groups
c21.01_49 <- c21.01_49 %>% 
  mutate(Functional = rep(NA, nrow(c21.01_49)))
for(i in 1:nrow(c21.01_49)) {
  if(c21.01_49$Common[i] %in% plant$per.grass == TRUE) {
    c21.01_49$Functional[i] <- "Perennial grass"
  } else if(c21.01_49$Common[i] %in% plant$an.grass == TRUE) {
    c21.01_49$Functional[i] <- "Annual grass"
  } else if(c21.01_49$Common[i] %in% plant$per.forb == TRUE) {
    c21.01_49$Functional[i] <- "Perennial forb"
  } else if(c21.01_49$Common[i] %in% plant$an.forb == TRUE) {
    c21.01_49$Functional[i] <- "Annual forb"
  } else if(c21.01_49$Common[i] %in% plant$shrub == TRUE) {
    c21.01_49$Functional[i] <- "Shrub"
  } else if(c21.01_49$Common[i] %in% plant$tree == TRUE) {
    c21.01_49$Functional[i] <- "Tree"
  } else if(c21.01_49$Common[i] %in% plant$ground == TRUE) {
    c21.01_49$Functional[i] <- "Ground cover"
  } else {
    c21.01_49$Functional[i] <- "assign unknown"
  }
}

c21.01_49[19, "Functional"] <- "Annual grass"
count(c21.01_49, Functional)

# Assign native status
c21.01_49 <- c21.01_49 %>% 
  mutate(Native = rep(NA, nrow(c21.01_49)))
for(i in 1:nrow(c21.01_49)) {
  if(c21.01_49$Common[i] %in% plant$invasive == TRUE) {
    c21.01_49$Native[i] <- "Invasive"
  } else if(c21.01_49$Common[i] %in% plant$native == TRUE) {
    c21.01_49$Native[i] <- "Native"
  } else if(c21.01_49$Common[i] %in% plant$ground == TRUE) {
    c21.01_49$Native[i] <- "Ground cover"
  } else {
    c21.01_49$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.01_49 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.01_49 <- c21.01_49 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.01_49 <- cbind(c21.01_49, cover)

wide.c21.01_49 <- c21.01_49

# Pivot data from wide to long
c21.01_49 <- wide.c21.01_49
c21.01_49 <- c21.01_49 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.01_49 <- c21.01_49 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.01_49)) {
  if(c21.01_49$Year[i] == "Mar12") {
    c21.01_49$Year[i] <- "2012-03-01"
  } else if(c21.01_49$Year[i] == "Nov12") {
    c21.01_49$Year[i] <- "2012-11-01"
  } else if(c21.01_49$Year[i] == "Nov13") {
    c21.01_49$Year[i] <- "2013-11-01"
  } else if(c21.01_49$Year[i] == "Nov14") {
    c21.01_49$Year[i] <- "2014-11-01"
  } else if(c21.01_49$Year[i] == "Nov15") {
    c21.01_49$Year[i] <- "2015-11-01"
  } else if(c21.01_49$Year[i] == "Nov18") {
    c21.01_49$Year[i] <- "2018-11-01"
  } else {
    c21.01_49$Year[i] <- "2021-11-01"
  }
}
c21.01_49$Year <- as.Date(c21.01_49$Year, format = "%Y-%m-%d")

# Add channel and station
c21.01_49$Station <- rep("Channel 21_Station 01 + 49 ORD", nrow(c21.01_49))



# C21 2 + 15 --------------------------------------------------------------

# Remove headers, rename columns, and remove unnecessary rows
c21.02_15 <- raw.c21.02_15[-c(1:7), -c(37:43)]
colnames(c21.02_15) <- names.raw

c21.02_15 <- c21.02_15 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c21.02_15$Common[c21.02_15$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c21.02_15$Common[c21.02_15$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.02_15$Common[c21.02_15$Common == "Cup grass"] <- "Cupgrass"
c21.02_15$Common[c21.02_15$Common == "spiderling"] <- "Coulter's spiderling"

# Assign missing scientific names (when known) and correct spelling
c21.02_15$Scientific[c21.02_15$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c21.02_15$Common, "Unk") == TRUE)
which(str_detect(c21.02_15$Common, "2021") == TRUE)
c21.02_15[19, 1] <- "Unknown annual grass, C21"

# Assign functional groups
c21.02_15 <- c21.02_15 %>% 
  mutate(Functional = rep(NA, nrow(c21.02_15)))
for(i in 1:nrow(c21.02_15)) {
  if(c21.02_15$Common[i] %in% plant$per.grass == TRUE) {
    c21.02_15$Functional[i] <- "Perennial grass"
  } else if(c21.02_15$Common[i] %in% plant$an.grass == TRUE) {
    c21.02_15$Functional[i] <- "Annual grass"
  } else if(c21.02_15$Common[i] %in% plant$per.forb == TRUE) {
    c21.02_15$Functional[i] <- "Perennial forb"
  } else if(c21.02_15$Common[i] %in% plant$an.forb == TRUE) {
    c21.02_15$Functional[i] <- "Annual forb"
  } else if(c21.02_15$Common[i] %in% plant$shrub == TRUE) {
    c21.02_15$Functional[i] <- "Shrub"
  } else if(c21.02_15$Common[i] %in% plant$tree == TRUE) {
    c21.02_15$Functional[i] <- "Tree"
  } else if(c21.02_15$Common[i] %in% plant$ground == TRUE) {
    c21.02_15$Functional[i] <- "Ground cover"
  } else {
    c21.02_15$Functional[i] <- "assign unknown"
  }
}

c21.02_15[19, "Functional"] <- "Annual grass"
count(c21.02_15, Functional)

# Assign native status
c21.02_15 <- c21.02_15 %>% 
  mutate(Native = rep(NA, nrow(c21.02_15)))
for(i in 1:nrow(c21.02_15)) {
  if(c21.02_15$Common[i] %in% plant$invasive == TRUE) {
    c21.02_15$Native[i] <- "Invasive"
  } else if(c21.02_15$Common[i] %in% plant$native == TRUE) {
    c21.02_15$Native[i] <- "Native"
  } else if(c21.02_15$Common[i] %in% plant$ground == TRUE) {
    c21.02_15$Native[i] <- "Ground cover"
  } else {
    c21.02_15$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.02_15 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.02_15 <- c21.02_15 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.02_15 <- cbind(c21.02_15, cover)

wide.c21.02_15 <- c21.02_15

# Pivot data from wide to long
c21.02_15 <- wide.c21.02_15
c21.02_15 <- c21.02_15 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.02_15 <- c21.02_15 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.02_15)) {
  if(c21.02_15$Year[i] == "Mar12") {
    c21.02_15$Year[i] <- "2012-03-01"
  } else if(c21.02_15$Year[i] == "Nov12") {
    c21.02_15$Year[i] <- "2012-11-01"
  } else if(c21.02_15$Year[i] == "Nov13") {
    c21.02_15$Year[i] <- "2013-11-01"
  } else if(c21.02_15$Year[i] == "Nov14") {
    c21.02_15$Year[i] <- "2014-11-01"
  } else if(c21.02_15$Year[i] == "Nov15") {
    c21.02_15$Year[i] <- "2015-11-01"
  } else if(c21.02_15$Year[i] == "Nov18") {
    c21.02_15$Year[i] <- "2018-11-01"
  } else {
    c21.02_15$Year[i] <- "2021-11-01"
  }
}
c21.02_15$Year <- as.Date(c21.02_15$Year, format = "%Y-%m-%d")

# Add channel and station
c21.02_15$Station <- rep("Channel 21_Station 02 + 15 BAF", nrow(c21.02_15))



# C21 3 + 80 --------------------------------------------------------------

# Remove headers and rename columns
c21.03_80 <- raw.c21.03_80[-c(1:7), -c(37:43)]
colnames(c21.03_80) <- names.raw

# Reassign CHs/Xs with values for Nov 2018 perennial forbs
which(c21.03_80 == "x", arr.ind = TRUE)
which(c21.03_80 == "xc", arr.ind = TRUE)

c21.03_80[28, 10] <- "0.5"
c21.03_80[29, 10] <- "0.5"
c21.03_80[30, 10] <- "0.5"

# Reassign CHs/Xs with values for Nov 2018 annual grasses
which(c21.03_80 == "x", arr.ind = TRUE)

c21.03_80[17, 10] <- "0"
c21.03_80[20, 10] <- "15"

# Reassign CHs/Xs with values for Nov 2018 annual forbs
which(c21.03_80 == "x", arr.ind = TRUE)

c21.03_80[39, 7] <- "0.5"
c21.03_80[40, 10] <- "2.5"

# Remove unnecessary rows
c21.03_80 <- c21.03_80 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c21.03_80$Common[c21.03_80$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.03_80$Common[c21.03_80$Common == "Cup grass"] <- "Cupgrass"

# Assign missing scientific names (when known) and correct spelling
c21.03_80$Scientific[c21.03_80$Common == "Spurge"] <- "Euphorbia"
c21.03_80$Scientific[c21.03_80$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c21.03_80$Scientific[c21.03_80$Scientific == "Mullugo vert."] <- "Mollugo verticillata"

# Rename unknowns
which(str_detect(c21.03_80$Common, "Unk") == TRUE)
which(str_detect(c21.03_80$Common, "2021") == TRUE)
c21.03_80[18, 1] <- "Unknown annual grass, C21"

# Assign functional groups
c21.03_80 <- c21.03_80 %>% 
  mutate(Functional = rep(NA, nrow(c21.03_80)))
for(i in 1:nrow(c21.03_80)) {
  if(c21.03_80$Common[i] %in% plant$per.grass == TRUE) {
    c21.03_80$Functional[i] <- "Perennial grass"
  } else if(c21.03_80$Common[i] %in% plant$an.grass == TRUE) {
    c21.03_80$Functional[i] <- "Annual grass"
  } else if(c21.03_80$Common[i] %in% plant$per.forb == TRUE) {
    c21.03_80$Functional[i] <- "Perennial forb"
  } else if(c21.03_80$Common[i] %in% plant$an.forb == TRUE) {
    c21.03_80$Functional[i] <- "Annual forb"
  } else if(c21.03_80$Common[i] %in% plant$shrub == TRUE) {
    c21.03_80$Functional[i] <- "Shrub"
  } else if(c21.03_80$Common[i] %in% plant$tree == TRUE) {
    c21.03_80$Functional[i] <- "Tree"
  } else if(c21.03_80$Common[i] %in% plant$ground == TRUE) {
    c21.03_80$Functional[i] <- "Ground cover"
  } else {
    c21.03_80$Functional[i] <- "assign unknown"
  }
}

c21.03_80[18, "Functional"] <- "Annual grass"
count(c21.03_80, Functional)

# Assign native status
c21.03_80 <- c21.03_80 %>% 
  mutate(Native = rep(NA, nrow(c21.03_80)))
for(i in 1:nrow(c21.03_80)) {
  if(c21.03_80$Common[i] %in% plant$invasive == TRUE) {
    c21.03_80$Native[i] <- "Invasive"
  } else if(c21.03_80$Common[i] %in% plant$native == TRUE) {
    c21.03_80$Native[i] <- "Native"
  } else if(c21.03_80$Common[i] %in% plant$ground == TRUE) {
    c21.03_80$Native[i] <- "Ground cover"
  } else {
    c21.03_80$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.03_80 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.03_80 <- c21.03_80 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.03_80 <- cbind(c21.03_80, cover)

wide.c21.03_80 <- c21.03_80

# Pivot data from wide to long
c21.03_80 <- wide.c21.03_80
c21.03_80 <- c21.03_80 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.03_80 <- c21.03_80 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.03_80)) {
  if(c21.03_80$Year[i] == "Mar12") {
    c21.03_80$Year[i] <- "2012-03-01"
  } else if(c21.03_80$Year[i] == "Nov12") {
    c21.03_80$Year[i] <- "2012-11-01"
  } else if(c21.03_80$Year[i] == "Nov13") {
    c21.03_80$Year[i] <- "2013-11-01"
  } else if(c21.03_80$Year[i] == "Nov14") {
    c21.03_80$Year[i] <- "2014-11-01"
  } else if(c21.03_80$Year[i] == "Nov15") {
    c21.03_80$Year[i] <- "2015-11-01"
  } else if(c21.03_80$Year[i] == "Nov18") {
    c21.03_80$Year[i] <- "2018-11-01"
  } else {
    c21.03_80$Year[i] <- "2021-11-01"
  }
}
c21.03_80$Year <- as.Date(c21.03_80$Year, format = "%Y-%m-%d")

# Add channel and station
c21.03_80$Station <- rep("Channel 21_Station 03 + 80 BAF", nrow(c21.03_80))



# C21 4 + 90 --------------------------------------------------------------

# Remove headers and rename columns
c21.04_90 <- raw.c21.04_90[-c(1:7), -c(37:43)]
colnames(c21.04_90) <- names.raw

# Add missing common names
c21.04_90$Common[c21.04_90$Scientific == "Chlor. Virginia"] <- "Feather fingergrass"

# Remove unnecessary rows
c21.04_90 <- c21.04_90 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover")) %>% 
  filter(Common != "Spurge") # remove empty row with weird scientific name to prevent double scientific naming of "spurge"

# Standardize common names
c21.04_90$Common[c21.04_90$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c21.04_90$Common[c21.04_90$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.04_90$Common[c21.04_90$Common == "Cup grass"] <- "Cupgrass"

# Assign missing scientific names (when known) and correct spelling
c21.04_90$Scientific[c21.04_90$Common == "African lovegrass"] <- "Eragrostis echinochloidea"
c21.04_90$Scientific[c21.04_90$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c21.04_90$Scientific[c21.04_90$Scientific == "Chlor. Virginia"] <- "Chloris virgata"

# Rename unknowns
which(str_detect(c21.04_90$Common, "Unk") == TRUE)
which(str_detect(c21.04_90$Common, "2021") == TRUE)
c21.04_90[19, 1] <- "Unknown annual grass, C21"

# Assign functional groups
c21.04_90 <- c21.04_90 %>% 
  mutate(Functional = rep(NA, nrow(c21.04_90)))
for(i in 1:nrow(c21.04_90)) {
  if(c21.04_90$Common[i] %in% plant$per.grass == TRUE) {
    c21.04_90$Functional[i] <- "Perennial grass"
  } else if(c21.04_90$Common[i] %in% plant$an.grass == TRUE) {
    c21.04_90$Functional[i] <- "Annual grass"
  } else if(c21.04_90$Common[i] %in% plant$per.forb == TRUE) {
    c21.04_90$Functional[i] <- "Perennial forb"
  } else if(c21.04_90$Common[i] %in% plant$an.forb == TRUE) {
    c21.04_90$Functional[i] <- "Annual forb"
  } else if(c21.04_90$Common[i] %in% plant$shrub == TRUE) {
    c21.04_90$Functional[i] <- "Shrub"
  } else if(c21.04_90$Common[i] %in% plant$tree == TRUE) {
    c21.04_90$Functional[i] <- "Tree"
  } else if(c21.04_90$Common[i] %in% plant$ground == TRUE) {
    c21.04_90$Functional[i] <- "Ground cover"
  } else {
    c21.04_90$Functional[i] <- "assign unknown"
  }
}

c21.04_90[19, "Functional"] <- "Annual grass"
count(c21.04_90, Functional)

# Assign native status
c21.04_90 <- c21.04_90 %>% 
  mutate(Native = rep(NA, nrow(c21.04_90)))
for(i in 1:nrow(c21.04_90)) {
  if(c21.04_90$Common[i] %in% plant$invasive == TRUE) {
    c21.04_90$Native[i] <- "Invasive"
  } else if(c21.04_90$Common[i] %in% plant$native == TRUE) {
    c21.04_90$Native[i] <- "Native"
  } else if(c21.04_90$Common[i] %in% plant$ground == TRUE) {
    c21.04_90$Native[i] <- "Ground cover"
  } else {
    c21.04_90$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.04_90 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.04_90 <- c21.04_90 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.04_90 <- cbind(c21.04_90, cover)

wide.c21.04_90 <- c21.04_90

# Pivot data from wide to long
c21.04_90 <- wide.c21.04_90
c21.04_90 <- c21.04_90 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.04_90 <- c21.04_90 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.04_90)) {
  if(c21.04_90$Year[i] == "Mar12") {
    c21.04_90$Year[i] <- "2012-03-01"
  } else if(c21.04_90$Year[i] == "Nov12") {
    c21.04_90$Year[i] <- "2012-11-01"
  } else if(c21.04_90$Year[i] == "Nov13") {
    c21.04_90$Year[i] <- "2013-11-01"
  } else if(c21.04_90$Year[i] == "Nov14") {
    c21.04_90$Year[i] <- "2014-11-01"
  } else if(c21.04_90$Year[i] == "Nov15") {
    c21.04_90$Year[i] <- "2015-11-01"
  } else if(c21.04_90$Year[i] == "Nov18") {
    c21.04_90$Year[i] <- "2018-11-01"
  } else {
    c21.04_90$Year[i] <- "2021-11-01"
  }
}
c21.04_90$Year <- as.Date(c21.04_90$Year, format = "%Y-%m-%d")

# Add channel and station
c21.04_90$Station <- rep("Channel 21_Station 04 + 90 ORD", nrow(c21.04_90))



# C21 6 + 36 --------------------------------------------------------------

# Remove headers, rename columns,
c21.06_36 <- raw.c21.06_36[-c(1:7), -c(37:43)]
colnames(c21.06_36) <- names.raw

# Add missing common names
c21.06_36$Common[c21.06_36$Scientific == "Port. unb"] <- "Purslane"
c21.06_36$Common[c21.06_36$Scientific == "Mullugo"] <- "Carpetweed"

# Reassign CHs/Xs with values for Nov 2018 perennial forbs
which(c21.06_36 == "x", arr.ind = TRUE)

c21.06_36[31, 7] <- "1.25"
c21.06_36[32, 7] <- "1.25"

# Remove unnecessary rows
c21.06_36 <- c21.06_36 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Reassign CHs/Xs with values for Nov 2018 annual grasses
which(c21.06_36 == "x", arr.ind = TRUE)
which(c21.06_36 == "CH", arr.ind = TRUE)

c21.06_36[14, 7] <- "0"
c21.06_36[17, 7] <- "7.5"
c21.06_36[19, 7] <- "7.5"

c21.06_36[14, 8] <- "0"
c21.06_36[17, 8] <- "2.5"

c21.06_36[14, 17] <- "0"
c21.06_36[22, 17] <- "0.5"

# Delete repetitive values in Nov 2018 annual forbs row
c21.06_36[28, 7] <- "0"
c21.06_36[28, 10] <- "0"

# Standardize common names
c21.06_36$Common[c21.06_36$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c21.06_36$Common[c21.06_36$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.06_36$Common[c21.06_36$Common == "Cup grass"] <- "Cupgrass"
c21.06_36$Common[c21.06_36$Common == "Indianwheat"] <- "Indian wheat"

# Assign missing scientific names (when known) and correct spelling
c21.06_36$Scientific[c21.06_36$Common == "Pepperweed"] <- "Lepidium"
c21.06_36$Scientific[c21.06_36$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c21.06_36$Scientific[c21.06_36$Scientific == "Mullugo"] <- "Mollugo sp."
c21.06_36$Scientific[c21.06_36$Scientific == "Port. unb"] <- "Portulaca"

# Rename unknowns
which(str_detect(c21.06_36$Common, "Unk") == TRUE)
which(str_detect(c21.06_36$Common, "2021") == TRUE)
c21.06_36[13, 1] <- "Unknown perennial grass, C21"
c21.06_36[21, 1] <- "Unknown annual grass, C21"
c21.06_36[35, 1] <- "Unknown annual forb, C21"

# Assign functional groups
c21.06_36 <- c21.06_36 %>% 
  mutate(Functional = rep(NA, nrow(c21.06_36)))
for(i in 1:nrow(c21.06_36)) {
  if(c21.06_36$Common[i] %in% plant$per.grass == TRUE) {
    c21.06_36$Functional[i] <- "Perennial grass"
  } else if(c21.06_36$Common[i] %in% plant$an.grass == TRUE) {
    c21.06_36$Functional[i] <- "Annual grass"
  } else if(c21.06_36$Common[i] %in% plant$per.forb == TRUE) {
    c21.06_36$Functional[i] <- "Perennial forb"
  } else if(c21.06_36$Common[i] %in% plant$an.forb == TRUE) {
    c21.06_36$Functional[i] <- "Annual forb"
  } else if(c21.06_36$Common[i] %in% plant$shrub == TRUE) {
    c21.06_36$Functional[i] <- "Shrub"
  } else if(c21.06_36$Common[i] %in% plant$tree == TRUE) {
    c21.06_36$Functional[i] <- "Tree"
  } else if(c21.06_36$Common[i] %in% plant$ground == TRUE) {
    c21.06_36$Functional[i] <- "Ground cover"
  } else {
    c21.06_36$Functional[i] <- "assign unknown"
  }
}

c21.06_36[13, "Functional"] <- "Perennial grass"
c21.06_36[21, "Functional"] <- "Annual grass"
c21.06_36[35, "Functional"] <- "Annual forb"
count(c21.06_36, Functional)

# Assign native status
c21.06_36 <- c21.06_36 %>% 
  mutate(Native = rep(NA, nrow(c21.06_36)))
for(i in 1:nrow(c21.06_36)) {
  if(c21.06_36$Common[i] %in% plant$invasive == TRUE) {
    c21.06_36$Native[i] <- "Invasive"
  } else if(c21.06_36$Common[i] %in% plant$native == TRUE) {
    c21.06_36$Native[i] <- "Native"
  } else if(c21.06_36$Common[i] %in% plant$ground == TRUE) {
    c21.06_36$Native[i] <- "Ground cover"
  } else {
    c21.06_36$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.06_36 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.06_36 <- c21.06_36 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.06_36 <- cbind(c21.06_36, cover)

wide.c21.06_36 <- c21.06_36

# Pivot data from wide to long
c21.06_36 <- wide.c21.06_36
c21.06_36 <- c21.06_36 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.06_36 <- c21.06_36 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.06_36)) {
  if(c21.06_36$Year[i] == "Mar12") {
    c21.06_36$Year[i] <- "2012-03-01"
  } else if(c21.06_36$Year[i] == "Nov12") {
    c21.06_36$Year[i] <- "2012-11-01"
  } else if(c21.06_36$Year[i] == "Nov13") {
    c21.06_36$Year[i] <- "2013-11-01"
  } else if(c21.06_36$Year[i] == "Nov14") {
    c21.06_36$Year[i] <- "2014-11-01"
  } else if(c21.06_36$Year[i] == "Nov15") {
    c21.06_36$Year[i] <- "2015-11-01"
  } else if(c21.06_36$Year[i] == "Nov18") {
    c21.06_36$Year[i] <- "2018-11-01"
  } else {
    c21.06_36$Year[i] <- "2021-11-01"
  }
}
c21.06_36$Year <- as.Date(c21.06_36$Year, format = "%Y-%m-%d")

# Add channel and station
c21.06_36$Station <- rep("Channel 21_Station 06 + 36 BAF", nrow(c21.06_36))



# C21 7 + 49.5 ------------------------------------------------------------

# Remove headers, rename columns,
c21.07_49.5 <- raw.c21.07_49.5[-c(1:7), -c(37:43)]
colnames(c21.07_49.5) <- names.raw

# Add missing common names
c21.07_49.5$Common[c21.07_49.5$Scientific == "Bracilis aril"] <- "Blue grama"
c21.07_49.5$Common[c21.07_49.5$Scientific == "Mach pin"] <- "Mach pin (?)"
c21.07_49.5$Common[c21.07_49.5$Scientific == "Boerhaavia coullcal"] <- "Coulter's spiderling"

# Remove unnecessary rows
c21.07_49.5 <- c21.07_49.5 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Remove repetitive values in Nov 2018 annual grass row
c21.07_49.5[13, 7:9] <- "0"

# Standardize common names
c21.07_49.5$Common[c21.07_49.5$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.07_49.5$Common[c21.07_49.5$Common == "Cup grass"] <- "Cupgrass"

# Assign missing scientific names (when known) and correct spelling
c21.07_49.5$Scientific[c21.07_49.5$Common == "Feather fingergrass"] <- "Chloris virgata"
c21.07_49.5$Scientific[c21.07_49.5$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c21.07_49.5$Scientific[c21.07_49.5$Scientific == "Bracilis aril"] <- "Bouteloua gracilis"
c21.07_49.5$Scientific[c21.07_49.5$Scientific == "Boerhaavia coullcal"] <- "Boerhavia coulteri"

# Rename unknowns
which(str_detect(c21.07_49.5$Common, "Unk") == TRUE)
which(str_detect(c21.07_49.5$Common, "2021") == TRUE)
c21.07_49.5[20, 1] <- "Unknown annual grass, C21"
c21.07_49.5[29, 1] <- "Unknown annual forb, C21"

# Assign functional groups
c21.07_49.5 <- c21.07_49.5 %>% 
  mutate(Functional = rep(NA, nrow(c21.07_49.5)))
for(i in 1:nrow(c21.07_49.5)) {
  if(c21.07_49.5$Common[i] %in% plant$per.grass == TRUE) {
    c21.07_49.5$Functional[i] <- "Perennial grass"
  } else if(c21.07_49.5$Common[i] %in% plant$an.grass == TRUE) {
    c21.07_49.5$Functional[i] <- "Annual grass"
  } else if(c21.07_49.5$Common[i] %in% plant$per.forb == TRUE) {
    c21.07_49.5$Functional[i] <- "Perennial forb"
  } else if(c21.07_49.5$Common[i] %in% plant$an.forb == TRUE) {
    c21.07_49.5$Functional[i] <- "Annual forb"
  } else if(c21.07_49.5$Common[i] %in% plant$shrub == TRUE) {
    c21.07_49.5$Functional[i] <- "Shrub"
  } else if(c21.07_49.5$Common[i] %in% plant$tree == TRUE) {
    c21.07_49.5$Functional[i] <- "Tree"
  } else if(c21.07_49.5$Common[i] %in% plant$ground == TRUE) {
    c21.07_49.5$Functional[i] <- "Ground cover"
  } else {
    c21.07_49.5$Functional[i] <- "assign unknown"
  }
}

c21.07_49.5[20, "Functional"] <- "Annual grass"
c21.07_49.5[29, "Functional"] <- "Annual forb"
count(c21.07_49.5, Functional)

# Assign native status
c21.07_49.5 <- c21.07_49.5 %>% 
  mutate(Native = rep(NA, nrow(c21.07_49.5)))
for(i in 1:nrow(c21.07_49.5)) {
  if(c21.07_49.5$Common[i] %in% plant$invasive == TRUE) {
    c21.07_49.5$Native[i] <- "Invasive"
  } else if(c21.07_49.5$Common[i] %in% plant$native == TRUE) {
    c21.07_49.5$Native[i] <- "Native"
  } else if(c21.07_49.5$Common[i] %in% plant$ground == TRUE) {
    c21.07_49.5$Native[i] <- "Ground cover"
  } else {
    c21.07_49.5$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.07_49.5 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.07_49.5 <- c21.07_49.5 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.07_49.5 <- cbind(c21.07_49.5, cover)

wide.c21.07_49.5 <- c21.07_49.5

# Pivot data from wide to long
c21.07_49.5 <- wide.c21.07_49.5
c21.07_49.5 <- c21.07_49.5 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.07_49.5 <- c21.07_49.5 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.07_49.5)) {
  if(c21.07_49.5$Year[i] == "Mar12") {
    c21.07_49.5$Year[i] <- "2012-03-01"
  } else if(c21.07_49.5$Year[i] == "Nov12") {
    c21.07_49.5$Year[i] <- "2012-11-01"
  } else if(c21.07_49.5$Year[i] == "Nov13") {
    c21.07_49.5$Year[i] <- "2013-11-01"
  } else if(c21.07_49.5$Year[i] == "Nov14") {
    c21.07_49.5$Year[i] <- "2014-11-01"
  } else if(c21.07_49.5$Year[i] == "Nov15") {
    c21.07_49.5$Year[i] <- "2015-11-01"
  } else if(c21.07_49.5$Year[i] == "Nov18") {
    c21.07_49.5$Year[i] <- "2018-11-01"
  } else {
    c21.07_49.5$Year[i] <- "2021-11-01"
  }
}
c21.07_49.5$Year <- as.Date(c21.07_49.5$Year, format = "%Y-%m-%d")

# Add channel and station
c21.07_49.5$Station <- rep("Channel 21_Station 07 + 49.5 ORD", nrow(c21.07_49.5))



# C21 7 + 66 --------------------------------------------------------------

# Remove headers, rename columns, and remove unnecessary rows
c21.07_66 <- raw.c21.07_66[-c(1:7), -c(37:43)]
colnames(c21.07_66) <- names.raw

c21.07_66 <- c21.07_66 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c21.07_66$Common[c21.07_66$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c21.07_66$Common[c21.07_66$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.07_66$Common[c21.07_66$Common == "Cup grass"] <- "Cupgrass"
c21.07_66$Common[c21.07_66$Common == "sixweek grama"] <- "Sixweek grama"

# Assign missing scientific names (when known) and correct spelling
c21.07_66$Scientific[c21.07_66$Common == "Carpetweed"] <- "Mollugo verticillata"
c21.07_66$Scientific[c21.07_66$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c21.07_66$Common, "Unk") == TRUE)
which(str_detect(c21.07_66$Common, "2021") == TRUE)
c21.07_66[19, 1] <- "Unknown annual grass, C21"
c21.07_66[28, 1] <- "Unknown annual forb, C21"

# Assign functional groups
c21.07_66 <- c21.07_66 %>% 
  mutate(Functional = rep(NA, nrow(c21.07_66)))
for(i in 1:nrow(c21.07_66)) {
  if(c21.07_66$Common[i] %in% plant$per.grass == TRUE) {
    c21.07_66$Functional[i] <- "Perennial grass"
  } else if(c21.07_66$Common[i] %in% plant$an.grass == TRUE) {
    c21.07_66$Functional[i] <- "Annual grass"
  } else if(c21.07_66$Common[i] %in% plant$per.forb == TRUE) {
    c21.07_66$Functional[i] <- "Perennial forb"
  } else if(c21.07_66$Common[i] %in% plant$an.forb == TRUE) {
    c21.07_66$Functional[i] <- "Annual forb"
  } else if(c21.07_66$Common[i] %in% plant$shrub == TRUE) {
    c21.07_66$Functional[i] <- "Shrub"
  } else if(c21.07_66$Common[i] %in% plant$tree == TRUE) {
    c21.07_66$Functional[i] <- "Tree"
  } else if(c21.07_66$Common[i] %in% plant$ground == TRUE) {
    c21.07_66$Functional[i] <- "Ground cover"
  } else {
    c21.07_66$Functional[i] <- "assign unknown"
  }
}

c21.07_66[19, "Functional"] <- "Annual grass"
c21.07_66[28, "Functional"] <- "Annual forb"
count(c21.07_66, Functional)

# Assign native status
c21.07_66 <- c21.07_66 %>% 
  mutate(Native = rep(NA, nrow(c21.07_66)))
for(i in 1:nrow(c21.07_66)) {
  if(c21.07_66$Common[i] %in% plant$invasive == TRUE) {
    c21.07_66$Native[i] <- "Invasive"
  } else if(c21.07_66$Common[i] %in% plant$native == TRUE) {
    c21.07_66$Native[i] <- "Native"
  } else if(c21.07_66$Common[i] %in% plant$ground == TRUE) {
    c21.07_66$Native[i] <- "Ground cover"
  } else {
    c21.07_66$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.07_66 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.07_66 <- c21.07_66 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.07_66 <- cbind(c21.07_66, cover)

wide.c21.07_66 <- c21.07_66

# Pivot data from wide to long
c21.07_66 <- wide.c21.07_66
c21.07_66 <- c21.07_66 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.07_66 <- c21.07_66 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.07_66)) {
  if(c21.07_66$Year[i] == "Mar12") {
    c21.07_66$Year[i] <- "2012-03-01"
  } else if(c21.07_66$Year[i] == "Nov12") {
    c21.07_66$Year[i] <- "2012-11-01"
  } else if(c21.07_66$Year[i] == "Nov13") {
    c21.07_66$Year[i] <- "2013-11-01"
  } else if(c21.07_66$Year[i] == "Nov14") {
    c21.07_66$Year[i] <- "2014-11-01"
  } else if(c21.07_66$Year[i] == "Nov15") {
    c21.07_66$Year[i] <- "2015-11-01"
  } else if(c21.07_66$Year[i] == "Nov18") {
    c21.07_66$Year[i] <- "2018-11-01"
  } else {
    c21.07_66$Year[i] <- "2021-11-01"
  }
}
c21.07_66$Year <- as.Date(c21.07_66$Year, format = "%Y-%m-%d")

# Add channel and station
c21.07_66$Station <- rep("Channel 21_Station 07 + 66 BAF", nrow(c21.07_66))



# C21 8 + 62 --------------------------------------------------------------

# Remove headers, rename columns, and remove unnecessary rows
c21.08_62 <- raw.c21.08_62[-c(1:7), -c(37:43)]
colnames(c21.08_62) <- names.raw

# Move value in Perennial Forbs row to an Unknown row
c21.08_62[34, 1] <- "Unknown"
c21.08_62[34, 8] <- "5"

c21.08_62 <- c21.08_62 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Remove repetitive values in Nov 2018 annual grass row
c21.08_62[13, 7:10] <- "0"

# Standardize common names
c21.08_62$Common[c21.08_62$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.08_62$Common[c21.08_62$Common == "Cup grass"] <- "Cupgrass"
c21.08_62$Common[c21.08_62$Common == "Dracherea"] <- "Dracherea (?)"
c21.08_62$Common[c21.08_62$Common == "feather fingergrass"] <- "Feather fingergrass"

# Assign missing scientific names (when known) and correct spelling
c21.08_62$Scientific[c21.08_62$Common == "Spurge"] <- "Euphorbia"
c21.08_62$Scientific[c21.08_62$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c21.08_62$Common, "Unk") == TRUE)
which(str_detect(c21.08_62$Common, "2021") == TRUE)
c21.08_62[20, 1] <- "Unknown annual grass, C21"
c21.08_62[26, 1] <- "Unknown perennial forb, C21"
c21.08_62[30, 1] <- "Unknown annual forb, C21"

# Assign functional groups
c21.08_62 <- c21.08_62 %>% 
  mutate(Functional = rep(NA, nrow(c21.08_62)))
for(i in 1:nrow(c21.08_62)) {
  if(c21.08_62$Common[i] %in% plant$per.grass == TRUE) {
    c21.08_62$Functional[i] <- "Perennial grass"
  } else if(c21.08_62$Common[i] %in% plant$an.grass == TRUE) {
    c21.08_62$Functional[i] <- "Annual grass"
  } else if(c21.08_62$Common[i] %in% plant$per.forb == TRUE) {
    c21.08_62$Functional[i] <- "Perennial forb"
  } else if(c21.08_62$Common[i] %in% plant$an.forb == TRUE) {
    c21.08_62$Functional[i] <- "Annual forb"
  } else if(c21.08_62$Common[i] %in% plant$shrub == TRUE) {
    c21.08_62$Functional[i] <- "Shrub"
  } else if(c21.08_62$Common[i] %in% plant$tree == TRUE) {
    c21.08_62$Functional[i] <- "Tree"
  } else if(c21.08_62$Common[i] %in% plant$ground == TRUE) {
    c21.08_62$Functional[i] <- "Ground cover"
  } else {
    c21.08_62$Functional[i] <- "assign unknown"
  }
}

c21.08_62[20, "Functional"] <- "Annual grass"
c21.08_62[26, "Functional"] <- "Perennial forb"
c21.08_62[30, "Functional"] <- "Annual forb"
count(c21.08_62, Functional)

# Assign native status
c21.08_62 <- c21.08_62 %>% 
  mutate(Native = rep(NA, nrow(c21.08_62)))
for(i in 1:nrow(c21.08_62)) {
  if(c21.08_62$Common[i] %in% plant$invasive == TRUE) {
    c21.08_62$Native[i] <- "Invasive"
  } else if(c21.08_62$Common[i] %in% plant$native == TRUE) {
    c21.08_62$Native[i] <- "Native"
  } else if(c21.08_62$Common[i] %in% plant$ground == TRUE) {
    c21.08_62$Native[i] <- "Ground cover"
  } else {
    c21.08_62$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.08_62 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.08_62 <- c21.08_62 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.08_62 <- cbind(c21.08_62, cover)

wide.c21.08_62 <- c21.08_62

# Pivot data from wide to long
c21.08_62 <- wide.c21.08_62
c21.08_62 <- c21.08_62 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.08_62 <- c21.08_62 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.08_62)) {
  if(c21.08_62$Year[i] == "Mar12") {
    c21.08_62$Year[i] <- "2012-03-01"
  } else if(c21.08_62$Year[i] == "Nov12") {
    c21.08_62$Year[i] <- "2012-11-01"
  } else if(c21.08_62$Year[i] == "Nov13") {
    c21.08_62$Year[i] <- "2013-11-01"
  } else if(c21.08_62$Year[i] == "Nov14") {
    c21.08_62$Year[i] <- "2014-11-01"
  } else if(c21.08_62$Year[i] == "Nov15") {
    c21.08_62$Year[i] <- "2015-11-01"
  } else if(c21.08_62$Year[i] == "Nov18") {
    c21.08_62$Year[i] <- "2018-11-01"
  } else {
    c21.08_62$Year[i] <- "2021-11-01"
  }
}
c21.08_62$Year <- as.Date(c21.08_62$Year, format = "%Y-%m-%d")

# Add channel and station
c21.08_62$Station <- rep("Channel 21_Station 08 + 62 BAF", nrow(c21.08_62))



# C21 9 + 99 --------------------------------------------------------------

# Remove headers, rename columns, and remove unnecessary rows
c21.09_99 <- raw.c21.09_99[-c(1:7), -c(37:43)]
colnames(c21.09_99) <- names.raw

c21.09_99 <- c21.09_99 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # Mesa threeawn is the same as spidergrass
c21.09_99[3, 6] <- "2.5"
c21.09_99 <- c21.09_99[-14, ]

# Standardize common names
c21.09_99$Common[c21.09_99$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.09_99$Common[c21.09_99$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.09_99$Common[c21.09_99$Common == "Cup grass"] <- "Cupgrass"

# Assign missing scientific names (when known) and correct spelling
c21.09_99$Scientific[c21.09_99$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c21.09_99$Common, "Unk") == TRUE)
which(str_detect(c21.09_99$Common, "2021") == TRUE)
c21.09_99[19, 1] <- "Unknown annual grass, C21"
c21.09_99[25, 1] <- "Unknown annual forb, C21"

# Assign functional groups
c21.09_99 <- c21.09_99 %>% 
  mutate(Functional = rep(NA, nrow(c21.09_99)))
for(i in 1:nrow(c21.09_99)) {
  if(c21.09_99$Common[i] %in% plant$per.grass == TRUE) {
    c21.09_99$Functional[i] <- "Perennial grass"
  } else if(c21.09_99$Common[i] %in% plant$an.grass == TRUE) {
    c21.09_99$Functional[i] <- "Annual grass"
  } else if(c21.09_99$Common[i] %in% plant$per.forb == TRUE) {
    c21.09_99$Functional[i] <- "Perennial forb"
  } else if(c21.09_99$Common[i] %in% plant$an.forb == TRUE) {
    c21.09_99$Functional[i] <- "Annual forb"
  } else if(c21.09_99$Common[i] %in% plant$shrub == TRUE) {
    c21.09_99$Functional[i] <- "Shrub"
  } else if(c21.09_99$Common[i] %in% plant$tree == TRUE) {
    c21.09_99$Functional[i] <- "Tree"
  } else if(c21.09_99$Common[i] %in% plant$ground == TRUE) {
    c21.09_99$Functional[i] <- "Ground cover"
  } else {
    c21.09_99$Functional[i] <- "assign unknown"
  }
}

c21.09_99[19, "Functional"] <- "Annual grass"
c21.09_99[25, "Functional"] <- "Annual forb"
count(c21.09_99, Functional)

# Assign native status
c21.09_99 <- c21.09_99 %>% 
  mutate(Native = rep(NA, nrow(c21.09_99)))
for(i in 1:nrow(c21.09_99)) {
  if(c21.09_99$Common[i] %in% plant$invasive == TRUE) {
    c21.09_99$Native[i] <- "Invasive"
  } else if(c21.09_99$Common[i] %in% plant$native == TRUE) {
    c21.09_99$Native[i] <- "Native"
  } else if(c21.09_99$Common[i] %in% plant$ground == TRUE) {
    c21.09_99$Native[i] <- "Ground cover"
  } else {
    c21.09_99$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.09_99 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.09_99 <- c21.09_99 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.09_99 <- cbind(c21.09_99, cover)

wide.c21.09_99 <- c21.09_99

# Pivot data from wide to long
c21.09_99 <- wide.c21.09_99
c21.09_99 <- c21.09_99 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.09_99 <- c21.09_99 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.09_99)) {
  if(c21.09_99$Year[i] == "Mar12") {
    c21.09_99$Year[i] <- "2012-03-01"
  } else if(c21.09_99$Year[i] == "Nov12") {
    c21.09_99$Year[i] <- "2012-11-01"
  } else if(c21.09_99$Year[i] == "Nov13") {
    c21.09_99$Year[i] <- "2013-11-01"
  } else if(c21.09_99$Year[i] == "Nov14") {
    c21.09_99$Year[i] <- "2014-11-01"
  } else if(c21.09_99$Year[i] == "Nov15") {
    c21.09_99$Year[i] <- "2015-11-01"
  } else if(c21.09_99$Year[i] == "Nov18") {
    c21.09_99$Year[i] <- "2018-11-01"
  } else {
    c21.09_99$Year[i] <- "2021-11-01"
  }
}
c21.09_99$Year <- as.Date(c21.09_99$Year, format = "%Y-%m-%d")

# Add channel and station
c21.09_99$Station <- rep("Channel 21_Station 09 + 99 ORD", nrow(c21.09_99))



# C21 10 + 13 -------------------------------------------------------------

# Remove headers, rename columns,
c21.10_13 <- raw.c21.10_13[-c(1:7), -c(37:43)]
colnames(c21.10_13) <- names.raw

# Collapse rows of identical species into 1 row 
  # AZ wrightwort is in 2 rows (one is under per forb, as "Carlo wrightii"), with data in both rows
  # Am manually moving/re-entering data to condense into 1 row
c21.10_13[49, 9] <- "0.5"
c21.10_13[49, 10] <- "0.5"
c21.10_13 <- c21.10_13[-33, ]

# Add missing common names
c21.10_13$Common[c21.10_13$Scientific == "Portuliea"] <- "Purslane"

# Remove unnecessary rows
c21.10_13 <- c21.10_13 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Remove repetitive value in Nov 2013 annual grass row
c21.10_13[15, 28] <- "0"

# Standardize common names
c21.10_13$Common[c21.10_13$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.10_13$Common[c21.10_13$Common == "Cup grass"] <- "Cupgrass"
c21.10_13$Common[c21.10_13$Common == "Spiderling"] <- "Coulter's spiderling"

# Assign missing scientific names (when known) and correct spelling
c21.10_13$Scientific[c21.10_13$Common == "African lovegrass"] <- "Eragrostis echinochloidea"
c21.10_13$Scientific[c21.10_13$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c21.10_13$Scientific[c21.10_13$Scientific == "Portuliea"] <- "Portulaca"

# Rename unknowns
which(str_detect(c21.10_13$Common, "Unk") == TRUE)
which(str_detect(c21.10_13$Common, "2021") == TRUE)
c21.10_13[20, 1] <- "Unknown annual grass, C21"
c21.10_13[29, 1] <- "Unknown annual forb, C21"

# Assign functional groups
c21.10_13 <- c21.10_13 %>% 
  mutate(Functional = rep(NA, nrow(c21.10_13)))
for(i in 1:nrow(c21.10_13)) {
  if(c21.10_13$Common[i] %in% plant$per.grass == TRUE) {
    c21.10_13$Functional[i] <- "Perennial grass"
  } else if(c21.10_13$Common[i] %in% plant$an.grass == TRUE) {
    c21.10_13$Functional[i] <- "Annual grass"
  } else if(c21.10_13$Common[i] %in% plant$per.forb == TRUE) {
    c21.10_13$Functional[i] <- "Perennial forb"
  } else if(c21.10_13$Common[i] %in% plant$an.forb == TRUE) {
    c21.10_13$Functional[i] <- "Annual forb"
  } else if(c21.10_13$Common[i] %in% plant$shrub == TRUE) {
    c21.10_13$Functional[i] <- "Shrub"
  } else if(c21.10_13$Common[i] %in% plant$tree == TRUE) {
    c21.10_13$Functional[i] <- "Tree"
  } else if(c21.10_13$Common[i] %in% plant$ground == TRUE) {
    c21.10_13$Functional[i] <- "Ground cover"
  } else {
    c21.10_13$Functional[i] <- "assign unknown"
  }
}

c21.10_13[20, "Functional"] <- "Annual grass"
c21.10_13[29, "Functional"] <- "Annual forb"
count(c21.10_13, Functional)

# Assign native status
c21.10_13 <- c21.10_13 %>% 
  mutate(Native = rep(NA, nrow(c21.10_13)))
for(i in 1:nrow(c21.10_13)) {
  if(c21.10_13$Common[i] %in% plant$invasive == TRUE) {
    c21.10_13$Native[i] <- "Invasive"
  } else if(c21.10_13$Common[i] %in% plant$native == TRUE) {
    c21.10_13$Native[i] <- "Native"
  } else if(c21.10_13$Common[i] %in% plant$ground == TRUE) {
    c21.10_13$Native[i] <- "Ground cover"
  } else {
    c21.10_13$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.10_13 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.10_13 <- c21.10_13 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.10_13 <- cbind(c21.10_13, cover)

wide.c21.10_13 <- c21.10_13

# Pivot data from wide to long
c21.10_13 <- wide.c21.10_13
c21.10_13 <- c21.10_13 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.10_13 <- c21.10_13 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.10_13)) {
  if(c21.10_13$Year[i] == "Mar12") {
    c21.10_13$Year[i] <- "2012-03-01"
  } else if(c21.10_13$Year[i] == "Nov12") {
    c21.10_13$Year[i] <- "2012-11-01"
  } else if(c21.10_13$Year[i] == "Nov13") {
    c21.10_13$Year[i] <- "2013-11-01"
  } else if(c21.10_13$Year[i] == "Nov14") {
    c21.10_13$Year[i] <- "2014-11-01"
  } else if(c21.10_13$Year[i] == "Nov15") {
    c21.10_13$Year[i] <- "2015-11-01"
  } else if(c21.10_13$Year[i] == "Nov18") {
    c21.10_13$Year[i] <- "2018-11-01"
  } else {
    c21.10_13$Year[i] <- "2021-11-01"
  }
}
c21.10_13$Year <- as.Date(c21.10_13$Year, format = "%Y-%m-%d")

# Add channel and station
c21.10_13$Station <- rep("Channel 21_Station 10 + 13 BAF", nrow(c21.10_13))



# C21 11 + 12 -------------------------------------------------------------

# Remove headers, rename columns, and remove unnecessary rows
c21.11_12 <- raw.c21.11_12[-c(1:7), -c(37:43)]
colnames(c21.11_12) <- names.raw

c21.11_12 <- c21.11_12 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row 
  # Shortleaf baccharis is in 2 rows, with data in both rows
  # Am manually moving/re-entering data to condense into 1 row
c21.11_12[33, 10] <- "2.5"
c21.11_12 <- c21.11_12[-25, ]

# Standardize common names
c21.11_12$Common[c21.11_12$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c21.11_12$Common[c21.11_12$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.11_12$Common[c21.11_12$Common == "Coulter wrinklefruit"] <- "Coulter's wrinklefruit"
c21.11_12$Common[c21.11_12$Common == "Cup grass"] <- "Cupgrass"

# Assign missing scientific names (when known) and correct spelling
c21.11_12$Scientific[c21.11_12$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c21.11_12$Common, "Unk") == TRUE)
which(str_detect(c21.11_12$Common, "2021") == TRUE)
c21.11_12[18, 1] <- "Unknown annual grass, C21"

# Assign functional groups
c21.11_12 <- c21.11_12 %>% 
  mutate(Functional = rep(NA, nrow(c21.11_12)))
for(i in 1:nrow(c21.11_12)) {
  if(c21.11_12$Common[i] %in% plant$per.grass == TRUE) {
    c21.11_12$Functional[i] <- "Perennial grass"
  } else if(c21.11_12$Common[i] %in% plant$an.grass == TRUE) {
    c21.11_12$Functional[i] <- "Annual grass"
  } else if(c21.11_12$Common[i] %in% plant$per.forb == TRUE) {
    c21.11_12$Functional[i] <- "Perennial forb"
  } else if(c21.11_12$Common[i] %in% plant$an.forb == TRUE) {
    c21.11_12$Functional[i] <- "Annual forb"
  } else if(c21.11_12$Common[i] %in% plant$shrub == TRUE) {
    c21.11_12$Functional[i] <- "Shrub"
  } else if(c21.11_12$Common[i] %in% plant$tree == TRUE) {
    c21.11_12$Functional[i] <- "Tree"
  } else if(c21.11_12$Common[i] %in% plant$ground == TRUE) {
    c21.11_12$Functional[i] <- "Ground cover"
  } else {
    c21.11_12$Functional[i] <- "assign unknown"
  }
}

c21.11_12[18, "Functional"] <- "Annual grass"
count(c21.11_12, Functional)

# Assign native status
c21.11_12 <- c21.11_12 %>% 
  mutate(Native = rep(NA, nrow(c21.11_12)))
for(i in 1:nrow(c21.11_12)) {
  if(c21.11_12$Common[i] %in% plant$invasive == TRUE) {
    c21.11_12$Native[i] <- "Invasive"
  } else if(c21.11_12$Common[i] %in% plant$native == TRUE) {
    c21.11_12$Native[i] <- "Native"
  } else if(c21.11_12$Common[i] %in% plant$ground == TRUE) {
    c21.11_12$Native[i] <- "Ground cover"
  } else {
    c21.11_12$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.11_12 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.11_12 <- c21.11_12 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.11_12 <- cbind(c21.11_12, cover)

wide.c21.11_12 <- c21.11_12

# Pivot data from wide to long
c21.11_12 <- wide.c21.11_12
c21.11_12 <- c21.11_12 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.11_12 <- c21.11_12 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.11_12)) {
  if(c21.11_12$Year[i] == "Mar12") {
    c21.11_12$Year[i] <- "2012-03-01"
  } else if(c21.11_12$Year[i] == "Nov12") {
    c21.11_12$Year[i] <- "2012-11-01"
  } else if(c21.11_12$Year[i] == "Nov13") {
    c21.11_12$Year[i] <- "2013-11-01"
  } else if(c21.11_12$Year[i] == "Nov14") {
    c21.11_12$Year[i] <- "2014-11-01"
  } else if(c21.11_12$Year[i] == "Nov15") {
    c21.11_12$Year[i] <- "2015-11-01"
  } else if(c21.11_12$Year[i] == "Nov18") {
    c21.11_12$Year[i] <- "2018-11-01"
  } else {
    c21.11_12$Year[i] <- "2021-11-01"
  }
}
c21.11_12$Year <- as.Date(c21.11_12$Year, format = "%Y-%m-%d")

# Add channel and station
c21.11_12$Station <- rep("Channel 21_Station 11 + 12 ORD", nrow(c21.11_12))



# C21 11 + 24 -------------------------------------------------------------

# Remove headers, rename columns, and remove unnecessary rows
c21.11_24 <- raw.c21.11_24[-c(1:7), -c(37:43)]
colnames(c21.11_24) <- names.raw

c21.11_24 <- c21.11_24 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row 
  # Cane beardgrass is in 2 rows, with data in both rows
  # Am manually moving/re-entering data to condense into 1 row
c21.11_24[9, 5] <- "15"
c21.11_24 <- c21.11_24[-13, ]

# Standardize common names
c21.11_24$Common[c21.11_24$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c21.11_24$Common[c21.11_24$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.11_24$Common[c21.11_24$Common == "Cup grass"] <- "Cupgrass"

# Assign missing scientific names (when known) and correct spelling
c21.11_24$Scientific[c21.11_24$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Remove Portubera (fungi)
c21.11_24 <- c21.11_24 %>% 
  filter(Common != "Portubera")

# Rename unknowns
which(str_detect(c21.11_24$Common, "Unk") == TRUE)
which(str_detect(c21.11_24$Common, "2021") == TRUE)

# Assign functional groups
c21.11_24 <- c21.11_24 %>% 
  mutate(Functional = rep(NA, nrow(c21.11_24)))
for(i in 1:nrow(c21.11_24)) {
  if(c21.11_24$Common[i] %in% plant$per.grass == TRUE) {
    c21.11_24$Functional[i] <- "Perennial grass"
  } else if(c21.11_24$Common[i] %in% plant$an.grass == TRUE) {
    c21.11_24$Functional[i] <- "Annual grass"
  } else if(c21.11_24$Common[i] %in% plant$per.forb == TRUE) {
    c21.11_24$Functional[i] <- "Perennial forb"
  } else if(c21.11_24$Common[i] %in% plant$an.forb == TRUE) {
    c21.11_24$Functional[i] <- "Annual forb"
  } else if(c21.11_24$Common[i] %in% plant$shrub == TRUE) {
    c21.11_24$Functional[i] <- "Shrub"
  } else if(c21.11_24$Common[i] %in% plant$tree == TRUE) {
    c21.11_24$Functional[i] <- "Tree"
  } else if(c21.11_24$Common[i] %in% plant$ground == TRUE) {
    c21.11_24$Functional[i] <- "Ground cover"
  } else {
    c21.11_24$Functional[i] <- "assign unknown"
  }
}

count(c21.11_24, Functional)

# Assign native status
c21.11_24 <- c21.11_24 %>% 
  mutate(Native = rep(NA, nrow(c21.11_24)))
for(i in 1:nrow(c21.11_24)) {
  if(c21.11_24$Common[i] %in% plant$invasive == TRUE) {
    c21.11_24$Native[i] <- "Invasive"
  } else if(c21.11_24$Common[i] %in% plant$native == TRUE) {
    c21.11_24$Native[i] <- "Native"
  } else if(c21.11_24$Common[i] %in% plant$ground == TRUE) {
    c21.11_24$Native[i] <- "Ground cover"
  } else {
    c21.11_24$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.11_24 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.11_24 <- c21.11_24 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.11_24 <- cbind(c21.11_24, cover)

wide.c21.11_24 <- c21.11_24

# Pivot data from wide to long
c21.11_24 <- wide.c21.11_24
c21.11_24 <- c21.11_24 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.11_24 <- c21.11_24 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.11_24)) {
  if(c21.11_24$Year[i] == "Mar12") {
    c21.11_24$Year[i] <- "2012-03-01"
  } else if(c21.11_24$Year[i] == "Nov12") {
    c21.11_24$Year[i] <- "2012-11-01"
  } else if(c21.11_24$Year[i] == "Nov13") {
    c21.11_24$Year[i] <- "2013-11-01"
  } else if(c21.11_24$Year[i] == "Nov14") {
    c21.11_24$Year[i] <- "2014-11-01"
  } else if(c21.11_24$Year[i] == "Nov15") {
    c21.11_24$Year[i] <- "2015-11-01"
  } else if(c21.11_24$Year[i] == "Nov18") {
    c21.11_24$Year[i] <- "2018-11-01"
  } else {
    c21.11_24$Year[i] <- "2021-11-01"
  }
}
c21.11_24$Year <- as.Date(c21.11_24$Year, format = "%Y-%m-%d")

# Add channel and station
c21.11_24$Station <- rep("Channel 21_Station 11 + 24 BAF", nrow(c21.11_24))



# C21 11 + 63 -------------------------------------------------------------

# Remove headers and rename columns
c21.11_63 <- raw.c21.11_63[-c(1:7), -c(37:43)]
colnames(c21.11_63) <- names.raw

# Add missing common names
c21.11_63$Common[c21.11_63$Scientific == "Sida neomexicanum"] <- "New Mexico fanpetals"
c21.11_63$Common[c21.11_63$Scientific == "Lorla writghtii"] <- "Lorla wrightii (?)"

# Reassign CHs/Xs with values for Nov 2013 annual grasses
which(c21.11_63 == "x", arr.ind = TRUE)

c21.11_63[26, 7] <- "3.75"
c21.11_63[27, 7] <- "3.75"
c21.11_63[29, 7] <- "3.75"
c21.11_63[35, 7] <- "3.75"

c21.11_63[26, 8] <- "3.75"
c21.11_63[27, 8] <- "3.75"
c21.11_63[29, 8] <- "3.75"
c21.11_63[34, 8] <- "3.75"

c21.11_63[26, 9] <- "0.5"
c21.11_63[27, 9] <- "0.5"
c21.11_63[29, 9] <- "0.5"

c21.11_63[28, 10] <- "7.5"
c21.11_63[35, 10] <- "7.5"

# Remove unnecessary rows
c21.11_63 <- c21.11_63 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c21.11_63$Common[c21.11_63$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.11_63$Common[c21.11_63$Common == "Coulters wrinklefruit"] <- "Coulter's wrinklefruit"
c21.11_63$Common[c21.11_63$Common == "Cup grass"] <- "Cupgrass"
c21.11_63$Common[c21.11_63$Common == "Trumpetflower"] <- "Buckwheat"

# Assign missing scientific names (when known) and correct spelling
c21.11_63$Scientific[c21.11_63$Common == "Buckwheat"] <- "Eriogonum inflatum"
c21.11_63$Scientific[c21.11_63$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c21.11_63$Scientific[c21.11_63$Scientific == "Sida neomexicanum"] <- "Sida neomexicana"

# Rename unknowns
which(str_detect(c21.11_63$Common, "Unk") == TRUE)
which(str_detect(c21.11_63$Common, "2021") == TRUE)
c21.11_63[19, 1] <- "Unknown annual grass, C21"

# Assign functional groups
c21.11_63 <- c21.11_63 %>% 
  mutate(Functional = rep(NA, nrow(c21.11_63)))
for(i in 1:nrow(c21.11_63)) {
  if(c21.11_63$Common[i] %in% plant$per.grass == TRUE) {
    c21.11_63$Functional[i] <- "Perennial grass"
  } else if(c21.11_63$Common[i] %in% plant$an.grass == TRUE) {
    c21.11_63$Functional[i] <- "Annual grass"
  } else if(c21.11_63$Common[i] %in% plant$per.forb == TRUE) {
    c21.11_63$Functional[i] <- "Perennial forb"
  } else if(c21.11_63$Common[i] %in% plant$an.forb == TRUE) {
    c21.11_63$Functional[i] <- "Annual forb"
  } else if(c21.11_63$Common[i] %in% plant$shrub == TRUE) {
    c21.11_63$Functional[i] <- "Shrub"
  } else if(c21.11_63$Common[i] %in% plant$tree == TRUE) {
    c21.11_63$Functional[i] <- "Tree"
  } else if(c21.11_63$Common[i] %in% plant$ground == TRUE) {
    c21.11_63$Functional[i] <- "Ground cover"
  } else {
    c21.11_63$Functional[i] <- "assign unknown"
  }
}

c21.11_63[19, "Functional"] <- "Annual grass"
count(c21.11_63, Functional)

# Assign native status
c21.11_63 <- c21.11_63 %>% 
  mutate(Native = rep(NA, nrow(c21.11_63)))
for(i in 1:nrow(c21.11_63)) {
  if(c21.11_63$Common[i] %in% plant$invasive == TRUE) {
    c21.11_63$Native[i] <- "Invasive"
  } else if(c21.11_63$Common[i] %in% plant$native == TRUE) {
    c21.11_63$Native[i] <- "Native"
  } else if(c21.11_63$Common[i] %in% plant$ground == TRUE) {
    c21.11_63$Native[i] <- "Ground cover"
  } else {
    c21.11_63$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.11_63 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.11_63 <- c21.11_63 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.11_63 <- cbind(c21.11_63, cover)

wide.c21.11_63 <- c21.11_63

# Pivot data from wide to long
c21.11_63 <- wide.c21.11_63
c21.11_63 <- c21.11_63 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.11_63 <- c21.11_63 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.11_63)) {
  if(c21.11_63$Year[i] == "Mar12") {
    c21.11_63$Year[i] <- "2012-03-01"
  } else if(c21.11_63$Year[i] == "Nov12") {
    c21.11_63$Year[i] <- "2012-11-01"
  } else if(c21.11_63$Year[i] == "Nov13") {
    c21.11_63$Year[i] <- "2013-11-01"
  } else if(c21.11_63$Year[i] == "Nov14") {
    c21.11_63$Year[i] <- "2014-11-01"
  } else if(c21.11_63$Year[i] == "Nov15") {
    c21.11_63$Year[i] <- "2015-11-01"
  } else if(c21.11_63$Year[i] == "Nov18") {
    c21.11_63$Year[i] <- "2018-11-01"
  } else {
    c21.11_63$Year[i] <- "2021-11-01"
  }
}
c21.11_63$Year <- as.Date(c21.11_63$Year, format = "%Y-%m-%d")

# Add channel and station
c21.11_63$Station <- rep("Channel 21_Station 11 + 63 ORD", nrow(c21.11_63))



# C21 11 + 73 -------------------------------------------------------------

# Remove headers, rename columns, and remove unnecessary rows
c21.11_73 <- raw.c21.11_73[-c(1:7), -c(37:43)]
colnames(c21.11_73) <- names.raw

c21.11_73 <- c21.11_73 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row 
  # AZ wrightwort is in 2 rows, with data in both rows
  # Am manually moving/re-entering data to condense into 1 row
c21.11_73[33, 7] <- "0.5"
c21.11_73[33, 10] <- "0.5"
c21.11_73 <- c21.11_73[-24, ]

# Standardize common names
c21.11_73$Common[c21.11_73$Common == "Annual Grass"] <- "Annual grass (year summed)"
c21.11_73$Common[c21.11_73$Common == "Bedstraw"] <- "Bracted bedstraw"
c21.11_73$Common[c21.11_73$Common == "Cup grass"] <- "Cupgrass"
c21.11_73$Common[c21.11_73$Common == "Spiderling"] <- "Coulter's spiderling"

# Assign missing scientific names (when known) and correct spelling
c21.11_73$Scientific[c21.11_73$Common == "Coulter's spiderling"] <- "Boerhavia coulteri"
c21.11_73$Scientific[c21.11_73$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c21.11_73$Scientific[c21.11_73$Scientific == "Galiuum microphyllum"] <- "Galium microphyllum"

# Rename unknowns
which(str_detect(c21.11_73$Common, "Unk") == TRUE)
which(str_detect(c21.11_73$Common, "2021") == TRUE)
c21.11_73[18, 1] <- "Unknown annual grass, C21"
c21.11_73[33, 1] <- "Unknown annual forb, C21"

# Assign functional groups
c21.11_73 <- c21.11_73 %>% 
  mutate(Functional = rep(NA, nrow(c21.11_73)))
for(i in 1:nrow(c21.11_73)) {
  if(c21.11_73$Common[i] %in% plant$per.grass == TRUE) {
    c21.11_73$Functional[i] <- "Perennial grass"
  } else if(c21.11_73$Common[i] %in% plant$an.grass == TRUE) {
    c21.11_73$Functional[i] <- "Annual grass"
  } else if(c21.11_73$Common[i] %in% plant$per.forb == TRUE) {
    c21.11_73$Functional[i] <- "Perennial forb"
  } else if(c21.11_73$Common[i] %in% plant$an.forb == TRUE) {
    c21.11_73$Functional[i] <- "Annual forb"
  } else if(c21.11_73$Common[i] %in% plant$shrub == TRUE) {
    c21.11_73$Functional[i] <- "Shrub"
  } else if(c21.11_73$Common[i] %in% plant$tree == TRUE) {
    c21.11_73$Functional[i] <- "Tree"
  } else if(c21.11_73$Common[i] %in% plant$ground == TRUE) {
    c21.11_73$Functional[i] <- "Ground cover"
  } else {
    c21.11_73$Functional[i] <- "assign unknown"
  }
}

c21.11_73[18, "Functional"] <- "Annual grass"
c21.11_73[33, "Functional"] <- "Annual forb"
count(c21.11_73, Functional)

# Assign native status
c21.11_73 <- c21.11_73 %>% 
  mutate(Native = rep(NA, nrow(c21.11_73)))
for(i in 1:nrow(c21.11_73)) {
  if(c21.11_73$Common[i] %in% plant$invasive == TRUE) {
    c21.11_73$Native[i] <- "Invasive"
  } else if(c21.11_73$Common[i] %in% plant$native == TRUE) {
    c21.11_73$Native[i] <- "Native"
  } else if(c21.11_73$Common[i] %in% plant$ground == TRUE) {
    c21.11_73$Native[i] <- "Ground cover"
  } else {
    c21.11_73$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c21.11_73 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover[cover == ",5"] <- "0.5" # this was a random typo
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c21.11_73 <- c21.11_73 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c21.11_73 <- cbind(c21.11_73, cover)

wide.c21.11_73 <- c21.11_73

# Pivot data from wide to long
c21.11_73 <- wide.c21.11_73
c21.11_73 <- c21.11_73 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c21.11_73 <- c21.11_73 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c21.11_73)) {
  if(c21.11_73$Year[i] == "Mar12") {
    c21.11_73$Year[i] <- "2012-03-01"
  } else if(c21.11_73$Year[i] == "Nov12") {
    c21.11_73$Year[i] <- "2012-11-01"
  } else if(c21.11_73$Year[i] == "Nov13") {
    c21.11_73$Year[i] <- "2013-11-01"
  } else if(c21.11_73$Year[i] == "Nov14") {
    c21.11_73$Year[i] <- "2014-11-01"
  } else if(c21.11_73$Year[i] == "Nov15") {
    c21.11_73$Year[i] <- "2015-11-01"
  } else if(c21.11_73$Year[i] == "Nov18") {
    c21.11_73$Year[i] <- "2018-11-01"
  } else {
    c21.11_73$Year[i] <- "2021-11-01"
  }
}
c21.11_73$Year <- as.Date(c21.11_73$Year, format = "%Y-%m-%d")

# Add channel and station
c21.11_73$Station <- rep("Channel 21_Station 11 + 73 BAF", nrow(c21.11_73))



# Combine and check for name standardization and corrections --------------

all.c21 <- rbind(c21.01_07.5, c21.01_49, c21.02_15, c21.03_80, c21.04_90,
                 c21.06_36, c21.07_49.5, c21.07_66, c21.08_62, c21.09_99, c21.10_13,
                 c21.11_12, c21.11_24, c21.11_63, c21.11_73)

names.common <- all.c21 %>% 
  distinct(Common, .keep_all = TRUE) %>% 
  select(Common, Scientific, Functional, Native) %>% 
  arrange(Common)
names.scientific <- all.c21 %>% 
  distinct(Common, .keep_all = TRUE) %>% 
  select(Common, Scientific, Functional, Native) %>% 
  arrange(Scientific)

unique(filter(all.c21, Scientific == "Portuliea")$Station)
unique(filter(all.c21, Common == "Mesa threeawn")$Station)

print(count(all.c21, Scientific), n = 100)
print(count(all.c21, Common), n = 100)

unique(filter(all.c21, Native == "Unknown native status")$Common)



# Save cleaned dataframes -------------------------------------------------

write.csv(all.c21,
          file = "data/cleaned/C21-cover.csv",
          row.names = FALSE)


save.image("RData-RMarkdown/C21-data-wrangling.RData")


