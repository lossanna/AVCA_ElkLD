library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

plant <- read_xlsx("data_wrangling/Plant functional groups by common name.xlsx")

# Manual changes to Excel sheets:
  # Changed 4+65 because some rows needed to be deleted and inserted
  # Added "CH" to Nov 2013 columns to match raw data sheets
raw.c12.1_04 <- read_xlsx("data_Excel_LO_edited/AVCA ElkLD Channel 12 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "1 + 4")
raw.c12.1_52 <- read_xlsx("data_Excel_LO_edited/AVCA ElkLD Channel 12 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "1 + 52")
raw.c12.1_83 <- read_xlsx("data_Excel_LO_edited/AVCA ElkLD Channel 12 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "1 + 83")
raw.c12.2_63 <- read_xlsx("data_Excel_LO_edited/AVCA ElkLD Channel 12 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "2 + 63")
raw.c12.2_98 <- read_xlsx("data_Excel_LO_edited/AVCA ElkLD Channel 12 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "2 + 98")
raw.c12.3_93 <- read_xlsx("data_Excel_LO_edited/AVCA ElkLD Channel 12 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "3 + 93")
raw.c12.4_00 <- read_xlsx("data_Excel_LO_edited/AVCA ElkLD Channel 12 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "4 + 00")
raw.c12.4_09 <- read_xlsx("data_Excel_LO_edited/AVCA ElkLD Channel 12 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "4 + 09")
raw.c12.4_56 <- read_xlsx("data_Excel_LO_edited/AVCA ElkLD Channel 12 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "4 + 56")
raw.c12.4_65 <- read_xlsx("data_Excel_LO_edited/AVCA ElkLD Channel 12 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "4 + 65")
raw.c12.5_13 <- read_xlsx("data_Excel_LO_edited/AVCA ElkLD Channel 12 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "5 +13")
raw.c12.5_48 <- read_xlsx("data_Excel_LO_edited/AVCA ElkLD Channel 12 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "5 + 48")
raw.c12.6_30 <- read_xlsx("data_Excel_LO_edited/AVCA ElkLD Channel 12 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "6 + 30")
raw.c12.6_93 <- read_xlsx("data_Excel_LO_edited/AVCA ElkLD Channel 12 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "6 + 97")

names.raw <- c("Common", "Scientific", "Nov21_2L", "Nov21_1L", "Nov21_1R", "Nov21_2R",
               "Nov18_2L", "Nov18_1L", "Nov18_1R", "Nov18_2R",
               "Mar12_3L", "Mar12_2L", "Mar12_1L", "Mar12_1R", "Mar12_2R", "Mar12_3R",
               "Nov12_3L", "Nov12_2L", "Nov12_1L", "Nov12_1R", "Nov12_2R", "Nov12_3R",
               "Nov13_3L", "Nov13_2L", "Nov13_1L", "Nov13_1R", "Nov13_2R", "Nov13_3R",
               "Nov14_2L", "Nov14_1L", "Nov14_1R", "Nov14_2R",
               "Nov15_2L", "Nov15_1L", "Nov15_1R", "Nov15_2R") 



# C12 1 + 4 ---------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c12.1_04 <- raw.c12.1_04[-c(1:7), -c(37:42)]
colnames(c12.1_04) <- names.raw

c12.1_04 <- c12.1_04 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Reassign CHs with values for Nov 2013
which(c12.1_04 == "CH", arr.ind = TRUE)

c12.1_04[11, 23] <- "0"
c12.1_04[12, 23] <- "1.25"
c12.1_04[13, 23] <- "1.25"

c12.1_04[11, 26] <- "0"
c12.1_04[14, 26] <- "2.5"

c12.1_04[11, 27] <- "0"
c12.1_04[14, 27] <- "2.5"

c12.1_04[11, 28] <- "0"
c12.1_04[13, 28] <- "1.25"
c12.1_04[14, 28] <- "1.25"

# Correct common names based on scientific name
c12.1_04$Common[c12.1_04$Scientific == "bouteloua barbata?"] <- "Sixweek grama"

# Standardize common names
c12.1_04$Common[c12.1_04$Common == "Annual Grass"] <- "Annual grass (year summed)"
c12.1_04$Common[c12.1_04$Common == "Cup grass"] <- "Cupgrass"
c12.1_04$Common[c12.1_04$Common == "Panic grass"] <- "Annual panic"

# Assign missing scientific names (when known) and correct spelling
c12.1_04$Scientific[c12.1_04$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c12.1_04$Scientific[c12.1_04$Scientific == "bouteloua barbata?"] <- "Bouteloua barbata"
c12.1_04$Scientific[c12.1_04$Scientific == "panicum sp."] <- "Panicum"

# Rename unknowns
which(str_detect(c12.1_04$Common, "Unk") == TRUE)
which(str_detect(c12.1_04$Common, "2021") == TRUE)
c12.1_04[17, 1] <- "Unknown annual grass 1, C12"

# Assign functional groups
c12.1_04 <- c12.1_04 %>% 
  mutate(Functional = rep(NA, nrow(c12.1_04)))
for(i in 1:nrow(c12.1_04)) {
  if(c12.1_04$Common[i] %in% plant$per.grass == TRUE) {
    c12.1_04$Functional[i] <- "Perennial grass"
  } else if(c12.1_04$Common[i] %in% plant$an.grass == TRUE) {
    c12.1_04$Functional[i] <- "Annual grass"
  } else if(c12.1_04$Common[i] %in% plant$per.forb == TRUE) {
    c12.1_04$Functional[i] <- "Perennial forb"
  } else if(c12.1_04$Common[i] %in% plant$an.forb == TRUE) {
    c12.1_04$Functional[i] <- "Annual forb"
  } else if(c12.1_04$Common[i] %in% plant$shrub == TRUE) {
    c12.1_04$Functional[i] <- "Shrub"
  } else if(c12.1_04$Common[i] %in% plant$tree == TRUE) {
    c12.1_04$Functional[i] <- "Tree"
  } else if(c12.1_04$Common[i] %in% plant$ground == TRUE) {
    c12.1_04$Functional[i] <- "Ground cover"
  } else {
    c12.1_04$Functional[i] <- "assign unknown"
  }
}

c12.1_04[17, "Functional"] <- "Annual grass"
count(c12.1_04, Functional)

# Assign native status
c12.1_04 <- c12.1_04 %>% 
  mutate(Native = rep(NA, nrow(c12.1_04)))
for(i in 1:nrow(c12.1_04)) {
  if(c12.1_04$Common[i] %in% plant$invasive == TRUE) {
    c12.1_04$Native[i] <- "Invasive"
  } else if(c12.1_04$Common[i] %in% plant$native == TRUE) {
    c12.1_04$Native[i] <- "Native"
  } else if(c12.1_04$Common[i] %in% plant$ground == TRUE) {
    c12.1_04$Native[i] <- "Ground cover"
  } else {
    c12.1_04$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c12.1_04 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c12.1_04 <- c12.1_04 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c12.1_04 <- cbind(c12.1_04, cover)

wide.c12.1_04 <- c12.1_04

# Pivot data from wide to long
c12.1_04 <- wide.c12.1_04
c12.1_04 <- c12.1_04 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c12.1_04 <- c12.1_04 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c12.1_04)) {
  if(c12.1_04$Year[i] == "Mar12") {
    c12.1_04$Year[i] <- "2012-03-1"
  } else if(c12.1_04$Year[i] == "Nov12") {
    c12.1_04$Year[i] <- "2012-11-01"
  } else if(c12.1_04$Year[i] == "Nov13") {
    c12.1_04$Year[i] <- "2013-11-01"
  } else if(c12.1_04$Year[i] == "Nov14") {
    c12.1_04$Year[i] <- "2014-11-01"
  } else if(c12.1_04$Year[i] == "Nov15") {
    c12.1_04$Year[i] <- "2015-11-01"
  } else if(c12.1_04$Year[i] == "Nov18") {
    c12.1_04$Year[i] <- "2018-11-01"
  } else {
    c12.1_04$Year[i] <- "2021-11-01"
  }
}
c12.1_04$Year <- as.Date(c12.1_04$Year, format = "%Y-%m-%d")

# Add channel and station
c12.1_04$Station <- rep("Channel 12_Station 1 + 04", nrow(c12.1_04))



# C12 1 + 52 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c12.1_52 <- raw.c12.1_52[-c(1:7), -c(37:42)]
colnames(c12.1_52) <- names.raw

c12.1_52 <- c12.1_52 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Reassign CHs with values for Nov 2013
which(c12.1_52 == "CH", arr.ind = TRUE)

c12.1_52[12, 23] <- "0"
c12.1_52[17, 23] <- "0.5"

c12.1_52[12, 24] <- "0"
c12.1_52[17, 24] <- "7.5"
c12.1_52[19, 24] <- "7.5"

c12.1_52[12, 26] <- "0"
c12.1_52[17, 26] <- "2.5"

c12.1_52[12, 27] <- "0"
c12.1_52[14, 27] <- "2.5"

c12.1_52[12, 28] <- "0"
c12.1_52[13, 28] <- "0.5"
c12.1_52[20, 28] <- "0.5"

# Standardize common names
c12.1_52$Common[c12.1_52$Common == "Annual Grass"] <- "Annual grass (year summed)"
c12.1_52$Common[c12.1_52$Common == "annual panic"] <- "Annual panic"
c12.1_52$Common[c12.1_52$Common == "brown panic"] <- "Brown panic"
c12.1_52$Common[c12.1_52$Common == "Buckwheat/desert trumpet"] <- "Buckwheat"
c12.1_52$Common[c12.1_52$Common == "Cup grass"] <- "Cupgrass"

# Assign missing scientific names (when known) and correct spelling
c12.1_52$Scientific[c12.1_52$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c12.1_52$Scientific[c12.1_52$Scientific == "Physallas"] <- "Physalis" 

# Rename unknowns
which(str_detect(c12.1_52$Common, "Unk") == TRUE)
which(str_detect(c12.1_52$Common, "2021") == TRUE)
c12.1_52[18, 1] <- "Unknown annual grass 2, C12"

# Assign functional groups
c12.1_52 <- c12.1_52 %>% 
  mutate(Functional = rep(NA, nrow(c12.1_52)))
for(i in 1:nrow(c12.1_52)) {
  if(c12.1_52$Common[i] %in% plant$per.grass == TRUE) {
    c12.1_52$Functional[i] <- "Perennial grass"
  } else if(c12.1_52$Common[i] %in% plant$an.grass == TRUE) {
    c12.1_52$Functional[i] <- "Annual grass"
  } else if(c12.1_52$Common[i] %in% plant$per.forb == TRUE) {
    c12.1_52$Functional[i] <- "Perennial forb"
  } else if(c12.1_52$Common[i] %in% plant$an.forb == TRUE) {
    c12.1_52$Functional[i] <- "Annual forb"
  } else if(c12.1_52$Common[i] %in% plant$shrub == TRUE) {
    c12.1_52$Functional[i] <- "Shrub"
  } else if(c12.1_52$Common[i] %in% plant$tree == TRUE) {
    c12.1_52$Functional[i] <- "Tree"
  } else if(c12.1_52$Common[i] %in% plant$ground == TRUE) {
    c12.1_52$Functional[i] <- "Ground cover"
  } else {
    c12.1_52$Functional[i] <- "assign unknown"
  }
}

c12.1_52[18, "Functional"] <- "Annual grass"
count(c12.1_52, Functional)

# Assign native status
c12.1_52 <- c12.1_52 %>% 
  mutate(Native = rep(NA, nrow(c12.1_52)))
for(i in 1:nrow(c12.1_52)) {
  if(c12.1_52$Common[i] %in% plant$invasive == TRUE) {
    c12.1_52$Native[i] <- "Invasive"
  } else if(c12.1_52$Common[i] %in% plant$native == TRUE) {
    c12.1_52$Native[i] <- "Native"
  } else if(c12.1_52$Common[i] %in% plant$ground == TRUE) {
    c12.1_52$Native[i] <- "Ground cover"
  } else {
    c12.1_52$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c12.1_52 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c12.1_52 <- c12.1_52 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c12.1_52 <- cbind(c12.1_52, cover)

wide.c12.1_52 <- c12.1_52

# Pivot data from wide to long
c12.1_52 <- wide.c12.1_52
c12.1_52 <- c12.1_52 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c12.1_52 <- c12.1_52 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c12.1_52)) {
  if(c12.1_52$Year[i] == "Mar12") {
    c12.1_52$Year[i] <- "2012-03-1"
  } else if(c12.1_52$Year[i] == "Nov12") {
    c12.1_52$Year[i] <- "2012-11-01"
  } else if(c12.1_52$Year[i] == "Nov13") {
    c12.1_52$Year[i] <- "2013-11-01"
  } else if(c12.1_52$Year[i] == "Nov14") {
    c12.1_52$Year[i] <- "2014-11-01"
  } else if(c12.1_52$Year[i] == "Nov15") {
    c12.1_52$Year[i] <- "2015-11-01"
  } else if(c12.1_52$Year[i] == "Nov18") {
    c12.1_52$Year[i] <- "2018-11-01"
  } else {
    c12.1_52$Year[i] <- "2021-11-01"
  }
}
c12.1_52$Year <- as.Date(c12.1_52$Year, format = "%Y-%m-%d")

# Add channel and station
c12.1_52$Station <- rep("Channel 12_Station 1 + 52", nrow(c12.1_52))



# C12 1 + 83 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c12.1_83 <- raw.c12.1_83[-c(1:7), -c(37:42)]
colnames(c12.1_83) <- names.raw

c12.1_83 <- c12.1_83 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Reassign CHs with values for Nov 2013
which(c12.1_83 == "CH", arr.ind = TRUE)

c12.1_83[11, 23] <- "0"
c12.1_83[12, 23] <- "7.5"
c12.1_83[16, 23] <- "7.5"

c12.1_83[11, 24] <- "0"
c12.1_83[16, 24] <- "2.5"

# Standardize common names
c12.1_83$Common[c12.1_83$Common == "Annual Grass"] <- "Annual grass (year summed)"
c12.1_83$Common[c12.1_83$Common == "Cup grass"] <- "Cupgrass"

# Assign missing scientific names (when known) and correct spelling
c12.1_83$Scientific[c12.1_83$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c12.1_83$Common, "Unk") == TRUE)
which(str_detect(c12.1_83$Common, "2021") == TRUE)

# Assign functional groups
c12.1_83 <- c12.1_83 %>% 
  mutate(Functional = rep(NA, nrow(c12.1_83)))
for(i in 1:nrow(c12.1_83)) {
  if(c12.1_83$Common[i] %in% plant$per.grass == TRUE) {
    c12.1_83$Functional[i] <- "Perennial grass"
  } else if(c12.1_83$Common[i] %in% plant$an.grass == TRUE) {
    c12.1_83$Functional[i] <- "Annual grass"
  } else if(c12.1_83$Common[i] %in% plant$per.forb == TRUE) {
    c12.1_83$Functional[i] <- "Perennial forb"
  } else if(c12.1_83$Common[i] %in% plant$an.forb == TRUE) {
    c12.1_83$Functional[i] <- "Annual forb"
  } else if(c12.1_83$Common[i] %in% plant$shrub == TRUE) {
    c12.1_83$Functional[i] <- "Shrub"
  } else if(c12.1_83$Common[i] %in% plant$tree == TRUE) {
    c12.1_83$Functional[i] <- "Tree"
  } else if(c12.1_83$Common[i] %in% plant$ground == TRUE) {
    c12.1_83$Functional[i] <- "Ground cover"
  } else {
    c12.1_83$Functional[i] <- "assign unknown"
  }
}

count(c12.1_83, Functional)

# Assign native status
c12.1_83 <- c12.1_83 %>% 
  mutate(Native = rep(NA, nrow(c12.1_83)))
for(i in 1:nrow(c12.1_83)) {
  if(c12.1_83$Common[i] %in% plant$invasive == TRUE) {
    c12.1_83$Native[i] <- "Invasive"
  } else if(c12.1_83$Common[i] %in% plant$native == TRUE) {
    c12.1_83$Native[i] <- "Native"
  } else if(c12.1_83$Common[i] %in% plant$ground == TRUE) {
    c12.1_83$Native[i] <- "Ground cover"
  } else {
    c12.1_83$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c12.1_83 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c12.1_83 <- c12.1_83 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c12.1_83 <- cbind(c12.1_83, cover)

wide.c12.1_83 <- c12.1_83

# Pivot data from wide to long
c12.1_83 <- wide.c12.1_83
c12.1_83 <- c12.1_83 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c12.1_83 <- c12.1_83 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c12.1_83)) {
  if(c12.1_83$Year[i] == "Mar12") {
    c12.1_83$Year[i] <- "2012-03-1"
  } else if(c12.1_83$Year[i] == "Nov12") {
    c12.1_83$Year[i] <- "2012-11-01"
  } else if(c12.1_83$Year[i] == "Nov13") {
    c12.1_83$Year[i] <- "2013-11-01"
  } else if(c12.1_83$Year[i] == "Nov14") {
    c12.1_83$Year[i] <- "2014-11-01"
  } else if(c12.1_83$Year[i] == "Nov15") {
    c12.1_83$Year[i] <- "2015-11-01"
  } else if(c12.1_83$Year[i] == "Nov18") {
    c12.1_83$Year[i] <- "2018-11-01"
  } else {
    c12.1_83$Year[i] <- "2021-11-01"
  }
}
c12.1_83$Year <- as.Date(c12.1_83$Year, format = "%Y-%m-%d")

# Add channel and station
c12.1_83$Station <- rep("Channel 12_Station 1 + 83", nrow(c12.1_83))



# C12 2 + 63 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c12.2_63 <- raw.c12.2_63[-c(1:7), -c(37:42)]
colnames(c12.2_63) <- names.raw

c12.2_63 <- c12.2_63 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Reassign CHs with values for Nov 2013
which(c12.2_63 == "CH", arr.ind = TRUE)

c12.2_63[11, 23] <- "0"
c12.2_63[12, 23] <- "2.5"

c12.2_63[11, 24] <- "0"
c12.2_63[15, 24] <- "1.25"
c12.2_63[16, 24] <- "1.25"

c12.2_63[11, 25] <- "0"
c12.2_63[15, 25] <- "2.5"

c12.2_63[11, 26] <- "0"
c12.2_63[15, 26] <- "2.5"

c12.2_63[11, 27] <- "0"
c12.2_63[13, 27] <- "1.25"
c12.2_63[17, 27] <- "1.25"

c12.2_63[11, 28] <- "0"
c12.2_63[13, 28] <- "7.5"
c12.2_63[18, 28] <- "7.5"

# Standardize common names
c12.2_63$Common[c12.2_63$Common == "Annual Grass"] <- "Annual grass (year summed)"
c12.2_63$Common[c12.2_63$Common == "Unknown cholla"] <- "Cholla"

# Assign missing scientific names (when known) and correct spelling
c12.2_63$Scientific[c12.2_63$Common == "Cholla"] <- "Cylindropuntia"
c12.2_63$Scientific[c12.2_63$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c12.2_63$Common, "Unk") == TRUE)
which(str_detect(c12.2_63$Common, "2021") == TRUE)

# Assign functional groups
c12.2_63 <- c12.2_63 %>% 
  mutate(Functional = rep(NA, nrow(c12.2_63)))
for(i in 1:nrow(c12.2_63)) {
  if(c12.2_63$Common[i] %in% plant$per.grass == TRUE) {
    c12.2_63$Functional[i] <- "Perennial grass"
  } else if(c12.2_63$Common[i] %in% plant$an.grass == TRUE) {
    c12.2_63$Functional[i] <- "Annual grass"
  } else if(c12.2_63$Common[i] %in% plant$per.forb == TRUE) {
    c12.2_63$Functional[i] <- "Perennial forb"
  } else if(c12.2_63$Common[i] %in% plant$an.forb == TRUE) {
    c12.2_63$Functional[i] <- "Annual forb"
  } else if(c12.2_63$Common[i] %in% plant$shrub == TRUE) {
    c12.2_63$Functional[i] <- "Shrub"
  } else if(c12.2_63$Common[i] %in% plant$tree == TRUE) {
    c12.2_63$Functional[i] <- "Tree"
  } else if(c12.2_63$Common[i] %in% plant$ground == TRUE) {
    c12.2_63$Functional[i] <- "Ground cover"
  } else {
    c12.2_63$Functional[i] <- "assign unknown"
  }
}

count(c12.2_63, Functional)

# Assign native status
c12.2_63 <- c12.2_63 %>% 
  mutate(Native = rep(NA, nrow(c12.2_63)))
for(i in 1:nrow(c12.2_63)) {
  if(c12.2_63$Common[i] %in% plant$invasive == TRUE) {
    c12.2_63$Native[i] <- "Invasive"
  } else if(c12.2_63$Common[i] %in% plant$native == TRUE) {
    c12.2_63$Native[i] <- "Native"
  } else if(c12.2_63$Common[i] %in% plant$ground == TRUE) {
    c12.2_63$Native[i] <- "Ground cover"
  } else {
    c12.2_63$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c12.2_63 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c12.2_63 <- c12.2_63 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c12.2_63 <- cbind(c12.2_63, cover)

wide.c12.2_63 <- c12.2_63

# Pivot data from wide to long
c12.2_63 <- wide.c12.2_63
c12.2_63 <- c12.2_63 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c12.2_63 <- c12.2_63 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c12.2_63)) {
  if(c12.2_63$Year[i] == "Mar12") {
    c12.2_63$Year[i] <- "2012-03-1"
  } else if(c12.2_63$Year[i] == "Nov12") {
    c12.2_63$Year[i] <- "2012-11-01"
  } else if(c12.2_63$Year[i] == "Nov13") {
    c12.2_63$Year[i] <- "2013-11-01"
  } else if(c12.2_63$Year[i] == "Nov14") {
    c12.2_63$Year[i] <- "2014-11-01"
  } else if(c12.2_63$Year[i] == "Nov15") {
    c12.2_63$Year[i] <- "2015-11-01"
  } else if(c12.2_63$Year[i] == "Nov18") {
    c12.2_63$Year[i] <- "2018-11-01"
  } else {
    c12.2_63$Year[i] <- "2021-11-01"
  }
}
c12.2_63$Year <-as.Date(c12.2_63$Year, format = "%Y-%m-%d")

# Add channel and station
c12.2_63$Station <- rep("Channel 12_Station 2 + 63", nrow(c12.2_63))



# C12 2 + 98 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c12.2_98 <- raw.c12.2_98[-c(1:7), -c(37:42)]
colnames(c12.2_98) <- names.raw

c12.2_98 <- c12.2_98 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row 
  # Am manually moving/re-entering data to condense into 1 row
  # Brittlebush has two rows, with data in both rows
c12.2_98[29, 7] <- "2.5"
c12.2_98 <- c12.2_98[-20, ]

# Reassign CHs with values for Nov 2013
which(c12.2_98 == "CH", arr.ind = TRUE)

c12.2_98[11, 23] <- "0"
c12.2_98[12, 23] <- "0.5"

c12.2_98[11, 26] <- "0"
c12.2_98[14, 26] <- "0.5"

c12.2_98[11, 27] <- "0"
c12.2_98[12, 27] <- "0.5"
c12.2_98[14, 27] <- "0.5"

# Standardize common names
c12.2_98$Common[c12.2_98$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c12.2_98$Common[c12.2_98$Common == "Annual Grass"] <- "Annual grass (year summed)"

# Assign missing scientific names (when known) and correct spelling
c12.2_98$Scientific[c12.2_98$Common == "Carpetweed"] <- "Mollugo verticillata"
c12.2_98$Scientific[c12.2_98$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c12.2_98$Common, "Unk") == TRUE)
which(str_detect(c12.2_98$Common, "2021") == TRUE)

# Assign functional groups
c12.2_98 <- c12.2_98 %>% 
  mutate(Functional = rep(NA, nrow(c12.2_98)))
for(i in 1:nrow(c12.2_98)) {
  if(c12.2_98$Common[i] %in% plant$per.grass == TRUE) {
    c12.2_98$Functional[i] <- "Perennial grass"
  } else if(c12.2_98$Common[i] %in% plant$an.grass == TRUE) {
    c12.2_98$Functional[i] <- "Annual grass"
  } else if(c12.2_98$Common[i] %in% plant$per.forb == TRUE) {
    c12.2_98$Functional[i] <- "Perennial forb"
  } else if(c12.2_98$Common[i] %in% plant$an.forb == TRUE) {
    c12.2_98$Functional[i] <- "Annual forb"
  } else if(c12.2_98$Common[i] %in% plant$shrub == TRUE) {
    c12.2_98$Functional[i] <- "Shrub"
  } else if(c12.2_98$Common[i] %in% plant$tree == TRUE) {
    c12.2_98$Functional[i] <- "Tree"
  } else if(c12.2_98$Common[i] %in% plant$ground == TRUE) {
    c12.2_98$Functional[i] <- "Ground cover"
  } else {
    c12.2_98$Functional[i] <- "assign unknown"
  }
}

count(c12.2_98, Functional)

# Assign native status
c12.2_98 <- c12.2_98 %>% 
  mutate(Native = rep(NA, nrow(c12.2_98)))
for(i in 1:nrow(c12.2_98)) {
  if(c12.2_98$Common[i] %in% plant$invasive == TRUE) {
    c12.2_98$Native[i] <- "Invasive"
  } else if(c12.2_98$Common[i] %in% plant$native == TRUE) {
    c12.2_98$Native[i] <- "Native"
  } else if(c12.2_98$Common[i] %in% plant$ground == TRUE) {
    c12.2_98$Native[i] <- "Ground cover"
  } else {
    c12.2_98$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c12.2_98 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c12.2_98 <- c12.2_98 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c12.2_98 <- cbind(c12.2_98, cover)

wide.c12.2_98 <- c12.2_98

# Pivot data from wide to long
c12.2_98 <- wide.c12.2_98
c12.2_98 <- c12.2_98 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c12.2_98 <- c12.2_98 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c12.2_98)) {
  if(c12.2_98$Year[i] == "Mar12") {
    c12.2_98$Year[i] <- "2012-03-1"
  } else if(c12.2_98$Year[i] == "Nov12") {
    c12.2_98$Year[i] <- "2012-11-01"
  } else if(c12.2_98$Year[i] == "Nov13") {
    c12.2_98$Year[i] <- "2013-11-01"
  } else if(c12.2_98$Year[i] == "Nov14") {
    c12.2_98$Year[i] <- "2014-11-01"
  } else if(c12.2_98$Year[i] == "Nov15") {
    c12.2_98$Year[i] <- "2015-11-01"
  } else if(c12.2_98$Year[i] == "Nov18") {
    c12.2_98$Year[i] <- "2018-11-01"
  } else {
    c12.2_98$Year[i] <- "2021-11-01"
  }
}
c12.2_98$Year <- as.Date(c12.2_98$Year, format = "%Y-%m-%d")

# Add channel and station
c12.2_98$Station <- rep("Channel 12_Station 2 + 98", nrow(c12.2_98))



# C12 3 + 93 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c12.3_93 <- raw.c12.3_93[-c(1:7), -c(37:42)]
colnames(c12.3_93) <- names.raw

c12.3_93 <- c12.3_93 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Reassign CHs with values for Nov 2013
which(c12.3_93 == "CH", arr.ind = TRUE)

c12.3_93[11, 23] <- "0"
c12.3_93[13, 23] <- "0.5"

c12.3_93[11, 26] <- "0"
c12.3_93[16, 26] <- "0.5"

c12.3_93[11, 27] <- "0"
c12.3_93[16, 27] <- "0.5"

# Correct common names based on scientific name
c12.3_93$Common[c12.3_93$Scientific == "Eriogonum"] <- "Annual buckwheat"

# Standardize common names
c12.3_93$Common[c12.3_93$Common == "Annual Grass"] <- "Annual grass (year summed)"
c12.3_93$Common[c12.3_93$Common == "Brickle bush"] <- "Brickellia"
c12.3_93$Common[c12.3_93$Common == "Cup grass"] <- "Cupgrass"
c12.3_93$Common[c12.3_93$Common == "Unknown cholla 2021"] <- "Cholla"

# Assign missing scientific names (when known) and correct spelling
c12.3_93$Scientific[c12.3_93$Common == "Cholla"] <- "Cylindropuntia"
c12.3_93$Scientific[c12.3_93$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c12.3_93$Common, "Unk") == TRUE)
which(str_detect(c12.3_93$Common, "2021") == TRUE)

# Assign functional groups
c12.3_93 <- c12.3_93 %>% 
  mutate(Functional = rep(NA, nrow(c12.3_93)))
for(i in 1:nrow(c12.3_93)) {
  if(c12.3_93$Common[i] %in% plant$per.grass == TRUE) {
    c12.3_93$Functional[i] <- "Perennial grass"
  } else if(c12.3_93$Common[i] %in% plant$an.grass == TRUE) {
    c12.3_93$Functional[i] <- "Annual grass"
  } else if(c12.3_93$Common[i] %in% plant$per.forb == TRUE) {
    c12.3_93$Functional[i] <- "Perennial forb"
  } else if(c12.3_93$Common[i] %in% plant$an.forb == TRUE) {
    c12.3_93$Functional[i] <- "Annual forb"
  } else if(c12.3_93$Common[i] %in% plant$shrub == TRUE) {
    c12.3_93$Functional[i] <- "Shrub"
  } else if(c12.3_93$Common[i] %in% plant$tree == TRUE) {
    c12.3_93$Functional[i] <- "Tree"
  } else if(c12.3_93$Common[i] %in% plant$ground == TRUE) {
    c12.3_93$Functional[i] <- "Ground cover"
  } else {
    c12.3_93$Functional[i] <- "assign unknown"
  }
}

count(c12.3_93, Functional)

# Assign native status
c12.3_93 <- c12.3_93 %>% 
  mutate(Native = rep(NA, nrow(c12.3_93)))
for(i in 1:nrow(c12.3_93)) {
  if(c12.3_93$Common[i] %in% plant$invasive == TRUE) {
    c12.3_93$Native[i] <- "Invasive"
  } else if(c12.3_93$Common[i] %in% plant$native == TRUE) {
    c12.3_93$Native[i] <- "Native"
  } else if(c12.3_93$Common[i] %in% plant$ground == TRUE) {
    c12.3_93$Native[i] <- "Ground cover"
  } else {
    c12.3_93$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c12.3_93 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c12.3_93 <- c12.3_93 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c12.3_93 <- cbind(c12.3_93, cover)

wide.c12.3_93 <- c12.3_93

# Pivot data from wide to long
c12.3_93 <- wide.c12.3_93
c12.3_93 <- c12.3_93 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c12.3_93 <- c12.3_93 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c12.3_93)) {
  if(c12.3_93$Year[i] == "Mar12") {
    c12.3_93$Year[i] <- "2012-03-1"
  } else if(c12.3_93$Year[i] == "Nov12") {
    c12.3_93$Year[i] <- "2012-11-01"
  } else if(c12.3_93$Year[i] == "Nov13") {
    c12.3_93$Year[i] <- "2013-11-01"
  } else if(c12.3_93$Year[i] == "Nov14") {
    c12.3_93$Year[i] <- "2014-11-01"
  } else if(c12.3_93$Year[i] == "Nov15") {
    c12.3_93$Year[i] <- "2015-11-01"
  } else if(c12.3_93$Year[i] == "Nov18") {
    c12.3_93$Year[i] <- "2018-11-01"
  } else {
    c12.3_93$Year[i] <- "2021-11-01"
  }
}
c12.3_93$Year <- as.Date(c12.3_93$Year, format = "%Y-%m-%d")

# Add channel and station
c12.3_93$Station <- rep("Channel 12_Station 3 + 93", nrow(c12.3_93))



# C12 4 + 00 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c12.4_00 <- raw.c12.4_00[-c(1:7), -c(37:43)]
colnames(c12.4_00) <- names.raw

c12.4_00 <- c12.4_00 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Reassign CHs with values for Nov 2013
which(c12.4_00 == "CH", arr.ind = TRUE)

c12.4_00[11, 23] <- "0"
c12.4_00[12, 23] <- "5"
c12.4_00[13, 23] <- "5"
c12.4_00[15, 23] <- "5"

c12.4_00[11, 24] <- "0"
c12.4_00[15, 24] <- "7.5"
c12.4_00[21, 24] <- "7.5"

c12.4_00[11, 25] <- "0"
c12.4_00[13, 25] <- "0.5"

c12.4_00[11, 26] <- "0"
c12.4_00[16, 26] <- "1.25"
c12.4_00[21, 26] <- "1.25"

c12.4_00[11, 27] <- "0"
c12.4_00[13, 27] <- "5"
c12.4_00[16, 27] <- "5"
c12.4_00[17, 27] <- "5"

c12.4_00[11, 28] <- "0"
c12.4_00[13, 28] <- "5"
c12.4_00[16, 28] <- "5"
c12.4_00[18, 28] <- "5"

# Correct common names based on scientific name
c12.4_00$Common[c12.4_00$Scientific == "bouteloua barbata?"] <- "Sixweek grama"

# Standardize common names
c12.4_00$Common[c12.4_00$Common == "Annual Grass"] <- "Annual grass (year summed)"
c12.4_00$Common[c12.4_00$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c12.4_00$Common[c12.4_00$Common == "Brickle bush"] <- "Brickellia"

# Assign missing scientific names (when known) and correct spelling
c12.4_00$Scientific[c12.4_00$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c12.4_00$Scientific[c12.4_00$Scientific == "bouteloua barbata?"] <- "Bouteloua barbata"

# Rename unknowns
which(str_detect(c12.4_00$Common, "Unk") == TRUE)
which(str_detect(c12.4_00$Common, "2021") == TRUE)
c12.4_00[20, 1] <- "Unknown annual grass 1, C12"

# Assign functional groups
c12.4_00 <- c12.4_00 %>% 
  mutate(Functional = rep(NA, nrow(c12.4_00)))
for(i in 1:nrow(c12.4_00)) {
  if(c12.4_00$Common[i] %in% plant$per.grass == TRUE) {
    c12.4_00$Functional[i] <- "Perennial grass"
  } else if(c12.4_00$Common[i] %in% plant$an.grass == TRUE) {
    c12.4_00$Functional[i] <- "Annual grass"
  } else if(c12.4_00$Common[i] %in% plant$per.forb == TRUE) {
    c12.4_00$Functional[i] <- "Perennial forb"
  } else if(c12.4_00$Common[i] %in% plant$an.forb == TRUE) {
    c12.4_00$Functional[i] <- "Annual forb"
  } else if(c12.4_00$Common[i] %in% plant$shrub == TRUE) {
    c12.4_00$Functional[i] <- "Shrub"
  } else if(c12.4_00$Common[i] %in% plant$tree == TRUE) {
    c12.4_00$Functional[i] <- "Tree"
  } else if(c12.4_00$Common[i] %in% plant$ground == TRUE) {
    c12.4_00$Functional[i] <- "Ground cover"
  } else {
    c12.4_00$Functional[i] <- "assign unknown"
  }
}

c12.4_00[20, "Functional"] <- "Annual grass"
count(c12.4_00, Functional)

# Assign native status
c12.4_00 <- c12.4_00 %>% 
  mutate(Native = rep(NA, nrow(c12.4_00)))
for(i in 1:nrow(c12.4_00)) {
  if(c12.4_00$Common[i] %in% plant$invasive == TRUE) {
    c12.4_00$Native[i] <- "Invasive"
  } else if(c12.4_00$Common[i] %in% plant$native == TRUE) {
    c12.4_00$Native[i] <- "Native"
  } else if(c12.4_00$Common[i] %in% plant$ground == TRUE) {
    c12.4_00$Native[i] <- "Ground cover"
  } else {
    c12.4_00$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c12.4_00 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c12.4_00 <- c12.4_00 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c12.4_00 <- cbind(c12.4_00, cover)

wide.c12.4_00 <- c12.4_00

# Pivot data from wide to long
c12.4_00 <- wide.c12.4_00
c12.4_00 <- c12.4_00 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c12.4_00 <- c12.4_00 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c12.4_00)) {
  if(c12.4_00$Year[i] == "Mar12") {
    c12.4_00$Year[i] <- "2012-03-1"
  } else if(c12.4_00$Year[i] == "Nov12") {
    c12.4_00$Year[i] <- "2012-11-01"
  } else if(c12.4_00$Year[i] == "Nov13") {
    c12.4_00$Year[i] <- "2013-11-01"
  } else if(c12.4_00$Year[i] == "Nov14") {
    c12.4_00$Year[i] <- "2014-11-01"
  } else if(c12.4_00$Year[i] == "Nov15") {
    c12.4_00$Year[i] <- "2015-11-01"
  } else if(c12.4_00$Year[i] == "Nov18") {
    c12.4_00$Year[i] <- "2018-11-01"
  } else {
    c12.4_00$Year[i] <- "2021-11-01"
  }
}
c12.4_00$Year <- as.Date(c12.4_00$Year, format = "%Y-%m-%d")

# Add channel and station
c12.4_00$Station <- rep("Channel 12_Station 4 + 00", nrow(c12.4_00))



# C12 4 + 09 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c12.4_09 <- raw.c12.4_09[-c(1:7), -c(37:42)]
colnames(c12.4_09) <- names.raw

c12.4_09 <- c12.4_09 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Reassign CHs with values for Nov 2013
which(c12.4_09 == "CH", arr.ind = TRUE)

c12.4_09[11, 23] <- "0"
c12.4_09[12, 23] <- "1.25"
c12.4_09[13, 23] <- "1.25"

c12.4_09[11, 24] <- "0"
c12.4_09[13, 24] <- "2.5"

c12.4_09[11, 25] <- "0"
c12.4_09[13, 25] <- "2.5"

# Correct common names based on scientific name
c12.4_09$Common[c12.4_09$Scientific == "Eriogonum"] <- "Annual buckwheat"

# Standardize common names
c12.4_09$Common[c12.4_09$Common == "Annual Grass"] <- "Annual grass (year summed)"
c12.4_09$Common[c12.4_09$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c12.4_09$Common[c12.4_09$Common == "Brickle bush"] <- "Brickellia"

# Assign missing scientific names (when known) and correct spelling
c12.4_09$Scientific[c12.4_09$Common == "Annual panic"] <- "Panicum"
c12.4_09$Scientific[c12.4_09$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c12.4_09$Common, "Unk") == TRUE)
which(str_detect(c12.4_09$Common, "2021") == TRUE)
c12.4_09[17, 1] <- "Unknown annual grass 3, C12"

# Assign functional groups
c12.4_09 <- c12.4_09 %>% 
  mutate(Functional = rep(NA, nrow(c12.4_09)))
for(i in 1:nrow(c12.4_09)) {
  if(c12.4_09$Common[i] %in% plant$per.grass == TRUE) {
    c12.4_09$Functional[i] <- "Perennial grass"
  } else if(c12.4_09$Common[i] %in% plant$an.grass == TRUE) {
    c12.4_09$Functional[i] <- "Annual grass"
  } else if(c12.4_09$Common[i] %in% plant$per.forb == TRUE) {
    c12.4_09$Functional[i] <- "Perennial forb"
  } else if(c12.4_09$Common[i] %in% plant$an.forb == TRUE) {
    c12.4_09$Functional[i] <- "Annual forb"
  } else if(c12.4_09$Common[i] %in% plant$shrub == TRUE) {
    c12.4_09$Functional[i] <- "Shrub"
  } else if(c12.4_09$Common[i] %in% plant$tree == TRUE) {
    c12.4_09$Functional[i] <- "Tree"
  } else if(c12.4_09$Common[i] %in% plant$ground == TRUE) {
    c12.4_09$Functional[i] <- "Ground cover"
  } else {
    c12.4_09$Functional[i] <- "assign unknown"
  }
}

c12.4_09[17, "Functional"] <- "Annual grass"
count(c12.4_09, Functional)

# Assign native status
c12.4_09 <- c12.4_09 %>% 
  mutate(Native = rep(NA, nrow(c12.4_09)))
for(i in 1:nrow(c12.4_09)) {
  if(c12.4_09$Common[i] %in% plant$invasive == TRUE) {
    c12.4_09$Native[i] <- "Invasive"
  } else if(c12.4_09$Common[i] %in% plant$native == TRUE) {
    c12.4_09$Native[i] <- "Native"
  } else if(c12.4_09$Common[i] %in% plant$ground == TRUE) {
    c12.4_09$Native[i] <- "Ground cover"
  } else {
    c12.4_09$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c12.4_09 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c12.4_09 <- c12.4_09 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c12.4_09 <- cbind(c12.4_09, cover)

wide.c12.4_09 <- c12.4_09

# Pivot data from wide to long
c12.4_09 <- wide.c12.4_09
c12.4_09 <- c12.4_09 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c12.4_09 <- c12.4_09 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c12.4_09)) {
  if(c12.4_09$Year[i] == "Mar12") {
    c12.4_09$Year[i] <- "2012-03-1"
  } else if(c12.4_09$Year[i] == "Nov12") {
    c12.4_09$Year[i] <- "2012-11-01"
  } else if(c12.4_09$Year[i] == "Nov13") {
    c12.4_09$Year[i] <- "2013-11-01"
  } else if(c12.4_09$Year[i] == "Nov14") {
    c12.4_09$Year[i] <- "2014-11-01"
  } else if(c12.4_09$Year[i] == "Nov15") {
    c12.4_09$Year[i] <- "2015-11-01"
  } else if(c12.4_09$Year[i] == "Nov18") {
    c12.4_09$Year[i] <- "2018-11-01"
  } else {
    c12.4_09$Year[i] <- "2021-11-01"
  }
}
c12.4_09$Year <- as.Date(c12.4_09$Year, format = "%Y-%m-%d")

# Add channel and station
c12.4_09$Station <- rep("Channel 12_Station 4 + 09", nrow(c12.4_09))



# C12 4 + 56 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c12.4_56 <- raw.c12.4_56[-c(1:7), -c(37:43)]
colnames(c12.4_56) <- names.raw

c12.4_56 <- c12.4_56 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # Mesa threeawn is the same as spidergrass, with data in both rows
c12.4_56[3, 31] <- "2.5"
c12.4_56 <- c12.4_56[-11, ]

# Reassign CHs with values for Nov 2013
which(c12.4_56 == "CH", arr.ind = TRUE)

c12.4_56[11, 23] <- "0"
c12.4_56[12, 23] <- "2.5"

c12.4_56[11, 24] <- "0"
c12.4_56[12, 24] <- "1.25"
c12.4_56[13, 24] <- "1.25"

c12.4_56[11, 26] <- "0"
c12.4_56[13, 26] <- "1.25"
c12.4_56[14, 26] <- "1.25"

c12.4_56[11, 28] <- "0"
c12.4_56[15, 28] <- "2.5"

# Standardize common names
c12.4_56$Common[c12.4_56$Common == "Annual Grass"] <- "Annual grass (year summed)"
c12.4_56$Common[c12.4_56$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c12.4_56$Common[c12.4_56$Common == "Annual panic grass"] <- "Annual panic"

# Assign missing scientific names (when known) and correct spelling
c12.4_56$Scientific[c12.4_56$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c12.4_56$Common, "Unk") == TRUE)
which(str_detect(c12.4_56$Common, "2021") == TRUE)
c12.4_56[17, 1] <- "Unknown annual grass 1, C12"

# Assign functional groups
c12.4_56 <- c12.4_56 %>% 
  mutate(Functional = rep(NA, nrow(c12.4_56)))
for(i in 1:nrow(c12.4_56)) {
  if(c12.4_56$Common[i] %in% plant$per.grass == TRUE) {
    c12.4_56$Functional[i] <- "Perennial grass"
  } else if(c12.4_56$Common[i] %in% plant$an.grass == TRUE) {
    c12.4_56$Functional[i] <- "Annual grass"
  } else if(c12.4_56$Common[i] %in% plant$per.forb == TRUE) {
    c12.4_56$Functional[i] <- "Perennial forb"
  } else if(c12.4_56$Common[i] %in% plant$an.forb == TRUE) {
    c12.4_56$Functional[i] <- "Annual forb"
  } else if(c12.4_56$Common[i] %in% plant$shrub == TRUE) {
    c12.4_56$Functional[i] <- "Shrub"
  } else if(c12.4_56$Common[i] %in% plant$tree == TRUE) {
    c12.4_56$Functional[i] <- "Tree"
  } else if(c12.4_56$Common[i] %in% plant$ground == TRUE) {
    c12.4_56$Functional[i] <- "Ground cover"
  } else {
    c12.4_56$Functional[i] <- "assign unknown"
  }
}

c12.4_56[17, "Functional"] <- "Annual grass"
count(c12.4_56, Functional)

# Assign native status
c12.4_56 <- c12.4_56 %>% 
  mutate(Native = rep(NA, nrow(c12.4_56)))
for(i in 1:nrow(c12.4_56)) {
  if(c12.4_56$Common[i] %in% plant$invasive == TRUE) {
    c12.4_56$Native[i] <- "Invasive"
  } else if(c12.4_56$Common[i] %in% plant$native == TRUE) {
    c12.4_56$Native[i] <- "Native"
  } else if(c12.4_56$Common[i] %in% plant$ground == TRUE) {
    c12.4_56$Native[i] <- "Ground cover"
  } else {
    c12.4_56$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c12.4_56 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover <- cover %>% 
  mutate_if(is.numeric, as.character)
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c12.4_56 <- c12.4_56 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c12.4_56 <- cbind(c12.4_56, cover)

wide.c12.4_56 <- c12.4_56

# Pivot data from wide to long
c12.4_56 <- wide.c12.4_56
c12.4_56 <- c12.4_56 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c12.4_56 <- c12.4_56 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c12.4_56)) {
  if(c12.4_56$Year[i] == "Mar12") {
    c12.4_56$Year[i] <- "2012-03-1"
  } else if(c12.4_56$Year[i] == "Nov12") {
    c12.4_56$Year[i] <- "2012-11-01"
  } else if(c12.4_56$Year[i] == "Nov13") {
    c12.4_56$Year[i] <- "2013-11-01"
  } else if(c12.4_56$Year[i] == "Nov14") {
    c12.4_56$Year[i] <- "2014-11-01"
  } else if(c12.4_56$Year[i] == "Nov15") {
    c12.4_56$Year[i] <- "2015-11-01"
  } else if(c12.4_56$Year[i] == "Nov18") {
    c12.4_56$Year[i] <- "2018-11-01"
  } else {
    c12.4_56$Year[i] <- "2021-11-01"
  }
}
c12.4_56$Year <- as.Date(c12.4_56$Year, format = "%Y-%m-%d")

# Add channel and station
c12.4_56$Station <- rep("Channel 12_Station 4 + 56", nrow(c12.4_56))



# C12 4 + 65 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c12.4_65 <- raw.c12.4_65[-c(1:7), -c(37:42)]
colnames(c12.4_65) <- names.raw

c12.4_65 <- c12.4_65 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # Mesa threeawn is the same as spidergrass, with data in both rows
c12.4_65[3, 30] <- "2.5"
c12.4_65 <- c12.4_65[-11, ]

# Standardize common names
c12.4_65$Common[c12.4_65$Common == "Annual Grass"] <- "Annual grass (year summed)"
c12.4_65$Common[c12.4_65$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c12.4_65$Common[c12.4_65$Common == "Annual panic grass"] <- "Annual panic"

# Assign missing scientific names (when known) and correct spelling
c12.4_65$Scientific[c12.4_65$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c12.4_65$Common, "Unk") == TRUE)
which(str_detect(c12.4_65$Common, "2021") == TRUE)

# Assign functional groups
c12.4_65 <- c12.4_65 %>% 
  mutate(Functional = rep(NA, nrow(c12.4_65)))
for(i in 1:nrow(c12.4_65)) {
  if(c12.4_65$Common[i] %in% plant$per.grass == TRUE) {
    c12.4_65$Functional[i] <- "Perennial grass"
  } else if(c12.4_65$Common[i] %in% plant$an.grass == TRUE) {
    c12.4_65$Functional[i] <- "Annual grass"
  } else if(c12.4_65$Common[i] %in% plant$per.forb == TRUE) {
    c12.4_65$Functional[i] <- "Perennial forb"
  } else if(c12.4_65$Common[i] %in% plant$an.forb == TRUE) {
    c12.4_65$Functional[i] <- "Annual forb"
  } else if(c12.4_65$Common[i] %in% plant$shrub == TRUE) {
    c12.4_65$Functional[i] <- "Shrub"
  } else if(c12.4_65$Common[i] %in% plant$tree == TRUE) {
    c12.4_65$Functional[i] <- "Tree"
  } else if(c12.4_65$Common[i] %in% plant$ground == TRUE) {
    c12.4_65$Functional[i] <- "Ground cover"
  } else {
    c12.4_65$Functional[i] <- "assign unknown"
  }
}

count(c12.4_65, Functional)

# Assign native status
c12.4_65 <- c12.4_65 %>% 
  mutate(Native = rep(NA, nrow(c12.4_65)))
for(i in 1:nrow(c12.4_65)) {
  if(c12.4_65$Common[i] %in% plant$invasive == TRUE) {
    c12.4_65$Native[i] <- "Invasive"
  } else if(c12.4_65$Common[i] %in% plant$native == TRUE) {
    c12.4_65$Native[i] <- "Native"
  } else if(c12.4_65$Common[i] %in% plant$ground == TRUE) {
    c12.4_65$Native[i] <- "Ground cover"
  } else {
    c12.4_65$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c12.4_65 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c12.4_65 <- c12.4_65 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c12.4_65 <- cbind(c12.4_65, cover)

wide.c12.4_65 <- c12.4_65

# Pivot data from wide to long
c12.4_65 <- wide.c12.4_65
c12.4_65 <- c12.4_65 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c12.4_65 <- c12.4_65 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c12.4_65)) {
  if(c12.4_65$Year[i] == "Mar12") {
    c12.4_65$Year[i] <- "2012-03-1"
  } else if(c12.4_65$Year[i] == "Nov12") {
    c12.4_65$Year[i] <- "2012-11-01"
  } else if(c12.4_65$Year[i] == "Nov13") {
    c12.4_65$Year[i] <- "2013-11-01"
  } else if(c12.4_65$Year[i] == "Nov14") {
    c12.4_65$Year[i] <- "2014-11-01"
  } else if(c12.4_65$Year[i] == "Nov15") {
    c12.4_65$Year[i] <- "2015-11-01"
  } else if(c12.4_65$Year[i] == "Nov18") {
    c12.4_65$Year[i] <- "2018-11-01"
  } else {
    c12.4_65$Year[i] <- "2021-11-01"
  }
}
c12.4_65$Year <- as.Date(c12.4_65$Year, format = "%Y-%m-%d")

# Add channel and station
c12.4_65$Station <- rep("Channel 12_Station 4 + 65", nrow(c12.4_65))



# C12 5 + 13 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c12.5_13 <- raw.c12.5_13[-c(1:7), -c(37:42)]
colnames(c12.5_13) <- names.raw

c12.5_13 <- c12.5_13 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Reassign CHs with values for Nov 2013
which(c12.5_13 == "CH", arr.ind = TRUE)

c12.5_13[11, 23] <- "0"
c12.5_13[12, 23] <- "2.5"

c12.5_13[11, 24] <- "0"
c12.5_13[16, 24] <- "0.5"

c12.5_13[11, 26] <- "0"
c12.5_13[14, 26] <- "0.5"

c12.5_13[11, 27] <- "0"
c12.5_13[13, 27] <- "2.5"

c12.5_13[11, 28] <- "0"
c12.5_13[13, 28] <- "1.25"
c12.5_13[14, 28] <- "1.25"

# Standardize common names
c12.5_13$Common[c12.5_13$Common == "Annual Grass"] <- "Annual grass (year summed)"
c12.5_13$Common[c12.5_13$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c12.5_13$Common[c12.5_13$Common == "AZ Wrightwort"] <- "AZ wrightwort"
c12.5_13$Common[c12.5_13$Common == "cupgrass"] <- "Cupgrass"

# Assign missing scientific names (when known) and correct spelling
c12.5_13$Scientific[c12.5_13$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c12.5_13$Common, "Unk") == TRUE)
which(str_detect(c12.5_13$Common, "2021") == TRUE)

# Assign functional groups
c12.5_13 <- c12.5_13 %>% 
  mutate(Functional = rep(NA, nrow(c12.5_13)))
for(i in 1:nrow(c12.5_13)) {
  if(c12.5_13$Common[i] %in% plant$per.grass == TRUE) {
    c12.5_13$Functional[i] <- "Perennial grass"
  } else if(c12.5_13$Common[i] %in% plant$an.grass == TRUE) {
    c12.5_13$Functional[i] <- "Annual grass"
  } else if(c12.5_13$Common[i] %in% plant$per.forb == TRUE) {
    c12.5_13$Functional[i] <- "Perennial forb"
  } else if(c12.5_13$Common[i] %in% plant$an.forb == TRUE) {
    c12.5_13$Functional[i] <- "Annual forb"
  } else if(c12.5_13$Common[i] %in% plant$shrub == TRUE) {
    c12.5_13$Functional[i] <- "Shrub"
  } else if(c12.5_13$Common[i] %in% plant$tree == TRUE) {
    c12.5_13$Functional[i] <- "Tree"
  } else if(c12.5_13$Common[i] %in% plant$ground == TRUE) {
    c12.5_13$Functional[i] <- "Ground cover"
  } else {
    c12.5_13$Functional[i] <- "assign unknown"
  }
}

count(c12.5_13, Functional)

# Assign native status
c12.5_13 <- c12.5_13 %>% 
  mutate(Native = rep(NA, nrow(c12.5_13)))
for(i in 1:nrow(c12.5_13)) {
  if(c12.5_13$Common[i] %in% plant$invasive == TRUE) {
    c12.5_13$Native[i] <- "Invasive"
  } else if(c12.5_13$Common[i] %in% plant$native == TRUE) {
    c12.5_13$Native[i] <- "Native"
  } else if(c12.5_13$Common[i] %in% plant$ground == TRUE) {
    c12.5_13$Native[i] <- "Ground cover"
  } else {
    c12.5_13$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c12.5_13 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c12.5_13 <- c12.5_13 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c12.5_13 <- cbind(c12.5_13, cover)

wide.c12.5_13 <- c12.5_13

# Pivot data from wide to long
c12.5_13 <- wide.c12.5_13
c12.5_13 <- c12.5_13 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c12.5_13 <- c12.5_13 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c12.5_13)) {
  if(c12.5_13$Year[i] == "Mar12") {
    c12.5_13$Year[i] <- "2012-03-1"
  } else if(c12.5_13$Year[i] == "Nov12") {
    c12.5_13$Year[i] <- "2012-11-01"
  } else if(c12.5_13$Year[i] == "Nov13") {
    c12.5_13$Year[i] <- "2013-11-01"
  } else if(c12.5_13$Year[i] == "Nov14") {
    c12.5_13$Year[i] <- "2014-11-01"
  } else if(c12.5_13$Year[i] == "Nov15") {
    c12.5_13$Year[i] <- "2015-11-01"
  } else if(c12.5_13$Year[i] == "Nov18") {
    c12.5_13$Year[i] <- "2018-11-01"
  } else {
    c12.5_13$Year[i] <- "2021-11-01"
  }
}
c12.5_13$Year <- as.Date(c12.5_13$Year, format = "%Y-%m-%d")

# Add channel and station
c12.5_13$Station <- rep("Channel 12_Station 5 + 13", nrow(c12.5_13))



# C12 5 + 48 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c12.5_48 <- raw.c12.5_48[-c(1:7), -c(37:42)]
colnames(c12.5_48) <- names.raw

c12.5_48 <- c12.5_48 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Reassign CHs with values for Nov 2013
which(c12.5_48 == "CH", arr.ind = TRUE)

c12.5_48[11, 23] <- "0"
c12.5_48[16, 23] <- "2.5"

c12.5_48[11, 24] <- "0"
c12.5_48[12, 24] <- "2.5"

c12.5_48[11, 25] <- "0"
c12.5_48[13, 25] <- "0.5"

c12.5_48[11, 27] <- "0"
c12.5_48[17, 27] <- "0.5"

c12.5_48[11, 28] <- "0"
c12.5_48[15, 28] <- "0.5"

# Correct common names based on scientific name
c12.5_48$Common[c12.5_48$Scientific == "Eriogonum"] <- "Annual buckwheat"

# Standardize common names
c12.5_48$Common[c12.5_48$Common == "Annual Grass"] <- "Annual grass (year summed)"
c12.5_48$Common[c12.5_48$Common == "AZ Wrightwort"] <- "AZ wrightwort"

# Assign missing scientific names (when known) and correct spelling
c12.5_48$Scientific[c12.5_48$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c12.5_48$Common, "Unk") == TRUE)
which(str_detect(c12.5_48$Common, "2021") == TRUE)

# Assign functional groups
c12.5_48 <- c12.5_48 %>% 
  mutate(Functional = rep(NA, nrow(c12.5_48)))
for(i in 1:nrow(c12.5_48)) {
  if(c12.5_48$Common[i] %in% plant$per.grass == TRUE) {
    c12.5_48$Functional[i] <- "Perennial grass"
  } else if(c12.5_48$Common[i] %in% plant$an.grass == TRUE) {
    c12.5_48$Functional[i] <- "Annual grass"
  } else if(c12.5_48$Common[i] %in% plant$per.forb == TRUE) {
    c12.5_48$Functional[i] <- "Perennial forb"
  } else if(c12.5_48$Common[i] %in% plant$an.forb == TRUE) {
    c12.5_48$Functional[i] <- "Annual forb"
  } else if(c12.5_48$Common[i] %in% plant$shrub == TRUE) {
    c12.5_48$Functional[i] <- "Shrub"
  } else if(c12.5_48$Common[i] %in% plant$tree == TRUE) {
    c12.5_48$Functional[i] <- "Tree"
  } else if(c12.5_48$Common[i] %in% plant$ground == TRUE) {
    c12.5_48$Functional[i] <- "Ground cover"
  } else {
    c12.5_48$Functional[i] <- "assign unknown"
  }
}

count(c12.5_48, Functional)

# Assign native status
c12.5_48 <- c12.5_48 %>% 
  mutate(Native = rep(NA, nrow(c12.5_48)))
for(i in 1:nrow(c12.5_48)) {
  if(c12.5_48$Common[i] %in% plant$invasive == TRUE) {
    c12.5_48$Native[i] <- "Invasive"
  } else if(c12.5_48$Common[i] %in% plant$native == TRUE) {
    c12.5_48$Native[i] <- "Native"
  } else if(c12.5_48$Common[i] %in% plant$ground == TRUE) {
    c12.5_48$Native[i] <- "Ground cover"
  } else {
    c12.5_48$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c12.5_48 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c12.5_48 <- c12.5_48 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c12.5_48 <- cbind(c12.5_48, cover)

wide.c12.5_48 <- c12.5_48

# Pivot data from wide to long
c12.5_48 <- wide.c12.5_48
c12.5_48 <- c12.5_48 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c12.5_48 <- c12.5_48 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c12.5_48)) {
  if(c12.5_48$Year[i] == "Mar12") {
    c12.5_48$Year[i] <- "2012-03-1"
  } else if(c12.5_48$Year[i] == "Nov12") {
    c12.5_48$Year[i] <- "2012-11-01"
  } else if(c12.5_48$Year[i] == "Nov13") {
    c12.5_48$Year[i] <- "2013-11-01"
  } else if(c12.5_48$Year[i] == "Nov14") {
    c12.5_48$Year[i] <- "2014-11-01"
  } else if(c12.5_48$Year[i] == "Nov15") {
    c12.5_48$Year[i] <- "2015-11-01"
  } else if(c12.5_48$Year[i] == "Nov18") {
    c12.5_48$Year[i] <- "2018-11-01"
  } else {
    c12.5_48$Year[i] <- "2021-11-01"
  }
}
c12.5_48$Year <- as.Date(c12.5_48$Year, format = "%Y-%m-%d")

# Add channel and station
c12.5_48$Station <- rep("Channel 12_Station 5 + 48", nrow(c12.5_48))



# C12 6 + 30 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c12.6_30 <- raw.c12.6_30[-c(1:7), -c(37:42)]
colnames(c12.6_30) <- names.raw

c12.6_30 <- c12.6_30 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # Shrubby buckwheat (E. wrightii) has two rows, with data in both rows
c12.6_30[30, 6] <- "37.5"
c12.6_30 <- c12.6_30[-21, ]

# Reassign CHs with values for Nov 2013
which(c12.6_30 == "CH", arr.ind = TRUE)

c12.6_30[12, 23] <- "0.5" # no sum total in Annual Grass row, so assigning 0.5
c12.6_30[13, 23] <- "0.5" # no sum total in Annual Grass row, so assigning 0.5

c12.6_30[11, 24] <- "0"
c12.6_30[13, 24] <- "7.5"
c12.6_30[15, 24] <- "7.5"

c12.6_30[11, 28] <- "0"
c12.6_30[15, 28] <- "7.5"

# Correct common names based on scientific name
c12.6_30$Common[c12.6_30$Scientific == "Eriogonum wrightii"] <- "Bastardsage"

# Standardize common names
c12.6_30$Common[c12.6_30$Common == "Annual Grass"] <- "Annual grass (year summed)"
c12.6_30$Common[c12.6_30$Common == "Annual Forbs"] <- "Annual forb (year summed)"

# Assign missing scientific names (when known) and correct spelling
c12.6_30$Scientific[c12.6_30$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c12.6_30$Common, "Unk") == TRUE)
which(str_detect(c12.6_30$Common, "2021") == TRUE)

# Assign functional groups
c12.6_30 <- c12.6_30 %>% 
  mutate(Functional = rep(NA, nrow(c12.6_30)))
for(i in 1:nrow(c12.6_30)) {
  if(c12.6_30$Common[i] %in% plant$per.grass == TRUE) {
    c12.6_30$Functional[i] <- "Perennial grass"
  } else if(c12.6_30$Common[i] %in% plant$an.grass == TRUE) {
    c12.6_30$Functional[i] <- "Annual grass"
  } else if(c12.6_30$Common[i] %in% plant$per.forb == TRUE) {
    c12.6_30$Functional[i] <- "Perennial forb"
  } else if(c12.6_30$Common[i] %in% plant$an.forb == TRUE) {
    c12.6_30$Functional[i] <- "Annual forb"
  } else if(c12.6_30$Common[i] %in% plant$shrub == TRUE) {
    c12.6_30$Functional[i] <- "Shrub"
  } else if(c12.6_30$Common[i] %in% plant$tree == TRUE) {
    c12.6_30$Functional[i] <- "Tree"
  } else if(c12.6_30$Common[i] %in% plant$ground == TRUE) {
    c12.6_30$Functional[i] <- "Ground cover"
  } else {
    c12.6_30$Functional[i] <- "assign unknown"
  }
}

count(c12.6_30, Functional)

# Assign native status
c12.6_30 <- c12.6_30 %>% 
  mutate(Native = rep(NA, nrow(c12.6_30)))
for(i in 1:nrow(c12.6_30)) {
  if(c12.6_30$Common[i] %in% plant$invasive == TRUE) {
    c12.6_30$Native[i] <- "Invasive"
  } else if(c12.6_30$Common[i] %in% plant$native == TRUE) {
    c12.6_30$Native[i] <- "Native"
  } else if(c12.6_30$Common[i] %in% plant$ground == TRUE) {
    c12.6_30$Native[i] <- "Ground cover"
  } else {
    c12.6_30$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c12.6_30 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c12.6_30 <- c12.6_30 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c12.6_30 <- cbind(c12.6_30, cover)

wide.c12.6_30 <- c12.6_30

# Pivot data from wide to long
c12.6_30 <- wide.c12.6_30
c12.6_30 <- c12.6_30 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c12.6_30 <- c12.6_30 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c12.6_30)) {
  if(c12.6_30$Year[i] == "Mar12") {
    c12.6_30$Year[i] <- "2012-03-1"
  } else if(c12.6_30$Year[i] == "Nov12") {
    c12.6_30$Year[i] <- "2012-11-01"
  } else if(c12.6_30$Year[i] == "Nov13") {
    c12.6_30$Year[i] <- "2013-11-01"
  } else if(c12.6_30$Year[i] == "Nov14") {
    c12.6_30$Year[i] <- "2014-11-01"
  } else if(c12.6_30$Year[i] == "Nov15") {
    c12.6_30$Year[i] <- "2015-11-01"
  } else if(c12.6_30$Year[i] == "Nov18") {
    c12.6_30$Year[i] <- "2018-11-01"
  } else {
    c12.6_30$Year[i] <- "2021-11-01"
  }
}
c12.6_30$Year <- as.Date(c12.6_30$Year, format = "%Y-%m-%d")

# Add channel and station
c12.6_30$Station <- rep("Channel 12_Station 6 + 30", nrow(c12.6_30))



# C12 6 + 93 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c12.6_93 <- raw.c12.6_93[-c(1:7), -c(37:42)]
colnames(c12.6_93) <- names.raw

c12.6_93 <- c12.6_93 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # Mesa threeawn is the same as spidergrass, with data in both rows
c12.6_93[3, 29] <- "2.5"
c12.6_93 <- c12.6_93[-11, ]

# Reassign CHs with values for Nov 2013
which(c12.6_93 == "CH", arr.ind = TRUE)

c12.6_93[11, 24] <- "0"
c12.6_93[16, 24] <- "2.5"

c12.6_93[11, 25] <- "0"
c12.6_93[19, 25] <- "2.5"

c12.6_93[11, 27] <- "0"
c12.6_93[14, 27] <- "2.5"

# Standardize common names
c12.6_93$Common[c12.6_93$Common == "Annual Grass"] <- "Annual grass (year summed)"
c12.6_93$Common[c12.6_93$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c12.6_93$Common[c12.6_93$Common == "Annual grama 2021"] <- "Sixweek grama"
c12.6_93$Common[c12.6_93$Common == "Desert zinia"] <- "Desert zinnia"

# Assign missing scientific names (when known) and correct spelling
c12.6_93$Scientific[c12.6_93$Common == "Sixweek grama"] <- "Bouteloua barbata"
c12.6_93$Scientific[c12.6_93$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c12.6_93$Scientific[c12.6_93$Scientific == "Zinia pumila"] <- "Zinnia acerosa"

# Rename unknowns
which(str_detect(c12.6_93$Common, "Unk") == TRUE)
which(str_detect(c12.6_93$Common, "2021") == TRUE)
c12.6_93[18, 1] <- "Unknown annual grass 1, C12"

# Assign functional groups
c12.6_93 <- c12.6_93 %>% 
  mutate(Functional = rep(NA, nrow(c12.6_93)))
for(i in 1:nrow(c12.6_93)) {
  if(c12.6_93$Common[i] %in% plant$per.grass == TRUE) {
    c12.6_93$Functional[i] <- "Perennial grass"
  } else if(c12.6_93$Common[i] %in% plant$an.grass == TRUE) {
    c12.6_93$Functional[i] <- "Annual grass"
  } else if(c12.6_93$Common[i] %in% plant$per.forb == TRUE) {
    c12.6_93$Functional[i] <- "Perennial forb"
  } else if(c12.6_93$Common[i] %in% plant$an.forb == TRUE) {
    c12.6_93$Functional[i] <- "Annual forb"
  } else if(c12.6_93$Common[i] %in% plant$shrub == TRUE) {
    c12.6_93$Functional[i] <- "Shrub"
  } else if(c12.6_93$Common[i] %in% plant$tree == TRUE) {
    c12.6_93$Functional[i] <- "Tree"
  } else if(c12.6_93$Common[i] %in% plant$ground == TRUE) {
    c12.6_93$Functional[i] <- "Ground cover"
  } else {
    c12.6_93$Functional[i] <- "assign unknown"
  }
}

c12.6_93[18, "Functional"] <- "Annual grass"
count(c12.6_93, Functional)

# Assign native status
c12.6_93 <- c12.6_93 %>% 
  mutate(Native = rep(NA, nrow(c12.6_93)))
for(i in 1:nrow(c12.6_93)) {
  if(c12.6_93$Common[i] %in% plant$invasive == TRUE) {
    c12.6_93$Native[i] <- "Invasive"
  } else if(c12.6_93$Common[i] %in% plant$native == TRUE) {
    c12.6_93$Native[i] <- "Native"
  } else if(c12.6_93$Common[i] %in% plant$ground == TRUE) {
    c12.6_93$Native[i] <- "Ground cover"
  } else {
    c12.6_93$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c12.6_93 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c12.6_93 <- c12.6_93 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c12.6_93 <- cbind(c12.6_93, cover)

wide.c12.6_93 <- c12.6_93

# Pivot data from wide to long
c12.6_93 <- wide.c12.6_93
c12.6_93 <- c12.6_93 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c12.6_93 <- c12.6_93 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c12.6_93)) {
  if(c12.6_93$Year[i] == "Mar12") {
    c12.6_93$Year[i] <- "2012-03-1"
  } else if(c12.6_93$Year[i] == "Nov12") {
    c12.6_93$Year[i] <- "2012-11-01"
  } else if(c12.6_93$Year[i] == "Nov13") {
    c12.6_93$Year[i] <- "2013-11-01"
  } else if(c12.6_93$Year[i] == "Nov14") {
    c12.6_93$Year[i] <- "2014-11-01"
  } else if(c12.6_93$Year[i] == "Nov15") {
    c12.6_93$Year[i] <- "2015-11-01"
  } else if(c12.6_93$Year[i] == "Nov18") {
    c12.6_93$Year[i] <- "2018-11-01"
  } else {
    c12.6_93$Year[i] <- "2021-11-01"
  }
}
c12.6_93$Year <- as.Date(c12.6_93$Year, format = "%Y-%m-%d")

# Add channel and station
c12.6_93$Station <- rep("Channel 12_Station 6 + 93", nrow(c12.6_93))



# Combine and check for name standardization and corrections --------------

all.c12 <- rbind(c12.1_04, c12.1_52, c12.1_83, c12.2_63, c12.2_98, c12.3_93,
                 c12.4_00, c12.4_09, c12.4_56, c12.4_65, c12.5_13, c12.5_48,
                 c12.6_30, c12.6_93)

names.common <- all.c12 %>% 
  distinct(Common, .keep_all = TRUE) %>% 
  select(Common, Scientific, Functional, Native) %>% 
  arrange(Common)
names.scientific <- all.c12 %>% 
  distinct(Common, .keep_all = TRUE) %>% 
  select(Common, Scientific, Functional, Native) %>% 
  arrange(Scientific)

unique(filter(all.c12, Scientific == "Gutierrezia sarothra")$Station)
unique(filter(all.c12, Common == "Mesa threeawn")$Station)

print(count(all.c12, Scientific), n = 100)
print(count(all.c12, Common), n = 100)

unique(filter(all.c12, Native == "Unknown native status")$Common)



# Save cleaned dataframes -------------------------------------------------

save(all.c12,
     file = ".RData/C12 long all stations.RData")

write.csv(all.c12,
          file = "data/cleaned/C12-cover.csv",
          row.names = FALSE)


save.image(".RData/C12 data wrangling.RData")


