library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

plant <- read_xlsx("Plant functional groups by common name.xlsx")

# Manual changes to Excel sheets:
  # Changed 1+38 because some data were in incorrect rows
  # Added "CH" to Nov 2013 columns to match raw data sheets
raw.c13.1_11.5 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                            sheet = "1 + 11.5 BAF")
raw.c13.1_38.5 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "1 + 38 ORD")
raw.c13.1_71 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "1 + 71 ORD")
raw.c13.1_77 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "1 + 77 BAF")
raw.c13.1_99 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "1 + 99 ORD")
raw.c13.2_16 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "2 + 16 BAF")
raw.c13.2_57 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "2 + 57")
raw.c13.2_67 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "2 + 67")
raw.c13.3_09 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "3 + 9 ORD")
raw.c13.3_31 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                         sheet = "3 + 31")
raw.c13.3_65 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                         sheet = "3 + 65 ORD")
raw.c13.3_95.5 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                         sheet = "3 + 95.5 ORD")
raw.c13.4_14.5 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                         sheet = "4 + 14.5")
raw.c13.4_36 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                            sheet = "4 + 36")
raw.c13.4_50 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "4 + 50")
raw.c13.4_78 <- read_xlsx("AVCA ElkLD Channel 13 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                          sheet = "4 + 78 ORD")

names.raw <- c("Common", "Scientific", "Nov21_2L", "Nov21_1L", "Nov21_1R", "Nov21_2R",
               "Nov18_2L", "Nov18_1L", "Nov18_1R", "Nov18_2R",
               "Mar12_3L", "Mar12_2L", "Mar12_1L", "Mar12_1R", "Mar12_2R", "Mar12_3R",
               "Nov12_3L", "Nov12_2L", "Nov12_1L", "Nov12_1R", "Nov12_2R", "Nov12_3R",
               "Nov13_3L", "Nov13_2L", "Nov13_1L", "Nov13_1R", "Nov13_2R", "Nov13_3R",
               "Nov14_2L", "Nov14_1L", "Nov14_1R", "Nov14_2R",
               "Nov15_2L", "Nov15_1L", "Nov15_1R", "Nov15_2R") 



# C13 1 + 11.5 ------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c13.1_11.5 <- raw.c13.1_11.5[-c(1:7), -c(37:43)]
colnames(c13.1_11.5) <- names.raw

c13.1_11.5 <- c13.1_11.5 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c13.1_11.5$Common[c13.1_11.5$Common == "Annual Grass"] <- "Annual grass (year summed)"
c13.1_11.5$Common[c13.1_11.5$Common == "Cane bluestem"] <- "Cane beardgrass"
c13.1_11.5$Common[c13.1_11.5$Common == "Barn yard"] <- "Barnyard"
c13.1_11.5$Common[c13.1_11.5$Common == "Cup grass"] <- "Cupgrass"

# Assign missing scientific names (when known) and correct spelling
c13.1_11.5$Scientific[c13.1_11.5$Common == "Cane beardgrass"] <- "Bothriochloa barbinodis"
c13.1_11.5$Scientific[c13.1_11.5$Common == "Brown panic"] <- "Panicum"
c13.1_11.5$Scientific[c13.1_11.5$Common == "Witch grass"] <- "Panicum"
c13.1_11.5$Scientific[c13.1_11.5$Common == "Barnyard"] <- "Echinochloa"
c13.1_11.5$Scientific[c13.1_11.5$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c13.1_11.5$Common, "Unk") == TRUE)
c13.1_11.5[13, 1] <- "Unknown perennial grass, C13"
c13.1_11.5[21, 1] <- "Unknown annual grass, C13"
c13.1_11.5[29, 1] <- "Unknown perennial forb, C13"
c13.1_11.5[31, 1] <- "Unknown annual forb, C13"

# Assign functional groups
c13.1_11.5 <- c13.1_11.5 %>% 
  mutate(Functional = rep(NA, nrow(c13.1_11.5)))
for(i in 1:nrow(c13.1_11.5)) {
  if(c13.1_11.5$Common[i] %in% plant$per.grass == TRUE) {
    c13.1_11.5$Functional[i] <- "Perennial grass"
  } else if(c13.1_11.5$Common[i] %in% plant$an.grass == TRUE) {
    c13.1_11.5$Functional[i] <- "Annual grass"
  } else if(c13.1_11.5$Common[i] %in% plant$per.forb == TRUE) {
    c13.1_11.5$Functional[i] <- "Perennial forb"
  } else if(c13.1_11.5$Common[i] %in% plant$an.forb == TRUE) {
    c13.1_11.5$Functional[i] <- "Annual forb"
  } else if(c13.1_11.5$Common[i] %in% plant$shrub == TRUE) {
    c13.1_11.5$Functional[i] <- "Shrub"
  } else if(c13.1_11.5$Common[i] %in% plant$tree == TRUE) {
    c13.1_11.5$Functional[i] <- "Tree"
  } else if(c13.1_11.5$Common[i] %in% plant$ground == TRUE) {
    c13.1_11.5$Functional[i] <- "Ground cover"
  } else {
    c13.1_11.5$Functional[i] <- "assign unknown"
  }
}

c13.1_11.5[13, "Functional"] <- "Perennial grass"
c13.1_11.5[21, "Functional"] <- "Annual grass"
c13.1_11.5[29, "Functional"] <- "Perennial forb"
c13.1_11.5[31, "Functional"] <- "Annual forb"
count(c13.1_11.5, Functional)

# Assign native status
c13.1_11.5 <- c13.1_11.5 %>% 
  mutate(Native = rep(NA, nrow(c13.1_11.5)))
for(i in 1:nrow(c13.1_11.5)) {
  if(c13.1_11.5$Common[i] %in% plant$invasive == TRUE) {
    c13.1_11.5$Native[i] <- "Invasive"
  } else if(c13.1_11.5$Common[i] %in% plant$native == TRUE) {
    c13.1_11.5$Native[i] <- "Native"
  } else if(c13.1_11.5$Common[i] %in% plant$ground == TRUE) {
    c13.1_11.5$Native[i] <- "Ground cover"
  } else {
    c13.1_11.5$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.1_11.5 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.1_11.5 <- c13.1_11.5 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.1_11.5 <- cbind(c13.1_11.5, cover)

wide.c13.1_11.5 <- c13.1_11.5

# Pivot data from wide to long
c13.1_11.5 <- wide.c13.1_11.5
c13.1_11.5 <- c13.1_11.5 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.1_11.5 <- c13.1_11.5 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.1_11.5)) {
  if(c13.1_11.5$Year[i] == "Mar12") {
    c13.1_11.5$Year[i] <- "2012-03-01"
  } else if(c13.1_11.5$Year[i] == "Nov12") {
    c13.1_11.5$Year[i] <- "2012-11-01"
  } else if(c13.1_11.5$Year[i] == "Nov13") {
    c13.1_11.5$Year[i] <- "2013-11-01"
  } else if(c13.1_11.5$Year[i] == "Nov14") {
    c13.1_11.5$Year[i] <- "2014-11-01"
  } else if(c13.1_11.5$Year[i] == "Nov15") {
    c13.1_11.5$Year[i] <- "2015-11-01"
  } else if(c13.1_11.5$Year[i] == "Nov18") {
    c13.1_11.5$Year[i] <- "2018-11-01"
  } else {
    c13.1_11.5$Year[i] <- "2021-11-01"
  }
}

c13.1_11.5$Year <- as.Date(c13.1_11.5$Year, format = "%Y-%m-%d")

# Add channel and station
c13.1_11.5$Station <- rep("Channel 13_Station 1 + 11.5 BAF", nrow(c13.1_11.5))



# C13 1 + 38.5 ------------------------------------------------------------

# Remove header and rename columns
c13.1_38.5 <- raw.c13.1_38.5[-c(1:7), -c(37:43)]
colnames(c13.1_38.5) <- names.raw

# Move values from Perennial Grass row to Unknown row
c13.1_38.5[13, 4] <- "0.5"
c13.1_38.5[13, 5] <- "15"

# Correct mis-typed value under gravel (652.5 should be 62.5)
c13.1_38.5[67, 3] <- "62.5"

# Remove unnecessary rows
c13.1_38.5 <- c13.1_38.5 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c13.1_38.5$Common[c13.1_38.5$Common == "Annual Grass"] <- "Annual grass (year summed)"
c13.1_38.5$Common[c13.1_38.5$Common == "annual panic"] <- "Annual panic"
c13.1_38.5$Common[c13.1_38.5$Common == "brown panic"] <- "Brown panic"
c13.1_38.5$Common[c13.1_38.5$Common == "Cup grass"] <- "Cupgrass"
c13.1_38.5$Common[c13.1_38.5$Common == "Pepper weed"] <- "Pepperweed"

# Assign missing scientific names (when known) and correct spelling
c13.1_38.5$Scientific[c13.1_38.5$Common == "Pepperweed"] <- "Lepidium"
c13.1_38.5$Scientific[c13.1_38.5$Scientific == "Echonocereus"] <- "Echinocereus"
c13.1_38.5$Scientific[c13.1_38.5$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c13.1_38.5$Common, "Unk") == TRUE)
c13.1_38.5[12, 1] <- "Unknown perennial grass, C13"

# Assign functional groups
c13.1_38.5 <- c13.1_38.5 %>% 
  mutate(Functional = rep(NA, nrow(c13.1_38.5)))
for(i in 1:nrow(c13.1_38.5)) {
  if(c13.1_38.5$Common[i] %in% plant$per.grass == TRUE) {
    c13.1_38.5$Functional[i] <- "Perennial grass"
  } else if(c13.1_38.5$Common[i] %in% plant$an.grass == TRUE) {
    c13.1_38.5$Functional[i] <- "Annual grass"
  } else if(c13.1_38.5$Common[i] %in% plant$per.forb == TRUE) {
    c13.1_38.5$Functional[i] <- "Perennial forb"
  } else if(c13.1_38.5$Common[i] %in% plant$an.forb == TRUE) {
    c13.1_38.5$Functional[i] <- "Annual forb"
  } else if(c13.1_38.5$Common[i] %in% plant$shrub == TRUE) {
    c13.1_38.5$Functional[i] <- "Shrub"
  } else if(c13.1_38.5$Common[i] %in% plant$tree == TRUE) {
    c13.1_38.5$Functional[i] <- "Tree"
  } else if(c13.1_38.5$Common[i] %in% plant$ground == TRUE) {
    c13.1_38.5$Functional[i] <- "Ground cover"
  } else {
    c13.1_38.5$Functional[i] <- "assign unknown"
  }
}

c13.1_38.5[12, "Functional"] <- "Perennial grass"
count(c13.1_38.5, Functional)

# Assign native status
c13.1_38.5 <- c13.1_38.5 %>% 
  mutate(Native = rep(NA, nrow(c13.1_38.5)))
for(i in 1:nrow(c13.1_38.5)) {
  if(c13.1_38.5$Common[i] %in% plant$invasive == TRUE) {
    c13.1_38.5$Native[i] <- "Invasive"
  } else if(c13.1_38.5$Common[i] %in% plant$native == TRUE) {
    c13.1_38.5$Native[i] <- "Native"
  } else if(c13.1_38.5$Common[i] %in% plant$ground == TRUE) {
    c13.1_38.5$Native[i] <- "Ground cover"
  } else {
    c13.1_38.5$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.1_38.5 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.1_38.5 <- c13.1_38.5 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.1_38.5 <- cbind(c13.1_38.5, cover)

wide.c13.1_38.5 <- c13.1_38.5

# Pivot data from wide to long
c13.1_38.5 <- wide.c13.1_38.5
c13.1_38.5 <- c13.1_38.5 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.1_38.5 <- c13.1_38.5 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.1_38.5)) {
  if(c13.1_38.5$Year[i] == "Mar12") {
    c13.1_38.5$Year[i] <- "2012-03-01"
  } else if(c13.1_38.5$Year[i] == "Nov12") {
    c13.1_38.5$Year[i] <- "2012-11-01"
  } else if(c13.1_38.5$Year[i] == "Nov13") {
    c13.1_38.5$Year[i] <- "2013-11-01"
  } else if(c13.1_38.5$Year[i] == "Nov14") {
    c13.1_38.5$Year[i] <- "2014-11-01"
  } else if(c13.1_38.5$Year[i] == "Nov15") {
    c13.1_38.5$Year[i] <- "2015-11-01"
  } else if(c13.1_38.5$Year[i] == "Nov18") {
    c13.1_38.5$Year[i] <- "2018-11-01"
  } else {
    c13.1_38.5$Year[i] <- "2021-11-01"
  }
}

c13.1_38.5$Year <- as.Date(c13.1_38.5$Year, format = "%Y-%m-%d")

# Add channel and station
c13.1_38.5$Station <- rep("Channel 13_Station 1 + 38.5 ORD", nrow(c13.1_38.5))



# C13 1 + 71 --------------------------------------------------------------

# Remove header and rename columns
c13.1_71 <- raw.c13.1_71[-c(1:7), -c(37:44)]
colnames(c13.1_71) <- names.raw

# Move value from Perennial Forbs row to Unknown row
c13.1_71[35, 15] <- "2.5"

# Remove unnecessary rows
c13.1_71 <- c13.1_71 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>%
  filter(!Common %in% c("Perennial Grass", "Perennial Forbs", 
                        "Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c13.1_71$Common[c13.1_71$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c13.1_71$Common[c13.1_71$Common == "Annual Grass"] <- "Annual grass (year summed)"
c13.1_71$Common[c13.1_71$Common == "Cup grass"] <- "Cupgrass"
c13.1_71$Common[c13.1_71$Common == "Pepper weed"] <- "Pepperweed"
c13.1_71$Common[c13.1_71$Common == "Bideus"] <- "Bidens"

# Assign missing scientific names (when known) and correct spelling
c13.1_71$Scientific[c13.1_71$Common == "Bidens"] <- "Bidens"
c13.1_71$Scientific[c13.1_71$Common == "Pepperweed"] <- "Lepidium"
c13.1_71$Scientific[c13.1_71$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c13.1_71$Scientific[c13.1_71$Scientific == "Echinechloa"] <- "Echinochloa"

# Rename unknowns
which(str_detect(c13.1_71$Common, "Unk") == TRUE)
which(c13.1_71$Common == "Perennial fern")
c13.1_71[26, 1] <- "Unknown perennial forb, C13"
c13.1_71[27, 1] <- "Unknown perennial fern, C13"

# Assign functional groups
c13.1_71 <- c13.1_71 %>% 
  mutate(Functional = rep(NA, nrow(c13.1_71)))
for(i in 1:nrow(c13.1_71)) {
  if(c13.1_71$Common[i] %in% plant$per.grass == TRUE) {
    c13.1_71$Functional[i] <- "Perennial grass"
  } else if(c13.1_71$Common[i] %in% plant$an.grass == TRUE) {
    c13.1_71$Functional[i] <- "Annual grass"
  } else if(c13.1_71$Common[i] %in% plant$per.forb == TRUE) {
    c13.1_71$Functional[i] <- "Perennial forb"
  } else if(c13.1_71$Common[i] %in% plant$an.forb == TRUE) {
    c13.1_71$Functional[i] <- "Annual forb"
  } else if(c13.1_71$Common[i] %in% plant$shrub == TRUE) {
    c13.1_71$Functional[i] <- "Shrub"
  } else if(c13.1_71$Common[i] %in% plant$tree == TRUE) {
    c13.1_71$Functional[i] <- "Tree"
  } else if(c13.1_71$Common[i] %in% plant$ground == TRUE) {
    c13.1_71$Functional[i] <- "Ground cover"
  } else {
    c13.1_71$Functional[i] <- "assign unknown"
  }
}

c13.1_71[26, "Functional"] <- "Perennial forb"
c13.1_71[27, "Functional"] <- "Perennial forb"
count(c13.1_71, Functional)

# Assign native status
c13.1_71 <- c13.1_71 %>% 
  mutate(Native = rep(NA, nrow(c13.1_71)))
for(i in 1:nrow(c13.1_71)) {
  if(c13.1_71$Common[i] %in% plant$invasive == TRUE) {
    c13.1_71$Native[i] <- "Invasive"
  } else if(c13.1_71$Common[i] %in% plant$native == TRUE) {
    c13.1_71$Native[i] <- "Native"
  } else if(c13.1_71$Common[i] %in% plant$ground == TRUE) {
    c13.1_71$Native[i] <- "Ground cover"
  } else {
    c13.1_71$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.1_71 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.1_71 <- c13.1_71 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.1_71 <- cbind(c13.1_71, cover)

wide.c13.1_71 <- c13.1_71

# Pivot data from wide to long
c13.1_71 <- wide.c13.1_71
c13.1_71 <- c13.1_71 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.1_71 <- c13.1_71 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.1_71)) {
  if(c13.1_71$Year[i] == "Mar12") {
    c13.1_71$Year[i] <- "2012-03-01"
  } else if(c13.1_71$Year[i] == "Nov12") {
    c13.1_71$Year[i] <- "2012-11-01"
  } else if(c13.1_71$Year[i] == "Nov13") {
    c13.1_71$Year[i] <- "2013-11-01"
  } else if(c13.1_71$Year[i] == "Nov14") {
    c13.1_71$Year[i] <- "2014-11-01"
  } else if(c13.1_71$Year[i] == "Nov15") {
    c13.1_71$Year[i] <- "2015-11-01"
  } else if(c13.1_71$Year[i] == "Nov18") {
    c13.1_71$Year[i] <- "2018-11-01"
  } else {
    c13.1_71$Year[i] <- "2021-11-01"
  }
}

c13.1_71$Year <- as.Date(c13.1_71$Year, format = "%Y-%m-%d")

# Add channel and station
c13.1_71$Station <- rep("Channel 13_Station 1 + 71 BAF", nrow(c13.1_71))



# C13 1 + 77 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c13.1_77 <- raw.c13.1_77[-c(1:7), -c(37:43)]
colnames(c13.1_77) <- names.raw

c13.1_77 <- c13.1_77 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>%  
  filter(!Common %in% c("Perennial Grass", "Perennial Forbs", "Annual Forbs", 
                        "Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c13.1_77$Common[c13.1_77$Common == "Annual Grass"] <- "Annual grass (year summed)"
c13.1_77$Common[c13.1_77$Common == "Barnyard grama"] <- "Barnyard"
c13.1_77$Common[c13.1_77$Common == "Pepper weed"] <- "Pepperweed"

# Assign missing scientific names (when known) and correct spelling
c13.1_77$Scientific[c13.1_77$Common == "Brown panic"] <- "Panicum"
c13.1_77$Scientific[c13.1_77$Common == "Bidens"] <- "Bidens"
c13.1_77$Scientific[c13.1_77$Common == "Pepperweed"] <- "Lepidium"
c13.1_77$Scientific[c13.1_77$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c13.1_77$Common, "Unk") == TRUE)
which(c13.1_77$Common == "Perennial fern")
c13.1_77[12, 1] <- "Unknown perennial grass, C13"
c13.1_77[26, 1] <- "Unknown perennial fern, C13"

# Assign functional groups
c13.1_77 <- c13.1_77 %>% 
  mutate(Functional = rep(NA, nrow(c13.1_77)))
for(i in 1:nrow(c13.1_77)) {
  if(c13.1_77$Common[i] %in% plant$per.grass == TRUE) {
    c13.1_77$Functional[i] <- "Perennial grass"
  } else if(c13.1_77$Common[i] %in% plant$an.grass == TRUE) {
    c13.1_77$Functional[i] <- "Annual grass"
  } else if(c13.1_77$Common[i] %in% plant$per.forb == TRUE) {
    c13.1_77$Functional[i] <- "Perennial forb"
  } else if(c13.1_77$Common[i] %in% plant$an.forb == TRUE) {
    c13.1_77$Functional[i] <- "Annual forb"
  } else if(c13.1_77$Common[i] %in% plant$shrub == TRUE) {
    c13.1_77$Functional[i] <- "Shrub"
  } else if(c13.1_77$Common[i] %in% plant$tree == TRUE) {
    c13.1_77$Functional[i] <- "Tree"
  } else if(c13.1_77$Common[i] %in% plant$ground == TRUE) {
    c13.1_77$Functional[i] <- "Ground cover"
  } else {
    c13.1_77$Functional[i] <- "assign unknown"
  }
}

c13.1_77[12, "Functional"] <- "Perennial grass"
c13.1_77[26, "Functional"] <- "Perennial forb"
count(c13.1_77, Functional)

# Assign native status
c13.1_77 <- c13.1_77 %>% 
  mutate(Native = rep(NA, nrow(c13.1_77)))
for(i in 1:nrow(c13.1_77)) {
  if(c13.1_77$Common[i] %in% plant$invasive == TRUE) {
    c13.1_77$Native[i] <- "Invasive"
  } else if(c13.1_77$Common[i] %in% plant$native == TRUE) {
    c13.1_77$Native[i] <- "Native"
  } else if(c13.1_77$Common[i] %in% plant$ground == TRUE) {
    c13.1_77$Native[i] <- "Ground cover"
  } else {
    c13.1_77$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.1_77 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.1_77 <- c13.1_77 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.1_77 <- cbind(c13.1_77, cover)

wide.c13.1_77 <- c13.1_77

# Pivot data from wide to long
c13.1_77 <- wide.c13.1_77
c13.1_77 <- c13.1_77 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.1_77 <- c13.1_77 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.1_77)) {
  if(c13.1_77$Year[i] == "Mar12") {
    c13.1_77$Year[i] <- "2012-03-01"
  } else if(c13.1_77$Year[i] == "Nov12") {
    c13.1_77$Year[i] <- "2012-11-01"
  } else if(c13.1_77$Year[i] == "Nov14") {
    c13.1_77$Year[i] <- "2014-11-01"
  } else if(c13.1_77$Year[i] == "Nov15") {
    c13.1_77$Year[i] <- "2015-11-01"
  } else if(c13.1_77$Year[i] == "Nov18") {
    c13.1_77$Year[i] <- "2018-11-01"
  } else {
    c13.1_77$Year[i] <- "2021-11-01"
  }
}

c13.1_77$Year <- as.Date(c13.1_77$Year, format = "%Y-%m-%d")

# Add channel and station
c13.1_77$Station <- rep("Channel 13_Station 1 + 77 ORD", nrow(c13.1_77))



# C13 1 + 99 --------------------------------------------------------------

# Remove header and rename columns
c13.1_99 <- raw.c13.1_99[-c(1:7), -c(37:43)]
colnames(c13.1_99) <- names.raw

# Add missing common names
c13.1_99$Common[c13.1_99$Scientific == "Bothriochloa barbinodis"] <- "Cane beardgrass"

# Remove unnecessary rows
c13.1_99 <- c13.1_99 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>%  
  filter(!str_detect(Common, "Perennial")) %>%  
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Reassign CHs with values for Nov 2014
which(c13.1_99 == "CH", arr.ind = TRUE)

c13.1_99[13, 32] <- "0"
c13.1_99[14, 32] <- "7.5"
c13.1_99[15, 32] <- "7.5"

# Standardize common names
c13.1_99$Common[c13.1_99$Common == "Annual Grass"] <- "Annual grass (year summed)"
c13.1_99$Common[c13.1_99$Common == "Matweed/Carpet weed"] <- "Carpetweed"
c13.1_99$Common[c13.1_99$Common == "Euphorbia"] <- "Spurge"
c13.1_99$Common[c13.1_99$Common == "AZ Wrightwort"] <- "AZ wrightwort"

# Assign missing scientific names (when known) and correct spelling
c13.1_99$Scientific[c13.1_99$Common == "Brown panic"] <- "Panicum"
c13.1_99$Scientific[c13.1_99$Common == "Spurge"] <- "Euphorbia"
c13.1_99$Scientific[c13.1_99$Common == "Pepperweed"] <- "Lepidium"
c13.1_99$Scientific[c13.1_99$Scientific == "Eriochloa acumintata"] <- "Eriochloa acuminata"
c13.1_99$Scientific[c13.1_99$Scientific == "Dentata Brichellia, maybe Aguitia"] <- "Brickellia dentata?"
c13.1_99$Scientific[c13.1_99$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c13.1_99$Scientific[c13.1_99$Scientific == "Mullugo sp."] <- "Mollugo sp."

# Rename unknowns
which(str_detect(c13.1_99$Common, "Unk") == TRUE)
c13.1_99[42, 1] <- "Unknown shrub, C13"
c13.1_99[43, 1] <- "Unknown dentata shrub, C13"

# Assign functional groups
c13.1_99 <- c13.1_99 %>% 
  mutate(Functional = rep(NA, nrow(c13.1_99)))
for(i in 1:nrow(c13.1_99)) {
  if(c13.1_99$Common[i] %in% plant$per.grass == TRUE) {
    c13.1_99$Functional[i] <- "Perennial grass"
  } else if(c13.1_99$Common[i] %in% plant$an.grass == TRUE) {
    c13.1_99$Functional[i] <- "Annual grass"
  } else if(c13.1_99$Common[i] %in% plant$per.forb == TRUE) {
    c13.1_99$Functional[i] <- "Perennial forb"
  } else if(c13.1_99$Common[i] %in% plant$an.forb == TRUE) {
    c13.1_99$Functional[i] <- "Annual forb"
  } else if(c13.1_99$Common[i] %in% plant$shrub == TRUE) {
    c13.1_99$Functional[i] <- "Shrub"
  } else if(c13.1_99$Common[i] %in% plant$tree == TRUE) {
    c13.1_99$Functional[i] <- "Tree"
  } else if(c13.1_99$Common[i] %in% plant$ground == TRUE) {
    c13.1_99$Functional[i] <- "Ground cover"
  } else {
    c13.1_99$Functional[i] <- "assign unknown"
  }
}

c13.1_99[42, "Functional"] <- "Shrub"
c13.1_99[43, "Functional"] <- "Shrub"
count(c13.1_99, Functional)

# Assign native status
c13.1_99 <- c13.1_99 %>% 
  mutate(Native = rep(NA, nrow(c13.1_99)))
for(i in 1:nrow(c13.1_99)) {
  if(c13.1_99$Common[i] %in% plant$invasive == TRUE) {
    c13.1_99$Native[i] <- "Invasive"
  } else if(c13.1_99$Common[i] %in% plant$native == TRUE) {
    c13.1_99$Native[i] <- "Native"
  } else if(c13.1_99$Common[i] %in% plant$ground == TRUE) {
    c13.1_99$Native[i] <- "Ground cover"
  } else {
    c13.1_99$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.1_99 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.1_99 <- c13.1_99 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.1_99 <- cbind(c13.1_99, cover)

wide.c13.1_99 <- c13.1_99

# Pivot data from wide to long
c13.1_99 <- wide.c13.1_99
c13.1_99 <- c13.1_99 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.1_99 <- c13.1_99 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.1_99)) {
  if(c13.1_99$Year[i] == "Mar12") {
    c13.1_99$Year[i] <- "2012-03-01"
  } else if(c13.1_99$Year[i] == "Nov12") {
    c13.1_99$Year[i] <- "2012-11-01"
  } else if(c13.1_99$Year[i] == "Nov13") {
    c13.1_99$Year[i] <- "2013-11-01"
  } else if(c13.1_99$Year[i] == "Nov14") {
    c13.1_99$Year[i] <- "2014-11-01"
  } else if(c13.1_99$Year[i] == "Nov15") {
    c13.1_99$Year[i] <- "2015-11-01"
  } else if(c13.1_99$Year[i] == "Nov18") {
    c13.1_99$Year[i] <- "2018-11-01"
  } else {
    c13.1_99$Year[i] <- "2021-11-01"
  }
}

c13.1_99$Year <- as.Date(c13.1_99$Year, format = "%Y-%m-%d")

# Add channel and station
c13.1_99$Station <- rep("Channel 13_Station 1 + 99 ORD", nrow(c13.1_99))



# C13 2 + 16 --------------------------------------------------------------

# Remove header and rename columns
c13.2_16 <- raw.c13.2_16[-c(1:7), -c(37:43)]
colnames(c13.2_16) <- names.raw

# Move value in Perennial Forbs row to an Unknown row
c13.2_16[36, 1] <- "Unknown"
c13.2_16[36, 25] <- "2.5"

# Add missing common names
c13.2_16$Common[c13.2_16$Scientific == "Bothriochloa barbinodis"] <- "Cane beardgrass"

# Remove unnecessary rows
c13.2_16 <- c13.2_16 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>%  
  filter(!str_detect(Common, "Perennial")) %>%  
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c13.2_16$Common[c13.2_16$Common == "Annual Grass"] <- "Annual grass (year summed)"
c13.2_16$Common[c13.2_16$Common == "Cup grass"] <- "Cupgrass"

# Assign missing scientific names (when known) and correct spelling
c13.2_16$Scientific[c13.2_16$Common == "Brown panic"] <- "Panicum"
c13.2_16$Scientific[c13.2_16$Common == "Buckwheat"] <- "Eriogonum inflatum"
c13.2_16$Scientific[c13.2_16$Scientific == "Bociharia"] <- "Boerhavia"
c13.2_16$Scientific[c13.2_16$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c13.2_16$Common, "Unk") == TRUE)
c13.2_16[13, 1] <- "Unknown perennial grass, C13"
c13.2_16[27, 1] <- "Unknown lavender, C13"
c13.2_16[28, 1] <- "Unknown perennial forb, C13"

# Assign functional groups
c13.2_16 <- c13.2_16 %>% 
  mutate(Functional = rep(NA, nrow(c13.2_16)))
for(i in 1:nrow(c13.2_16)) {
  if(c13.2_16$Common[i] %in% plant$per.grass == TRUE) {
    c13.2_16$Functional[i] <- "Perennial grass"
  } else if(c13.2_16$Common[i] %in% plant$an.grass == TRUE) {
    c13.2_16$Functional[i] <- "Annual grass"
  } else if(c13.2_16$Common[i] %in% plant$per.forb == TRUE) {
    c13.2_16$Functional[i] <- "Perennial forb"
  } else if(c13.2_16$Common[i] %in% plant$an.forb == TRUE) {
    c13.2_16$Functional[i] <- "Annual forb"
  } else if(c13.2_16$Common[i] %in% plant$shrub == TRUE) {
    c13.2_16$Functional[i] <- "Shrub"
  } else if(c13.2_16$Common[i] %in% plant$tree == TRUE) {
    c13.2_16$Functional[i] <- "Tree"
  } else if(c13.2_16$Common[i] %in% plant$ground == TRUE) {
    c13.2_16$Functional[i] <- "Ground cover"
  } else {
    c13.2_16$Functional[i] <- "assign unknown"
  }
}

c13.2_16[13, "Functional"] <- "Perennial grass"
c13.2_16[27, "Functional"] <- "Perennial forb"
c13.2_16[28, "Functional"] <- "Perennial forb"
count(c13.2_16, Functional)

# Assign native status
c13.2_16 <- c13.2_16 %>% 
  mutate(Native = rep(NA, nrow(c13.2_16)))
for(i in 1:nrow(c13.2_16)) {
  if(c13.2_16$Common[i] %in% plant$invasive == TRUE) {
    c13.2_16$Native[i] <- "Invasive"
  } else if(c13.2_16$Common[i] %in% plant$native == TRUE) {
    c13.2_16$Native[i] <- "Native"
  } else if(c13.2_16$Common[i] %in% plant$ground == TRUE) {
    c13.2_16$Native[i] <- "Ground cover"
  } else {
    c13.2_16$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.2_16 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.2_16 <- c13.2_16 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.2_16 <- cbind(c13.2_16, cover)

wide.c13.2_16 <- c13.2_16

# Pivot data from wide to long
c13.2_16 <- wide.c13.2_16
c13.2_16 <- c13.2_16 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.2_16 <- c13.2_16 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.2_16)) {
  if(c13.2_16$Year[i] == "Mar12") {
    c13.2_16$Year[i] <- "2012-03-01"
  } else if(c13.2_16$Year[i] == "Nov12") {
    c13.2_16$Year[i] <- "2012-11-01"
  } else if(c13.2_16$Year[i] == "Nov13") {
    c13.2_16$Year[i] <- "2013-11-01"
  } else if(c13.2_16$Year[i] == "Nov14") {
    c13.2_16$Year[i] <- "2014-11-01"
  } else if(c13.2_16$Year[i] == "Nov15") {
    c13.2_16$Year[i] <- "2015-11-01"
  } else if(c13.2_16$Year[i] == "Nov18") {
    c13.2_16$Year[i] <- "2018-11-01"
  } else {
    c13.2_16$Year[i] <- "2021-11-01"
  }
}

c13.2_16$Year <- as.Date(c13.2_16$Year, format = "%Y-%m-%d")

# Add channel and station
c13.2_16$Station <- rep("Channel 13_Station 2 + 16 BAF", nrow(c13.2_16))



# C13 2 + 57 --------------------------------------------------------------

# Remove header and rename columns
c13.2_57 <- raw.c13.2_57[-c(1:7), -c(37:43)]
colnames(c13.2_57) <- names.raw

# Remove unnecessary rows
c13.2_57 <- c13.2_57 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>%
  filter(!str_detect(Common, "Perennial")) %>%
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c13.2_57$Common[c13.2_57$Common == "Annual Grass"] <- "Annual grass (year summed)"
c13.2_57$Common[c13.2_57$Common == "Rothrock Grama"] <- "Rothrock grama"

# Assign missing scientific names (when known) and correct spelling
c13.2_57$Scientific[c13.2_57$Common == "Brown panic"] <- "Panicum"
c13.2_57$Scientific[c13.2_57$Common == "Pepperweed"] <- "Lepidium"
c13.2_57$Scientific[c13.2_57$Common == "Matweed"] <- "Guilleminea"
c13.2_57$Scientific[c13.2_57$Scientific == "Boerharia"] <- "Boerhavia"
c13.2_57$Scientific[c13.2_57$Scientific == "Bouteloua rothrochii"] <- "Bouteloua rothrockii"
c13.2_57$Scientific[c13.2_57$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c13.2_57$Common, "Unk") == TRUE)
c13.2_57[29, 1] <- "Unknown perennial forb, C13"

# Assign functional groups
c13.2_57 <- c13.2_57 %>% 
  mutate(Functional = rep(NA, nrow(c13.2_57)))
for(i in 1:nrow(c13.2_57)) {
  if(c13.2_57$Common[i] %in% plant$per.grass == TRUE) {
    c13.2_57$Functional[i] <- "Perennial grass"
  } else if(c13.2_57$Common[i] %in% plant$an.grass == TRUE) {
    c13.2_57$Functional[i] <- "Annual grass"
  } else if(c13.2_57$Common[i] %in% plant$per.forb == TRUE) {
    c13.2_57$Functional[i] <- "Perennial forb"
  } else if(c13.2_57$Common[i] %in% plant$an.forb == TRUE) {
    c13.2_57$Functional[i] <- "Annual forb"
  } else if(c13.2_57$Common[i] %in% plant$shrub == TRUE) {
    c13.2_57$Functional[i] <- "Shrub"
  } else if(c13.2_57$Common[i] %in% plant$tree == TRUE) {
    c13.2_57$Functional[i] <- "Tree"
  } else if(c13.2_57$Common[i] %in% plant$ground == TRUE) {
    c13.2_57$Functional[i] <- "Ground cover"
  } else {
    c13.2_57$Functional[i] <- "assign unknown"
  }
}

c13.2_57[29, "Functional"] <- "Perennial forb"
count(c13.2_57, Functional)

# Assign native status
c13.2_57 <- c13.2_57 %>% 
  mutate(Native = rep(NA, nrow(c13.2_57)))
for(i in 1:nrow(c13.2_57)) {
  if(c13.2_57$Common[i] %in% plant$invasive == TRUE) {
    c13.2_57$Native[i] <- "Invasive"
  } else if(c13.2_57$Common[i] %in% plant$native == TRUE) {
    c13.2_57$Native[i] <- "Native"
  } else if(c13.2_57$Common[i] %in% plant$ground == TRUE) {
    c13.2_57$Native[i] <- "Ground cover"
  } else {
    c13.2_57$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.2_57 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.2_57 <- c13.2_57 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.2_57 <- cbind(c13.2_57, cover)

wide.c13.2_57 <- c13.2_57

# Pivot data from wide to long
c13.2_57 <- wide.c13.2_57
c13.2_57 <- c13.2_57 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.2_57 <- c13.2_57 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.2_57)) {
  if(c13.2_57$Year[i] == "Mar12") {
    c13.2_57$Year[i] <- "2012-03-01"
  } else if(c13.2_57$Year[i] == "Nov12") {
    c13.2_57$Year[i] <- "2012-11-01"
  } else if(c13.2_57$Year[i] == "Nov13") {
    c13.2_57$Year[i] <- "2013-11-01"
  } else if(c13.2_57$Year[i] == "Nov14") {
    c13.2_57$Year[i] <- "2014-11-01"
  } else if(c13.2_57$Year[i] == "Nov15") {
    c13.2_57$Year[i] <- "2015-11-01"
  } else if(c13.2_57$Year[i] == "Nov18") {
    c13.2_57$Year[i] <- "2018-11-01"
  } else {
    c13.2_57$Year[i] <- "2021-11-01"
  }
}

c13.2_57$Year <- as.Date(c13.2_57$Year, format = "%Y-%m-%d")

# Add channel and station
c13.2_57$Station <- rep("Channel 13_Station 2 + 57 ORD", nrow(c13.2_57))



# C13 2 + 67 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c13.2_67 <- raw.c13.2_67[-c(1:7), -c(37:43)]
colnames(c13.2_67) <- names.raw

c13.2_67 <- c13.2_67 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c13.2_67$Common[c13.2_67$Common == "Annual Grass"] <- "Annual grass (year summed)"
c13.2_67$Common[c13.2_67$Common == "Pepper weed"] <- "Pepperweed"

# Assign missing scientific names (when known) and correct spelling
c13.2_67$Scientific[c13.2_67$Common == "Brown panic"] <- "Panicum"
c13.2_67$Scientific[c13.2_67$Common == "Pepperweed"] <- "Lepidium"
c13.2_67$Scientific[c13.2_67$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c13.2_67$Common, "Unk") == TRUE)
c13.2_67[25, 1] <- "Unknown lavender, C13"

# Assign functional groups
c13.2_67 <- c13.2_67 %>% 
  mutate(Functional = rep(NA, nrow(c13.2_67)))
for(i in 1:nrow(c13.2_67)) {
  if(c13.2_67$Common[i] %in% plant$per.grass == TRUE) {
    c13.2_67$Functional[i] <- "Perennial grass"
  } else if(c13.2_67$Common[i] %in% plant$an.grass == TRUE) {
    c13.2_67$Functional[i] <- "Annual grass"
  } else if(c13.2_67$Common[i] %in% plant$per.forb == TRUE) {
    c13.2_67$Functional[i] <- "Perennial forb"
  } else if(c13.2_67$Common[i] %in% plant$an.forb == TRUE) {
    c13.2_67$Functional[i] <- "Annual forb"
  } else if(c13.2_67$Common[i] %in% plant$shrub == TRUE) {
    c13.2_67$Functional[i] <- "Shrub"
  } else if(c13.2_67$Common[i] %in% plant$tree == TRUE) {
    c13.2_67$Functional[i] <- "Tree"
  } else if(c13.2_67$Common[i] %in% plant$ground == TRUE) {
    c13.2_67$Functional[i] <- "Ground cover"
  } else {
    c13.2_67$Functional[i] <- "assign unknown"
  }
}

c13.2_67[25, "Functional"] <- "Perennial forb"
count(c13.2_67, Functional)

# Assign native status
c13.2_67 <- c13.2_67 %>% 
  mutate(Native = rep(NA, nrow(c13.2_67)))
for(i in 1:nrow(c13.2_67)) {
  if(c13.2_67$Common[i] %in% plant$invasive == TRUE) {
    c13.2_67$Native[i] <- "Invasive"
  } else if(c13.2_67$Common[i] %in% plant$native == TRUE) {
    c13.2_67$Native[i] <- "Native"
  } else if(c13.2_67$Common[i] %in% plant$ground == TRUE) {
    c13.2_67$Native[i] <- "Ground cover"
  } else {
    c13.2_67$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.2_67 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.2_67 <- c13.2_67 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.2_67 <- cbind(c13.2_67, cover)

wide.c13.2_67 <- c13.2_67

# Pivot data from wide to long
c13.2_67 <- wide.c13.2_67
c13.2_67 <- c13.2_67 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.2_67 <- c13.2_67 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.2_67)) {
  if(c13.2_67$Year[i] == "Mar12") {
    c13.2_67$Year[i] <- "2012-03-01"
  } else if(c13.2_67$Year[i] == "Nov12") {
    c13.2_67$Year[i] <- "2012-11-01"
  } else if(c13.2_67$Year[i] == "Nov13") {
    c13.2_67$Year[i] <- "2013-11-01"
  } else if(c13.2_67$Year[i] == "Nov14") {
    c13.2_67$Year[i] <- "2014-11-01"
  } else if(c13.2_67$Year[i] == "Nov15") {
    c13.2_67$Year[i] <- "2015-11-01"
  } else if(c13.2_67$Year[i] == "Nov18") {
    c13.2_67$Year[i] <- "2018-11-01"
  } else {
    c13.2_67$Year[i] <- "2021-11-01"
  }
}

c13.2_67$Year <- as.Date(c13.2_67$Year, format = "%Y-%m-%d")

# Add channel and station
c13.2_67$Station <- rep("Channel 13_Station 2 + 67 BAF", nrow(c13.2_67))



# C13 3 + 09 -------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c13.3_09 <- raw.c13.3_09[-c(1:7), -c(37:43)]
colnames(c13.3_09) <- names.raw

c13.3_09 <- c13.3_09 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover")) %>% 
  filter(Common != "Mesa threeawn")  # Mesa threeawn is the same as spidergrass and row is empty

# Standardize common names
c13.3_09$Common[c13.3_09$Common == "Annual Grass"] <- "Annual grass (year summed)"

# Assign missing scientific names (when known) and correct spelling
c13.3_09$Scientific[c13.3_09$Common == "Brown panic"] <- "Panicum"
c13.3_09$Scientific[c13.3_09$Common == "Matweed"] <- "Guilleminea"
c13.3_09$Scientific[c13.3_09$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c13.3_09$Common, "Unk") == TRUE)
c13.3_09[11, 1] <- "Unknown perennial grass, C13"
c13.3_09[27, 1] <- "Unknown annual forb, C13"

# Assign functional groups
c13.3_09 <- c13.3_09 %>% 
  mutate(Functional = rep(NA, nrow(c13.3_09)))
for(i in 1:nrow(c13.3_09)) {
  if(c13.3_09$Common[i] %in% plant$per.grass == TRUE) {
    c13.3_09$Functional[i] <- "Perennial grass"
  } else if(c13.3_09$Common[i] %in% plant$an.grass == TRUE) {
    c13.3_09$Functional[i] <- "Annual grass"
  } else if(c13.3_09$Common[i] %in% plant$per.forb == TRUE) {
    c13.3_09$Functional[i] <- "Perennial forb"
  } else if(c13.3_09$Common[i] %in% plant$an.forb == TRUE) {
    c13.3_09$Functional[i] <- "Annual forb"
  } else if(c13.3_09$Common[i] %in% plant$shrub == TRUE) {
    c13.3_09$Functional[i] <- "Shrub"
  } else if(c13.3_09$Common[i] %in% plant$tree == TRUE) {
    c13.3_09$Functional[i] <- "Tree"
  } else if(c13.3_09$Common[i] %in% plant$ground == TRUE) {
    c13.3_09$Functional[i] <- "Ground cover"
  } else {
    c13.3_09$Functional[i] <- "assign unknown"
  }
}

c13.3_09[11, "Functional"] <- "Perennial grass"
c13.3_09[27, "Functional"] <- "Annual forb"
count(c13.3_09, Functional)

# Assign native status
c13.3_09 <- c13.3_09 %>% 
  mutate(Native = rep(NA, nrow(c13.3_09)))
for(i in 1:nrow(c13.3_09)) {
  if(c13.3_09$Common[i] %in% plant$invasive == TRUE) {
    c13.3_09$Native[i] <- "Invasive"
  } else if(c13.3_09$Common[i] %in% plant$native == TRUE) {
    c13.3_09$Native[i] <- "Native"
  } else if(c13.3_09$Common[i] %in% plant$ground == TRUE) {
    c13.3_09$Native[i] <- "Ground cover"
  } else {
    c13.3_09$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.3_09 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.3_09 <- c13.3_09 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.3_09 <- cbind(c13.3_09, cover)

wide.c13.3_09 <- c13.3_09

# Pivot data from wide to long
c13.3_09 <- wide.c13.3_09
c13.3_09 <- c13.3_09 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.3_09 <- c13.3_09 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.3_09)) {
  if(c13.3_09$Year[i] == "Mar12") {
    c13.3_09$Year[i] <- "2012-03-01"
  } else if(c13.3_09$Year[i] == "Nov12") {
    c13.3_09$Year[i] <- "2012-11-01"
  } else if(c13.3_09$Year[i] == "Nov13") {
    c13.3_09$Year[i] <- "2013-11-01"
  } else if(c13.3_09$Year[i] == "Nov14") {
    c13.3_09$Year[i] <- "2014-11-01"
  } else if(c13.3_09$Year[i] == "Nov15") {
    c13.3_09$Year[i] <- "2015-11-01"
  } else if(c13.3_09$Year[i] == "Nov18") {
    c13.3_09$Year[i] <- "2018-11-01"
  } else {
    c13.3_09$Year[i] <- "2021-11-01"
  }
}

c13.3_09$Year <- as.Date(c13.3_09$Year, format = "%Y-%m-%d")

# Add channel and station
c13.3_09$Station <- rep("Channel 13_Station 3 + 09 ORD", nrow(c13.3_09))



# C13 3 + 31 --------------------------------------------------------------

# Remove header and rename columns
c13.3_31 <- raw.c13.3_31[-c(1:7), -c(37:43)]
colnames(c13.3_31) <- names.raw

# Add missing common names
c13.3_31$Common[c13.3_31$Scientific == "Eragostis cilianersis"] <- "Stinkgrass"

# Remove unnecessary rows
c13.3_31 <- c13.3_31 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover")) %>% 
  filter(Common != "Mesa threeawn")  # Mesa threeawn is the same as spidergrass and row is empty

# Standardize common names
c13.3_31$Common[c13.3_31$Common == "Annual Grass"] <- "Annual grass (year summed)"

# Assign missing scientific names (when known) and correct spelling
c13.3_31$Scientific[c13.3_31$Common == "Pepperweed"] <- "Lepidium"
c13.3_31$Scientific[c13.3_31$Common == "Matweed"] <- "Guilleminea"
c13.3_31$Scientific[c13.3_31$Scientific == "Eragostis cilianersis"] <- "Eragrostis cilianensis"
c13.3_31$Scientific[c13.3_31$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c13.3_31$Common, "Unk") == TRUE)
c13.3_31[12, 1] <- "Unknown perennial grass, C13"
c13.3_31[23, 1] <- "Unknown lavender, C13"

# Assign functional groups
c13.3_31 <- c13.3_31 %>% 
  mutate(Functional = rep(NA, nrow(c13.3_31)))
for(i in 1:nrow(c13.3_31)) {
  if(c13.3_31$Common[i] %in% plant$per.grass == TRUE) {
    c13.3_31$Functional[i] <- "Perennial grass"
  } else if(c13.3_31$Common[i] %in% plant$an.grass == TRUE) {
    c13.3_31$Functional[i] <- "Annual grass"
  } else if(c13.3_31$Common[i] %in% plant$per.forb == TRUE) {
    c13.3_31$Functional[i] <- "Perennial forb"
  } else if(c13.3_31$Common[i] %in% plant$an.forb == TRUE) {
    c13.3_31$Functional[i] <- "Annual forb"
  } else if(c13.3_31$Common[i] %in% plant$shrub == TRUE) {
    c13.3_31$Functional[i] <- "Shrub"
  } else if(c13.3_31$Common[i] %in% plant$tree == TRUE) {
    c13.3_31$Functional[i] <- "Tree"
  } else if(c13.3_31$Common[i] %in% plant$ground == TRUE) {
    c13.3_31$Functional[i] <- "Ground cover"
  } else {
    c13.3_31$Functional[i] <- "assign unknown"
  }
}

c13.3_31[12, "Functional"] <- "Perennial grass"
c13.3_31[23, "Functional"] <- "Perennial forb"
count(c13.3_31, Functional)

# Assign native status
c13.3_31 <- c13.3_31 %>% 
  mutate(Native = rep(NA, nrow(c13.3_31)))
for(i in 1:nrow(c13.3_31)) {
  if(c13.3_31$Common[i] %in% plant$invasive == TRUE) {
    c13.3_31$Native[i] <- "Invasive"
  } else if(c13.3_31$Common[i] %in% plant$native == TRUE) {
    c13.3_31$Native[i] <- "Native"
  } else if(c13.3_31$Common[i] %in% plant$ground == TRUE) {
    c13.3_31$Native[i] <- "Ground cover"
  } else {
    c13.3_31$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.3_31 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.3_31 <- c13.3_31 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.3_31 <- cbind(c13.3_31, cover)

wide.c13.3_31 <- c13.3_31

# Pivot data from wide to long
c13.3_31 <- wide.c13.3_31
c13.3_31 <- c13.3_31 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.3_31 <- c13.3_31 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.3_31)) {
  if(c13.3_31$Year[i] == "Mar12") {
    c13.3_31$Year[i] <- "2012-03-01"
  } else if(c13.3_31$Year[i] == "Nov12") {
    c13.3_31$Year[i] <- "2012-11-01"
  } else if(c13.3_31$Year[i] == "Nov13") {
    c13.3_31$Year[i] <- "2013-11-01"
  } else if(c13.3_31$Year[i] == "Nov14") {
    c13.3_31$Year[i] <- "2014-11-01"
  } else if(c13.3_31$Year[i] == "Nov15") {
    c13.3_31$Year[i] <- "2015-11-01"
  } else if(c13.3_31$Year[i] == "Nov18") {
    c13.3_31$Year[i] <- "2018-11-01"
  } else {
    c13.3_31$Year[i] <- "2021-11-01"
  }
}

c13.3_31$Year <- as.Date(c13.3_31$Year, format = "%Y-%m-%d")

# Add channel and station
c13.3_31$Station <- rep("Channel 13_Station 3 + 31 BAF", nrow(c13.3_31))



# C13 3 + 65 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c13.3_65 <- raw.c13.3_65[-c(1:7), -c(37:43)]
colnames(c13.3_65) <- names.raw

c13.3_65 <- c13.3_65 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>%  
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row 
  # Cane beardgrass is listed twice, but one row is empty (row 9)
c13.3_65 <- c13.3_65[-9, ]

# Standardize common names
c13.3_65$Common[c13.3_65$Common == "Annual Grass"] <- "Annual grass (year summed)"
c13.3_65$Common[c13.3_65$Common == "cupgrass"] <- "Cupgrass"
c13.3_65$Common[c13.3_65$Common == "Barnyard grama"] <- "Barnyard"
c13.3_65$Common[c13.3_65$Common == "AZ Wrightwort"] <- "AZ wrightwort"

# Assign missing scientific names (when known) and correct spelling
c13.3_65$Scientific[c13.3_65$Common == "Cane beardgrass"] <- "Bothriochloa barbinodis"
c13.3_65$Scientific[c13.3_65$Common == "Matweed"] <- "Guilleminea"
c13.3_65$Scientific[c13.3_65$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c13.3_65$Common, "Unk") == TRUE)
c13.3_65[16, 1] <- "Unknown annual grass, C13"

# Assign functional groups
c13.3_65 <- c13.3_65 %>% 
  mutate(Functional = rep(NA, nrow(c13.3_65)))
for(i in 1:nrow(c13.3_65)) {
  if(c13.3_65$Common[i] %in% plant$per.grass == TRUE) {
    c13.3_65$Functional[i] <- "Perennial grass"
  } else if(c13.3_65$Common[i] %in% plant$an.grass == TRUE) {
    c13.3_65$Functional[i] <- "Annual grass"
  } else if(c13.3_65$Common[i] %in% plant$per.forb == TRUE) {
    c13.3_65$Functional[i] <- "Perennial forb"
  } else if(c13.3_65$Common[i] %in% plant$an.forb == TRUE) {
    c13.3_65$Functional[i] <- "Annual forb"
  } else if(c13.3_65$Common[i] %in% plant$shrub == TRUE) {
    c13.3_65$Functional[i] <- "Shrub"
  } else if(c13.3_65$Common[i] %in% plant$tree == TRUE) {
    c13.3_65$Functional[i] <- "Tree"
  } else if(c13.3_65$Common[i] %in% plant$ground == TRUE) {
    c13.3_65$Functional[i] <- "Ground cover"
  } else {
    c13.3_65$Functional[i] <- "assign unknown"
  }
}

c13.3_65[16, "Functional"] <- "Annual grass"
count(c13.3_65, Functional)

# Assign native status
c13.3_65 <- c13.3_65 %>% 
  mutate(Native = rep(NA, nrow(c13.3_65)))
for(i in 1:nrow(c13.3_65)) {
  if(c13.3_65$Common[i] %in% plant$invasive == TRUE) {
    c13.3_65$Native[i] <- "Invasive"
  } else if(c13.3_65$Common[i] %in% plant$native == TRUE) {
    c13.3_65$Native[i] <- "Native"
  } else if(c13.3_65$Common[i] %in% plant$ground == TRUE) {
    c13.3_65$Native[i] <- "Ground cover"
  } else {
    c13.3_65$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.3_65 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.3_65 <- c13.3_65 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.3_65 <- cbind(c13.3_65, cover)

wide.c13.3_65 <- c13.3_65

# Pivot data from wide to long
c13.3_65 <- wide.c13.3_65
c13.3_65 <- c13.3_65 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.3_65 <- c13.3_65 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.3_65)) {
  if(c13.3_65$Year[i] == "Mar12") {
    c13.3_65$Year[i] <- "2012-03-01"
  } else if(c13.3_65$Year[i] == "Nov12") {
    c13.3_65$Year[i] <- "2012-11-01"
  } else if(c13.3_65$Year[i] == "Nov13") {
    c13.3_65$Year[i] <- "2013-11-01"
  } else if(c13.3_65$Year[i] == "Nov14") {
    c13.3_65$Year[i] <- "2014-11-01"
  } else if(c13.3_65$Year[i] == "Nov15") {
    c13.3_65$Year[i] <- "2015-11-01"
  } else if(c13.3_65$Year[i] == "Nov18") {
    c13.3_65$Year[i] <- "2018-11-01"
  } else {
    c13.3_65$Year[i] <- "2021-11-01"
  }
}

c13.3_65$Year <- as.Date(c13.3_65$Year, format = "%Y-%m-%d")

# Add channel and station
c13.3_65$Station <- rep("Channel 13_Station 3 + 65 ORD", nrow(c13.3_65))



# C13 3 + 95.5 --------------------------------------------------------------

# Remove header and rename columns
c13.3_95.5 <- raw.c13.3_95.5[-c(1:7), -c(37:43)]
colnames(c13.3_95.5) <- names.raw

# Reassign CHs with values for Nov 2012 perennial grasses
which(c13.3_95.5 == "CH", arr.ind = TRUE)

c13.3_95.5[13, 1] <- "Unknown"
c13.3_95.5[13, 17] <- "2.5"
c13.3_95.5[13, 19] <- "2.5"
c13.3_95.5[13, 22] <- "2.5"

c13.3_95.5[2, 18] <- "0.5"
c13.3_95.5[3, 20] <- "5"
c13.3_95.5[4, 20] <- "5"
c13.3_95.5[8, 20] <- "5"
c13.3_95.5[8, 21] <- "62.5"

# Add missing common names
c13.3_95.5$Common[c13.3_95.5$Scientific == "Ambrosia psilotacleria - annual or weak perennial?"] <- "Cuman ragweed"

# Remove unnecessary rows
c13.3_95.5 <- c13.3_95.5 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>%  
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Standardize common 
c13.3_95.5$Common[c13.3_95.5$Common == "Annual Grass"] <- "Annual grass (year summed)"
c13.3_95.5$Common[c13.3_95.5$Common == "Barnyard grama"] <- "Barnyard"
c13.3_95.5$Common[c13.3_95.5$Common == "AZ Wrightwort"] <- "AZ wrightwort"

# Assign missing scientific names (when known) and correct spelling
c13.3_95.5$Scientific[c13.3_95.5$Common == "Barnyard"] <- "Echinochloa"
c13.3_95.5$Scientific[c13.3_95.5$Common == "Matweed"] <- "Guilleminea"
c13.3_95.5$Scientific[c13.3_95.5$Scientific == "Ferocactus wizliseni"] <- "Ferocactus wislizenii"
c13.3_95.5$Scientific[c13.3_95.5$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c13.3_95.5$Common, "Unk") == TRUE)
c13.3_95.5[12, 1] <- "Unknown perennial grass, C13"

# Assign functional groups
c13.3_95.5 <- c13.3_95.5 %>% 
  mutate(Functional = rep(NA, nrow(c13.3_95.5)))
for(i in 1:nrow(c13.3_95.5)) {
  if(c13.3_95.5$Common[i] %in% plant$per.grass == TRUE) {
    c13.3_95.5$Functional[i] <- "Perennial grass"
  } else if(c13.3_95.5$Common[i] %in% plant$an.grass == TRUE) {
    c13.3_95.5$Functional[i] <- "Annual grass"
  } else if(c13.3_95.5$Common[i] %in% plant$per.forb == TRUE) {
    c13.3_95.5$Functional[i] <- "Perennial forb"
  } else if(c13.3_95.5$Common[i] %in% plant$an.forb == TRUE) {
    c13.3_95.5$Functional[i] <- "Annual forb"
  } else if(c13.3_95.5$Common[i] %in% plant$shrub == TRUE) {
    c13.3_95.5$Functional[i] <- "Shrub"
  } else if(c13.3_95.5$Common[i] %in% plant$tree == TRUE) {
    c13.3_95.5$Functional[i] <- "Tree"
  } else if(c13.3_95.5$Common[i] %in% plant$ground == TRUE) {
    c13.3_95.5$Functional[i] <- "Ground cover"
  } else {
    c13.3_95.5$Functional[i] <- "assign unknown"
  }
}

c13.3_95.5[12, "Functional"] <- "Perennial grass"
count(c13.3_95.5, Functional)

# Assign native status
c13.3_95.5 <- c13.3_95.5 %>% 
  mutate(Native = rep(NA, nrow(c13.3_95.5)))
for(i in 1:nrow(c13.3_95.5)) {
  if(c13.3_95.5$Common[i] %in% plant$invasive == TRUE) {
    c13.3_95.5$Native[i] <- "Invasive"
  } else if(c13.3_95.5$Common[i] %in% plant$native == TRUE) {
    c13.3_95.5$Native[i] <- "Native"
  } else if(c13.3_95.5$Common[i] %in% plant$ground == TRUE) {
    c13.3_95.5$Native[i] <- "Ground cover"
  } else {
    c13.3_95.5$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.3_95.5 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.3_95.5 <- c13.3_95.5 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.3_95.5 <- cbind(c13.3_95.5, cover)

wide.c13.3_95.5 <- c13.3_95.5

# Pivot data from wide to long
c13.3_95.5 <- wide.c13.3_95.5
c13.3_95.5 <- c13.3_95.5 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.3_95.5 <- c13.3_95.5 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.3_95.5)) {
  if(c13.3_95.5$Year[i] == "Mar12") {
    c13.3_95.5$Year[i] <- "2012-03-01"
  } else if(c13.3_95.5$Year[i] == "Nov12") {
    c13.3_95.5$Year[i] <- "2012-11-01"
  } else if(c13.3_95.5$Year[i] == "Nov13") {
    c13.3_95.5$Year[i] <- "2013-11-01"
  } else if(c13.3_95.5$Year[i] == "Nov14") {
    c13.3_95.5$Year[i] <- "2014-11-01"
  } else if(c13.3_95.5$Year[i] == "Nov15") {
    c13.3_95.5$Year[i] <- "2015-11-01"
  } else if(c13.3_95.5$Year[i] == "Nov18") {
    c13.3_95.5$Year[i] <- "2018-11-01"
  } else {
    c13.3_95.5$Year[i] <- "2021-11-01"
  }
}

c13.3_95.5$Year <- as.Date(c13.3_95.5$Year, format = "%Y-%m-%d")

# Add channel and station
c13.3_95.5$Station <- rep("Channel 13_Station 3 + 95.5 ORD", nrow(c13.3_95.5))



# C13 4 + 14.5 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c13.4_14.5 <- raw.c13.4_14.5[-c(1:7), -c(37:43)]
colnames(c13.4_14.5) <- names.raw

c13.4_14.5 <- c13.4_14.5 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>%  
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c13.4_14.5$Common[c13.4_14.5$Common == "Annual Grass"] <- "Annual grass (year summed)"
c13.4_14.5$Common[c13.4_14.5$Common == "Feather five-finger"] <- "Feather fingergrass"
c13.4_14.5$Common[c13.4_14.5$Common == "Barnyard grama"] <- "Barnyard"

# Assign missing scientific names (when known) and correct spelling
c13.4_14.5$Scientific[c13.4_14.5$Common == "Feather fingergrass"] <- "Chloris virgata"
c13.4_14.5$Scientific[c13.4_14.5$Common == "Matweed"] <- "Guilleminea"
c13.4_14.5$Scientific[c13.4_14.5$Common == "Pepperweed"] <- "Lepidium"
c13.4_14.5$Scientific[c13.4_14.5$Scientific == "Amaranthus palmeri/fimbriatus?"] <- "Amaranthus palmeri/fimbriatus"
c13.4_14.5$Scientific[c13.4_14.5$Scientific == "Echinchloa"] <- "Echinochloa"
c13.4_14.5$Scientific[c13.4_14.5$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c13.4_14.5$Common, "Unk") == TRUE)
c13.4_14.5[26, 1] <- "Unknown euphorbia, C13"

# Assign functional groups
c13.4_14.5 <- c13.4_14.5 %>% 
  mutate(Functional = rep(NA, nrow(c13.4_14.5)))
for(i in 1:nrow(c13.4_14.5)) {
  if(c13.4_14.5$Common[i] %in% plant$per.grass == TRUE) {
    c13.4_14.5$Functional[i] <- "Perennial grass"
  } else if(c13.4_14.5$Common[i] %in% plant$an.grass == TRUE) {
    c13.4_14.5$Functional[i] <- "Annual grass"
  } else if(c13.4_14.5$Common[i] %in% plant$per.forb == TRUE) {
    c13.4_14.5$Functional[i] <- "Perennial forb"
  } else if(c13.4_14.5$Common[i] %in% plant$an.forb == TRUE) {
    c13.4_14.5$Functional[i] <- "Annual forb"
  } else if(c13.4_14.5$Common[i] %in% plant$shrub == TRUE) {
    c13.4_14.5$Functional[i] <- "Shrub"
  } else if(c13.4_14.5$Common[i] %in% plant$tree == TRUE) {
    c13.4_14.5$Functional[i] <- "Tree"
  } else if(c13.4_14.5$Common[i] %in% plant$ground == TRUE) {
    c13.4_14.5$Functional[i] <- "Ground cover"
  } else {
    c13.4_14.5$Functional[i] <- "assign unknown"
  }
}

c13.4_14.5[26, "Functional"] <- "Perennial forb"
count(c13.4_14.5, Functional)

# Assign native status
c13.4_14.5 <- c13.4_14.5 %>% 
  mutate(Native = rep(NA, nrow(c13.4_14.5)))
for(i in 1:nrow(c13.4_14.5)) {
  if(c13.4_14.5$Common[i] %in% plant$invasive == TRUE) {
    c13.4_14.5$Native[i] <- "Invasive"
  } else if(c13.4_14.5$Common[i] %in% plant$native == TRUE) {
    c13.4_14.5$Native[i] <- "Native"
  } else if(c13.4_14.5$Common[i] %in% plant$ground == TRUE) {
    c13.4_14.5$Native[i] <- "Ground cover"
  } else {
    c13.4_14.5$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.4_14.5 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.4_14.5 <- c13.4_14.5 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.4_14.5 <- cbind(c13.4_14.5, cover)

wide.c13.4_14.5 <- c13.4_14.5

# Pivot data from wide to long
c13.4_14.5 <- wide.c13.4_14.5
c13.4_14.5 <- c13.4_14.5 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.4_14.5 <- c13.4_14.5 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.4_14.5)) {
  if(c13.4_14.5$Year[i] == "Mar12") {
    c13.4_14.5$Year[i] <- "2012-03-01"
  } else if(c13.4_14.5$Year[i] == "Nov12") {
    c13.4_14.5$Year[i] <- "2012-11-01"
  } else if(c13.4_14.5$Year[i] == "Nov13") {
    c13.4_14.5$Year[i] <- "2013-11-01"
  } else if(c13.4_14.5$Year[i] == "Nov14") {
    c13.4_14.5$Year[i] <- "2014-11-01"
  } else if(c13.4_14.5$Year[i] == "Nov15") {
    c13.4_14.5$Year[i] <- "2015-11-01"
  } else if(c13.4_14.5$Year[i] == "Nov18") {
    c13.4_14.5$Year[i] <- "2018-11-01"
  } else {
    c13.4_14.5$Year[i] <- "2021-11-01"
  }
}

c13.4_14.5$Year <- as.Date(c13.4_14.5$Year, format = "%Y-%m-%d")

# Add channel and station
c13.4_14.5$Station <- rep("Channel 13_Station 4 + 14.5 BAF", nrow(c13.4_14.5))



# C13 4 + 36 --------------------------------------------------------------

# Remove header and rename columns
c13.4_36 <- raw.c13.4_36[-c(1:7), -c(37:44)]
colnames(c13.4_36) <- names.raw

# Reassign CHs with values for Nov 2012 perennial grasses
which(c13.4_36 == "CH", arr.ind = TRUE)

c13.4_36[2, 17] <- "15"
c13.4_36[2, 18] <- "31.25"
c13.4_36[8, 18] <- "31.25"
c13.4_36[2, 19] <- "12.5"
c13.4_36[3, 19] <- "12.5"
c13.4_36[8, 19] <- "12.5"
c13.4_36[3, 20] <- "7.5"
c13.4_36[8, 20] <- "7.5"

# Remove unnecessary rows
c13.4_36 <- c13.4_36 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>%  
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover")) %>% 
  filter(Common != "Mesa threeawn")  # Mesa threeawn is the same as spidergrass and row is empty

# Collapse rows of identical species into 1 row 
  # Cane beardgrass is in 2 rows, with data in both rows
  # Am manually moving/re-entering data to condense into 1 row
c13.4_36[9, 3] <- "15.0"
c13.4_36[9, 4] <- "2.5"
c13.4_36 <- c13.4_36[-11, ]

# Standardize common names
c13.4_36$Common[c13.4_36$Common == "Annual Grass"] <- "Annual grass (year summed)"
c13.4_36$Common[c13.4_36$Common == "Barnyard grama"] <- "Barnyard"

# Assign missing scientific names (when known) and correct spelling
c13.4_36$Scientific[c13.4_36$Common == "Matweed"] <- "Guilleminea"
c13.4_36$Scientific[c13.4_36$Common == "Prickly pear"] <- "Opuntia"
c13.4_36$Scientific[c13.4_36$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c13.4_36$Common, "Unk") == TRUE)
c13.4_36[24, 1] <- "Unknown euphorbia, C13"

# Assign functional groups
c13.4_36 <- c13.4_36 %>% 
  mutate(Functional = rep(NA, nrow(c13.4_36)))
for(i in 1:nrow(c13.4_36)) {
  if(c13.4_36$Common[i] %in% plant$per.grass == TRUE) {
    c13.4_36$Functional[i] <- "Perennial grass"
  } else if(c13.4_36$Common[i] %in% plant$an.grass == TRUE) {
    c13.4_36$Functional[i] <- "Annual grass"
  } else if(c13.4_36$Common[i] %in% plant$per.forb == TRUE) {
    c13.4_36$Functional[i] <- "Perennial forb"
  } else if(c13.4_36$Common[i] %in% plant$an.forb == TRUE) {
    c13.4_36$Functional[i] <- "Annual forb"
  } else if(c13.4_36$Common[i] %in% plant$shrub == TRUE) {
    c13.4_36$Functional[i] <- "Shrub"
  } else if(c13.4_36$Common[i] %in% plant$tree == TRUE) {
    c13.4_36$Functional[i] <- "Tree"
  } else if(c13.4_36$Common[i] %in% plant$ground == TRUE) {
    c13.4_36$Functional[i] <- "Ground cover"
  } else {
    c13.4_36$Functional[i] <- "assign unknown"
  }
}

c13.4_36[24, "Functional"] <- "Perennial forb"
count(c13.4_36, Functional)

# Assign native status
c13.4_36 <- c13.4_36 %>% 
  mutate(Native = rep(NA, nrow(c13.4_36)))
for(i in 1:nrow(c13.4_36)) {
  if(c13.4_36$Common[i] %in% plant$invasive == TRUE) {
    c13.4_36$Native[i] <- "Invasive"
  } else if(c13.4_36$Common[i] %in% plant$native == TRUE) {
    c13.4_36$Native[i] <- "Native"
  } else if(c13.4_36$Common[i] %in% plant$ground == TRUE) {
    c13.4_36$Native[i] <- "Ground cover"
  } else {
    c13.4_36$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.4_36 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.4_36 <- c13.4_36 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.4_36 <- cbind(c13.4_36, cover)

wide.c13.4_36 <- c13.4_36

# Pivot data from wide to long
c13.4_36 <- wide.c13.4_36
c13.4_36 <- c13.4_36 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.4_36 <- c13.4_36 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.4_36)) {
  if(c13.4_36$Year[i] == "Mar12") {
    c13.4_36$Year[i] <- "2012-03-01"
  } else if(c13.4_36$Year[i] == "Nov12") {
    c13.4_36$Year[i] <- "2012-11-01"
  } else if(c13.4_36$Year[i] == "Nov13") {
    c13.4_36$Year[i] <- "2013-11-01"
  } else if(c13.4_36$Year[i] == "Nov14") {
    c13.4_36$Year[i] <- "2014-11-01"
  } else if(c13.4_36$Year[i] == "Nov15") {
    c13.4_36$Year[i] <- "2015-11-01"
  } else if(c13.4_36$Year[i] == "Nov18") {
    c13.4_36$Year[i] <- "2018-11-01"
  } else {
    c13.4_36$Year[i] <- "2021-11-01"
  }
}

c13.4_36$Year <- as.Date(c13.4_36$Year, format = "%Y-%m-%d")

# Add channel and station
c13.4_36$Station <- rep("Channel 13_Station 4 + 36 ORD", nrow(c13.4_36))



# C13 4 + 50 --------------------------------------------------------------

# Remove header and rename columns
c13.4_50 <- raw.c13.4_50[-c(1:7), -c(37:43)]
colnames(c13.4_50) <- names.raw

# Reassign CHs with values for Nov 2012 perennial grasses
which(c13.4_50 == "CH", arr.ind = TRUE)

c13.4_50[15, 1] <- "Unknown"
c13.4_50[15, 17] <- "15"

c13.4_50[2, 18] <- "15.625"
c13.4_50[6, 18] <- "15.625"
c13.4_50[7, 18] <- "15.625"
c13.4_50[8, 18] <- "15.625"

c13.4_50[2, 19] <- "18.75"
c13.4_50[8, 19] <- "18.75"

# Add missing common names
c13.4_50$Common[c13.4_50$Scientific == "Ambrosia psylotachya"] <- "Cuman ragweed"

# Remove remove unnecessary rows
c13.4_50 <- c13.4_50 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>%
  filter(!str_detect(Common, "Perennial")) %>%
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover")) %>% 
  filter(Common != "Mesa threeawn")  # Mesa threeawn is the same as spidergrass and row is empty

# Collapse rows of identical species into 1 row 
  # Cane beardgrass is in 2 rows, with data in both rows
  # Am manually moving/re-entering data to condense into 1 row
c13.4_50[9, 4] <- "2.5"
c13.4_50 <- c13.4_50[-12, ]

# Standardize common names
c13.4_50$Common[c13.4_50$Common == "Annual Grass"] <- "Annual grass (year summed)"

# Assign missing scientific names (when known) and correct spelling
c13.4_50$Scientific[c13.4_50$Common == "Matweed"] <- "Guilleminea"
c13.4_50$Scientific[c13.4_50$Common == "Prickly pear"] <- "Opuntia"
c13.4_50$Scientific[c13.4_50$Scientific == "Ambrosia psylotachya"] <- "Ambrosia psilostachya"
c13.4_50$Scientific[c13.4_50$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c13.4_50$Scientific[c13.4_50$Scientific == "Bouteloua barbarta"] <- "Bouteloua barbata"

# Rename unknowns
which(str_detect(c13.4_50$Common, "Unk") == TRUE)
c13.4_50[12, 1] <- "Unknown perennial grass, C13"
c13.4_50[26, 1] <- "Unknown euphorbia, C13"

# Assign functional groups
c13.4_50 <- c13.4_50 %>% 
  mutate(Functional = rep(NA, nrow(c13.4_50)))
for(i in 1:nrow(c13.4_50)) {
  if(c13.4_50$Common[i] %in% plant$per.grass == TRUE) {
    c13.4_50$Functional[i] <- "Perennial grass"
  } else if(c13.4_50$Common[i] %in% plant$an.grass == TRUE) {
    c13.4_50$Functional[i] <- "Annual grass"
  } else if(c13.4_50$Common[i] %in% plant$per.forb == TRUE) {
    c13.4_50$Functional[i] <- "Perennial forb"
  } else if(c13.4_50$Common[i] %in% plant$an.forb == TRUE) {
    c13.4_50$Functional[i] <- "Annual forb"
  } else if(c13.4_50$Common[i] %in% plant$shrub == TRUE) {
    c13.4_50$Functional[i] <- "Shrub"
  } else if(c13.4_50$Common[i] %in% plant$tree == TRUE) {
    c13.4_50$Functional[i] <- "Tree"
  } else if(c13.4_50$Common[i] %in% plant$ground == TRUE) {
    c13.4_50$Functional[i] <- "Ground cover"
  } else {
    c13.4_50$Functional[i] <- "assign unknown"
  }
}

c13.4_50[12, "Functional"] <- "Perennial grass"
c13.4_50[26, "Functional"] <- "Perennial forb"
count(c13.4_50, Functional)

# Assign native status
c13.4_50 <- c13.4_50 %>% 
  mutate(Native = rep(NA, nrow(c13.4_50)))
for(i in 1:nrow(c13.4_50)) {
  if(c13.4_50$Common[i] %in% plant$invasive == TRUE) {
    c13.4_50$Native[i] <- "Invasive"
  } else if(c13.4_50$Common[i] %in% plant$native == TRUE) {
    c13.4_50$Native[i] <- "Native"
  } else if(c13.4_50$Common[i] %in% plant$ground == TRUE) {
    c13.4_50$Native[i] <- "Ground cover"
  } else {
    c13.4_50$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.4_50 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.4_50 <- c13.4_50 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.4_50 <- cbind(c13.4_50, cover)

wide.c13.4_50 <- c13.4_50

# Pivot data from wide to long
c13.4_50 <- wide.c13.4_50
c13.4_50 <- c13.4_50 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.4_50 <- c13.4_50 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.4_50)) {
  if(c13.4_50$Year[i] == "Mar12") {
    c13.4_50$Year[i] <- "2012-03-01"
  } else if(c13.4_50$Year[i] == "Nov12") {
    c13.4_50$Year[i] <- "2012-11-01"
  } else if(c13.4_50$Year[i] == "Nov13") {
    c13.4_50$Year[i] <- "2013-11-01"
  } else if(c13.4_50$Year[i] == "Nov14") {
    c13.4_50$Year[i] <- "2014-11-01"
  } else if(c13.4_50$Year[i] == "Nov15") {
    c13.4_50$Year[i] <- "2015-11-01"
  } else if(c13.4_50$Year[i] == "Nov18") {
    c13.4_50$Year[i] <- "2018-11-01"
  } else {
    c13.4_50$Year[i] <- "2021-11-01"
  }
}

c13.4_50$Year <- as.Date(c13.4_50$Year, format = "%Y-%m-%d")

# Add channel and station
c13.4_50$Station <- rep("Channel 13_Station 4 + 50 BAF", nrow(c13.4_50))



# C13 4 + 78 --------------------------------------------------------------

# Remove header, rename columns,
c13.4_78 <- raw.c13.4_78[-c(1:7), -c(37:43)]
colnames(c13.4_78) <- names.raw

# Add missing common names
c13.4_78$Common[c13.4_78$Scientific == "Eragostis curvula"] <- "Boer lovegrass"
c13.4_78$Common[c13.4_78$Scientific == "Melinis repens"] <- "Rose Natal grass"
c13.4_78$Common[c13.4_78$Scientific == "Eragostis ciliancois"] <- "Stinkgrass"

# Reassign CHs with values for Nov 2012 perennial grasses
which(c13.4_78 == "CH", arr.ind = TRUE)

c13.4_78[3, 17] <- "18.75"
c13.4_78[4, 17] <- "18.75"
c13.4_78[2, 18] <- "7.5"
c13.4_78[4, 18] <- "7.5"
c13.4_78[2, 19] <- "37.5"
c13.4_78[7, 20] <- "2.5"
c13.4_78[2, 21] <- "7.5"
c13.4_78[7, 21] <- "7.5"
c13.4_78[2, 22] <- "62.5"

# Move value in Perennial Forbs row to an Unknown row
c13.4_78[35, 1] <- "Unknown"
c13.4_78[36, 17] <- "2.5"

# Remove remove unnecessary rows
c13.4_78 <- c13.4_78 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>%  
  filter(!str_detect(Common, "Perennial")) %>%  
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover")) %>% 
  filter(Common != "Mesa threeawn")  # Mesa threeawn is the same as spidergrass and row is empty

# Reassign CHs with values for Nov 2012 perennial grasses
which(c13.4_78 == "CH", arr.ind = TRUE)

c13.4_78[13, 20] <- "0"
c13.4_78[15, 20] <- "2.5"

# Standardize common names
c13.4_78$Common[c13.4_78$Common == "Annual Grass"] <- "Annual grass (year summed)"
c13.4_78$Common[c13.4_78$Common == "Barnyard grama"] <- "Barnyard"

# Assign missing scientific names (when known) and correct spelling
c13.4_78$Scientific[c13.4_78$Common == "Barnyard"] <- "Echinochloa"
c13.4_78$Scientific[c13.4_78$Common == "Matweed"] <- "Guilleminea"
c13.4_78$Scientific[c13.4_78$Common == "Pepperweed"] <- "Lepidium"
c13.4_78$Scientific[c13.4_78$Scientific == "Eragostis curvula"] <- "Eragrostis curvula"
c13.4_78$Scientific[c13.4_78$Scientific == "Eragostis ciliancois"] <- "Eragrostis cilianensis"
c13.4_50$Scientific[c13.4_50$Scientific == "Ambrosia psilostachya"] <- "Ambrosia psilostachya"
c13.4_78$Scientific[c13.4_78$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c13.4_78$Common, "Unk") == TRUE)
c13.4_78[27, 1] <- "Unknown perennial forb, C13"

# Assign functional groups
c13.4_78 <- c13.4_78 %>% 
  mutate(Functional = rep(NA, nrow(c13.4_78)))
for(i in 1:nrow(c13.4_78)) {
  if(c13.4_78$Common[i] %in% plant$per.grass == TRUE) {
    c13.4_78$Functional[i] <- "Perennial grass"
  } else if(c13.4_78$Common[i] %in% plant$an.grass == TRUE) {
    c13.4_78$Functional[i] <- "Annual grass"
  } else if(c13.4_78$Common[i] %in% plant$per.forb == TRUE) {
    c13.4_78$Functional[i] <- "Perennial forb"
  } else if(c13.4_78$Common[i] %in% plant$an.forb == TRUE) {
    c13.4_78$Functional[i] <- "Annual forb"
  } else if(c13.4_78$Common[i] %in% plant$shrub == TRUE) {
    c13.4_78$Functional[i] <- "Shrub"
  } else if(c13.4_78$Common[i] %in% plant$tree == TRUE) {
    c13.4_78$Functional[i] <- "Tree"
  } else if(c13.4_78$Common[i] %in% plant$ground == TRUE) {
    c13.4_78$Functional[i] <- "Ground cover"
  } else {
    c13.4_78$Functional[i] <- "assign unknown"
  }
}

c13.4_78[27, "Functional"] <- "Perennial forb"
count(c13.4_78, Functional)

# Assign native status
c13.4_78 <- c13.4_78 %>% 
  mutate(Native = rep(NA, nrow(c13.4_78)))
for(i in 1:nrow(c13.4_78)) {
  if(c13.4_78$Common[i] %in% plant$invasive == TRUE) {
    c13.4_78$Native[i] <- "Invasive"
  } else if(c13.4_78$Common[i] %in% plant$native == TRUE) {
    c13.4_78$Native[i] <- "Native"
  } else if(c13.4_78$Common[i] %in% plant$ground == TRUE) {
    c13.4_78$Native[i] <- "Ground cover"
  } else {
    c13.4_78$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.05 for cover
cover <- c13.4_78 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c13.4_78 <- c13.4_78 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c13.4_78 <- cbind(c13.4_78, cover)

wide.c13.4_78 <- c13.4_78

# Pivot data from wide to long
c13.4_78 <- wide.c13.4_78
c13.4_78 <- c13.4_78 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c13.4_78 <- c13.4_78 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c13.4_78)) {
  if(c13.4_78$Year[i] == "Mar12") {
    c13.4_78$Year[i] <- "2012-03-01"
  } else if(c13.4_78$Year[i] == "Nov12") {
    c13.4_78$Year[i] <- "2012-11-01"
  } else if(c13.4_78$Year[i] == "Nov13") {
    c13.4_78$Year[i] <- "2013-11-01"
  } else if(c13.4_78$Year[i] == "Nov14") {
    c13.4_78$Year[i] <- "2014-11-01"
  } else if(c13.4_78$Year[i] == "Nov15") {
    c13.4_78$Year[i] <- "2015-11-01"
  } else if(c13.4_78$Year[i] == "Nov18") {
    c13.4_78$Year[i] <- "2018-11-01"
  } else {
    c13.4_78$Year[i] <- "2021-11-01"
  }
}

c13.4_78$Year <- as.Date(c13.4_78$Year, format = "%Y-%m-%d")

# Add channel and station
c13.4_78$Station <- rep("Channel 13_Station 4 + 78 ORD", nrow(c13.4_78))



# Combine and check for name standardization and corrections --------------

all.c13 <- rbind(c13.1_11.5, c13.1_38.5, c13.1_71, c13.1_77, c13.1_99, 
                 c13.2_16, c13.2_57, c13.2_67,
                 c13.3_09, c13.3_31, c13.3_65, c13.3_95.5,
                 c13.4_14.5, c13.4_36, c13.4_50, c13.4_78)

names.common <- all.c13 %>% 
  distinct(Common, .keep_all = TRUE) %>% 
  select(Common, Scientific, Functional, Native) %>% 
  arrange(Common)
names.scientific <- all.c13 %>% 
  distinct(Common, .keep_all = TRUE) %>% 
  select(Common, Scientific, Functional, Native) %>% 
  arrange(Scientific)

unique(filter(all.c13, Scientific == "Mullugo sp.")$Station)
unique(filter(all.c13, Common == "Mesa threeawn")$Station)

print(count(all.c13, Scientific), n = 100)
print(count(all.c13, Common), n = 100)

unique(filter(all.c13, Native == "Unknown native status")$Common)



# Save cleaned dataframes as .RData ---------------------------------------

save(all.c13,
     file = "C13 long all stations.RData")


save.image("C13 data wrangling.RData")



