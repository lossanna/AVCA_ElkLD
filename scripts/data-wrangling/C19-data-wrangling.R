# Purpose: Standardize scientific and common names, lifeform, date; address trace values; 
#   and resolve problems noted in Excel sheet in terms of other values.


library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

plant <- read_xlsx("scripts/data-wrangling/Plant-functional-groups-by-common-name.xlsx")

raw.c19.01_04 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                             sheet = "1 + 04")
raw.c19.02_04 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "2 + 04")
raw.c19.02_52 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "2 +52")
raw.c19.03_63 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "3 + 63")
raw.c19.03_98 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "3 + 98")
raw.c19.05_00 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "5 + 00")
raw.c19.05_09 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                             sheet = "5 + 09")
raw.c19.05_62 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "5 + 62")
raw.c19.06_13 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "6 + 13")
raw.c19.06_97 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "6 + 97")
raw.c19.07_30 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "7 + 30")
raw.c19.07_50 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "7 + 50")
raw.c19.07_97 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "7 + 97")
raw.c19.09_05 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "9 + 05")
raw.c19.09_26 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "9 +26")
raw.c19.09_68 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "9 + 68")
raw.c19.10_98 <- read_xlsx("data/Excel_LO_edited/AVCA ElkLD Channel 19 Data - USE FOR 2021 ANALYSIS_LO 220201.xlsx",
                           sheet = "10 + 80")

names.raw <- c("Common", "Scientific", "Nov21_2L", "Nov21_1L", "Nov21_1R", "Nov21_2R",
               "Nov18_2L", "Nov18_1L", "Nov18_1R", "Nov18_2R",
               "Mar12_3L", "Mar12_2L", "Mar12_1L", "Mar12_1R", "Mar12_2R", "Mar12_3R",
               "Nov12_3L", "Nov12_2L", "Nov12_1L", "Nov12_1R", "Nov12_2R", "Nov12_3R",
               "Nov13_3L", "Nov13_2L", "Nov13_1L", "Nov13_1R", "Nov13_2R", "Nov13_3R",
               "Nov14_2L", "Nov14_1L", "Nov14_1R", "Nov14_2R",
               "Nov15_2L", "Nov15_1L", "Nov15_1R", "Nov15_2R") 



# C19 1 + 4 ---------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c19.01_04 <- raw.c19.01_04[-c(1:7), -c(37:43)]
colnames(c19.01_04) <- names.raw

c19.01_04 <- c19.01_04 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c19.01_04$Common[c19.01_04$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c19.01_04$Common[c19.01_04$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.01_04$Common[c19.01_04$Common == "Cup grass"] <- "Cupgrass"
c19.01_04$Common[c19.01_04$Common == "Unknown aster"] <- "Annual aster"

# Assign missing scientific names (when known) and correct spelling
c19.01_04$Scientific[c19.01_04$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c19.01_04$Common, "Unk") == TRUE)
which(str_detect(c19.01_04$Common, "2021") == TRUE)
c19.01_04[18, 1] <- "Unknown annual grass, C19"
c19.01_04[24, 1] <- "Unknown annual forb, C19"

# Assign functional groups
c19.01_04 <- c19.01_04 %>% 
  mutate(Functional = rep(NA, nrow(c19.01_04)))
for(i in 1:nrow(c19.01_04)) {
  if(c19.01_04$Common[i] %in% plant$per.grass == TRUE) {
    c19.01_04$Functional[i] <- "Perennial grass"
  } else if(c19.01_04$Common[i] %in% plant$an.grass == TRUE) {
    c19.01_04$Functional[i] <- "Annual grass"
  } else if(c19.01_04$Common[i] %in% plant$per.forb == TRUE) {
    c19.01_04$Functional[i] <- "Perennial forb"
  } else if(c19.01_04$Common[i] %in% plant$an.forb == TRUE) {
    c19.01_04$Functional[i] <- "Annual forb"
  } else if(c19.01_04$Common[i] %in% plant$shrub == TRUE) {
    c19.01_04$Functional[i] <- "Shrub"
  } else if(c19.01_04$Common[i] %in% plant$tree == TRUE) {
    c19.01_04$Functional[i] <- "Tree"
  } else if(c19.01_04$Common[i] %in% plant$ground == TRUE) {
    c19.01_04$Functional[i] <- "Ground cover"
  } else {
    c19.01_04$Functional[i] <- "assign unknown"
  }
}

c19.01_04[18, "Functional"] <- "Annual grass"
c19.01_04[24, "Functional"] <- "Annual forb"
count(c19.01_04, Functional)

# Assign native status
c19.01_04 <- c19.01_04 %>% 
  mutate(Native = rep(NA, nrow(c19.01_04)))
for(i in 1:nrow(c19.01_04)) {
  if(c19.01_04$Common[i] %in% plant$invasive == TRUE) {
    c19.01_04$Native[i] <- "Invasive"
  } else if(c19.01_04$Common[i] %in% plant$native == TRUE) {
    c19.01_04$Native[i] <- "Native"
  } else if(c19.01_04$Common[i] %in% plant$ground == TRUE) {
    c19.01_04$Native[i] <- "Ground cover"
  } else {
    c19.01_04$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.01_04 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.01_04 <- c19.01_04 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.01_04 <- cbind(c19.01_04, cover)

wide.c19.01_04 <- c19.01_04

# Pivot data from wide to long
c19.01_04 <- wide.c19.01_04
c19.01_04 <- c19.01_04 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.01_04 <- c19.01_04 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.01_04)) {
  if(c19.01_04$Year[i] == "Mar12") {
    c19.01_04$Year[i] <- "2012-03-01"
  } else if(c19.01_04$Year[i] == "Nov12") {
    c19.01_04$Year[i] <- "2012-11-01"
  } else if(c19.01_04$Year[i] == "Nov13") {
    c19.01_04$Year[i] <- "2013-11-01"
  } else if(c19.01_04$Year[i] == "Nov14") {
    c19.01_04$Year[i] <- "2014-11-01"
  } else if(c19.01_04$Year[i] == "Nov15") {
    c19.01_04$Year[i] <- "2015-11-01"
  } else if(c19.01_04$Year[i] == "Nov18") {
    c19.01_04$Year[i] <- "2018-11-01"
  } else {
    c19.01_04$Year[i] <- "2021-11-01"
  }
}
c19.01_04$Year <- as.Date(c19.01_04$Year, format = "%Y-%m-%d")

# Add channel and station
c19.01_04$Station <- rep("Channel 19_Station 01 + 04", nrow(c19.01_04))



# C19 2 + 4 ---------------------------------------------------------------

# Remove header and rename columns 
c19.02_04 <- raw.c19.02_04[-c(1:7), -c(37:43)]
colnames(c19.02_04) <- names.raw

# Collapse rows of identical species into 1 row 
  # Cottsia gracilis is the same as Janusia gracilis
  # Am manually moving/re-entering data to condense into 1 row
c19.02_04[32, 8] <- "0.5"
c19.02_04 <- c19.02_04[-35, ]

# Remove unnecessary rows
c19.02_04 <- c19.02_04 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Standardize common names
c19.02_04$Common[c19.02_04$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.02_04$Common[c19.02_04$Common == "annual panic"] <- "Annual panic"
c19.02_04$Common[c19.02_04$Common == "AZ Wrightwort"] <- "AZ wrightwort"
c19.02_04$Common[c19.02_04$Common == "brown panic"] <- "Brown panic"
c19.02_04$Common[c19.02_04$Common == "Cup grass"] <- "Cupgrass"
c19.02_04$Common[c19.02_04$Common == "Rock hybiscus"] <- "Rock hibiscus"

# Assign missing scientific names (when known) and correct spelling
c19.02_04$Scientific[c19.02_04$Common == "Rock hibiscus"] <- "Hibiscus denudatus"
c19.02_04$Scientific[c19.02_04$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c19.02_04$Common, "Unk") == TRUE)
which(str_detect(c19.02_04$Common, "2021") == TRUE)
c19.02_04[18:19, 1] <- "Unknown annual grass, C19"
c19.02_04[36, 1] <- "Unknown annual forb, C19"

# Assign functional groups
c19.02_04 <- c19.02_04 %>% 
  mutate(Functional = rep(NA, nrow(c19.02_04)))
for(i in 1:nrow(c19.02_04)) {
  if(c19.02_04$Common[i] %in% plant$per.grass == TRUE) {
    c19.02_04$Functional[i] <- "Perennial grass"
  } else if(c19.02_04$Common[i] %in% plant$an.grass == TRUE) {
    c19.02_04$Functional[i] <- "Annual grass"
  } else if(c19.02_04$Common[i] %in% plant$per.forb == TRUE) {
    c19.02_04$Functional[i] <- "Perennial forb"
  } else if(c19.02_04$Common[i] %in% plant$an.forb == TRUE) {
    c19.02_04$Functional[i] <- "Annual forb"
  } else if(c19.02_04$Common[i] %in% plant$shrub == TRUE) {
    c19.02_04$Functional[i] <- "Shrub"
  } else if(c19.02_04$Common[i] %in% plant$tree == TRUE) {
    c19.02_04$Functional[i] <- "Tree"
  } else if(c19.02_04$Common[i] %in% plant$ground == TRUE) {
    c19.02_04$Functional[i] <- "Ground cover"
  } else {
    c19.02_04$Functional[i] <- "assign unknown"
  }
}

c19.02_04[18:19, "Functional"] <- "Annual grass"
c19.02_04[36, "Functional"] <- "Annual forb"
count(c19.02_04, Functional)

# Assign native status
c19.02_04 <- c19.02_04 %>% 
  mutate(Native = rep(NA, nrow(c19.02_04)))
for(i in 1:nrow(c19.02_04)) {
  if(c19.02_04$Common[i] %in% plant$invasive == TRUE) {
    c19.02_04$Native[i] <- "Invasive"
  } else if(c19.02_04$Common[i] %in% plant$native == TRUE) {
    c19.02_04$Native[i] <- "Native"
  } else if(c19.02_04$Common[i] %in% plant$ground == TRUE) {
    c19.02_04$Native[i] <- "Ground cover"
  } else {
    c19.02_04$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.02_04 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.02_04 <- c19.02_04 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.02_04 <- cbind(c19.02_04, cover)

wide.c19.02_04 <- c19.02_04

# Pivot data from wide to long
c19.02_04 <- wide.c19.02_04
c19.02_04 <- c19.02_04 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.02_04 <- c19.02_04 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.02_04)) {
  if(c19.02_04$Year[i] == "Mar12") {
    c19.02_04$Year[i] <- "2012-03-01"
  } else if(c19.02_04$Year[i] == "Nov12") {
    c19.02_04$Year[i] <- "2012-11-01"
  } else if(c19.02_04$Year[i] == "Nov13") {
    c19.02_04$Year[i] <- "2013-11-01"
  } else if(c19.02_04$Year[i] == "Nov14") {
    c19.02_04$Year[i] <- "2014-11-01"
  } else if(c19.02_04$Year[i] == "Nov15") {
    c19.02_04$Year[i] <- "2015-11-01"
  } else if(c19.02_04$Year[i] == "Nov18") {
    c19.02_04$Year[i] <- "2018-11-01"
  } else {
    c19.02_04$Year[i] <- "2021-11-01"
  }
}
c19.02_04$Year <- as.Date(c19.02_04$Year, format = "%Y-%m-%d")

# Add channel and station
c19.02_04$Station <- rep("Channel 19_Station 02 + 04", nrow(c19.02_04))



# C19 2 + 52 --------------------------------------------------------------

# Remove header and rename columns
c19.02_52 <- raw.c19.02_52[-c(1:7), -c(37:43)]
colnames(c19.02_52) <- names.raw

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # Slender grama is in two rows, with data in both
c19.02_52[12, 5] <- "15"
c19.02_52 <- c19.02_52[-14, ]
  # Cane beardgrass is in two rows, with data in both
c19.02_52[10, 5] <- "15"
c19.02_52 <- c19.02_52[-13, ]
  # Cottsia gracilis is the same as Janusia gracilis
c19.02_52[33, 8] <- "0.5"
c19.02_52 <- c19.02_52[-38, ]
  # AZ wrightwort is in two rows, with data in both
c19.02_52[57, 4] <- "2.5"
c19.02_52 <- c19.02_52[-38, ]

# Move value in Perennial Forbs row to an Unknown row
c19.02_52[40, 1] <- "Unknown"
c19.02_52[40, 11] <- "2.5"

# Remove unnecessary rows
c19.02_52 <- c19.02_52 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover")) %>% 
  filter(Common != "Anual") # this is a random row with no data

# Standardize common names
c19.02_52$Common[c19.02_52$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c19.02_52$Common[c19.02_52$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.02_52$Common[c19.02_52$Common == "AZ Wrightwort"] <- "AZ wrightwort"
c19.02_52$Common[c19.02_52$Common == "Cup grass"] <- "Cupgrass"
c19.02_52$Common[c19.02_52$Common == "lambsquarters"] <- "Lambsquarters"
c19.02_52$Common[c19.02_52$Common == "snake cotton"] <- "Snakecotton"
c19.02_52$Common[c19.02_52$Common == "spiderling"] <- "Coulter's spiderling"
c19.02_52$Common[c19.02_52$Common == "sorrel buckwheat"] <- "Sorrel buckwheat"
c19.02_52$Common[c19.02_52$Common == "Tansey mustard"] <- "Tansymustard"
c19.02_52$Common[c19.02_52$Common == "trumpetflower"] <- "Buckwheat"

# Assign missing scientific names (when known) and correct spelling
c19.02_52$Scientific[c19.02_52$Common == "Buckwheat"] <- "Eriogonum inflatum"
c19.02_52$Scientific[c19.02_52$Common == "Coulter's spiderling"] <- "Boerhavia coulteri"
c19.02_52$Scientific[c19.02_52$Common == "Snakecotton"] <- "Froelichia"
c19.02_52$Scientific[c19.02_52$Common == "Tansymustard"] <- "Descurainia pinnata"
c19.02_52$Scientific[c19.02_52$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c19.02_52$Scientific[c19.02_52$Scientific == "Carlowrightii arizonica"] <- "Carlowrightia arizonica"

# Rename unknowns
which(str_detect(c19.02_52$Common, "Unk") == TRUE)
which(str_detect(c19.02_52$Common, "2021") == TRUE)
c19.02_52[29, 1] <- "Unknown annual grass, C19"
c19.02_52[30, 1] <- "Unknown perennial forb, C19"

# Assign functional groups
c19.02_52 <- c19.02_52 %>% 
  mutate(Functional = rep(NA, nrow(c19.02_52)))
for(i in 1:nrow(c19.02_52)) {
  if(c19.02_52$Common[i] %in% plant$per.grass == TRUE) {
    c19.02_52$Functional[i] <- "Perennial grass"
  } else if(c19.02_52$Common[i] %in% plant$an.grass == TRUE) {
    c19.02_52$Functional[i] <- "Annual grass"
  } else if(c19.02_52$Common[i] %in% plant$per.forb == TRUE) {
    c19.02_52$Functional[i] <- "Perennial forb"
  } else if(c19.02_52$Common[i] %in% plant$an.forb == TRUE) {
    c19.02_52$Functional[i] <- "Annual forb"
  } else if(c19.02_52$Common[i] %in% plant$shrub == TRUE) {
    c19.02_52$Functional[i] <- "Shrub"
  } else if(c19.02_52$Common[i] %in% plant$tree == TRUE) {
    c19.02_52$Functional[i] <- "Tree"
  } else if(c19.02_52$Common[i] %in% plant$ground == TRUE) {
    c19.02_52$Functional[i] <- "Ground cover"
  } else {
    c19.02_52$Functional[i] <- "assign unknown"
  }
}

c19.02_52[29, "Functional"] <- "Annual grass"
c19.02_52[30, "Functional"] <- "Perennial forb"
count(c19.02_52, Functional)

# Assign native status
c19.02_52 <- c19.02_52 %>% 
  mutate(Native = rep(NA, nrow(c19.02_52)))
for(i in 1:nrow(c19.02_52)) {
  if(c19.02_52$Common[i] %in% plant$invasive == TRUE) {
    c19.02_52$Native[i] <- "Invasive"
  } else if(c19.02_52$Common[i] %in% plant$native == TRUE) {
    c19.02_52$Native[i] <- "Native"
  } else if(c19.02_52$Common[i] %in% plant$ground == TRUE) {
    c19.02_52$Native[i] <- "Ground cover"
  } else {
    c19.02_52$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.02_52 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.02_52 <- c19.02_52 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.02_52 <- cbind(c19.02_52, cover)

wide.c19.02_52 <- c19.02_52

# Pivot data from wide to long
c19.02_52 <- wide.c19.02_52
c19.02_52 <- c19.02_52 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.02_52 <- c19.02_52 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.02_52)) {
  if(c19.02_52$Year[i] == "Mar12") {
    c19.02_52$Year[i] <- "2012-03-01"
  } else if(c19.02_52$Year[i] == "Nov12") {
    c19.02_52$Year[i] <- "2012-11-01"
  } else if(c19.02_52$Year[i] == "Nov13") {
    c19.02_52$Year[i] <- "2013-11-01"
  } else if(c19.02_52$Year[i] == "Nov14") {
    c19.02_52$Year[i] <- "2014-11-01"
  } else if(c19.02_52$Year[i] == "Nov15") {
    c19.02_52$Year[i] <- "2015-11-01"
  } else if(c19.02_52$Year[i] == "Nov18") {
    c19.02_52$Year[i] <- "2018-11-01"
  } else {
    c19.02_52$Year[i] <- "2021-11-01"
  }
}
c19.02_52$Year <- as.Date(c19.02_52$Year, format = "%Y-%m-%d")

# Add channel and station
c19.02_52$Station <- rep("Channel 19_Station 02 + 52", nrow(c19.02_52))



# C19 3 + 63 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c19.03_63 <- raw.c19.03_63[-c(1:7), -c(37:43)]
colnames(c19.03_63) <- names.raw

c19.03_63 <- c19.03_63 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Reassign CHs with values for Nov 2012 annual grasses
which(c19.03_63 == "CH", arr.ind = TRUE)

c19.03_63[12, 17] <- "0"
c19.03_63[17, 17] <- "5"
c19.03_63[19, 17] <- "5"
c19.03_63[20, 17] <- "5"

# Correct common names based on scientific name
c19.03_63$Common[c19.03_63$Scientific == "Eriogonum fasciculatum"] <- "Eastern Mojave buckwheat"
c19.03_63$Common[c19.03_63$Scientific == "Eriogonum polycladon"] <- "Sorrel buckwheat"

# Standardize common names
c19.03_63$Common[c19.03_63$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c19.03_63$Common[c19.03_63$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.03_63$Common[c19.03_63$Common == "AZ Wrightwort"] <- "AZ wrightwort"
c19.03_63$Common[c19.03_63$Common == "Cup grass"] <- "Cupgrass"
c19.03_63$Common[c19.03_63$Common == "trumpetflower"] <- "Buckwheat"

# Assign missing scientific names (when known) and correct spelling
c19.03_63$Scientific[c19.03_63$Common == "Pepperweed"] <- "Lepidium"
c19.03_63$Scientific[c19.03_63$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c19.03_63$Scientific[c19.03_63$Scientific == "chenopodcezemisc"] <- NA

# Rename unknowns
which(str_detect(c19.03_63$Common, "Unk") == TRUE)
which(str_detect(c19.03_63$Common, "2021") == TRUE)
which(c19.03_63$Common == "Annual forb") # this had to be corrected to just "unknown" because it has a weird scientific name
c19.03_63[18, 1] <- "Unknown annual grass, C19"
c19.03_63[26, 1] <- "Unknown perennial forb, C19"
c19.03_63[32, 1] <- "Unknown annual forb, C19"

# Assign functional groups
c19.03_63 <- c19.03_63 %>% 
  mutate(Functional = rep(NA, nrow(c19.03_63)))
for(i in 1:nrow(c19.03_63)) {
  if(c19.03_63$Common[i] %in% plant$per.grass == TRUE) {
    c19.03_63$Functional[i] <- "Perennial grass"
  } else if(c19.03_63$Common[i] %in% plant$an.grass == TRUE) {
    c19.03_63$Functional[i] <- "Annual grass"
  } else if(c19.03_63$Common[i] %in% plant$per.forb == TRUE) {
    c19.03_63$Functional[i] <- "Perennial forb"
  } else if(c19.03_63$Common[i] %in% plant$an.forb == TRUE) {
    c19.03_63$Functional[i] <- "Annual forb"
  } else if(c19.03_63$Common[i] %in% plant$shrub == TRUE) {
    c19.03_63$Functional[i] <- "Shrub"
  } else if(c19.03_63$Common[i] %in% plant$tree == TRUE) {
    c19.03_63$Functional[i] <- "Tree"
  } else if(c19.03_63$Common[i] %in% plant$ground == TRUE) {
    c19.03_63$Functional[i] <- "Ground cover"
  } else {
    c19.03_63$Functional[i] <- "assign unknown"
  }
}

c19.03_63[18, "Functional"] <- "Annual grass"
c19.03_63[26, "Functional"] <- "Perennial forb"
c19.03_63[32, "Functional"] <- "Annual forb"
count(c19.03_63, Functional)

# Assign native status
c19.03_63 <- c19.03_63 %>% 
  mutate(Native = rep(NA, nrow(c19.03_63)))
for(i in 1:nrow(c19.03_63)) {
  if(c19.03_63$Common[i] %in% plant$invasive == TRUE) {
    c19.03_63$Native[i] <- "Invasive"
  } else if(c19.03_63$Common[i] %in% plant$native == TRUE) {
    c19.03_63$Native[i] <- "Native"
  } else if(c19.03_63$Common[i] %in% plant$ground == TRUE) {
    c19.03_63$Native[i] <- "Ground cover"
  } else {
    c19.03_63$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.03_63 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.03_63 <- c19.03_63 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.03_63 <- cbind(c19.03_63, cover)

wide.c19.03_63 <- c19.03_63

# Pivot data from wide to long
c19.03_63 <- wide.c19.03_63
c19.03_63 <- c19.03_63 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.03_63 <- c19.03_63 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.03_63)) {
  if(c19.03_63$Year[i] == "Mar12") {
    c19.03_63$Year[i] <- "2012-03-01"
  } else if(c19.03_63$Year[i] == "Nov12") {
    c19.03_63$Year[i] <- "2012-11-01"
  } else if(c19.03_63$Year[i] == "Nov13") {
    c19.03_63$Year[i] <- "2013-11-01"
  } else if(c19.03_63$Year[i] == "Nov14") {
    c19.03_63$Year[i] <- "2014-11-01"
  } else if(c19.03_63$Year[i] == "Nov15") {
    c19.03_63$Year[i] <- "2015-11-01"
  } else if(c19.03_63$Year[i] == "Nov18") {
    c19.03_63$Year[i] <- "2018-11-01"
  } else {
    c19.03_63$Year[i] <- "2021-11-01"
  }
}
c19.03_63$Year <- as.Date(c19.03_63$Year, format = "%Y-%m-%d")

# Add channel and station
c19.03_63$Station <- rep("Channel 19_Station 03 + 63", nrow(c19.03_63))



# C19 3 + 98 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c19.03_98 <- raw.c19.03_98[-c(1:7), -c(37:43)]
colnames(c19.03_98) <- names.raw

c19.03_98 <- c19.03_98 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row 
  # Prickly pear has two rows, with data in both rows
  # Am manually moving/re-entering data to condense into 1 row
c19.03_98[41, 3] <- "15"
c19.03_98 <- c19.03_98[-43, ]

# Standardize common names
c19.03_98$Common[c19.03_98$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c19.03_98$Common[c19.03_98$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.03_98$Common[c19.03_98$Common == "AZ Wrightwort"] <- "AZ wrightwort"
c19.03_98$Common[c19.03_98$Common == "Cup grass"] <- "Cupgrass"
c19.03_98$Common[c19.03_98$Common == "trumpetflower"] <- "Buckwheat"

# Assign missing scientific names (when known) and correct spelling
c19.03_98$Scientific[c19.03_98$Common == "Pepperweed"] <- "Lepidium"
c19.03_98$Scientific[c19.03_98$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c19.03_98$Scientific[c19.03_98$Scientific == "Bouteloua chondrosoides"] <- "Bouteloua chondrosioides"

# Rename unknowns
which(str_detect(c19.03_98$Common, "Unk") == TRUE)
which(str_detect(c19.03_98$Common, "2021") == TRUE)
c19.03_98[21, 1] <- "Unknown annual grass, C19"
c19.03_98[27, 1] <- "Unknown perennial forb, C19"
c19.03_98 <- c19.03_98[-20, ] # remove random unknown row with no data

# Assign functional groups
c19.03_98 <- c19.03_98 %>% 
  mutate(Functional = rep(NA, nrow(c19.03_98)))
for(i in 1:nrow(c19.03_98)) {
  if(c19.03_98$Common[i] %in% plant$per.grass == TRUE) {
    c19.03_98$Functional[i] <- "Perennial grass"
  } else if(c19.03_98$Common[i] %in% plant$an.grass == TRUE) {
    c19.03_98$Functional[i] <- "Annual grass"
  } else if(c19.03_98$Common[i] %in% plant$per.forb == TRUE) {
    c19.03_98$Functional[i] <- "Perennial forb"
  } else if(c19.03_98$Common[i] %in% plant$an.forb == TRUE) {
    c19.03_98$Functional[i] <- "Annual forb"
  } else if(c19.03_98$Common[i] %in% plant$shrub == TRUE) {
    c19.03_98$Functional[i] <- "Shrub"
  } else if(c19.03_98$Common[i] %in% plant$tree == TRUE) {
    c19.03_98$Functional[i] <- "Tree"
  } else if(c19.03_98$Common[i] %in% plant$ground == TRUE) {
    c19.03_98$Functional[i] <- "Ground cover"
  } else {
    c19.03_98$Functional[i] <- "assign unknown"
  }
}

which(str_detect(c19.03_98$Common, "Unk") == TRUE) # rows have changed since random blank one was removed
c19.03_98[20, "Functional"] <- "Annual grass"
c19.03_98[26, "Functional"] <- "Perennial forb"
count(c19.03_98, Functional)

# Assign native status
c19.03_98 <- c19.03_98 %>% 
  mutate(Native = rep(NA, nrow(c19.03_98)))
for(i in 1:nrow(c19.03_98)) {
  if(c19.03_98$Common[i] %in% plant$invasive == TRUE) {
    c19.03_98$Native[i] <- "Invasive"
  } else if(c19.03_98$Common[i] %in% plant$native == TRUE) {
    c19.03_98$Native[i] <- "Native"
  } else if(c19.03_98$Common[i] %in% plant$ground == TRUE) {
    c19.03_98$Native[i] <- "Ground cover"
  } else {
    c19.03_98$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.03_98 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.03_98 <- c19.03_98 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.03_98 <- cbind(c19.03_98, cover)

wide.c19.03_98 <- c19.03_98

# Pivot data from wide to long
c19.03_98 <- wide.c19.03_98
c19.03_98 <- c19.03_98 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.03_98 <- c19.03_98 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.03_98)) {
  if(c19.03_98$Year[i] == "Mar12") {
    c19.03_98$Year[i] <- "2012-03-01"
  } else if(c19.03_98$Year[i] == "Nov12") {
    c19.03_98$Year[i] <- "2012-11-01"
  } else if(c19.03_98$Year[i] == "Nov13") {
    c19.03_98$Year[i] <- "2013-11-01"
  } else if(c19.03_98$Year[i] == "Nov14") {
    c19.03_98$Year[i] <- "2014-11-01"
  } else if(c19.03_98$Year[i] == "Nov15") {
    c19.03_98$Year[i] <- "2015-11-01"
  } else if(c19.03_98$Year[i] == "Nov18") {
    c19.03_98$Year[i] <- "2018-11-01"
  } else {
    c19.03_98$Year[i] <- "2021-11-01"
  }
}
c19.03_98$Year <- as.Date(c19.03_98$Year, format = "%Y-%m-%d")

# Add channel and station
c19.03_98$Station <- rep("Channel 19_Station 03 + 98", nrow(c19.03_98))



# C19 5 + 00 --------------------------------------------------------------

# Remove header and rename columns
c19.05_00 <- raw.c19.05_00[-c(1:7), -c(37:43)]
colnames(c19.05_00) <- names.raw

# Move value in Perennial Forbs row to an Unknown row
c19.05_00[36, 1] <- "Unknown"
c19.05_00[36, 25] <- "2.5"
c19.05_00[36, 32] <- "0.5"

# Remove unnecessary rows
c19.05_00 <- c19.05_00 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row 
  # "Unknown" under annual forbs and "Annual forb 2021" should be combined into a single unknown
  # Am manually moving/re-entering data to condense into 1 row
c19.05_00[33, 3] <- "15"
c19.05_00[33, 4] <- "2.5"
c19.05_00 <- c19.05_00[-34, ]

# Correct common names based on scientific name
c19.05_00$Common[c19.05_00$Scientific == "Eriogonum"] <- "Annual buckwheat"
c19.05_00$Common[c19.05_00$Scientific == "Eriogonum fasciculatum"] <- "Eastern Mojave buckwheat"

# Standardize common names
c19.05_00$Common[c19.05_00$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c19.05_00$Common[c19.05_00$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.05_00$Common[c19.05_00$Common == "AZ Wrightwort"] <- "AZ wrightwort"
c19.05_00$Common[c19.05_00$Common == "Cup grass"] <- "Cupgrass"
c19.05_00$Common[c19.05_00$Common == "Seniorita Threeawn"] <- "Senorita threeawn"
c19.05_00$Common[c19.05_00$Common == "spiderling"] <- "Coulter's spiderling"
c19.05_00$Common[c19.05_00$Common == "trumpetflower"] <- "Buckwheat"

# Assign missing scientific names (when known) and correct spelling
c19.05_00$Scientific[c19.05_00$Common == "Coulter's spiderling"] <- "Boerhavia coulteri"
c19.05_00$Scientific[c19.05_00$Common == "Senorita threeawn"] <- "Aristida"
c19.05_00$Scientific[c19.05_00$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c19.05_00$Common, "Unk") == TRUE)
which(str_detect(c19.05_00$Common, "2021") == TRUE)
c19.05_00[21, 1] <- "Unknown annual grass, C19"
c19.05_00[28, 1] <- "Unknown perennial forb, C19"
c19.05_00[33, 1] <- "Unknown annual forb, C19"

# Assign functional groups
c19.05_00 <- c19.05_00 %>% 
  mutate(Functional = rep(NA, nrow(c19.05_00)))
for(i in 1:nrow(c19.05_00)) {
  if(c19.05_00$Common[i] %in% plant$per.grass == TRUE) {
    c19.05_00$Functional[i] <- "Perennial grass"
  } else if(c19.05_00$Common[i] %in% plant$an.grass == TRUE) {
    c19.05_00$Functional[i] <- "Annual grass"
  } else if(c19.05_00$Common[i] %in% plant$per.forb == TRUE) {
    c19.05_00$Functional[i] <- "Perennial forb"
  } else if(c19.05_00$Common[i] %in% plant$an.forb == TRUE) {
    c19.05_00$Functional[i] <- "Annual forb"
  } else if(c19.05_00$Common[i] %in% plant$shrub == TRUE) {
    c19.05_00$Functional[i] <- "Shrub"
  } else if(c19.05_00$Common[i] %in% plant$tree == TRUE) {
    c19.05_00$Functional[i] <- "Tree"
  } else if(c19.05_00$Common[i] %in% plant$ground == TRUE) {
    c19.05_00$Functional[i] <- "Ground cover"
  } else {
    c19.05_00$Functional[i] <- "assign unknown"
  }
}

c19.05_00[21, "Functional"] <- "Annual grass"
c19.05_00[28, "Functional"] <- "Perennial forb"
c19.05_00[33, "Functional"] <- "Annual forb"
count(c19.05_00, Functional)

# Assign native status
c19.05_00 <- c19.05_00 %>% 
  mutate(Native = rep(NA, nrow(c19.05_00)))
for(i in 1:nrow(c19.05_00)) {
  if(c19.05_00$Common[i] %in% plant$invasive == TRUE) {
    c19.05_00$Native[i] <- "Invasive"
  } else if(c19.05_00$Common[i] %in% plant$native == TRUE) {
    c19.05_00$Native[i] <- "Native"
  } else if(c19.05_00$Common[i] %in% plant$ground == TRUE) {
    c19.05_00$Native[i] <- "Ground cover"
  } else {
    c19.05_00$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.05_00 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.05_00 <- c19.05_00 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.05_00 <- cbind(c19.05_00, cover)

wide.c19.05_00 <- c19.05_00

# Pivot data from wide to long
c19.05_00 <- wide.c19.05_00
c19.05_00 <- c19.05_00 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.05_00 <- c19.05_00 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.05_00)) {
  if(c19.05_00$Year[i] == "Mar12") {
    c19.05_00$Year[i] <- "2012-03-01"
  } else if(c19.05_00$Year[i] == "Nov12") {
    c19.05_00$Year[i] <- "2012-11-01"
  } else if(c19.05_00$Year[i] == "Nov13") {
    c19.05_00$Year[i] <- "2013-11-01"
  } else if(c19.05_00$Year[i] == "Nov14") {
    c19.05_00$Year[i] <- "2014-11-01"
  } else if(c19.05_00$Year[i] == "Nov15") {
    c19.05_00$Year[i] <- "2015-11-01"
  } else if(c19.05_00$Year[i] == "Nov18") {
    c19.05_00$Year[i] <- "2018-11-01"
  } else {
    c19.05_00$Year[i] <- "2021-11-01"
  }
}
c19.05_00$Year <- as.Date(c19.05_00$Year, format = "%Y-%m-%d")

# Add channel and station
c19.05_00$Station <- rep("Channel 19_Station 05 + 00", nrow(c19.05_00))



# C19 5 + 09 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c19.05_09 <- raw.c19.05_09[-c(1:7), -c(37:43)]
colnames(c19.05_09) <- names.raw

c19.05_09 <- c19.05_09 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Correct common names based on scientific name
c19.05_09$Common[c19.05_09$Scientific == "Eriogonum fasciculatum"] <- "Eastern Mojave buckwheat"

# Standardize common names
c19.05_09$Common[c19.05_09$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c19.05_09$Common[c19.05_09$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.05_09$Common[c19.05_09$Common == "AZ Wrightwort"] <- "AZ wrightwort"

# Assign missing scientific names (when known) and correct spelling
c19.05_09$Scientific[c19.05_09$Common == "Brittlebush"] <- "Encelia farinosa"
c19.05_09$Scientific[c19.05_09$Common == "Buckwheat"] <- "Eriogonum inflatum"
c19.05_09$Scientific[c19.05_09$Common == "Pepperweed"] <- "Lepidium"
c19.05_09$Scientific[c19.05_09$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c19.05_09$Common, "Unk") == TRUE)
which(str_detect(c19.05_09$Common, "2021") == TRUE)
c19.05_09[22, 1] <- "Unknown annual grass, C19"
c19.05_09[32, 1] <- "Unknown annual forb, C19"

# Assign functional groups
c19.05_09 <- c19.05_09 %>% 
  mutate(Functional = rep(NA, nrow(c19.05_09)))
for(i in 1:nrow(c19.05_09)) {
  if(c19.05_09$Common[i] %in% plant$per.grass == TRUE) {
    c19.05_09$Functional[i] <- "Perennial grass"
  } else if(c19.05_09$Common[i] %in% plant$an.grass == TRUE) {
    c19.05_09$Functional[i] <- "Annual grass"
  } else if(c19.05_09$Common[i] %in% plant$per.forb == TRUE) {
    c19.05_09$Functional[i] <- "Perennial forb"
  } else if(c19.05_09$Common[i] %in% plant$an.forb == TRUE) {
    c19.05_09$Functional[i] <- "Annual forb"
  } else if(c19.05_09$Common[i] %in% plant$shrub == TRUE) {
    c19.05_09$Functional[i] <- "Shrub"
  } else if(c19.05_09$Common[i] %in% plant$tree == TRUE) {
    c19.05_09$Functional[i] <- "Tree"
  } else if(c19.05_09$Common[i] %in% plant$ground == TRUE) {
    c19.05_09$Functional[i] <- "Ground cover"
  } else {
    c19.05_09$Functional[i] <- "assign unknown"
  }
}

c19.05_09[22, "Functional"] <- "Annual grass"
c19.05_09[32, "Functional"] <- "Annual forb"
count(c19.05_09, Functional)

# Assign native status
c19.05_09 <- c19.05_09 %>% 
  mutate(Native = rep(NA, nrow(c19.05_09)))
for(i in 1:nrow(c19.05_09)) {
  if(c19.05_09$Common[i] %in% plant$invasive == TRUE) {
    c19.05_09$Native[i] <- "Invasive"
  } else if(c19.05_09$Common[i] %in% plant$native == TRUE) {
    c19.05_09$Native[i] <- "Native"
  } else if(c19.05_09$Common[i] %in% plant$ground == TRUE) {
    c19.05_09$Native[i] <- "Ground cover"
  } else {
    c19.05_09$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.05_09 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.05_09 <- c19.05_09 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.05_09 <- cbind(c19.05_09, cover)

wide.c19.05_09 <- c19.05_09

# Pivot data from wide to long
c19.05_09 <- wide.c19.05_09
c19.05_09 <- c19.05_09 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.05_09 <- c19.05_09 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.05_09)) {
  if(c19.05_09$Year[i] == "Mar12") {
    c19.05_09$Year[i] <- "2012-03-01"
  } else if(c19.05_09$Year[i] == "Nov12") {
    c19.05_09$Year[i] <- "2012-11-01"
  } else if(c19.05_09$Year[i] == "Nov13") {
    c19.05_09$Year[i] <- "2013-11-01"
  } else if(c19.05_09$Year[i] == "Nov14") {
    c19.05_09$Year[i] <- "2014-11-01"
  } else if(c19.05_09$Year[i] == "Nov15") {
    c19.05_09$Year[i] <- "2015-11-01"
  } else if(c19.05_09$Year[i] == "Nov18") {
    c19.05_09$Year[i] <- "2018-11-01"
  } else {
    c19.05_09$Year[i] <- "2021-11-01"
  }
}
c19.05_09$Year <- as.Date(c19.05_09$Year, format = "%Y-%m-%d")

# Add channel and station
c19.05_09$Station <- rep("Channel 19_Station 05 + 09", nrow(c19.05_09))



# C19 5 + 62 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c19.05_62 <- raw.c19.05_62[-c(1:7), -c(37:43)]
colnames(c19.05_62) <- names.raw

c19.05_62 <- c19.05_62 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # "Unknown 2018" and "Unknown annual grass 2021" can be combined to single unknown
c19.05_62[20, 8] <- "0.5"
c19.05_62 <- c19.05_62[-19, ]
  # Buckwheat/trumpetflower is in two rows, with data in both
c19.05_62[25, 10] <- "2.5"
c19.05_62 <- c19.05_62[-27, ]
  # "Unknown" and "Unknown annual forb 2021" can be combined to single unknown
c19.05_62[32, 7] <- "2.5"
c19.05_62[32, 8] <- "0.5"
c19.05_62 <- c19.05_62[-31, ]

# Correct common names based on scientific name
c19.05_62$Common[c19.05_62$Scientific == "Eriogonum"] <- "Annual buckwheat"
c19.05_62$Common[c19.05_62$Scientific == "Eriogonum fasciculatum"] <- "Eastern Mojave buckwheat"

# Standardize common names
c19.05_62$Common[c19.05_62$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c19.05_62$Common[c19.05_62$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.05_62$Common[c19.05_62$Common == "AZ Wrightwort"] <- "AZ wrightwort"
c19.05_62$Common[c19.05_62$Common == "buckwheat trumpetflower"] <- "Buckwheat"
c19.05_62$Common[c19.05_62$Common == "cholla"] <- "Cholla"
c19.05_62$Common[c19.05_62$Common == "spiderling"] <- "Coulter's spiderling"

# Assign missing scientific names (when known) and correct spelling
c19.05_62$Scientific[c19.05_62$Common == "Coulter's spiderling"] <- "Boerhavia coulteri"
c19.05_62$Scientific[c19.05_62$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c19.05_62$Scientific[c19.05_62$Scientific == "Chloris vigata"] <- "Chloris virgata"

# Rename unknowns
which(str_detect(c19.05_62$Common, "Unk") == TRUE)
which(str_detect(c19.05_62$Common, "2021") == TRUE)
c19.05_62[19, 1] <- "Unknown annual grass, C19"
c19.05_62[31, 1] <- "Unknown annual forb, C19"

# Assign functional groups
c19.05_62 <- c19.05_62 %>% 
  mutate(Functional = rep(NA, nrow(c19.05_62)))
for(i in 1:nrow(c19.05_62)) {
  if(c19.05_62$Common[i] %in% plant$per.grass == TRUE) {
    c19.05_62$Functional[i] <- "Perennial grass"
  } else if(c19.05_62$Common[i] %in% plant$an.grass == TRUE) {
    c19.05_62$Functional[i] <- "Annual grass"
  } else if(c19.05_62$Common[i] %in% plant$per.forb == TRUE) {
    c19.05_62$Functional[i] <- "Perennial forb"
  } else if(c19.05_62$Common[i] %in% plant$an.forb == TRUE) {
    c19.05_62$Functional[i] <- "Annual forb"
  } else if(c19.05_62$Common[i] %in% plant$shrub == TRUE) {
    c19.05_62$Functional[i] <- "Shrub"
  } else if(c19.05_62$Common[i] %in% plant$tree == TRUE) {
    c19.05_62$Functional[i] <- "Tree"
  } else if(c19.05_62$Common[i] %in% plant$ground == TRUE) {
    c19.05_62$Functional[i] <- "Ground cover"
  } else {
    c19.05_62$Functional[i] <- "assign unknown"
  }
}

c19.05_62[19, "Functional"] <- "Annual grass"
c19.05_62[31, "Functional"] <- "Annual forb"
count(c19.05_62, Functional)

# Assign native status
c19.05_62 <- c19.05_62 %>% 
  mutate(Native = rep(NA, nrow(c19.05_62)))
for(i in 1:nrow(c19.05_62)) {
  if(c19.05_62$Common[i] %in% plant$invasive == TRUE) {
    c19.05_62$Native[i] <- "Invasive"
  } else if(c19.05_62$Common[i] %in% plant$native == TRUE) {
    c19.05_62$Native[i] <- "Native"
  } else if(c19.05_62$Common[i] %in% plant$ground == TRUE) {
    c19.05_62$Native[i] <- "Ground cover"
  } else {
    c19.05_62$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.05_62 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.05_62 <- c19.05_62 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.05_62 <- cbind(c19.05_62, cover)

wide.c19.05_62 <- c19.05_62

# Pivot data from wide to long
c19.05_62 <- wide.c19.05_62
c19.05_62 <- c19.05_62 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.05_62 <- c19.05_62 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.05_62)) {
  if(c19.05_62$Year[i] == "Mar12") {
    c19.05_62$Year[i] <- "2012-03-01"
  } else if(c19.05_62$Year[i] == "Nov12") {
    c19.05_62$Year[i] <- "2012-11-01"
  } else if(c19.05_62$Year[i] == "Nov13") {
    c19.05_62$Year[i] <- "2013-11-01"
  } else if(c19.05_62$Year[i] == "Nov14") {
    c19.05_62$Year[i] <- "2014-11-01"
  } else if(c19.05_62$Year[i] == "Nov15") {
    c19.05_62$Year[i] <- "2015-11-01"
  } else if(c19.05_62$Year[i] == "Nov18") {
    c19.05_62$Year[i] <- "2018-11-01"
  } else {
    c19.05_62$Year[i] <- "2021-11-01"
  }
}
c19.05_62$Year <- as.Date(c19.05_62$Year, format = "%Y-%m-%d")

# Add channel and station
c19.05_62$Station <- rep("Channel 19_Station 05 + 62", nrow(c19.05_62))



# C19 6 + 13 --------------------------------------------------------------

# Remove header, rename columns, 
c19.06_13 <- raw.c19.06_13[-c(1:7), -c(37:43)]
colnames(c19.06_13) <- names.raw

# Add missing common names
c19.06_13$Common[c19.06_13$Scientific == "Euphorbia"] <- "Spurge"

# Remove unnecessary rows
c19.06_13 <- c19.06_13 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # "Unknown" and "Annual grass 2021" can be combined to single unknown
c19.06_13[20, 7] <- "2.5"
c19.06_13 <- c19.06_13[-19, ]
  # Mesa threeawn is the same as spidergrass, with data in both rows
c19.06_13[3, 33] <- "2.5"
c19.06_13 <- c19.06_13[-11, ]

# Correct common names based on scientific name
c19.06_13$Common[c19.06_13$Scientific == "Eriogonum fasciculatum"] <- "Eastern Mojave buckwheat"

# Standardize common names
c19.06_13$Common[c19.06_13$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c19.06_13$Common[c19.06_13$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.06_13$Common[c19.06_13$Common == "AZ Wrightwort"] <- "AZ wrightwort"
c19.06_13$Common[c19.06_13$Common == "Buckwheat/Trumpetflower"] <- "Buckwheat"

# Assign missing scientific names (when known) and correct spelling
c19.06_13$Scientific[c19.06_13$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c19.06_13$Scientific[c19.06_13$Scientific == "Tilinum"] <- "Talinum"

# Rename unknowns
which(str_detect(c19.06_13$Common, "Unk") == TRUE)
which(str_detect(c19.06_13$Common, "2021") == TRUE)
c19.06_13[18, 1] <- "Unknown annual grass, C19"
c19.06_13[29, 1] <- "Unknown annual forb, C19"

# Assign functional groups
c19.06_13 <- c19.06_13 %>% 
  mutate(Functional = rep(NA, nrow(c19.06_13)))
for(i in 1:nrow(c19.06_13)) {
  if(c19.06_13$Common[i] %in% plant$per.grass == TRUE) {
    c19.06_13$Functional[i] <- "Perennial grass"
  } else if(c19.06_13$Common[i] %in% plant$an.grass == TRUE) {
    c19.06_13$Functional[i] <- "Annual grass"
  } else if(c19.06_13$Common[i] %in% plant$per.forb == TRUE) {
    c19.06_13$Functional[i] <- "Perennial forb"
  } else if(c19.06_13$Common[i] %in% plant$an.forb == TRUE) {
    c19.06_13$Functional[i] <- "Annual forb"
  } else if(c19.06_13$Common[i] %in% plant$shrub == TRUE) {
    c19.06_13$Functional[i] <- "Shrub"
  } else if(c19.06_13$Common[i] %in% plant$tree == TRUE) {
    c19.06_13$Functional[i] <- "Tree"
  } else if(c19.06_13$Common[i] %in% plant$ground == TRUE) {
    c19.06_13$Functional[i] <- "Ground cover"
  } else {
    c19.06_13$Functional[i] <- "assign unknown"
  }
}

c19.06_13[18, "Functional"] <- "Annual grass"
c19.06_13[29, "Functional"] <- "Annual forb"
count(c19.06_13, Functional)

# Assign native status
c19.06_13 <- c19.06_13 %>% 
  mutate(Native = rep(NA, nrow(c19.06_13)))
for(i in 1:nrow(c19.06_13)) {
  if(c19.06_13$Common[i] %in% plant$invasive == TRUE) {
    c19.06_13$Native[i] <- "Invasive"
  } else if(c19.06_13$Common[i] %in% plant$native == TRUE) {
    c19.06_13$Native[i] <- "Native"
  } else if(c19.06_13$Common[i] %in% plant$ground == TRUE) {
    c19.06_13$Native[i] <- "Ground cover"
  } else {
    c19.06_13$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.06_13 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.06_13 <- c19.06_13 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.06_13 <- cbind(c19.06_13, cover)

wide.c19.06_13 <- c19.06_13

# Pivot data from wide to long
c19.06_13 <- wide.c19.06_13
c19.06_13 <- c19.06_13 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.06_13 <- c19.06_13 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.06_13)) {
  if(c19.06_13$Year[i] == "Mar12") {
    c19.06_13$Year[i] <- "2012-03-01"
  } else if(c19.06_13$Year[i] == "Nov12") {
    c19.06_13$Year[i] <- "2012-11-01"
  } else if(c19.06_13$Year[i] == "Nov13") {
    c19.06_13$Year[i] <- "2013-11-01"
  } else if(c19.06_13$Year[i] == "Nov14") {
    c19.06_13$Year[i] <- "2014-11-01"
  } else if(c19.06_13$Year[i] == "Nov15") {
    c19.06_13$Year[i] <- "2015-11-01"
  } else if(c19.06_13$Year[i] == "Nov18") {
    c19.06_13$Year[i] <- "2018-11-01"
  } else {
    c19.06_13$Year[i] <- "2021-11-01"
  }
}
c19.06_13$Year <- as.Date(c19.06_13$Year, format = "%Y-%m-%d")

# Add channel and station
c19.06_13$Station <- rep("Channel 19_Station 06 + 13", nrow(c19.06_13))



# C19 6 + 97 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c19.06_97 <- raw.c19.06_97[-c(1:7), -c(37:43)]
colnames(c19.06_97) <- names.raw

c19.06_97 <- c19.06_97 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # Mesa threeawn is the same as spidergrass, with data in both rows
c19.06_97[3, 22] <- "37.5"
c19.06_97 <- c19.06_97[-11, ]

# Correct common names based on scientific name
c19.06_97$Common[c19.06_97$Scientific == "Eriogonum fasciculatum"] <- "Eastern Mojave buckwheat"

# Standardize common names
c19.06_97$Common[c19.06_97$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.06_97$Common[c19.06_97$Common == "annual panic"] <- "Annual panic"
c19.06_97$Common[c19.06_97$Common == "AZ Wrightwort"] <- "AZ wrightwort"

# Assign missing scientific names (when known) and correct spelling
c19.06_97$Scientific[c19.06_97$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c19.06_97$Common, "Unk") == TRUE)
which(str_detect(c19.06_97$Common, "2021") == TRUE)
c19.06_97[17, 1] <- "Unknown annual grass, C19"
c19.06_97[24, 1] <- "Unknown annual forb, C19"

# Assign functional groups
c19.06_97 <- c19.06_97 %>% 
  mutate(Functional = rep(NA, nrow(c19.06_97)))
for(i in 1:nrow(c19.06_97)) {
  if(c19.06_97$Common[i] %in% plant$per.grass == TRUE) {
    c19.06_97$Functional[i] <- "Perennial grass"
  } else if(c19.06_97$Common[i] %in% plant$an.grass == TRUE) {
    c19.06_97$Functional[i] <- "Annual grass"
  } else if(c19.06_97$Common[i] %in% plant$per.forb == TRUE) {
    c19.06_97$Functional[i] <- "Perennial forb"
  } else if(c19.06_97$Common[i] %in% plant$an.forb == TRUE) {
    c19.06_97$Functional[i] <- "Annual forb"
  } else if(c19.06_97$Common[i] %in% plant$shrub == TRUE) {
    c19.06_97$Functional[i] <- "Shrub"
  } else if(c19.06_97$Common[i] %in% plant$tree == TRUE) {
    c19.06_97$Functional[i] <- "Tree"
  } else if(c19.06_97$Common[i] %in% plant$ground == TRUE) {
    c19.06_97$Functional[i] <- "Ground cover"
  } else {
    c19.06_97$Functional[i] <- "assign unknown"
  }
}

c19.06_97[17, "Functional"] <- "Annual grass"
c19.06_97[24, "Functional"] <- "Annual forb"
count(c19.06_97, Functional)

# Assign native status
c19.06_97 <- c19.06_97 %>% 
  mutate(Native = rep(NA, nrow(c19.06_97)))
for(i in 1:nrow(c19.06_97)) {
  if(c19.06_97$Common[i] %in% plant$invasive == TRUE) {
    c19.06_97$Native[i] <- "Invasive"
  } else if(c19.06_97$Common[i] %in% plant$native == TRUE) {
    c19.06_97$Native[i] <- "Native"
  } else if(c19.06_97$Common[i] %in% plant$ground == TRUE) {
    c19.06_97$Native[i] <- "Ground cover"
  } else {
    c19.06_97$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.06_97 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.06_97 <- c19.06_97 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.06_97 <- cbind(c19.06_97, cover)

wide.c19.06_97 <- c19.06_97

# Pivot data from wide to long
c19.06_97 <- wide.c19.06_97
c19.06_97 <- c19.06_97 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.06_97 <- c19.06_97 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.06_97)) {
  if(c19.06_97$Year[i] == "Mar12") {
    c19.06_97$Year[i] <- "2012-03-01"
  } else if(c19.06_97$Year[i] == "Nov12") {
    c19.06_97$Year[i] <- "2012-11-01"
  } else if(c19.06_97$Year[i] == "Nov13") {
    c19.06_97$Year[i] <- "2013-11-01"
  } else if(c19.06_97$Year[i] == "Nov14") {
    c19.06_97$Year[i] <- "2014-11-01"
  } else if(c19.06_97$Year[i] == "Nov15") {
    c19.06_97$Year[i] <- "2015-11-01"
  } else if(c19.06_97$Year[i] == "Nov18") {
    c19.06_97$Year[i] <- "2018-11-01"
  } else {
    c19.06_97$Year[i] <- "2021-11-01"
  }
}
c19.06_97$Year <- as.Date(c19.06_97$Year, format = "%Y-%m-%d")

# Add channel and station
c19.06_97$Station <- rep("Channel 19_Station 06 + 97", nrow(c19.06_97))



# C19 7 + 30 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c19.07_30 <- raw.c19.07_30[-c(1:7), -c(37:43)]
colnames(c19.07_30) <- names.raw

c19.07_30 <- c19.07_30 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # Cane beardgrass has two rows, with data in both rows
c19.07_30[9, 6] <- "15"
c19.07_30 <- c19.07_30[-12, ]
  # "Unknown" and "Annual grass 2021" can be combined to single unknown
c19.07_30[18, 10] <- "0.5"
c19.07_30 <- c19.07_30[-17, ]
  # "Unknown 2018" and "Annual Forb 2021" can be combined to single unknown
c19.07_30[27, 3] <- "15"
c19.07_30[27, 4] <- "2.5"
c19.07_30 <- c19.07_30[-28, ]
  # Shrubby buckwheat has two rows, with data in both rows
c19.07_30[35, 3] <- "15"
c19.07_30 <- c19.07_30[-37, ]

# Correct common names based on scientific name
c19.07_30$Common[c19.07_30$Scientific == "Eriogonum fasciculatum"] <- "Eastern Mojave buckwheat"

# Standardize common names
c19.07_30$Common[c19.07_30$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c19.07_30$Common[c19.07_30$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.07_30$Common[c19.07_30$Common == "cupgrass"] <- "Cupgrass"
c19.07_30$Common[c19.07_30$Common == "AZ Wrightwort"] <- "AZ wrightwort"
c19.07_30$Common[c19.07_30$Common == "spiderling"] <- "Coulter's spiderling"

# Assign missing scientific names (when known) and correct spelling
c19.07_30$Scientific[c19.07_30$Common == "Coulter's spiderling"] <- "Boerhavia coulteri"
c19.07_30$Scientific[c19.07_30$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c19.07_30$Common, "Unk") == TRUE)
which(str_detect(c19.07_30$Common, "2021") == TRUE)
c19.07_30[17, 1] <- "Unknown annual grass, C19"
c19.07_30[27, 1] <- "Unknown annual forb, C19"

# Assign functional groups
c19.07_30 <- c19.07_30 %>% 
  mutate(Functional = rep(NA, nrow(c19.07_30)))
for(i in 1:nrow(c19.07_30)) {
  if(c19.07_30$Common[i] %in% plant$per.grass == TRUE) {
    c19.07_30$Functional[i] <- "Perennial grass"
  } else if(c19.07_30$Common[i] %in% plant$an.grass == TRUE) {
    c19.07_30$Functional[i] <- "Annual grass"
  } else if(c19.07_30$Common[i] %in% plant$per.forb == TRUE) {
    c19.07_30$Functional[i] <- "Perennial forb"
  } else if(c19.07_30$Common[i] %in% plant$an.forb == TRUE) {
    c19.07_30$Functional[i] <- "Annual forb"
  } else if(c19.07_30$Common[i] %in% plant$shrub == TRUE) {
    c19.07_30$Functional[i] <- "Shrub"
  } else if(c19.07_30$Common[i] %in% plant$tree == TRUE) {
    c19.07_30$Functional[i] <- "Tree"
  } else if(c19.07_30$Common[i] %in% plant$ground == TRUE) {
    c19.07_30$Functional[i] <- "Ground cover"
  } else {
    c19.07_30$Functional[i] <- "assign unknown"
  }
}

c19.07_30[17, "Functional"] <- "Annual grass"
c19.07_30[27, "Functional"] <- "Annual forb"
count(c19.07_30, Functional)

# Assign native status
c19.07_30 <- c19.07_30 %>% 
  mutate(Native = rep(NA, nrow(c19.07_30)))
for(i in 1:nrow(c19.07_30)) {
  if(c19.07_30$Common[i] %in% plant$invasive == TRUE) {
    c19.07_30$Native[i] <- "Invasive"
  } else if(c19.07_30$Common[i] %in% plant$native == TRUE) {
    c19.07_30$Native[i] <- "Native"
  } else if(c19.07_30$Common[i] %in% plant$ground == TRUE) {
    c19.07_30$Native[i] <- "Ground cover"
  } else {
    c19.07_30$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.07_30 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.07_30 <- c19.07_30 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.07_30 <- cbind(c19.07_30, cover)

wide.c19.07_30 <- c19.07_30

# Pivot data from wide to long
c19.07_30 <- wide.c19.07_30
c19.07_30 <- c19.07_30 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.07_30 <- c19.07_30 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.07_30)) {
  if(c19.07_30$Year[i] == "Mar12") {
    c19.07_30$Year[i] <- "2012-03-01"
  } else if(c19.07_30$Year[i] == "Nov12") {
    c19.07_30$Year[i] <- "2012-11-01"
  } else if(c19.07_30$Year[i] == "Nov13") {
    c19.07_30$Year[i] <- "2013-11-01"
  } else if(c19.07_30$Year[i] == "Nov14") {
    c19.07_30$Year[i] <- "2014-11-01"
  } else if(c19.07_30$Year[i] == "Nov15") {
    c19.07_30$Year[i] <- "2015-11-01"
  } else if(c19.07_30$Year[i] == "Nov18") {
    c19.07_30$Year[i] <- "2018-11-01"
  } else {
    c19.07_30$Year[i] <- "2021-11-01"
  }
}
c19.07_30$Year <- as.Date(c19.07_30$Year, format = "%Y-%m-%d")

# Add channel and station
c19.07_30$Station <- rep("Channel 19_Station 07 + 30", nrow(c19.07_30))



# C19 7 + 50 --------------------------------------------------------------

# Remove header and rename columns
c19.07_50 <- raw.c19.07_50[-c(1:7), -c(37:43)]
colnames(c19.07_50) <- names.raw

# Move value in Perennial Forbs row to an Unknown row
c19.07_50[37, 1] <- "Unknown"
c19.07_50[37, 11] <- "0.5"

# Remove unnecessary rows
c19.07_50 <- c19.07_50 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))
c19.07_50 <- c19.07_50[-44, ] # remove duplicate AZ wrightwort row that is empty

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # "Unknown 2018" and "Annual grass 2021" can be combined to single unknown
c19.07_50[20, 10] <- "2.5"
c19.07_50 <- c19.07_50[-19, ]
  # "Unknown 2018" and "Annual Forb 2021" can be combined to single unknown
c19.07_50[33, 3] <- "0.5"
c19.07_50 <- c19.07_50[-34, ]

# Correct common names based on scientific name
c19.07_50$Common[c19.07_50$Scientific == "Eriogonum"] <- "Annual buckwheat"
c19.07_50$Common[c19.07_50$Scientific == "Eriogonum fasciculatum"] <- "Eastern Mojave buckwheat"

# Standardize common names
c19.07_50$Common[c19.07_50$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c19.07_50$Common[c19.07_50$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.07_50$Common[c19.07_50$Common == "AZ Wrightwort"] <- "AZ wrightwort"
c19.07_50$Common[c19.07_50$Common == "spiderling"] <- "Coulter's spiderling"

# Assign missing scientific names (when known) and correct spelling
c19.07_50$Scientific[c19.07_50$Common == "Buckwheat"] <- "Eriogonum inflatum"
c19.07_50$Scientific[c19.07_50$Common == "Coulter's spiderling"] <- "Boerhavia coulteri"
c19.07_50$Scientific[c19.07_50$Common == "Pepperweed"] <- "Lepidium"
c19.07_50$Scientific[c19.07_50$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c19.07_50$Scientific[c19.07_50$Scientific == "Gutierrezia sarothra"] <- "Gutierrezia sarothrae"

# Rename unknowns
which(str_detect(c19.07_50$Common, "Unk") == TRUE)
which(str_detect(c19.07_50$Common, "2021") == TRUE)
c19.07_50[19, 1] <- "Unknown annual grass, C19"
c19.07_50[27, 1] <- "Unknown perennial forb, C19"
c19.07_50[33, 1] <- "Unknown annual forb, C19"

# Assign functional groups
c19.07_50 <- c19.07_50 %>% 
  mutate(Functional = rep(NA, nrow(c19.07_50)))
for(i in 1:nrow(c19.07_50)) {
  if(c19.07_50$Common[i] %in% plant$per.grass == TRUE) {
    c19.07_50$Functional[i] <- "Perennial grass"
  } else if(c19.07_50$Common[i] %in% plant$an.grass == TRUE) {
    c19.07_50$Functional[i] <- "Annual grass"
  } else if(c19.07_50$Common[i] %in% plant$per.forb == TRUE) {
    c19.07_50$Functional[i] <- "Perennial forb"
  } else if(c19.07_50$Common[i] %in% plant$an.forb == TRUE) {
    c19.07_50$Functional[i] <- "Annual forb"
  } else if(c19.07_50$Common[i] %in% plant$shrub == TRUE) {
    c19.07_50$Functional[i] <- "Shrub"
  } else if(c19.07_50$Common[i] %in% plant$tree == TRUE) {
    c19.07_50$Functional[i] <- "Tree"
  } else if(c19.07_50$Common[i] %in% plant$ground == TRUE) {
    c19.07_50$Functional[i] <- "Ground cover"
  } else {
    c19.07_50$Functional[i] <- "assign unknown"
  }
}

c19.07_50[19, "Functional"] <- "Annual grass"
c19.07_50[27, "Functional"] <- "Perennial forb"
c19.07_50[33, "Functional"] <- "Annual forb"
count(c19.07_50, Functional)

# Assign native status
c19.07_50 <- c19.07_50 %>% 
  mutate(Native = rep(NA, nrow(c19.07_50)))
for(i in 1:nrow(c19.07_50)) {
  if(c19.07_50$Common[i] %in% plant$invasive == TRUE) {
    c19.07_50$Native[i] <- "Invasive"
  } else if(c19.07_50$Common[i] %in% plant$native == TRUE) {
    c19.07_50$Native[i] <- "Native"
  } else if(c19.07_50$Common[i] %in% plant$ground == TRUE) {
    c19.07_50$Native[i] <- "Ground cover"
  } else {
    c19.07_50$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.07_50 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.07_50 <- c19.07_50 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.07_50 <- cbind(c19.07_50, cover)

wide.c19.07_50 <- c19.07_50

# Pivot data from wide to long
c19.07_50 <- wide.c19.07_50
c19.07_50 <- c19.07_50 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.07_50 <- c19.07_50 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.07_50)) {
  if(c19.07_50$Year[i] == "Mar12") {
    c19.07_50$Year[i] <- "2012-03-01"
  } else if(c19.07_50$Year[i] == "Nov12") {
    c19.07_50$Year[i] <- "2012-11-01"
  } else if(c19.07_50$Year[i] == "Nov13") {
    c19.07_50$Year[i] <- "2013-11-01"
  } else if(c19.07_50$Year[i] == "Nov14") {
    c19.07_50$Year[i] <- "2014-11-01"
  } else if(c19.07_50$Year[i] == "Nov15") {
    c19.07_50$Year[i] <- "2015-11-01"
  } else if(c19.07_50$Year[i] == "Nov18") {
    c19.07_50$Year[i] <- "2018-11-01"
  } else {
    c19.07_50$Year[i] <- "2021-11-01"
  }
}
c19.07_50$Year <- as.Date(c19.07_50$Year, format = "%Y-%m-%d")

# Add channel and station
c19.07_50$Station <- rep("Channel 19_Station 07 + 50", nrow(c19.07_50))



# C19 7 + 97 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c19.07_97 <- raw.c19.07_97[-c(1:7), -c(37:43)]
colnames(c19.07_97) <- names.raw

c19.07_97 <- c19.07_97 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover")) %>% 
  filter(Common != "Shrubby buckwheat") # row is blank and it has a weird scientific name

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # "Unknown" and "Annual grass 2021" can be combined to single unknown
c19.07_97[18, 10] <- "0.5"
c19.07_97 <- c19.07_97[-17, ]
  # "Unknown forb" and "Annual forb 2021" can be combined to single unknown
c19.07_97[27, 8] <- "0.5"
c19.07_97 <- c19.07_97[-26, ]

# Standardize common names
c19.07_97$Common[c19.07_97$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c19.07_97$Common[c19.07_97$Common == "Annual Grass"] <- "Annual grass (year summed)"

# Assign missing scientific names (when known) and correct spelling
c19.07_97$Scientific[c19.07_97$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c19.07_97$Common, "Unk") == TRUE)
which(str_detect(c19.07_97$Common, "2021") == TRUE)
c19.07_97[17, 1] <- "Unknown annual grass, C19"
c19.07_97[26, 1] <- "Unknown annual forb, C19"

# Assign functional groups
c19.07_97 <- c19.07_97 %>% 
  mutate(Functional = rep(NA, nrow(c19.07_97)))
for(i in 1:nrow(c19.07_97)) {
  if(c19.07_97$Common[i] %in% plant$per.grass == TRUE) {
    c19.07_97$Functional[i] <- "Perennial grass"
  } else if(c19.07_97$Common[i] %in% plant$an.grass == TRUE) {
    c19.07_97$Functional[i] <- "Annual grass"
  } else if(c19.07_97$Common[i] %in% plant$per.forb == TRUE) {
    c19.07_97$Functional[i] <- "Perennial forb"
  } else if(c19.07_97$Common[i] %in% plant$an.forb == TRUE) {
    c19.07_97$Functional[i] <- "Annual forb"
  } else if(c19.07_97$Common[i] %in% plant$shrub == TRUE) {
    c19.07_97$Functional[i] <- "Shrub"
  } else if(c19.07_97$Common[i] %in% plant$tree == TRUE) {
    c19.07_97$Functional[i] <- "Tree"
  } else if(c19.07_97$Common[i] %in% plant$ground == TRUE) {
    c19.07_97$Functional[i] <- "Ground cover"
  } else {
    c19.07_97$Functional[i] <- "assign unknown"
  }
}

c19.07_97[17, "Functional"] <- "Annual grass"
c19.07_97[26, "Functional"] <- "Annual forb"
count(c19.07_97, Functional)

# Assign native status
c19.07_97 <- c19.07_97 %>% 
  mutate(Native = rep(NA, nrow(c19.07_97)))
for(i in 1:nrow(c19.07_97)) {
  if(c19.07_97$Common[i] %in% plant$invasive == TRUE) {
    c19.07_97$Native[i] <- "Invasive"
  } else if(c19.07_97$Common[i] %in% plant$native == TRUE) {
    c19.07_97$Native[i] <- "Native"
  } else if(c19.07_97$Common[i] %in% plant$ground == TRUE) {
    c19.07_97$Native[i] <- "Ground cover"
  } else {
    c19.07_97$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.07_97 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.07_97 <- c19.07_97 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.07_97 <- cbind(c19.07_97, cover)

wide.c19.07_97 <- c19.07_97

# Pivot data from wide to long
c19.07_97 <- wide.c19.07_97
c19.07_97 <- c19.07_97 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.07_97 <- c19.07_97 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.07_97)) {
  if(c19.07_97$Year[i] == "Mar12") {
    c19.07_97$Year[i] <- "2012-03-01"
  } else if(c19.07_97$Year[i] == "Nov12") {
    c19.07_97$Year[i] <- "2012-11-01"
  } else if(c19.07_97$Year[i] == "Nov13") {
    c19.07_97$Year[i] <- "2013-11-01"
  } else if(c19.07_97$Year[i] == "Nov14") {
    c19.07_97$Year[i] <- "2014-11-01"
  } else if(c19.07_97$Year[i] == "Nov15") {
    c19.07_97$Year[i] <- "2015-11-01"
  } else if(c19.07_97$Year[i] == "Nov18") {
    c19.07_97$Year[i] <- "2018-11-01"
  } else {
    c19.07_97$Year[i] <- "2021-11-01"
  }
}
c19.07_97$Year <- as.Date(c19.07_97$Year, format = "%Y-%m-%d")

# Add channel and station
c19.07_97$Station <- rep("Channel 19_Station 07 + 97", nrow(c19.07_97))



# C19 9 + 05 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c19.09_05 <- raw.c19.09_05[-c(1:7), -c(37:43)]
colnames(c19.09_05) <- names.raw

c19.09_05 <- c19.09_05 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # Mesa threeawn is the same as spidergrass
c19.09_05[3, 20] <- "37.5"
c19.09_05 <- c19.09_05[-12, ]
  # "Unknown" and "Annual grass 2021" can be combined to single unknown
c19.09_05[20, 7] <- "0.5"
c19.09_05 <- c19.09_05[-19, ]
  # "Unknown ann forb 2018" and "Annual forb 2021" can be combined to single unknown
c19.09_05[28, 7] <- "0.5"
c19.09_05 <- c19.09_05[-27, ]

# Standardize common names
c19.09_05$Common[c19.09_05$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.09_05$Common[c19.09_05$Common == "annual lovegrass"] <- "Annual lovegrass"

# Assign missing scientific names (when known) and correct spelling
c19.09_05$Scientific[c19.09_05$Common == "Pepperweed"] <- "Lepidium"
c19.09_05$Scientific[c19.09_05$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c19.09_05$Common, "Unk") == TRUE)
which(str_detect(c19.09_05$Common, "2021") == TRUE)
c19.09_05[19, 1] <- "Unknown annual grass, C19"
c19.09_05[27, 1] <- "Unknown annual forb, C19"

# Assign functional groups
c19.09_05 <- c19.09_05 %>% 
  mutate(Functional = rep(NA, nrow(c19.09_05)))
for(i in 1:nrow(c19.09_05)) {
  if(c19.09_05$Common[i] %in% plant$per.grass == TRUE) {
    c19.09_05$Functional[i] <- "Perennial grass"
  } else if(c19.09_05$Common[i] %in% plant$an.grass == TRUE) {
    c19.09_05$Functional[i] <- "Annual grass"
  } else if(c19.09_05$Common[i] %in% plant$per.forb == TRUE) {
    c19.09_05$Functional[i] <- "Perennial forb"
  } else if(c19.09_05$Common[i] %in% plant$an.forb == TRUE) {
    c19.09_05$Functional[i] <- "Annual forb"
  } else if(c19.09_05$Common[i] %in% plant$shrub == TRUE) {
    c19.09_05$Functional[i] <- "Shrub"
  } else if(c19.09_05$Common[i] %in% plant$tree == TRUE) {
    c19.09_05$Functional[i] <- "Tree"
  } else if(c19.09_05$Common[i] %in% plant$ground == TRUE) {
    c19.09_05$Functional[i] <- "Ground cover"
  } else {
    c19.09_05$Functional[i] <- "assign unknown"
  }
}

c19.09_05[19, "Functional"] <- "Annual grass"
c19.09_05[27, "Functional"] <- "Annual forb"
count(c19.09_05, Functional)

# Assign native status
c19.09_05 <- c19.09_05 %>% 
  mutate(Native = rep(NA, nrow(c19.09_05)))
for(i in 1:nrow(c19.09_05)) {
  if(c19.09_05$Common[i] %in% plant$invasive == TRUE) {
    c19.09_05$Native[i] <- "Invasive"
  } else if(c19.09_05$Common[i] %in% plant$native == TRUE) {
    c19.09_05$Native[i] <- "Native"
  } else if(c19.09_05$Common[i] %in% plant$ground == TRUE) {
    c19.09_05$Native[i] <- "Ground cover"
  } else {
    c19.09_05$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.09_05 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.09_05 <- c19.09_05 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.09_05 <- cbind(c19.09_05, cover)

wide.c19.09_05 <- c19.09_05

# Pivot data from wide to long
c19.09_05 <- wide.c19.09_05
c19.09_05 <- c19.09_05 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.09_05 <- c19.09_05 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.09_05)) {
  if(c19.09_05$Year[i] == "Mar12") {
    c19.09_05$Year[i] <- "2012-03-01"
  } else if(c19.09_05$Year[i] == "Nov12") {
    c19.09_05$Year[i] <- "2012-11-01"
  } else if(c19.09_05$Year[i] == "Nov13") {
    c19.09_05$Year[i] <- "2013-11-01"
  } else if(c19.09_05$Year[i] == "Nov14") {
    c19.09_05$Year[i] <- "2014-11-01"
  } else if(c19.09_05$Year[i] == "Nov15") {
    c19.09_05$Year[i] <- "2015-11-01"
  } else if(c19.09_05$Year[i] == "Nov18") {
    c19.09_05$Year[i] <- "2018-11-01"
  } else {
    c19.09_05$Year[i] <- "2021-11-01"
  }
}
c19.09_05$Year <- as.Date(c19.09_05$Year, format = "%Y-%m-%d")

# Add channel and station
c19.09_05$Station <- rep("Channel 19_Station 09 + 05", nrow(c19.09_05))



# C19 9 + 26 --------------------------------------------------------------

# Remove header and rename columns 
c19.09_26 <- raw.c19.09_26[-c(1:7), -c(37:43)]
colnames(c19.09_26) <- names.raw

# Add missing common names
c19.09_26$Common[c19.09_26$Scientific == "Baccharis seritnroides"] <- "Desert broom"

# Remove unnecessary rows
c19.09_26 <- c19.09_26 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # Cane beardgrass has two rows, with data in both
c19.09_26[9, 5] <- "15"
c19.09_26 <- c19.09_26[-14, ]
  # Desert broom has two rows, with data in both
c19.09_26[34, 7] <- "37.5"
c19.09_26 <- c19.09_26[-35, ]

# Standardize common names
c19.09_26$Common[c19.09_26$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.09_26$Common[c19.09_26$Common == "Unknown aster"] <- "Annual aster"

# Assign missing scientific names (when known) and correct spelling
c19.09_26$Scientific[c19.09_26$Common == "Pepperweed"] <- "Lepidium"
c19.09_26$Scientific[c19.09_26$Scientific == "Baccharis sarothrae"] <- "Baccharis sarothroides"
c19.09_26$Scientific[c19.09_26$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"

# Rename unknowns
which(str_detect(c19.09_26$Common, "Unk") == TRUE)
which(str_detect(c19.09_26$Common, "2021") == TRUE)
c19.09_26[20, 1] <- "Unknown annual grass, C19"

# Assign functional groups
c19.09_26 <- c19.09_26 %>% 
  mutate(Functional = rep(NA, nrow(c19.09_26)))
for(i in 1:nrow(c19.09_26)) {
  if(c19.09_26$Common[i] %in% plant$per.grass == TRUE) {
    c19.09_26$Functional[i] <- "Perennial grass"
  } else if(c19.09_26$Common[i] %in% plant$an.grass == TRUE) {
    c19.09_26$Functional[i] <- "Annual grass"
  } else if(c19.09_26$Common[i] %in% plant$per.forb == TRUE) {
    c19.09_26$Functional[i] <- "Perennial forb"
  } else if(c19.09_26$Common[i] %in% plant$an.forb == TRUE) {
    c19.09_26$Functional[i] <- "Annual forb"
  } else if(c19.09_26$Common[i] %in% plant$shrub == TRUE) {
    c19.09_26$Functional[i] <- "Shrub"
  } else if(c19.09_26$Common[i] %in% plant$tree == TRUE) {
    c19.09_26$Functional[i] <- "Tree"
  } else if(c19.09_26$Common[i] %in% plant$ground == TRUE) {
    c19.09_26$Functional[i] <- "Ground cover"
  } else {
    c19.09_26$Functional[i] <- "assign unknown"
  }
}

c19.09_26[20, "Functional"] <- "Annual grass"
count(c19.09_26, Functional)

# Assign native status
c19.09_26 <- c19.09_26 %>% 
  mutate(Native = rep(NA, nrow(c19.09_26)))
for(i in 1:nrow(c19.09_26)) {
  if(c19.09_26$Common[i] %in% plant$invasive == TRUE) {
    c19.09_26$Native[i] <- "Invasive"
  } else if(c19.09_26$Common[i] %in% plant$native == TRUE) {
    c19.09_26$Native[i] <- "Native"
  } else if(c19.09_26$Common[i] %in% plant$ground == TRUE) {
    c19.09_26$Native[i] <- "Ground cover"
  } else {
    c19.09_26$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.09_26 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.09_26 <- c19.09_26 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.09_26 <- cbind(c19.09_26, cover)

wide.c19.09_26 <- c19.09_26

# Pivot data from wide to long
c19.09_26 <- wide.c19.09_26
c19.09_26 <- c19.09_26 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.09_26 <- c19.09_26 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.09_26)) {
  if(c19.09_26$Year[i] == "Mar12") {
    c19.09_26$Year[i] <- "2012-03-01"
  } else if(c19.09_26$Year[i] == "Nov12") {
    c19.09_26$Year[i] <- "2012-11-01"
  } else if(c19.09_26$Year[i] == "Nov13") {
    c19.09_26$Year[i] <- "2013-11-01"
  } else if(c19.09_26$Year[i] == "Nov14") {
    c19.09_26$Year[i] <- "2014-11-01"
  } else if(c19.09_26$Year[i] == "Nov15") {
    c19.09_26$Year[i] <- "2015-11-01"
  } else if(c19.09_26$Year[i] == "Nov18") {
    c19.09_26$Year[i] <- "2018-11-01"
  } else {
    c19.09_26$Year[i] <- "2021-11-01"
  }
}
c19.09_26$Year <- as.Date(c19.09_26$Year, format = "%Y-%m-%d")

# Add channel and station
c19.09_26$Station <- rep("Channel 19_Station 09 + 26", nrow(c19.09_26))



# C19 9 + 68 --------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c19.09_68 <- raw.c19.09_68[-c(1:7), -c(37:43)]
colnames(c19.09_68) <- names.raw

c19.09_68 <- c19.09_68 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Annual Forbs", "Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # Mesa threeawn is the same as spidergrass, and has data in both rows
c19.09_68[3, 6] <- "15"
c19.09_68[3, 22] <- "15"
c19.09_68 <- c19.09_68[-12, ]

# Standardize common names
c19.09_68$Common[c19.09_68$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.09_68$Common[c19.09_68$Common == "Arizona Wrightwort"] <- "AZ wrightwort"
c19.09_68$Common[c19.09_68$Common == "trumpetflower"] <- "Buckwheat"

# Assign missing scientific names (when known) and correct spelling
c19.09_68$Scientific[c19.09_68$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c19.09_68$Scientific[c19.09_68$Scientific == "Carlo wrightiii"] <- "Carlowrightia arizonica"

# Rename unknowns
which(str_detect(c19.09_68$Common, "Unk") == TRUE)
which(str_detect(c19.09_68$Common, "2021") == TRUE)
c19.09_68[18, 1] <- "Unknown annual grass, C19"

# Assign functional groups
c19.09_68 <- c19.09_68 %>% 
  mutate(Functional = rep(NA, nrow(c19.09_68)))
for(i in 1:nrow(c19.09_68)) {
  if(c19.09_68$Common[i] %in% plant$per.grass == TRUE) {
    c19.09_68$Functional[i] <- "Perennial grass"
  } else if(c19.09_68$Common[i] %in% plant$an.grass == TRUE) {
    c19.09_68$Functional[i] <- "Annual grass"
  } else if(c19.09_68$Common[i] %in% plant$per.forb == TRUE) {
    c19.09_68$Functional[i] <- "Perennial forb"
  } else if(c19.09_68$Common[i] %in% plant$an.forb == TRUE) {
    c19.09_68$Functional[i] <- "Annual forb"
  } else if(c19.09_68$Common[i] %in% plant$shrub == TRUE) {
    c19.09_68$Functional[i] <- "Shrub"
  } else if(c19.09_68$Common[i] %in% plant$tree == TRUE) {
    c19.09_68$Functional[i] <- "Tree"
  } else if(c19.09_68$Common[i] %in% plant$ground == TRUE) {
    c19.09_68$Functional[i] <- "Ground cover"
  } else {
    c19.09_68$Functional[i] <- "assign unknown"
  }
}

c19.09_68[18, "Functional"] <- "Annual grass"
count(c19.09_68, Functional)

# Assign native status
c19.09_68 <- c19.09_68 %>% 
  mutate(Native = rep(NA, nrow(c19.09_68)))
for(i in 1:nrow(c19.09_68)) {
  if(c19.09_68$Common[i] %in% plant$invasive == TRUE) {
    c19.09_68$Native[i] <- "Invasive"
  } else if(c19.09_68$Common[i] %in% plant$native == TRUE) {
    c19.09_68$Native[i] <- "Native"
  } else if(c19.09_68$Common[i] %in% plant$ground == TRUE) {
    c19.09_68$Native[i] <- "Ground cover"
  } else {
    c19.09_68$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.09_68 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.09_68 <- c19.09_68 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.09_68 <- cbind(c19.09_68, cover)

wide.c19.09_68 <- c19.09_68

# Pivot data from wide to long
c19.09_68 <- wide.c19.09_68
c19.09_68 <- c19.09_68 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.09_68 <- c19.09_68 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.09_68)) {
  if(c19.09_68$Year[i] == "Mar12") {
    c19.09_68$Year[i] <- "2012-03-01"
  } else if(c19.09_68$Year[i] == "Nov12") {
    c19.09_68$Year[i] <- "2012-11-01"
  } else if(c19.09_68$Year[i] == "Nov13") {
    c19.09_68$Year[i] <- "2013-11-01"
  } else if(c19.09_68$Year[i] == "Nov14") {
    c19.09_68$Year[i] <- "2014-11-01"
  } else if(c19.09_68$Year[i] == "Nov15") {
    c19.09_68$Year[i] <- "2015-11-01"
  } else if(c19.09_68$Year[i] == "Nov18") {
    c19.09_68$Year[i] <- "2018-11-01"
  } else {
    c19.09_68$Year[i] <- "2021-11-01"
  }
}
c19.09_68$Year <- as.Date(c19.09_68$Year, format = "%Y-%m-%d")

# Add channel and station
c19.09_68$Station <- rep("Channel 19_Station 09 + 68", nrow(c19.09_68))



# C19 10 + 98 -------------------------------------------------------------

# Remove header, rename columns, and remove unnecessary rows
c19.10_98 <- raw.c19.10_98[-c(1:7), -c(37:43)]
colnames(c19.10_98) <- names.raw

c19.10_98 <- c19.10_98 %>% 
  filter(!is.na(Common)) %>% 
  filter(!str_detect(Common, "Total")) %>% 
  filter(!str_detect(Common, "Perennial")) %>% 
  filter(!Common %in% c("Shrubs", "Trees", "Ground Cover"))

# Collapse rows of identical species into 1 row
  # Am manually moving/re-entering data to condense into 1 row
  # Mesa threeawn is the same as spidergrass, with data in both rows
c19.10_98[3, 21] <- "37.5"
c19.10_98 <- c19.10_98[-12, ]
  # "Unknown 2018" and "Unknown 2021" can be combined to single unknown
c19.10_98[19, 10] <- "0.5"
c19.10_98 <- c19.10_98[-18, ]
  # AZ wrightwort has two rows, with data in both
c19.10_98[26, 33] <- "0.5"
c19.10_98 <- c19.10_98[-35, ]

# Standardize common names
c19.10_98$Common[c19.10_98$Common == "Annual Forbs"] <- "Annual forb (year summed)"
c19.10_98$Common[c19.10_98$Common == "Annual Grass"] <- "Annual grass (year summed)"
c19.10_98$Common[c19.10_98$Common == "Arizona wrightwort"] <- "AZ wrightwort"
c19.10_98$Common[c19.10_98$Common == "Trumpetflower"] <- "Buckwheat"

# Assign missing scientific names (when known) and correct spelling
c19.10_98$Scientific[c19.10_98$Common == "Shrubby buckwheat"] <- "Eriogonum"
c19.10_98$Scientific[c19.10_98$Scientific == "Ambrosia confertifolia"] <- "Ambrosia confertiflora"
c19.10_98$Scientific[c19.10_98$Scientific == "Carlowrightii"] <- "Carlowrightia arizonica"

# Rename unknowns
which(str_detect(c19.10_98$Common, "Unk") == TRUE)
which(str_detect(c19.10_98$Common, "2021") == TRUE)
c19.10_98[18, 1] <- "Unknown annual grass, C19"
c19.10_98[29, 1] <- "Unknown annual forb, C19"

# Assign functional groups
c19.10_98 <- c19.10_98 %>% 
  mutate(Functional = rep(NA, nrow(c19.10_98)))
for(i in 1:nrow(c19.10_98)) {
  if(c19.10_98$Common[i] %in% plant$per.grass == TRUE) {
    c19.10_98$Functional[i] <- "Perennial grass"
  } else if(c19.10_98$Common[i] %in% plant$an.grass == TRUE) {
    c19.10_98$Functional[i] <- "Annual grass"
  } else if(c19.10_98$Common[i] %in% plant$per.forb == TRUE) {
    c19.10_98$Functional[i] <- "Perennial forb"
  } else if(c19.10_98$Common[i] %in% plant$an.forb == TRUE) {
    c19.10_98$Functional[i] <- "Annual forb"
  } else if(c19.10_98$Common[i] %in% plant$shrub == TRUE) {
    c19.10_98$Functional[i] <- "Shrub"
  } else if(c19.10_98$Common[i] %in% plant$tree == TRUE) {
    c19.10_98$Functional[i] <- "Tree"
  } else if(c19.10_98$Common[i] %in% plant$ground == TRUE) {
    c19.10_98$Functional[i] <- "Ground cover"
  } else {
    c19.10_98$Functional[i] <- "assign unknown"
  }
}

c19.10_98[18, "Functional"] <- "Annual grass"
c19.10_98[29, "Functional"] <- "Annual forb"
count(c19.10_98, Functional)

# Assign native status
c19.10_98 <- c19.10_98 %>% 
  mutate(Native = rep(NA, nrow(c19.10_98)))
for(i in 1:nrow(c19.10_98)) {
  if(c19.10_98$Common[i] %in% plant$invasive == TRUE) {
    c19.10_98$Native[i] <- "Invasive"
  } else if(c19.10_98$Common[i] %in% plant$native == TRUE) {
    c19.10_98$Native[i] <- "Native"
  } else if(c19.10_98$Common[i] %in% plant$ground == TRUE) {
    c19.10_98$Native[i] <- "Ground cover"
  } else {
    c19.10_98$Native[i] <- "Unknown native status"
  }
}

# Replace NAs with 0 and "t" with 0.5 for cover
cover <- c19.10_98 %>% 
  select(!c("Common", "Scientific", "Functional", "Native"))
cover[is.na(cover)] <- "0"
cover[cover == "t"] <- "0.5"
cover <- cover %>% 
  mutate_if(is.character, as.numeric)

# Combine dataframes
c19.10_98 <- c19.10_98 %>% 
  select(c("Common", "Scientific", "Functional", "Native"))
c19.10_98 <- cbind(c19.10_98, cover)

wide.c19.10_98 <- c19.10_98

# Pivot data from wide to long
c19.10_98 <- wide.c19.10_98
c19.10_98 <- c19.10_98 %>% 
  pivot_longer(!c("Common", "Scientific", "Functional", "Native"), 
               names_to = "Measurement", values_to = "Cover") 
c19.10_98 <- c19.10_98 %>% 
  separate(Measurement, c("Year", "Quadrat"), "_")

for(i in 1:nrow(c19.10_98)) {
  if(c19.10_98$Year[i] == "Mar12") {
    c19.10_98$Year[i] <- "2012-03-01"
  } else if(c19.10_98$Year[i] == "Nov12") {
    c19.10_98$Year[i] <- "2012-11-01"
  } else if(c19.10_98$Year[i] == "Nov13") {
    c19.10_98$Year[i] <- "2013-11-01"
  } else if(c19.10_98$Year[i] == "Nov14") {
    c19.10_98$Year[i] <- "2014-11-01"
  } else if(c19.10_98$Year[i] == "Nov15") {
    c19.10_98$Year[i] <- "2015-11-01"
  } else if(c19.10_98$Year[i] == "Nov18") {
    c19.10_98$Year[i] <- "2018-11-01"
  } else {
    c19.10_98$Year[i] <- "2021-11-01"
  }
}
c19.10_98$Year <- as.Date(c19.10_98$Year, format = "%Y-%m-%d")

# Add channel and station
c19.10_98$Station <- rep("Channel 19_Station 10 + 98", nrow(c19.10_98))



# Combine and check for name standardization and corrections --------------

all.c19 <- rbind(c19.01_04, c19.02_04, c19.02_52, c19.03_63, c19.03_98, 
                 c19.05_00, c19.05_09, c19.05_62, c19.06_13, c19.06_97,
                 c19.07_30, c19.07_50, c19.07_97, c19.09_05, c19.09_26,
                 c19.09_68, c19.10_98)

names.common <- all.c19 %>% 
  distinct(Common, .keep_all = TRUE) %>% 
  select(Common, Scientific, Functional, Native) %>% 
  arrange(Common)
names.scientific <- all.c19 %>% 
  distinct(Common, .keep_all = TRUE) %>% 
  select(Common, Scientific, Functional, Native) %>% 
  arrange(Scientific)

unique(filter(all.c19, Scientific == "Gutierrezia sarothra")$Station)
unique(filter(all.c19, Common == "Mesa threeawn")$Station)

print(count(all.c19, Scientific), n = 100)
print(count(all.c19, Common), n = 100)

unique(filter(all.c19, Native == "Unknown native status")$Common)



# Save cleaned dataframes -------------------------------------------------

write.csv(all.c19,
          file = "data/cleaned/C19-cover.csv",
          row.names = FALSE)


save.image("RData/C19-data-wrangling.RData")


