library(tidyverse)

# Load data ---------------------------------------------------------------

# Raw elevation data
setwd("data/cross-section_raw/") # setwd() to get list.files() to work
#   including the relative path directly in list.files() does not work; it needs absolute path
#   or defaults to current working directory
elev.raw <- list.files(pattern = "CH*") %>% 
  lapply(read_csv, show_col_types = FALSE) %>% 
  bind_rows
setwd("C:/Users/liaos/OneDrive - University of Arizona/grad school/Gornish lab/02_AVCA Elkhorn-Las Delicias/AVCA_ElkLD")
#   setwd() back to project directory

# Station metadata
station.metadata <- read_csv("data/station-metadata.csv")

# Fix col name typo -------------------------------------------------------

elevatin2011 <- elev.raw |> 
  filter(!is.na(`Elevatin 2011`)) 
elevatin2011$`Elevation 2011` <- elevatin2011$`Elevatin 2011`
elevatin2011 <- elevatin2011 |> 
  select(-`Elevatin 2011`)

elev <- elev.raw |> 
  select(-`Elevatin 2011`) |> 
  filter(Name != "1+83")

elev <- bind_rows(elev, elevatin2011)



# Rename columns, standardize names ---------------------------------------

# Rename columns
colnames(elev) <- c("Station.elev", "Channel.elev", "Distance", "elev2011", "elev2019")

# Standardize names
channel.station.names <- elev |> 
  select(Channel.elev, Station.elev) |> 
  unique() 
write_csv(channel.station.names,
          file = "data/cross-section_raw/Names_raw.csv")

# **Translate/standardize names manually in a separate file**
channel.station.names.translate <- read_csv("data/cross-section_raw/Names_translated.csv")

# Join dfs to assign names
elev <- left_join(elev, channel.station.names.translate) 
elev$Channel <- paste("Channel", elev$Channel.elev, sep = " ")
elev <- elev |> 
  select(Channel, Name, Distance, elev2011, elev2019)

elev0 <- elev


# Find minimum elevations for each station from 2011 ----------------------

# Find minimums (slice) and average (summarise) for each channel
#   Some channels are separated because they are of different widths, so slicing varies
c12.19 <- elev0 |> 
  filter(Channel %in% c("Channel 12", "Channel 19")) |> 
  group_by(Name) |> 
  slice_min(elev2011, n = 5) |> 
  summarise(dist.range = max(Distance) - min(Distance),
            elev2011 = mean(elev2011),
            elev2019 = mean(elev2019),
            .groups = "keep")

c13 <- elev0 |> 
  filter(Channel == "Channel 13")  |> 
  group_by(Name) |> 
  slice_min(elev2011, n = 3) |> 
  summarise(dist.range = max(Distance) - min(Distance),
            elev2011 = mean(elev2011),
            elev2019 = mean(elev2019),
            .groups = "keep")

c21 <- elev0 |> 
  filter(Channel == "Channel 21")  |> 
  group_by(Name) |> 
  slice_min(elev2011, n = 7) |> 
  summarise(dist.range = max(Distance) - min(Distance),
            elev2011 = mean(elev2011),
            elev2019 = mean(elev2019),
            .groups = "keep")

# Distance range is too large for Channel 19 Stations 5+62 and 2+52
#   The elevation measurements aren't coming from the same channel dip (see cross section graph)
#   These two will have to be done manually, to select the correct points

# Adjust C19 2+52 & 5+62
c19.2.52 <- elev0 |> 
  filter(Name == "Channel 19, Station 02 + 52") |> 
  arrange(Distance)
which.min(c19.2.52$elev2011) # position 25 is minimum; must use rows 23:27 to average
c19.2.52 <- c19.2.52[23:27, ]
c19.2.52 <- c19.2.52 |> 
  group_by(Name) |> 
  summarise(dist.range = max(Distance) - min(Distance),
            elev2011 = mean(elev2011),
            elev2019 = mean(elev2019),
            .groups = "keep")

c19.5.62 <- elev0 |> 
  filter(Name == "Channel 19, Station 05 + 62") |> 
  arrange(Distance)
which.min(c19.5.62$elev2011) # position 21 is min; use rows 19:23 to average
c19.5.62 <- c19.5.62[19:23, ]
c19.5.62 <- c19.5.62 |> 
  group_by(Name) |> 
  summarise(dist.range = max(Distance) - min(Distance),
            elev2011 = mean(elev2011),
            elev2019 = mean(elev2019),
            .groups = "keep")


# Replace manually-calculated rows in df
elev <- bind_rows(c12.19, c13, c21) 
elev <- elev |> 
  filter(!Name %in% c("Channel 19, Station 02 + 52", "Channel 19, Station 05 + 62")) 
elev <- bind_rows(elev, c19.2.52, c19.5.62)


# Add metadata ------------------------------------------------------------

elev <- elev |> 
  left_join(station.metadata) |> 
  arrange(Sample) |> 
  select(Sample, Name, Channel, Station, Treatment1, Treatment2, Treatment3, elev2011, elev2019)


# Add col for change in elevation -----------------------------------------

elev$dElev <- elev$elev2019 - elev$elev2011

# 2011 has vertical accuracy of 10.4 cm
#   Changes in elevation <0.052 or >-0.052 should be considered no change
elev <- elev |> 
  mutate(dElev_corrected = case_when(
    between(dElev, -0.052, 0.052) ~ 0,
    TRUE ~ dElev))


# Write to csv ------------------------------------------------------------

write_csv(elev,
          file = "data/cleaned/Cross-section-elevation_clean.csv")

