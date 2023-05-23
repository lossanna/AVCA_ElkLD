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


# Add metadata ------------------------------------------------------------

elev <- elev |> 
  select(-Channel.elev, -Station.elev) |> 
  left_join(station.metadata) |> 
  arrange(Sample) |> 
  select(Sample, Name, Channel, Station, Treatment1, Treatment2, Treatment3, Distance, elev2011, elev2019)


# Find minimum elevation for each station from 2011 -----------------------


