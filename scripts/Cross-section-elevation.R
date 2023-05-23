library(tidyverse)

# Load data ---------------------------------------------------------------

setwd("data/cross-section_raw/") # setwd() to get list.files() to work
#   including the relative path directly in list.files() does not work; it needs absolute path
#   or defaults to current working directory
elev.raw <- list.files(pattern = "CH*") %>% 
  lapply(read_csv, show_col_types = FALSE) %>% 
  bind_rows
setwd("C:/Users/liaos/OneDrive - University of Arizona/grad school/Gornish lab/02_AVCA Elkhorn-Las Delicias/AVCA_ElkLD")
#   setwd() back to project directory


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



# Rename columns, Station & Channel ---------------------------------------

# Rename columns
colnames(elev) <- c("Station.elev", "Channel.elev", "Distance", "elev2011", "elev2019")

channel.station.names <- elev |> 
  select(Channel.elev, Station.elev) |> 
  unique()
write_csv(channel.station.names,
          file = "data/cross-section_raw/Names_raw.csv")
