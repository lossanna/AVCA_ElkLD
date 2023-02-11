library(tidyverse)

# Load data ---------------------------------------------------------------

all.c12 <- read.csv("data/cleaned/C12-cover.csv")
all.c13 <- read.csv("data/cleaned/C13-cover.csv")
all.c19 <- read.csv("data/cleaned/C19-cover.csv")
all.c21 <- read.csv("data/cleaned/C21-cover.csv")


# Summarise by plant species and ground cover class -----------------------

# Channel 13
plant.c13 <- all.c13 %>% 
  filter(!Common %in% c("Rock", "Gravel", "Soil", "Litter", "Biocrust")) %>% 
  group_by(Station, Year, Functional, Native, Common, Scientific) %>% 
  summarise(Cover = mean(Cover), .groups = "keep")

ground.c13 <- all.c13 %>% 
  filter(Common %in% c("Rock", "Gravel", "Soil", "Litter", "Biocrust")) %>% 
  group_by(Station, Year, Common) %>% 
  summarise(Cover = mean(Cover), .groups = "keep")


# Channel 21
plant.c21 <- all.c21 %>% 
  filter(!Common %in% c("Rock", "Gravel", "Soil", "Litter", "Biocrust")) %>% 
  group_by(Station, Year, Functional, Native, Common, Scientific) %>% 
  summarise(Cover = mean(Cover), .groups = "keep")

ground.c21 <- all.c21 %>% 
  filter(Common %in% c("Rock", "Gravel", "Soil", "Litter", "Biocrust")) %>% 
  group_by(Station, Year, Common) %>% 
  summarise(Cover = mean(Cover), .groups = "keep")


# Channel 19 
plant.c19 <- all.c19 %>% 
  filter(!Common %in% c("Rock", "Gravel", "Soil", "Litter", "Biocrust")) %>% 
  group_by(Station, Year, Functional, Native, Common, Scientific) %>% 
  summarise(Cover = mean(Cover), .groups = "keep")

ground.c19 <- all.c19 %>% 
  filter(Common %in% c("Rock", "Gravel", "Soil", "Litter", "Biocrust")) %>% 
  group_by(Station, Year, Common) %>% 
  summarise(Cover = mean(Cover), .groups = "keep")


# Channel 12 
plant.c12 <- all.c12 %>% 
  filter(!Common %in% c("Rock", "Gravel", "Soil", "Litter", "Biocrust")) %>% 
  group_by(Station, Year, Functional, Native, Common, Scientific) %>% 
  summarise(Cover = mean(Cover), .groups = "keep")

ground.c12 <- all.c12 %>% 
  filter(Common %in% c("Rock", "Gravel", "Soil", "Litter", "Biocrust")) %>% 
  group_by(Station, Year, Common) %>% 
  summarise(Cover = mean(Cover), .groups = "keep")


# Combine and add treatment, gfst, and woody/herbaceous -------------------

# Combine and rows of remove 0% cover 
plant.all <- rbind(plant.c12, plant.c13, plant.c19, plant.c21)
plant.all <- plant.all %>% 
  filter(Cover > 0) %>% 
  separate(Station, c("Channel", "Station"), "_")

ground.all <- rbind(ground.c12, ground.c13, ground.c19, ground.c21)
ground.all <- ground.all %>% 
  filter(Cover > 0) %>% 
  separate(Station, c("Channel", "Station"), "_")

# Add station treatment
plant.all[ , "station.trt"] <- NA
for(i in 1:nrow(plant.all)) {
  if(str_detect(plant.all$Station, "ORD")[i] == TRUE) {
    plant.all$station.trt[i] <- "One station dam"
  } else if(str_detect(plant.all$Station, "BAF")[i] == TRUE) {
    plant.all$station.trt[i] <- "Baffle"
  } else {
    plant.all$station.trt[i] <- "No treatment"
  }
}

ground.all[ , "station.trt"] <- NA
for(i in 1:nrow(ground.all)) {
  if(str_detect(ground.all$Station, "ORD")[i] == TRUE) {
    ground.all$station.trt[i] <- "One station dam"
  } else if(str_detect(ground.all$Station, "BAF")[i] == TRUE) {
    ground.all$station.trt[i] <- "Baffle"
  } else {
    ground.all$station.trt[i] <- "No treatment"
  }
}

# Add channel treatment
plant.all[ , "channel.trt"] <- NA
for(i in 1:nrow(plant.all)) {
  if(plant.all$Channel[i] == "Channel 12") {
    plant.all$channel.trt[i] <- "Channel 12: No treatment"
  } else if(plant.all$Channel[i] == "Channel 13") {
    plant.all$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(plant.all$Channel[i] == "Channel 19") {
    plant.all$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    plant.all$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

ground.all[ , "channel.trt"] <- NA
for(i in 1:nrow(ground.all)) {
  if(ground.all$Channel[i] == "Channel 12") {
    ground.all$channel.trt[i] <- "Channel 12: No treatment"
  } else if(ground.all$Channel[i] == "Channel 13") {
    ground.all$channel.trt[i] <- "Channel 13: In-channel treatment"
  }  else if(ground.all$Channel[i] == "Channel 19") {
    ground.all$channel.trt[i] <- "Channel 19: Upland treatment"
  } else {
    ground.all$channel.trt[i] <- "Channel 21: In-channel treatment"
  }
}

# Add channel treatment as dummy binary variables: upland treatment
plant.all[ , "up.trt"] <- NA
for(i in 1:nrow(plant.all)) {
  if(plant.all$Channel[i] == "Channel 19") {
    plant.all$up.trt[i] <- 1
  } else {
    plant.all$up.trt[i] <- 0
  }
}

ground.all[ , "up.trt"] <- NA
for(i in 1:nrow(ground.all)) {
  if(ground.all$Channel[i] == "Channel 19") {
    ground.all$up.trt[i] <- 1
  } else {
    ground.all$up.trt[i] <- 0
  }
}


# Add channel treatment as dummy binary variables: in-channel treatment
plant.all[ , "inch.trt"] <- NA
for(i in 1:nrow(plant.all)) {
  if(plant.all$Channel[i] == "Channel 21") {
    plant.all$inch.trt[i] <- 1
  } else if(plant.all$Channel[i] == "Channel 13") {
    plant.all$inch.trt[i] <- 1
  } else {
    plant.all$inch.trt[i] <- 0
  }
}

ground.all[ , "inch.trt"] <- NA
for(i in 1:nrow(ground.all)) {
  if(ground.all$Channel[i] == "Channel 21") {
    ground.all$inch.trt[i] <- 1
  } else if(ground.all$Channel[i] == "Channel 13") {
    ground.all$inch.trt[i] <- 1
  } else {
    ground.all$inch.trt[i] <- 0
  }
}


# Add grass/forb/shrub/tree
plant.all[ , "gfst"] <- NA
for(i in 1:nrow(plant.all)) {
  if(str_detect(plant.all$Functional, "grass")[i] == TRUE) {
    plant.all$gfst[i] <- "Grass"
  } else if(str_detect(plant.all$Functional, "forb")[i] == TRUE) {
    plant.all$gfst[i] <- "Forb"
  } else if(str_detect(plant.all$Functional, "Shrub")[i] == TRUE) {
    plant.all$gfst[i] <- "Shrub"
  } else {
    plant.all$gfst[i] <- "Tree"
  }
} 

# Add woody/herbaceous
plant.all[ , "woody"] <- NA
for(i in 1:nrow(plant.all)) {
  if(str_detect(plant.all$Functional, "grass")[i] == TRUE) {
    plant.all$woody[i] <- "Herbaceous"
  } else if(str_detect(plant.all$Functional, "forb")[i] == TRUE) {
    plant.all$woody[i] <- "Herbaceous"
  } else if(str_detect(plant.all$Functional, "Shrub")[i] == TRUE) {
    plant.all$woody[i] <- "Woody"
  } else {
    plant.all$woody[i] <- "Woody"
  }
} 


# Summarise by total, functional group, and native status -----------------

total.all <- plant.all %>% 
  group_by(Channel, Station, Year, station.trt, channel.trt, up.trt, inch.trt) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")

notree.all <- plant.all %>% 
  filter(gfst != "Tree") %>% 
  group_by(Channel, Station, Year, station.trt, channel.trt, up.trt, inch.trt) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")

fungr.all <- plant.all %>% 
  group_by(Channel, Station, Year, station.trt, channel.trt, up.trt, inch.trt, 
           Functional) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")

gfst.all <- plant.all %>% 
  group_by(Channel, Station, Year, station.trt, channel.trt, up.trt, inch.trt, 
           gfst) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")

woody.all <- plant.all %>% 
  group_by(Channel, Station, Year, station.trt, channel.trt, up.trt, inch.trt,
           woody) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")

inwood.all <- plant.all %>% 
  group_by(Channel, Station, Year, station.trt, channel.trt, up.trt, inch.trt,
           Native, woody) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")

ingfst.all <- plant.all %>% 
  group_by(Channel, Station, Year, station.trt, channel.trt, up.trt, inch.trt,
           gfst, Native) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")

innat.all <- plant.all %>% 
  group_by(Channel, Station, Year, station.trt, channel.trt, up.trt, inch.trt,
           Native) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")


# Fix invasive/native and woody/herbaceous names (inwood) -----------------

inwood.all <- inwood.all %>% 
  unite("inwood", Native:woody, sep = " ") 

for(i in 1:nrow(inwood.all)) {
  if(inwood.all$inwood[i] == "Unknown native status Herbaceous") {
    inwood.all$inwood[i] <- "Unknown herb"
  } else if(inwood.all$inwood[i] == "Native Herbaceous") {
    inwood.all$inwood[i] <- "Native herb"
  }  else if(inwood.all$inwood[i] == "Native Woody") {
    inwood.all$inwood[i] <- "Native woody"
  }  else if(inwood.all$inwood[i] == "Invasive Herbaceous") {
    inwood.all$inwood[i] <- "Invasive herb"
  }  else if(inwood.all$inwood[i] == "Invasive Woody") {
    inwood.all$inwood[i] <- "Invasive woody"
  } else {
    inwood.all$inwood[i] <- "Unknown woody"
  }
}


# Fix invasive/native and gfst names (ingfst) -----------------------------

ingfst.all <- ingfst.all %>% 
  unite("ingfst", Native:gfst, sep = " ")

for(i in 1:nrow(ingfst.all)) {
  if(ingfst.all$ingfst[i] == "Unknown native status Grass") {
    ingfst.all$ingfst[i] <- "Unknown grass"
  } else if(ingfst.all$ingfst[i] == "Unknown native status Forb") {
    ingfst.all$ingfst[i] <- "Unknown forb"
  } else if(ingfst.all$ingfst[i] == "Unknown native status Shrub") {
    ingfst.all$ingfst[i] <- "Unknown shrub"
  } else if(ingfst.all$ingfst[i] == "Native Forb") {
    ingfst.all$ingfst[i] <- "Native forb"
  } else if(ingfst.all$ingfst[i] == "Native Grass") {
    ingfst.all$ingfst[i] <- "Native grass"
  } else if(ingfst.all$ingfst[i] == "Native Shrub") {
    ingfst.all$ingfst[i] <- "Native shrub"
  } else if(ingfst.all$ingfst[i] == "Native Tree") {
    ingfst.all$ingfst[i] <- "Native tree"
  } else if(ingfst.all$ingfst[i] == "Invasive Forb") {
    ingfst.all$ingfst[i] <- "Invasive forb"
  } else {
    ingfst.all$ingfst[i] <- "Invasive grass"
  }
}



# Assign record number for each sampling event ----------------------------

# Create ID
all.c <- bind_rows(all.c12, all.c13, all.c19, all.c21) %>% 
  select(Year, Station) %>% 
  distinct(.keep_all = TRUE) %>% 
  arrange(Year) 
all.c <- all.c[c(63:433, 1:62), ]
all.c$PlotTimeID <- c(1:nrow(all.c)) # 371 for Nov samples bc one of the data sheets was lost
all.c <- all.c %>% 
  separate(Station, c("Channel", "Station"), "_") %>% 
  select(PlotTimeID, Year, Channel, Station)
  
  
# Add to dataframes
plant.all <- left_join(all.c, plant.all)
ground.all <- left_join(all.c, ground.all)
total.all <- left_join(all.c, total.all)
notree.all <- left_join(all.c, notree.all)
fungr.all <- left_join(all.c, fungr.all)
gfst.all <- left_join(all.c, gfst.all)
woody.all <- left_join(all.c, woody.all)
inwood.all <- left_join(all.c, inwood.all)
innat.all <- left_join(all.c, innat.all)



# Save dataframes ---------------------------------------------------------

write.csv(plant.all,
          file = "data/cleaned/Summarised-all_plant-species-cover.csv",
          row.names = FALSE)
write.csv(ground.all,
          file = "data/cleaned/Summarised-all_ground-cover.csv",
          row.names = FALSE)
write.csv(total.all,
          file = "data/cleaned/Summarised-all_total-plant-cover.csv",
          row.names = FALSE)
write.csv(notree.all,
          file = "data/cleaned/Summarised-all_notree-cover.csv",
          row.names = FALSE)
write.csv(fungr.all,
          file = "data/cleaned/Summarised-all_functional-group-cover.csv",
          row.names = FALSE)
write.csv(gfst.all,
          file = "data/cleaned/Summarised-all_grass-forb-shrub-tree-cover.csv",
          row.names = FALSE)
write.csv(woody.all,
          file = "data/cleaned/Summarised-all_woody-herb-cover.csv",
          row.names = FALSE)
write.csv(inwood.all,
          file = "data/cleaned/Summarised-all_invasive-woody-cover.csv",
          row.names = FALSE)
write.csv(ingfst.all,
          file = "data/cleaned/Summarised-all_invasive-grassforbshrubtree-cover.csv",
          row.names = FALSE)
write.csv(innat.all,
          file = "data/cleaned/Summarised-all_invasive-native-cover.csv",
          row.names = FALSE)


save.image("RData/Summarise-all-channels.RData")

