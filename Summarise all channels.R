library(tidyverse)

# Load data ---------------------------------------------------------------

load("C13 long all stations.RData")
load("C21 long all stations.RData")
load("C19 long all stations.RData")
load("C12 long all stations.RData")


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

# Add treatment
plant.all[ , "Treatment"] <- NA
for(i in 1:nrow(plant.all)) {
  if(str_detect(plant.all$Station, "ORD")[i] == TRUE) {
    plant.all$Treatment[i] <- "One rock dam"
  } else if(str_detect(plant.all$Station, "BAF")[i] == TRUE) {
    plant.all$Treatment[i] <- "Baffle"
  } else {
    plant.all$Treatment[i] <- "No treatment"
  }
}

ground.all[ , "Treatment"] <- NA
for(i in 1:nrow(ground.all)) {
  if(str_detect(ground.all$Station, "ORD")[i] == TRUE) {
    ground.all$Treatment[i] <- "One rock dam"
  } else if(str_detect(ground.all$Station, "BAF")[i] == TRUE) {
    ground.all$Treatment[i] <- "Baffle"
  } else {
    ground.all$Treatment[i] <- "No treatment"
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
  group_by(Channel, Station, Year, Treatment) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")

fungr.all <- plant.all %>% 
  group_by(Channel, Station, Year, Treatment, Functional) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")

gfst.all <- plant.all %>% 
  group_by(Channel, Station, Year, Treatment, gfst) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")

woody.all <- plant.all %>% 
  group_by(Channel, Station, Year, Treatment, woody) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")

inwood.all <- plant.all %>% 
  group_by(Channel, Station, Year, Treatment, Native, woody) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")

ingfst.all <- plant.all %>% 
  group_by(Channel, Station, Year, Treatment, gfst, Native) %>% 
  summarise(Cover = sum(Cover), .groups = "keep")

innat.all <- plant.all %>% 
  group_by(Channel, Station, Year, Treatment, Native) %>% 
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

  

# Save as .RData ----------------------------------------------------------

save(plant.all, ground.all, total.all, fungr.all, gfst.all, woody.all, 
     inwood.all, ingfst.all, innat.all,
     file = "Summarised plant and ground cover.RData")

save(plant.all, file = "plant.all.RData")


save.image("Summarise all channels.RData")

