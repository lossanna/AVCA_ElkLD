library(dplyr)
library(ggplot2)

# Load data ---------------------------------------------------------------

load("Cover by year and channel.RData")


# Total plant cover -------------------------------------------------------

# All
total.channel.all1321 <- total.channel %>% 
  filter(Channel %in% c("Channel 13", "Channel 21"))

total.all1321.plot <- ggplot(total.channel.all1321, aes(x = Year, y = mean, 
                                            group = Channel, color = Channel, shape = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover (all)") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
total.all1321.plot

total13.letters


# ORD only
total.channel.ord1321 <- total.ord.channel %>% 
  filter(Channel %in% c("Channel 13", "Channel 21"))

total.ord1321.plot <- ggplot(total.channel.ord1321, aes(x = Year, y = mean, 
                                                group = Channel, color = Channel, shape = Channel)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Total plant cover (ORD only)") +
  scale_color_brewer(palette = "Set1") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
total.ord1321.plot


# Soil cover --------------------------------------------------------------

# All
ground.channel.grs1321 <- ground.channel.grs %>% 
  filter(Channel %in% c("Channel 13", "Channel 21"))

ground.grs1321.plot <- ggplot(ground.channel.grs1321, aes(x = Year, y = mean, 
                                                  group = Common, color = Common, shape = Common)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Ground cover (all)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ground.grs1321.plot

hsd.soil13$groups
soil21.letters


# ORD only
ground.ord.channel.grs1321 <- ground.ord.channel.grs %>% 
  filter(Channel %in% c("Channel 13", "Channel 21"))

ground.ord.grs1321.plot <- ggplot(ground.ord.channel.grs1321, aes(x = Year, y = mean, 
                                                          group = Common, color = Common, shape = Common)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Ground cover (ORD only)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
ground.ord.grs1321.plot

hsd.soil.ord13$groups
hsd.soil.ord21$groups


# Herbaceous cover --------------------------------------------------------

# All
woody.channel1321 <- woody.channel %>% 
  filter(Channel %in% c("Channel 13", "Channel 21"))

woody.all1321.plot <- ggplot(woody.channel1321, aes(x = Year, y = mean, 
                                            group = woody, color = woody, shape = woody)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by woody/herbaceous (all)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
woody.all1321.plot

herb13.letters
hsd.herb21$groups


# ORD only
woody.ord.channel1321 <- woody.ord.channel %>% 
  filter(Channel %in% c("Channel 13", "Channel 21"))

woody.ord1321.plot <- ggplot(woody.ord.channel1321, aes(x = Year, y = mean, 
                                                group = woody, color = woody, shape = woody)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE), 
                position = position_dodge(0.05)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_wrap(~Channel) +
  xlab(NULL) +
  ylab("Cover (%)") +
  ggtitle("Plant cover by woody/herbaceous (ORD only)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw(base_size = 13) +
  theme(legend.title = element_blank()) +
  theme(legend.position = "bottom")
woody.ord1321.plot

herb.ord13.letters
hsd.herb.ord21$groups


save.image("ORD vs all.RData")
