# Whale BRAN PLOT
# By Hayden 13/8/21


library(tidyverse)

UV_dat <- read_csv("BRAN_Surface_Currents_Whale.csv") %>% filter(Season != "NotWhales")
t_data_list <- read_csv("BRAN SST_Whale.csv") %>% filter(Season != "NotWhales") %>%
  filter(xt_ocean > 145 & xt_ocean < 156) %>%
  filter(yt_ocean > -40 & yt_ocean < -25)


# UV_dat <- UV_dat %>% mutate(Season = case_when(Season == "Summer" ~ "a) Summer",
#                                                Season == "Autumn" ~ "b) Autumn",
#                                                Season == "Winter" ~ "c) Winter",
#                                                Season == "Spring" ~ "d) Spring"))
# 
# t_data_list <- t_data_list %>% mutate(Season = case_when(Season == "Summer" ~ "a) Summer",
#                                                Season == "Autumn" ~ "b) Autumn",
#                                                Season == "Winter" ~ "c) Winter",
#                                                Season == "Spring" ~ "d) Spring"))


library("rnaturalearth")
library("rnaturalearthdata")
library(metR)

world <- ne_states(country = 'australia', returnclass = "sf")
#class(world)
#x <- ncmeta::nc_atts("AUS/Ocean_u_1994_01.nc", "Time")
#x$value[2]



f1 <- ggplot(UV_dat, aes(x=xu_ocean, y=yu_ocean)) + #facet_wrap(~Season, nrow=2)+
    geom_tile(data=t_data_list, aes(x=xt_ocean, y=yt_ocean, col=temp, fill=temp))+
    geom_sf(data=world, col="black", fill = "grey80", inherit.aes = FALSE)+
    geom_vector(aes(angle = atan2(dlat(v), dlon(u, yu_ocean))*180/pi,
                    mag = Mag(v, u)), skip = 2, pivot = 0.5, size=.4) + #
    scale_mag(name="Velocity\n(m/s)", max=1) +
    coord_sf(xlim = c(145, 156),
             ylim = c(-40, -25), expand = FALSE)+
    #scale_colour_viridis_c(name="Temperature (?C)", option="inferno", limits=c(min(t_data_list$temp),max(t_data_list$temp))) + # alternate colour scheme
    #scale_fill_viridis_c(name="Temperature (?C)", option="inferno", limits=c(min(t_data_list$temp),max(t_data_list$temp))) +
  scale_colour_viridis_c(name="Temperature\n(°C)", option="turbo", limits=c(min(t_data_list$temp),max(t_data_list$temp))) +
  scale_fill_viridis_c(name="Temperature\n(°C)", option="turbo", limits=c(min(t_data_list$temp),max(t_data_list$temp)))+
  labs(x="Longitude", y="Latitude")+ theme(axis.text.y = element_text(colour = "black", size=12),
                                           axis.text.x = element_text(colour = "black", size=12, angle =45, vjust=0.5),
                                             axis.title = element_text(face="bold", size = 14),
                                             legend.title = element_text(face="bold", size=12),
                                             legend.position = "right",
                                             legend.text = element_text(size=10),
                                             legend.background = element_blank(),
                                             legend.key = element_blank(),
                                             legend.key.height = unit(1.3, "cm"))+
    guides(color = guide_colourbar(order = 2),
           fill = guide_colourbar(order = 2),
           mag = guide_vector(order=1, title.vjust = -5))+
  geom_point(inherit.aes = F, aes(x=151.27, y = -33.8), fill="black", col = "white", shape=21, size=2) +
  geom_point(inherit.aes = F, aes(x=153.1, y = -30.3), fill="black", col = "white", shape=21, size=2)+
  geom_text(inherit.aes = F, aes(x=147, y = -30, label="NSW"), size=8)# +
f1
#ggsave(paste0("/srv/scratch/z3374139/BRAN_AUS/BRAN_MAPS_SMALLER/Spawning Season Dec_Jan ", floor(full_dat$Year[1]),".pdf"), units="cm", height=21, width=14.8)

ggsave("BRAN average map.png", dpi= 600, width = 14.8, height = 21, units="cm")

### inset map
p3 <- ggplot()+
  geom_sf(data=world, col="black", fill = "grey70", inherit.aes = FALSE)+
  coord_sf(xlim = c(110, 158), ylim = c(-45, -7), expand = FALSE) +
  geom_rect(aes(xmin=145, xmax=156, ymin=-40, ymax=-25), fill=NA, col="black")+
  theme_classic() + labs(x=NULL, y= NULL)+
  theme(axis.text = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        rect=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
p3

### INSET MAP CODE
library(patchwork)
F1F <- f1 + inset_element(p3, 
                         left = 0, right = 0.4, # this took some fiddling
                         bottom = 0.75, top = 1
)
F1F
ggsave("BRAN average map.pdf", dpi= 600, width = 14.8, height = 21, units="cm")
