# BRAN MAP 

#setwd("C:/Users/htsch/Desktop/Charlie")


library(tidyverse)
library(tidync)
library(lubridate)
#library(metR)
#library(stringr)

u_files <- list.files("/srv/scratch/z3374139/BRAN_AUS/Charlie/", pattern="Ocean_u_", full.names = T)
#u_files <- list.files("BRAN/", pattern="Ocean_u_", full.names = T)
#u_files_1 <- str_extract(u_files, pattern=".+_12.nc") %>% na.omit() # Dec files only
#u_files_2 <- str_extract(u_files, pattern=".+_01.nc") %>% na.omit() # Jan files only

v_files <- list.files("/srv/scratch/z3374139/BRAN_AUS/Charlie/", pattern="Ocean_v_", full.names = T)
#v_files <- list.files("BRAN/", pattern="Ocean_v_", full.names = T)
#v_files_1 <- str_extract(v_files, pattern=".+_12.nc") %>% na.omit() # Dec files only
#v_files_2 <- str_extract(v_files, pattern=".+_01.nc") %>% na.omit() # Jan files only

temp_files <- list.files("/srv/scratch/z3374139/BRAN_AUS/Charlie/", pattern="Ocean_temp_", full.names = T)
#temp_files <- list.files("BRAN/", pattern="Ocean_temp_", full.names = T)
#temp_files_1 <- str_extract(temp_files, pattern=".+_12.nc") %>% na.omit() # Dec files only
#temp_files_2 <- str_extract(temp_files, pattern=".+_01.nc") %>% na.omit() # Jan files only


u_data_list <- list()

for(i in (1:length(u_files))){
  u_data_list[[i]] <- tidync(u_files[i]) %>% hyper_filter(st_ocean  = st_ocean  == 2.5) %>%
    hyper_tibble() %>% mutate(Time = as.Date(Time, origin="1979-01-01"), Day = day(Time), Month = month(Time), Year=year(Time))
}

u_data_list <- bind_rows(u_data_list) %>% mutate(Season = case_when((Month >= 6 & Month <11) ~ "WhaleSeason",
                                                                    (TRUE ~ "NotWhales"))) %>%
  group_by(Season, xu_ocean, yu_ocean) %>% summarise(u = mean(u, na.rm=T))

v_data_list <- list()

for(i in (1:length(v_files))){
  v_data_list[[i]] <- tidync(v_files[i]) %>% hyper_filter(st_ocean  = st_ocean  == 2.5) %>%
    hyper_tibble() %>% mutate(Time = as.Date(Time, origin="1979-01-01"), Day = day(Time), Month = month(Time), Year=year(Time))
}

v_data_list <- bind_rows(v_data_list) %>% mutate(Season = case_when((Month >= 6 & Month <11) ~ "WhaleSeason",
                                                                    (TRUE ~ "NotWhales")))  %>%
  group_by(Season, xu_ocean, yu_ocean) %>% summarise(v = mean(v, na.rm=T))

t_data_list <- list()

for(i in (1:length(temp_files))){
  t_data_list[[i]] <- tidync(temp_files[i]) %>% hyper_filter(st_ocean  = st_ocean  == 2.5) %>%
    hyper_tibble() %>% mutate(Time = as.Date(Time, origin="1979-01-01"), Day = day(Time), Month = month(Time), Year=year(Time))
}

t_data_list <- bind_rows(t_data_list) %>% mutate(Season = case_when((Month >= 6 & Month <11) ~ "WhaleSeason",
                                                                    (TRUE ~ "NotWhales")) ) %>%
  group_by(Season, xt_ocean, yt_ocean) %>% summarise(temp = mean(temp, na.rm=T))


UV_dat <- left_join(u_data_list, v_data_list)

write_csv(UV_dat, "BRAN_Surface_Currents_Whale.csv")
write_csv(t_data_list, "BRAN SST_Whale.csv")


# library("rnaturalearth")
# library("rnaturalearthdata")
# 
# world <- ne_countries(country = 'australia',scale = "large", returnclass = "sf")
# #class(world)
# #x <- ncmeta::nc_atts("AUS/Ocean_u_1994_01.nc", "Time")
# #x$value[2]
# 
#   
#   
# ggplot(UV_dat, aes(x=xu_ocean, y=yu_ocean)) + facet_wrap(~Season)+
#     geom_tile(data=t_data_list, aes(x=xt_ocean, y=yt_ocean, col=temp, fill=temp))+
#     geom_sf(data=world, col="grey70", fill = "grey80", inherit.aes = FALSE)+
#     geom_vector(aes(angle = atan2(dlat(v), dlon(u, yu_ocean))*180/pi,
#                     mag = Mag(v, u)), skip = 6, pivot = 0.5, size=.4) + #
#     scale_mag(name="Velocity\n(m/s)", max=1.5) +
#     coord_sf(xlim = c(min(t_data_list$xt_ocean), max(t_data_list$xt_ocean)),
#              ylim = c(min(t_data_list$yt_ocean), max(t_data_list$yt_ocean)), expand = FALSE)+
#     #scale_colour_viridis_c(name="Temperature (°C)", option="inferno", limits=c(min(t_data_list$temp),max(t_data_list$temp))) + # alternate colour scheme
#     #scale_fill_viridis_c(name="Temperature (°C)", option="inferno", limits=c(min(t_data_list$temp),max(t_data_list$temp))) +
#   scale_colour_viridis_c(name="Temperature (°C)", option="turbo", limits=c(min(t_data_list$temp),max(t_data_list$temp))) + 
#   scale_fill_viridis_c(name="Temperature (°C)", option="turbo", limits=c(min(t_data_list$temp),max(t_data_list$temp)))+
#   labs(x="Longitude", y="Latitude")+ theme(axis.text = element_text(colour = "black", size=12),
#                                              axis.title = element_text(face="bold", size = 14),
#                                              legend.title = element_text(face="bold", size=12),
#                                              legend.position = "right",
#                                              legend.text = element_text(size=10),
#                                              legend.background = element_blank(),
#                                              legend.key = element_blank(),
#                                              legend.key.height = unit(1.3, "cm"))+
#     guides(color = guide_colourbar(order = 2),
#            fill = guide_colourbar(order = 2),
#            mag = guide_vector(order=1, title.vjust = -5))# +
#   #ggsave(paste0("/srv/scratch/z3374139/BRAN_AUS/BRAN_MAPS_SMALLER/Spawning Season Dec_Jan ", floor(full_dat$Year[1]),".pdf"), units="cm", height=21, width=14.8) 
#   
# ggsave("BRAN average map.png", dpi= 600, width = 14.8, height = 21, units="cm")

  