### BRAN ATTEMPT TO ID SEPARATION ZONE

library(tidyverse)
library(tidync)
library(lubridate)
library(metR)

t1 <- tidync("C:/Users/htsch/Documents/GitHub/portunid_particle_tracking/BRAN_2020/AUS/Ocean_v_1993_01.nc") %>% hyper_filter(st_ocean  = st_ocean  == 2.5) %>%
  hyper_tibble() %>% mutate(Time = as.Date(Time, origin="1979-01-01"), Day = day(Time), Month = month(Time), Year=year(Time)) %>% 
  group_by(Month,xu_ocean ,yu_ocean ) %>% summarise(across(where(is.double),~ mean(.x, na.rm = TRUE))) %>%
  filter(xu_ocean >150 & yu_ocean>-35 & xu_ocean <155 & yu_ocean < -20)

ggplot(t1, aes(x=xu_ocean, y = yu_ocean, fill = v, col=v)) + geom_tile()

unique(t1$yu_ocean)
t2 <- t1 %>% ungroup() %>% filter(yu_ocean == -28) %>% slice_min(order_by = v, n =1)


h1 <- tidync("C:/Users/htsch/Documents/GitHub/portunid_particle_tracking/BRAN_2020/AUS/Ocean_eta_t_1993_01.nc") %>% #hyper_filter(st_ocean  = st_ocean  == 2.5) %>%
  hyper_tibble() %>% mutate(Time = as.Date(Time, origin="1979-01-01"), Day = day(Time), Month = month(Time), Year=year(Time)) %>% 
  group_by(Month,xt_ocean ,yt_ocean ) %>% summarise(across(where(is.double),~ mean(.x, na.rm = TRUE))) %>%
  filter(xt_ocean >150 & yt_ocean>-35 & xt_ocean <155 & yt_ocean < -20)
ggplot(t1, aes(x=xu_ocean, y = yu_ocean, fill = v, col=v)) + geom_tile()

ggplot(h1, aes(x=xt_ocean, y = yt_ocean, fill = eta_t, col=eta_t)) + geom_tile()



#### attempt raster
library(raster)

f1 <- brick("C:/Users/htsch/Documents/GitHub/portunid_particle_tracking/BRAN_2020/AUS/Ocean_v_1993_01.nc", level=1)
plot(f1[[1]])

e <- extent(149, 160, -37, -27)
f1 <- crop(f1, e)

f2 <- mean(f1)#[[30]]
plot(f2)

y1 <- rasterToContour(f2,  levels = -0.5)
lines(y1)

# set line to extract v from
x <- c(153,158)
y <- c(-28,-28)
L1 <- SpatialLines(list(Lines(Line(cbind(x,y)), ID="a")))
lines(L1)

v3 <- extract(f2, L1, cellnumbers=TRUE)

cords <- xyFromCell(f2, v3[[1]][,1])
df = data.frame(v = v3[[1]][,2], lat = cords[,2], lon = cords[,1])
head(df)

# get location of max flow
df_cords <- dplyr::filter(df, v == min(v, na.rm = T))

# load sea surface height
ssh <- stack("C:/Users/htsch/Documents/GitHub/portunid_particle_tracking/BRAN_2020/AUS/Ocean_eta_t_1993_01.nc")
plot(ssh[[1]])

ssh <- crop(ssh, e)
ssh <- mean(ssh)
plot(ssh)#[[30]])

#ssh2 <- mean(ssh)
#plot(ssh2)

# get SSH at the point of max flow 28S
df_cordsSP <- SpatialPoints(coords = cbind(df_cords$lon, df_cords$lat))

SSH_value <- raster::extract(ssh, df_cordsSP)

y1 <- rasterToContour(ssh,  levels = SSH_value)
lines(y1)
points(x = sep_points$sep_lon, y = sep_points$sep_lat)
y1

## extract the lines

y1b <- sf::st_as_sf(y1)
y1c <- y1b$geometry[[1]]
y1d <- data.frame()
for(i in (1:length(y1c))){
  temp_var <- as.data.frame(y1c[[i]])
  temp_var$contour_num <- as.character(i)
  y1d <- dplyr::bind_rows(y1d, temp_var)
}

y1d$bearing <- geosphere::bearing(p1 = cbind(y1d$V1, y1d$V2))
library(ggplot2)
ggplot(y1d, aes(x=V1, y=V2, group=contour_num, col=bearing)) + geom_path(size=2) +
  coord_quickmap() + scale_colour_viridis_c()

y1d <- y1d %>% mutate(bearing_fix = case_when(bearing<0 ~ 360 + bearing,
                                              T ~bearing))

ggplot(y1d, aes(x=V1, y=V2, group=contour_num, col=bearing_fix)) + geom_path(size=2) +
  coord_quickmap() + scale_colour_viridis_c()

y1d <- y1d %>% mutate(bearing_fix2 = case_when(bearing_fix <180 ~ bearing_fix+180,
                                               bearing_fix >=180 ~ bearing_fix-180))

ggplot(y1d, aes(x=V1, y=V2, group=contour_num, col=bearing_fix2)) + geom_path(size=2) +
  coord_quickmap() + scale_colour_viridis_c()

y1d <- y1d %>% mutate(possible_sep = FALSE) %>%
  mutate(possible_sep2 = case_when(lead(bearing_fix2) > 135 & lead(bearing_fix2) <300 & bearing_fix2 <=135 ~ "Yes",
                                   T ~ "No")) %>%
  mutate(sep_lon = case_when(possible_sep2 == "Yes" ~ V1,
                             T ~ 0),
         sep_lat = case_when(possible_sep2 == "Yes" ~ V2,
                             T ~ 0))
y1d$sep_lon <- na_if(x=y1d$sep_lon,y = 0)
y1d$sep_lat <- na_if(x=y1d$sep_lat,y = 0)
y1d$possible_sep2 <- na_if(x=y1d$possible_sep2,y = "No")


ggplot(y1d, aes(x=V1, y=V2, group=contour_num, col=bearing_fix2)) + geom_path(size=2) +
  coord_quickmap() + scale_colour_viridis_c() + 
  geom_point(inherit.aes = F, data= y1d, aes(sep_lon, sep_lat), size=3)

sep_points <- y1d %>% filter(!is.na(sep_lat))


### make final plot to check with Temp
library(raster)

t1 <- brick("C:/Users/htsch/Documents/GitHub/portunid_particle_tracking/BRAN_2020/AUS/Ocean_temp_1993_01.nc", level=1)
plot(t1[[1]])

e <- extent(149, 160, -37, -27)
t1 <- crop(t1, e)

t2 <- mean(t1)#[[30]]
plot(t2)
lines(y1)
points(x = sep_points$sep_lon, y = sep_points$sep_lat)

