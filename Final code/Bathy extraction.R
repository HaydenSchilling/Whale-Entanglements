## Bathy data

library(raster)

## Aus Seabed - useless, too many gaps
bath1 <- raster("Aus SeaBed/1_Multibeam Dataset of Australia 2018 50m.tiff")
plot(bath1)


### Two step process, get fine resolution then get missing from GEBCO


bath2 <- raster("GEBCO/GEBCO_08_Nov_2021_b3166d6aceb4/gebco_2021_n-27.0_s-39.0_w149.0_e155.0.tif")
plot(bath2)


library(tidyverse)


mydata <- read_csv("DPI Data/OTLD_2009-2021_RAW lat_lons_R.csv")
mydata$Latitude <- round(mydata$LatitudeDecimal, 1) -0.05
mydata$Longitude <- round(mydata$LongitudeDecimal, 1) + 0.05

dat <- mydata
coordinates(dat) <- ~LongitudeDecimal+LatitudeDecimal

#test <- dat[1:4,]
test2 <- raster::extract(bath1, dat)
t1 <- as.data.frame(test2) %>% rename(Bathy = test2)
mydata2 <- mydata %>% bind_cols(t1)

mydata_NA <- mydata2 %>% filter(is.na(Bathy))
mydataX <- mydata2 %>% filter(!is.na(Bathy))

dat2 <- mydata_NA
coordinates(dat2) <- ~LongitudeDecimal+LatitudeDecimal

test3 <- raster::extract(bath2, dat2)

t2 <- as.data.frame(test3) %>% rename(Bathy = test3)
mydata3 <- mydata_NA %>% dplyr::select(-Bathy) %>% bind_cols(t2)

sum(nrow(mydata3), nrow(mydataX))
final_with_bathy <- bind_rows(mydata3, mydataX)
#write_csv(final_with_bathy, "DPI Data/OTLD_2009-2021_RAW lat_lons_R_bathy.csv")


### Attempt Bathy with Fab Package
#remotes::install_github('IMOS-AnimalTracking/remora', build_vignettes = TRUE, ref = "dev")
library(remora)

final_with_bathy2 <- 
  final_with_bathy %>% mutate(EventDate=lubridate::dmy(EventDate)) %>%
  extractEnv(X = "LongitudeDecimal", 
             Y = "LatitudeDecimal", 
             datetime = "EventDate", 
             env_var = "bathy", 
             cache_layers = TRUE,
             crop_layers = TRUE,
             fill_gaps = TRUE,
             full_timeperiod = FALSE,
             folder_name = "test",
             .parallel = TRUE)

plot(final_with_bathy2$bathy ~ final_with_bathy2$Bathy)


mydataB <- final_with_bathy2 %>% mutate(Month = lubridate::month((EventDate))) %>%
  filter(Month >= 6) %>% filter(Month <= 10)
ggplot(mydataB, aes(x=Bathy)) + geom_histogram(binwidth=10) +
  xlim(c(-250,100))

ggplot(mydataB, aes(x=bathy)) + geom_histogram(binwidth=10) +
  xlim(c(-250,100))

final_with_bathy2 <- final_with_bathy2 %>% rename(bathy250m = bathy, bathy_GEBCO=Bathy)
#write_csv(final_with_bathy2, "DPI Data/OTLD_2009-2021_RAW lat_lons_R_bathy.csv")

#### Can we get distance from coast
#Load map data
library(rgdal) 
library(sp)
Aus <- readOGR(dsn = "Shape files/australia",layer = "cstauscd_r")
#plot(Aus)
Aus_coast <- subset(Aus, FEAT_CODE != "sea" )
Aus_coast <- subset (Aus_coast, FEAT_CODE != "island")
unique(Aus_coast$FEAT_CODE)
head(Aus_coast)
plot(Aus_coast)

fdat <- read_csv("DPI Data/OTLD_2009-2021_RAW lat_lons_R_bathy.csv" ,lazy=F)
library(geosphere)
ff <- fdat %>% dplyr::select(Longitude, Latitude) %>% distinct()
ff <- as.matrix(ff)
tt<- dist2Line(p =  ff, line=Aus_coast)
tt2 <- as.data.frame(tt) %>% bind_cols(as.data.frame(ff)) %>% dplyr::select(-ID, -lat, -lon) %>% dplyr::rename(Distance_Coast = distance)
fdat2 <- full_join(fdat, tt2)
#write_csv(fdat2, "DPI Data/OTLD_2009-2021_RAW lat_lons_R_bathy_dist_coast.csv")
range(fdat2$Distance_Coast, na.rm=T)
hist(fdat2$Distance_Coast)
####