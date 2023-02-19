# Velocity Prep

library(tidync)
library(tidyverse)
 # library(ncdf4)
 # test <- nc_open("IMOS SSH/SSH_Delayed_Mode.nc")
 # test
 # nc_close(test)

mydata <- tidync("IMOS SSH/SSH_Delayed_Mode01.nc") %>% hyper_tibble()

mydata <- mydata %>% mutate(Date = as.Date(as.POSIXct(TIME*24*60*60, origin = "1985-01-01") ))

library(remora)
mydata <- 
  mydata %>% #mutate(EventDate=Date) %>%
  extractEnv(X = "LONGITUDE", 
             Y = "LATITUDE", 
             datetime = "Date", 
             env_var = "bathy", 
             cache_layers = F,
             crop_layers = TRUE,
             fill_gaps = TRUE,
             full_timeperiod = FALSE,
             folder_name = "test",
             .parallel = TRUE)




sum <- mydata %>%  filter(LATITUDE > -32.25) %>% filter(bathy > -250) %>%
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date)) %>%
  filter(Month >5) %>% filter(Month<11) %>%
  group_by(LONGITUDE, LATITUDE, Year) %>% summarise(VCUR_mean = mean(VCUR, na.rm=T), UCUR_mean = mean(UCUR, na.rm=T)) #%>% 
  #mutate(Month = lubridate::month(Date), Year = lubridate::year(Date)) %>% select(-Date)

  
ggplot(sum, aes(LONGITUDE, LATITUDE, fill=VCUR_mean, col=VCUR_mean)) + geom_tile() + facet_wrap(~Year, nrow=3) +
  scale_colour_gradient2(low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    space = "Lab",
    na.value = "grey50",
    guide = "colourbar",
    aesthetics = "colour"
  ) + scale_fill_gradient2(low = "blue",
                             mid = "white",
                             high = "red",
                             midpoint = 0,
                             space = "Lab",
                             na.value = "grey50",
                             guide = "colourbar",
                             aesthetics = "fill"
  ) + coord_quickmap() + theme(legend.position = "bottom")
  
ggsave("VCUR test_north.png", dpi =600, units="cm", width = 25)

# June/July only
sum <- mydata %>%  filter(LATITUDE > -32.25) %>% filter(bathy > -250) %>%
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date)) %>%
  filter(Month >5) %>% filter(Month<8) %>%
  group_by(LONGITUDE, LATITUDE, Year) %>% summarise(VCUR_mean = mean(VCUR, na.rm=T), UCUR_mean = mean(UCUR, na.rm=T)) #%>% 
#mutate(Month = lubridate::month(Date), Year = lubridate::year(Date)) %>% select(-Date)


ggplot(sum, aes(LONGITUDE, LATITUDE, fill=VCUR_mean, col=VCUR_mean)) + geom_tile() + facet_wrap(~Year, nrow=3) +
  scale_colour_gradient2(low = "blue",
                         mid = "white",
                         high = "red",
                         midpoint = 0,
                         space = "Lab",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "colour"
  ) + scale_fill_gradient2(low = "blue",
                           mid = "white",
                           high = "red",
                           midpoint = 0,
                           space = "Lab",
                           na.value = "grey50",
                           guide = "colourbar",
                           aesthetics = "fill"
  ) + coord_quickmap() + theme(legend.position = "bottom")

ggsave("VCUR test_north june_july.png", dpi =600, units="cm", width = 25)

# Aug/Oct only
sum <- mydata %>%  filter(LATITUDE > -32.25) %>% filter(bathy > -250) %>%
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date)) %>%
  filter(Month >7) %>% filter(Month<10) %>%
  group_by(LONGITUDE, LATITUDE, Year) %>% summarise(VCUR_mean = mean(VCUR, na.rm=T), UCUR_mean = mean(UCUR, na.rm=T)) #%>% 
#mutate(Month = lubridate::month(Date), Year = lubridate::year(Date)) %>% select(-Date)


ggplot(sum, aes(LONGITUDE, LATITUDE, fill=VCUR_mean, col=VCUR_mean)) + geom_tile() + facet_wrap(~Year, nrow=3) +
  scale_colour_gradient2(low = "blue",
                         mid = "white",
                         high = "red",
                         midpoint = 0,
                         space = "Lab",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "colour"
  ) + scale_fill_gradient2(low = "blue",
                           mid = "white",
                           high = "red",
                           midpoint = 0,
                           space = "Lab",
                           na.value = "grey50",
                           guide = "colourbar",
                           aesthetics = "fill"
  ) + coord_quickmap() + theme(legend.position = "bottom")

ggsave("VCUR test_north aug_sept.png", dpi =600, units="cm", width = 25)



# prepare actual data
sum <- mydata %>% filter(LATITUDE > -32.25) %>% filter(bathy > -250) %>%
  mutate(Month = lubridate::month(Date), Year = lubridate::year(Date)) %>%
  group_by(Month, Year, LATITUDE) %>% filter(LONGITUDE == min(LONGITUDE)) %>%
  ungroup() %>% group_by(Month, Year) %>%
  summarise(VCUR_mean = mean(VCUR, na.rm=T), UCUR_mean = mean(UCUR, na.rm=T))


write_csv(sum, "VCUR_monthly test02.csv")


### Now do separation zone

sum <- mydata %>%  filter(LATITUDE < -32.25) %>% filter (LATITUDE > -35) %>% filter(bathy > -250) %>%
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date)) %>%
  filter(Month >7) %>% filter(Month<11) %>%
  group_by(LONGITUDE, LATITUDE, Year) %>% summarise(VCUR_mean = mean(VCUR, na.rm=T), UCUR_mean = mean(UCUR, na.rm=T))

ggplot(sum, aes(LONGITUDE, LATITUDE, fill=VCUR_mean, col=VCUR_mean)) + geom_tile() + facet_wrap(~Year, nrow=3) +
  scale_colour_gradient2(low = "blue",
                         mid = "white",
                         high = "red",
                         midpoint = 0,
                         space = "Lab",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "colour"
  ) + scale_fill_gradient2(low = "blue",
                           mid = "white",
                           high = "red",
                           midpoint = 0,
                           space = "Lab",
                           na.value = "grey50",
                           guide = "colourbar",
                           aesthetics = "fill"
  ) + coord_quickmap() + theme(legend.position = "bottom")

ggsave("VCUR test_south.png", dpi =600, units="cm", width = 25)


# June/July only
sum <- mydata %>%  filter(LATITUDE < -32.25) %>% filter (LATITUDE > -35) %>% filter(bathy > -250) %>%
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date)) %>%
  filter(Month >5) %>% filter(Month<8) %>%
  group_by(LONGITUDE, LATITUDE, Year) %>% summarise(VCUR_mean = mean(VCUR, na.rm=T), UCUR_mean = mean(UCUR, na.rm=T)) #%>% 
#mutate(Month = lubridate::month(Date), Year = lubridate::year(Date)) %>% select(-Date)


ggplot(sum, aes(LONGITUDE, LATITUDE, fill=VCUR_mean, col=VCUR_mean)) + geom_tile() + facet_wrap(~Year, nrow=3) +
  scale_colour_gradient2(low = "blue",
                         mid = "white",
                         high = "red",
                         midpoint = 0,
                         space = "Lab",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "colour"
  ) + scale_fill_gradient2(low = "blue",
                           mid = "white",
                           high = "red",
                           midpoint = 0,
                           space = "Lab",
                           na.value = "grey50",
                           guide = "colourbar",
                           aesthetics = "fill"
  ) + coord_quickmap() + theme(legend.position = "bottom")

ggsave("VCUR test_south june_july.png", dpi =600, units="cm", width = 25)

# Aug/Oct only
sum <- mydata %>%  filter(LATITUDE < -32.25) %>% filter (LATITUDE > -35) %>% filter(bathy > -250) %>%
  mutate(Year = lubridate::year(Date), Month = lubridate::month(Date)) %>%
  filter(Month >7) %>% filter(Month<11) %>%
  group_by(LONGITUDE, LATITUDE, Year) %>% summarise(VCUR_mean = mean(VCUR, na.rm=T), UCUR_mean = mean(UCUR, na.rm=T)) #%>% 
#mutate(Month = lubridate::month(Date), Year = lubridate::year(Date)) %>% select(-Date)


ggplot(sum, aes(LONGITUDE, LATITUDE, fill=VCUR_mean, col=VCUR_mean)) + geom_tile() + facet_wrap(~Year, nrow=3) +
  scale_colour_gradient2(low = "blue",
                         mid = "white",
                         high = "red",
                         midpoint = 0,
                         space = "Lab",
                         na.value = "grey50",
                         guide = "colourbar",
                         aesthetics = "colour"
  ) + scale_fill_gradient2(low = "blue",
                           mid = "white",
                           high = "red",
                           midpoint = 0,
                           space = "Lab",
                           na.value = "grey50",
                           guide = "colourbar",
                           aesthetics = "fill"
  ) + coord_quickmap() + theme(legend.position = "bottom")

ggsave("VCUR test_south aug_oct.png", dpi =600, units="cm", width = 25)




### prepare actual data
sum <- mydata %>% filter(LATITUDE < -32.25) %>% filter (LATITUDE > -35) %>% filter(bathy > -250) %>%
  mutate(Month = lubridate::month(Date), Year = lubridate::year(Date)) %>%
  group_by(Month, Year, LATITUDE) %>% filter(LONGITUDE == min(LONGITUDE)) %>%
  ungroup() %>% group_by(Month, Year) %>%
  summarise(VCUR_mean_Sep = mean(VCUR, na.rm=T), UCUR_mean_Sep = mean(UCUR, na.rm=T))



write_csv(sum, "VCUR_sep_monthly test02.csv")




#### Can i get a time-series of a single cell
table(mydata$LATITUDE)

syd_single <- mydata %>% filter(LATITUDE == -33.8) %>% drop_na(VCUR) %>%
  filter(LONGITUDE == min(LONGITUDE)) %>% mutate(Month = lubridate::month(Date), Year = lubridate::year(Date)) %>%
  group_by(Year, Month) %>% summarise(VCUR_mean = mean(VCUR, na.rm=T)) %>%
  mutate(Date = lubridate::dmy(paste0("15/",Month,"/",Year)))
head(syd_single)

ggplot(syd_single, aes(x=Date, y = VCUR_mean)) + geom_line() + theme_classic()+
  geom_hline(aes(yintercept=0), col= "red")+ theme(axis.text = element_text(colour="black"))+
  ggtitle("Sydney (33.8 deg S) Single cell time-series")
ggsave("Sydney coastal cell monthly timeseries.png", dpi = 600, width = 21, height=14.8, units="cm")

## June-July only
syd_single_jun_july <- mydata %>% filter(LATITUDE == -33.8) %>% drop_na(VCUR) %>%
  filter(LONGITUDE == min(LONGITUDE)) %>% mutate(Month = lubridate::month(Date), Year = lubridate::year(Date)) %>%
  filter(Month >5) %>% filter(Month<8) %>%
  group_by(Year) %>% summarise(VCUR_mean = mean(VCUR, na.rm=T))
ggplot(syd_single_jun_july, aes(x=Year, y = VCUR_mean)) + geom_line() + theme_classic()+
  geom_hline(aes(yintercept=0), col= "red")+ theme(axis.text = element_text(colour="black"))+
  ggtitle("Sydney (33.8 deg S) Single cell time-series (June-July mean)")
ggsave("Sydney coastal cell timeseries June_July_mean.png", dpi = 600, width = 21, height=14.8, units="cm")


## Aug-Sept only
syd_single_jun_july <- mydata %>% filter(LATITUDE == -33.8) %>% drop_na(VCUR) %>%
  filter(LONGITUDE == min(LONGITUDE)) %>% mutate(Month = lubridate::month(Date), Year = lubridate::year(Date)) %>%
  filter(Month >7) %>% filter(Month<10) %>%
  group_by(Year) %>% summarise(VCUR_mean = mean(VCUR, na.rm=T))
ggplot(syd_single_jun_july, aes(x=Year, y = VCUR_mean)) + geom_line() + theme_classic()+
  geom_hline(aes(yintercept=0), col= "red")+ theme(axis.text = element_text(colour="black"))+
  ggtitle("Sydney (33.8 deg S) Single cell time-series (Aug-Sept mean)")
ggsave("Sydney coastal cell timeseries Aug_Sept_mean.png", dpi = 600, width = 21, height=14.8, units="cm")


## Coffs
coffs_single <- mydata %>% filter(LATITUDE == -30) %>% drop_na(VCUR) %>%
  filter(LONGITUDE == min(LONGITUDE)) %>% mutate(Month = lubridate::month(Date), Year = lubridate::year(Date)) %>%
  group_by(Year, Month) %>% summarise(VCUR_mean = mean(VCUR, na.rm=T)) %>%
  mutate(Date = lubridate::dmy(paste0("15/",Month,"/",Year)))
head(coffs_single)

ggplot(coffs_single, aes(x=Date, y = VCUR_mean)) + geom_line() + theme_classic()+
  geom_hline(aes(yintercept=0), col= "red") + theme(axis.text = element_text(colour="black"))+
  ggtitle("Coffs Harbour (30 deg S) Single cell time-series")
ggsave("Coffs coastal cell monthly timeseries.png", dpi = 600, width = 21, height=14.8, units="cm")

coffs_single_jun_july <- mydata %>% filter(LATITUDE == -30) %>% drop_na(VCUR) %>%
  filter(LONGITUDE == min(LONGITUDE)) %>% mutate(Month = lubridate::month(Date), Year = lubridate::year(Date)) %>%
  filter(Month >5) %>% filter(Month<8) %>%
  group_by(Year) %>% summarise(VCUR_mean = mean(VCUR, na.rm=T))
ggplot(coffs_single_jun_july, aes(x=Year, y = VCUR_mean)) + geom_line() + theme_classic()+
  geom_hline(aes(yintercept=0), col= "red")+ theme(axis.text = element_text(colour="black"))+
  ggtitle("Coffs Harbour (30 deg S) Single cell time-series (June-July mean)")
ggsave("Coffs coastal cell timeseries June_July_mean.png", dpi = 600, width = 21, height=14.8, units="cm")


coffs_single_Aug_Sept <- mydata %>% filter(LATITUDE == -30) %>% drop_na(VCUR) %>%
  filter(LONGITUDE == min(LONGITUDE)) %>% mutate(Month = lubridate::month(Date), Year = lubridate::year(Date)) %>%
  filter(Month >7) %>% filter(Month<11) %>%
  group_by(Year) %>% summarise(VCUR_mean = mean(VCUR, na.rm=T))
ggplot(coffs_single_Aug_Sept, aes(x=Year, y = VCUR_mean)) + geom_line() + theme_classic()+
  geom_hline(aes(yintercept=0), col= "red")+ theme(axis.text = element_text(colour="black"))+
  ggtitle("Coffs Harbour (30 deg S) Single cell time-series (Aug-Sept mean)")
ggsave("Coffs coastal cell timeseries Aug_Sept_mean.png", dpi = 600, width = 21, height=14.8, units="cm")
