# DPI Mapping attempt

library(tidyverse)

mydata <- read_csv("DPI Data/OTLD_2009-2021_RAW lat_lons_R_bathy_dist_coast.csv")
range(mydata$EventDate)
mydata$Latitude <- round(mydata$LatitudeDecimal, 1) -0.05
mydata$Longitude <- round(mydata$LongitudeDecimal, 1) + 0.05

mydata2 <- mydata %>% group_by(Latitude, Longitude, CalendarYear) %>% summarise(Effort = sum(EffortQty, na.rm=T)) %>% filter(Latitude != -0.05)

library(rnaturalearth)
library(rnaturalearthhires)

world <- ne_states(country = 'australia', returnclass = "sf")


library(scales)

ggplot(mydata2, aes(x=Longitude, y = Latitude, fill=Effort)) + geom_tile()+
  geom_sf(data=world, col="black", fill = "grey80", inherit.aes = FALSE, size=0.2)+
  coord_sf(xlim = c(149, 154), ylim = c(-37.5, -28))+ theme_classic()+
  facet_wrap(~CalendarYear, nrow=2) +
  scale_fill_viridis_c(trans="log10", limits=c(1,1000), oob=squish)+
  theme(axis.text = element_text(colour="black", size=10),
        axis.title = element_text(face="bold", size=12))

#ggsave("Spatial Effort over time.pdf", dpi= 600, height = 21, width = 14.8*2, units = "cm")


## Limit to winter --- TO DO NEXT

mydata2 <- mydata %>% mutate(Month = lubridate::month((EventDate))) %>% 
  filter(Month >= 6) %>% filter(Month <= 10) %>% filter(CalendarYear != 2008) %>%
  group_by(Latitude, Longitude, CalendarYear) %>% 
  summarise(Effort = sum(EffortQty, na.rm=T)) %>% filter(Latitude != -0.05)

# library(rnaturalearth)
# library(rnaturalearthhires)
# 
# world <- ne_states(country = 'australia', returnclass = "sf")
# 
# 
# library(scales)

ggplot(mydata2, aes(x=Longitude, y = Latitude, fill=Effort)) + geom_tile()+
  geom_sf(data=world, col="black", fill = "grey80", inherit.aes = FALSE, size=0.2)+
  coord_sf(xlim = c(149, 154), ylim = c(-37.5, -28))+ theme_classic()+
  facet_wrap(~CalendarYear, nrow=2) +
  scale_fill_viridis_c(trans="sqrt", oob=squish, option="mako", direction = -1,
                       name="Number\nof trap\nlifts ('000)", labels=c(0,10,20,30,40))+
  scale_x_continuous(breaks= c(149,152))+
  #scale_fill_viridis_c(trans="log10", limits=c(1,1000), oob=squish)+
  theme(axis.text = element_text(colour="black", size=10),
        axis.title = element_text(face="bold", size=12),
        legend.position = c(0.93,0.2))

ggsave("Spatial Effort over time_winter only.pdf", dpi= 600, height = 21, width = 14.8*2, units = "cm")
ggsave("Spatial Effort over time_winter only.png", dpi= 600, height = 21, width = 14.8*2, units = "cm")


# Change over time

mydata_time <- mydata %>% mutate(Month = lubridate::month((EventDate))) %>% 
  filter(Month >= 6) %>% filter(Month <= 10) %>% filter(Latitude != -0.05) %>%
  group_by(CalendarYear) %>% 
  summarise(Effort = sum(EffortQty, na.rm=T)) %>% filter(CalendarYear != 2008) %>%
  filter(CalendarYear != 2021)

mydata_time

ggplot(mydata_time, aes(x= CalendarYear, y = Effort)) + geom_point()+
  geom_smooth(method="lm") + theme_classic()

### Total effort all years winter
mydata3 <- mydata %>% mutate(Month = lubridate::month((EventDate))) %>% 
  filter(Month >= 6) %>% filter(Month <= 10) %>%
  group_by(Latitude, Longitude) %>% 
  summarise(Effort = sum(EffortQty, na.rm=T)) %>% filter(Latitude != -0.05)

# get continental shelf
shelf <- read.csv("Hayden_200m_contour.csv", header = T)

p1 <- ggplot(mydata3, aes(x=Longitude, y = Latitude, fill=Effort)) + geom_tile()+
  geom_sf(data=world, col="black", fill = "grey80", inherit.aes = FALSE, size=0.2)+
  coord_sf(xlim = c(149, 154), ylim = c(-37.5, -28))+ theme_classic()+
  scale_x_continuous(breaks = c(149,151,153))+
  geom_path(data=shelf, aes(Var1,Var2),colour="black", size=0.3, inherit.aes = F, lty="dotted")+
  #facet_wrap(~CalendarYear, nrow=2) +
  scale_fill_viridis_c(trans="sqrt", oob=squish, option="mako", direction = -1,
                       name="Number\nof Traps\n('000)", labels=c(0,10,20,30,40))+ #, limits=c(1,1000)
  theme(axis.text = element_text(colour="black", size=10),
        axis.title = element_text(face="bold", size=12),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face="bold", size=12),
        legend.text = element_text(size=10),
        legend.position = c(0.88,0.23))
p1

### depth ranges - only use this data for bathy histogram, weigthed by effort
mydataBa <- mydata %>% mutate(Month = lubridate::month((EventDate))) %>%
  mutate(EffortQty = as.integer(EffortQty)) %>%
  filter(Month >= 6) %>% filter(Month <= 10) %>% uncount(weights= EffortQty)
p2 <- ggplot(mydataBa, aes(x=bathy250m*-1)) + geom_histogram(binwidth=20) +
  theme_classic() +
  ylab("Number of Trap\nLifts ('000)")+
  xlab("Bathymetry (m)")+
  scale_y_continuous(expand = c(0,0), labels=c(0,50,100,150), breaks=c(0,50000,100000,150000))+
  scale_x_continuous(expand = c(0,0), limits = c(-10,250))+
  theme(axis.text = element_text(colour="black", size=10),
        axis.title = element_text(face="bold", size=12),
        axis.ticks = element_line(colour="black"))
p2

ggsave("Trap depth weighted by effort.png", dpi=600, width=21, height = 14.8, units="cm")

# Facet by month
# mydata <- read_csv("DPI Data/OTLD_2009-2021_RAW lat_lons_R_bathy_dist_coast.csv")
# range(mydata$EventDate)
# mydata$Latitude <- round(mydata$LatitudeDecimal, 1) -0.05
# mydata$Longitude <- round(mydata$LongitudeDecimal, 1) + 0.05
# 
# 
# mydataBa2 <- mydata %>% mutate(Month = lubridate::month((EventDate))) %>%
#   mutate(EffortQty = as.integer(EffortQty)) %>%
#   uncount(weights= EffortQty)
# p2m <- ggplot(mydataBa2, aes(x=bathy250m*-1)) + geom_histogram(binwidth=20) + facet_wrap(~Month)+
#   theme_classic() +
#   ylab("Number of Trap\nLifts ('000)")+
#   xlab("Bathymetry (m)")+
#   scale_y_continuous(expand = c(0,0), labels=c(0,50,100,150), breaks=c(0,50000,100000,150000))+
#   scale_x_continuous(expand = c(0,0), limits = c(-10,250))+
#   theme(axis.text = element_text(colour="black", size=10),
#         axis.title = element_text(face="bold", size=12),
#         axis.ticks = element_line(colour="black"))
# p2m
# 
# ggsave("Trap bathymetry by month.png", dpi=600, width=28, height = 21, units="cm")

### repeat for distance from coast
mydataB <- mydata %>% mutate(Month = lubridate::month((EventDate))) %>%
  mutate(EffortQty = as.integer(EffortQty)) %>% filter(Distance_Coast/1000 <1000) %>%
  filter(Month >= 6) %>% filter(Month <= 10) %>% uncount(weights= EffortQty)
p2d <- ggplot(mydataB, aes(x=Distance_Coast/1000)) + geom_histogram(binwidth=5) +
  theme_classic() +
  ylab("Number of Trap\nLifts ('000)")+
  xlab("Distance from Coast (km)")+
  scale_y_continuous(expand = c(0,0), labels=c(0,50,100,150,200,250), breaks=c(0,50000,100000,150000,200000,250000))+
  scale_x_continuous(expand = c(0,0), limits = c(-5,120))+
  theme(axis.text = element_text(colour="black", size=10),
        axis.title = element_text(face="bold", size=12),
        axis.ticks = element_line(colour="black"))
p2d

ggsave("Trap Distance from coast weighted by effort.png", dpi=600, width=21, height = 14.8, units="cm")

###
p4 <- ggplot(mydataB, aes(x=Latitude)) + geom_histogram(binwidth=0.5) +
  theme_classic() +
  ylab("Number of Trap\nLifts ('000)")+
  xlab("Latitude")+
  scale_y_continuous(expand = c(0,0), labels=c(0,50,100,150,200), breaks=c(0,50000,100000,150000,200000))+
  scale_x_continuous(expand = c(0,0), limits = c(-38,-27.5), breaks=seq(-38,-28,2))+
  theme(axis.text = element_text(colour="black", size=10),
        axis.title = element_text(face="bold", size=12),
        axis.ticks = element_line(colour="black")) +
  coord_flip()
p4

# ## now unweighted
# mydataB2 <- mydata %>% mutate(Month = lubridate::month((EventDate))) %>%
#   mutate(EffortQty = as.integer(EffortQty)) %>%
#   filter(Month >= 6) %>% filter(Month <= 10) #%>% uncount(weights= EffortQty)
# ggplot(mydataB2, aes(x=bathy250m)) + geom_histogram(binwidth=10) +
#   xlim(c(-250,10))
# ggsave("Trap depth unweighted effort.png", dpi=600, width=21, height = 14.8, units="cm")


### make inset of australia
p3 <- ggplot() + 
  geom_sf(data=world, col="black", fill = "grey80", inherit.aes = FALSE, size=0.2)+
  coord_sf(xlim = c(114, 154), ylim = c(-43, -10))+ theme_classic()+
  geom_rect(aes(xmin=149, xmax=154, ymin=-37.5, ymax=-28), fill=NA, colour="red")+
  #scale_fill_viridis_c(trans="sqrt", oob=squish, option="mako", direction = -1,
  #                     name="Number\nof Traps\n('000)", labels=c(0,10,20,30,40))+ #, limits=c(1,1000)
  theme(axis.text.x =element_blank(),axis.text.y= element_blank(), axis.ticks=element_blank(),axis.title.x =element_blank(),
        axis.title.y= element_blank(), plot.background = element_rect(fill = "transparent", color = NA), 
        panel.background =  element_rect(fill = "white", color = "black"))
p3


library(patchwork)
p1+ inset_element(p3, left = -0.05, bottom = 0.7, right = 0.5, top = 1.1,
                  align_to = "panel", ignore_tag = TRUE) + p2 + plot_annotation(tag_levels = "a") 
ggsave("Figure 2.png", dpi=600, units = "cm", width=21, height=14.8)
ggsave("Figure 2.pdf", dpi=600, units = "cm", width=21, height=14.8)


p4+p1 + inset_element(p3, left = -0.05, bottom = 0.7, right = 0.5, top = 1.1,
                      align_to = "panel", ignore_tag = TRUE) +
p2d +p2 + plot_annotation(tag_levels = "a")
ggsave("Figure 2v3.png", dpi=600, units = "cm", width=21, height=21)
ggsave("Figure 2v3.pdf", dpi=600, units = "cm", width=21, height=21)



#### Rope Days

table(mydata$AreaName)

## Step 1 - mean number of traps lifted each day in each zone
daily_mean_lifts <- mydata %>% group_by(AreaName, EventDate, FishingBusinessOwner) %>% 
  summarise(mean_daily_lifts = mean(EffortQty, na.rm=T)) %>% ungroup() %>%
  mutate(Month = lubridate::month(EventDate), Year = lubridate::year(EventDate), 
         month_Year = paste0(Month,"_",Year)) %>% group_by(AreaName, month_Year, Month) %>%
  summarise(mean_daily_lifts = mean(mean_daily_lifts, na.rm=T)) %>% ungroup() %>%
  group_by(AreaName, Month) %>% summarise(mean_daily_lifts = mean(mean_daily_lifts, na.rm=T)) %>%
  ungroup() %>% group_by(AreaName) %>% summarise(mean_daily_lifts = mean(mean_daily_lifts, na.rm=T))

write_csv(daily_mean_lifts, "DPI Data/mean trap lifts per day per region.csv")

# step 2 - calculate number of unique fishers per zone each month
mydata <- mydata %>% mutate(Month = lubridate::month(EventDate), Year = lubridate::year(EventDate), 
                             month_Year = paste0(Month,"_",Year))
# make each day unique
mydata2 <- mydata %>% distinct(month_Year, AreaName, FishingBusinessOwner, EventDate, .keep_all = T)

# Remove fishers only there 1 time in the month
mydata3 <- mydata2 %>% group_by(month_Year, AreaName, FishingBusinessOwner) %>% summarise(n=n()) %>%
  filter(n == 1)

mydata4 <- anti_join(mydata2, mydata3)

# Count unique fisheries per zone each month
mydata5 <- mydata4 %>% ungroup() %>% group_by(AreaName, month_Year) %>%
  summarise(unique_fishers = n_distinct(FishingBusinessOwner)) %>% filter(AreaName != "unknown")

# mutliply fishers by mean number of traps each day per zone
mydata6 <- mydata5 %>% left_join(daily_mean_lifts)

# calculate rope days
mydata6 <- mydata6 %>% mutate(RopeDays = unique_fishers * mean_daily_lifts,
                              Date = lubridate::dmy(paste0("15_",month_Year)))

write_csv(mydata6, "DPI Data/Rope Days 2009 onward.csv")

ggplot(mydata6, aes(Date, RopeDays)) + geom_line() + theme_classic() + facet_wrap(~AreaName)
ggsave("DPI Data/Rope Days 2009 onward.png", dpi=600, width = 25, height=14.8, units="cm")
