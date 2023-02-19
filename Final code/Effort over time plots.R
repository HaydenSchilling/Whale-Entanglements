## Effort plots over time

library(tidyverse)

mydata <- read_csv("19102021_ComC Results_raw.csv", lazy = F)

monthly_summary <- mydata %>% group_by(CalendarYear, MonthMM) %>%
  summarise(Total_Days_Effort = sum(SumDaysFishedScaled, na.rm=T)) %>% # Scaled is the good one to use
  mutate(Date = lubridate::dmy(paste0("15/",MonthMM,"/",CalendarYear))) %>%
  rename(Month = MonthMM, Year = CalendarYear) %>% filter(Year != 2021)

p1 <-
  ggplot(monthly_summary, aes(Date, y = Total_Days_Effort)) + geom_line() +
  theme_classic() + theme(
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(face = "bold", size = 14)
  ) +
  scale_x_date(
    limits = c(as.Date("1997-01-01"), as.Date("2021-01-01")),
    breaks = c(
      as.Date("1998-01-01"),
      as.Date("2002-01-01"),
      as.Date("2006-01-01"),
      as.Date("2010-01-01"),
              as.Date("2014-01-01"), as.Date("2018-01-01")),
    labels=c("1998","2002","2006","2010","2014","2018")
    ) +
      ylab("Total Fisher Days\n(Monthly)"
      )
p1

winter_monthly <- monthly_summary %>% filter(Month > 5) %>% filter(Month <11) %>% group_by(Year) %>%
  summarise(Total_Winter_Effort = sum(Total_Days_Effort))

p2 <- ggplot(winter_monthly, aes(Year, y = Total_Winter_Effort)) + geom_line() +
  theme_classic() + theme(axis.text = element_text(colour="black", size=12),
                          axis.title = element_text(face="bold", size=14)) +
  scale_x_continuous(breaks = seq(1998,2020,4), limits=c(1997,2020))+
  ylab("Total Fisher Days\n(June - October)")
p2


library(patchwork)
p1+p2 + plot_layout(ncol=1)

#ggsave("Effort over time.png", dpi=600, units="cm", height=14.8, width=21)
#ggsave("Effort over time.pdf", dpi=600, units="cm", height=14.8, width=21)


### Attempt double axis plot

coeff <- max(winter_monthly$Total_Winter_Effort)/max(monthly_summary$Total_Days_Effort)
winter_monthly$Date <- lubridate::dmy(paste0("1/7/",winter_monthly$Year))

p1 + scale_y_continuous(sec.axis = sec_axis(~.*coeff, name="Total Fisher Days\n(June - October)")) +
  geom_line(data=winter_monthly, aes(Date, y = Total_Winter_Effort/coeff), col="red", linetype="dashed")+
  theme(axis.text.y.right = element_text(size=12, colour="red"),
        axis.title.y.right = element_text(face="bold", size=14, colour="red"))


#ggsave("Effort over time single panel.png", dpi=600, units="cm", height=14.8, width=21)
#ggsave("Effort over time single panel.pdf", dpi=600, units="cm", height=14.8, width=21)
# 
# 
##### Rope Days attempt
# load mean lifts per day
lpd <- read_csv("DPI Data/mean trap lifts per day per region.csv")

mydata2 <- mydata %>% rename(AreaName=Area) %>% left_join(lpd)

# Remove single fishing days and add column for number of days

mydata3 <- mydata2 %>% filter(SumDaysFished != 1) %>% 
  filter(SumDaysFishedScaled != 1) %>% 
  mutate(DaysInMonth = case_when(MonthMM %in% c(1, 3, 5,7, 8,10,12) ~ 31,
                                 MonthMM %in% c(4, 6,7, 9,11) ~ 30,
                                 TRUE ~ 28)) %>%
  distinct(FishingBusinessID, AreaName, CalendarYear, MonthMM, .keep_all = T)
### Calculate Rope Days
mydata4 <- mydata3 %>% mutate(RopeDays = DaysInMonth*mean_daily_lifts) %>% #*DaysInMonth this was previously in twice - causing erroneous result
  group_by(CalendarYear, MonthMM, AreaName) %>%
  summarise(TotalRopeDays = sum(RopeDays, na.rm=T)) %>% 
  mutate(Date = lubridate::dmy(paste0("15/",MonthMM,"/",CalendarYear))) %>%
  filter(AreaName != "28° 9' N Into Qld Waters") %>% 
  filter(AreaName != "37 Deg. 30 Min. Latitude South Into Victorian Waters")


ggplot(mydata4, aes(x=Date, y = TotalRopeDays/1000)) + geom_line() +
  facet_wrap(~AreaName) + theme_classic() +
  ylab("Total Rope Days ('000)")+
  theme(axis.text = element_text(colour="black", size=12),
        axis.ticks = element_line(colour = "black"),
        axis.title = element_text(face="bold", size=14),
        strip.text = element_text(colour="black", size=9),
        panel.border = element_rect(fill=NA, colour = "black"))
ggsave("Rope Days by zone.png", width=29, height=21, units="cm", dpi=600)

mydata5 <- mydata4 %>% ungroup() %>% group_by(CalendarYear, MonthMM, Date) %>%
  summarise(TotalRopeDays = sum(TotalRopeDays, na.rm=T))

ggplot(mydata5, aes(x=Date, y = TotalRopeDays)) + geom_line() #+

write_csv(mydata5, "DPI Data/RopeDays Monthly long term.csv")

# Winter only
winter_monthly <- mydata5 %>% filter(MonthMM > 5) %>% filter(MonthMM <11) %>% group_by(CalendarYear) %>%
  summarise(Total_Winter_RopeDays = sum(TotalRopeDays))


p1 <-
  ggplot(mydata5, aes(Date, y = TotalRopeDays/1000)) + geom_line() +
  theme_classic() + theme(
    axis.text = element_text(colour = "black", size = 12),
    axis.title = element_text(face = "bold", size = 14)
  ) +
  scale_x_date(
    limits = c(as.Date("1997-01-01"), as.Date("2021-01-01")),
    breaks = c(
      as.Date("1998-01-01"),
      as.Date("2002-01-01"),
      as.Date("2006-01-01"),
      as.Date("2010-01-01"),
      as.Date("2014-01-01"), as.Date("2018-01-01")),
    labels=c("1998","2002","2006","2010","2014","2018")
  ) +
  ylab("Total Rope Days\n('000, Monthly)"
  )
p1




coeff <- max(winter_monthly$Total_Winter_RopeDays)/max(mydata5$TotalRopeDays)
winter_monthly$Date <- lubridate::dmy(paste0("1/7/",winter_monthly$CalendarYear))

p1 + scale_y_continuous(sec.axis = sec_axis(~.*coeff, name="Total Rope Days\n('000, June - October)")) +
  geom_line(data=winter_monthly, aes(Date, y = (Total_Winter_RopeDays/1000)/coeff), col="red", linetype="dashed")+
  theme(axis.text.y.right = element_text(size=12, colour="red"),
        axis.title.y.right = element_text(face="bold", size=14, colour="red"))


ggsave("RopeDays over time single panel.png", dpi=600, units="cm", height=14.8, width=21)
ggsave("RopeDays over time single panel.pdf", dpi=600, units="cm", height=14.8, width=21)


write_csv(winter_monthly, "DPI Data/Rope Days Winter Annual.csv")
# 