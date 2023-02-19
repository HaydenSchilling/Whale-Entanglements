# Annual plots for Amandine

library(tidyverse)
## SST
mydata <- read_csv("SST_monthly test.csv") %>% filter(Month>5) %>% filter(Month<11) %>%
  group_by(Year) %>% summarise(SST_annual_mean = mean(SST_mean, na.rm=T), SST_SD = sd(SST_mean, na.rm=T), n=n(),
                               SST_SE = SST_SD/sqrt(n))

ggplot(mydata, aes(Year, SST_annual_mean)) + geom_line() + theme_classic() + ylab("June-Oct Mean SST (+SE)") +
  theme(axis.text=element_text(colour="black", size=12),
         axis.title =element_text(face="bold", size=14)) +
  geom_errorbar(aes(ymin=SST_annual_mean-SST_SE,ymax=SST_annual_mean+SST_SE))+
  scale_x_continuous(breaks=seq(1992,2020,4))
  
ggsave("Annual winter SST.png", dpi=600, width=21, height=14.8, units="cm")

### EAC Distance from Coast
EAC <- read_csv("EAC Shoreward Intrusion (Area & Distance) 1992 to 2018.csv")
EAC <- EAC %>% mutate(`...4` = NULL, `...5` = NULL,`...6` = NULL,
                      Date = lubridate::ym(`Date (yyyymm)`),
                      Year = as.integer(lubridate::year(Date)),
                      Month = lubridate::month(Date),
                      Distance = `Distance (km)`,
                      Area = `Area (km2)`) %>%
  filter(Month>5) %>% filter(Month<11) %>% ungroup() %>%
  dplyr::group_by(Year) %>% summarise(EAC_Distance = mean(Distance, na.rm=T), Distance_SD = sd(Distance, na.rm=T), n=n(),
                               Distance_SE = EAC_Distance/sqrt(n))

ggplot(EAC, aes(Year, EAC_Distance)) + geom_line() + theme_classic() + ylab("June-Oct Mean EAC Distance from Coast") +
  theme(axis.text=element_text(colour="black", size=12),
        axis.title =element_text(face="bold", size=14)) +
  #geom_errorbar(aes(ymin=EAC_Distance-Distance_SE,ymax=EAC_Distance+Distance_SE))+
  scale_x_continuous(breaks=seq(1992,2020,4))

ggsave("Annual winter EAC Distance.png", dpi=600, width=21, height=14.8, units="cm")


### Heatwaves
hdat <- read_csv("Heatwave modelling Data.csv") %>% 
  filter(Month>5) %>% filter(Month<11) %>% ungroup() %>%
  dplyr::group_by(Year) %>% summarise(Heatwave_Duration = sum(total_duration, na.rm=T))

ggplot(hdat, aes(Year, Heatwave_Duration)) + geom_line() + theme_classic() + ylab("June-Oct Total Heatwave Duration (days)") +
  theme(axis.text=element_text(colour="black", size=12),
        axis.title =element_text(face="bold", size=14)) +
  #geom_errorbar(aes(ymin=EAC_Distance-Distance_SE,ymax=EAC_Distance+Distance_SE))+
  scale_x_continuous(breaks=seq(1982,2020,4))
ggsave("Annual winter Heatwave Duration.png", dpi=600, width=21, height=14.8, units="cm")


### velocity
vcur_dat <- read_csv("VCUR_monthly test.csv")  %>% 
  filter(Month>5) %>% filter(Month<11) %>% ungroup() %>%
  dplyr::group_by(Year) %>% summarise(VCUR = mean(VCUR_mean, na.rm=T),
                                      UCUR = mean(UCUR_mean, na.rm=T))
ggplot(vcur_dat, aes(Year, UCUR)) + geom_line() + theme_classic() + ylab("June-Oct U-Current (m/s)") +
  theme(axis.text=element_text(colour="black", size=12),
        axis.title =element_text(face="bold", size=14)) +
  #geom_errorbar(aes(ymin=EAC_Distance-Distance_SE,ymax=EAC_Distance+Distance_SE))+
  scale_x_continuous(breaks=seq(1982,2020,4))
ggsave("Annual winter UCUR.png", dpi=600, width=21, height=14.8, units="cm")


ggplot(vcur_dat, aes(Year, VCUR)) + geom_line() + theme_classic() + ylab("June-Oct V-Current (m/s)") +
  theme(axis.text=element_text(colour="black", size=12),
        axis.title =element_text(face="bold", size=14)) +
  #geom_errorbar(aes(ymin=EAC_Distance-Distance_SE,ymax=EAC_Distance+Distance_SE))+
  scale_x_continuous(breaks=seq(1982,2020,4))
ggsave("Annual winter VCUR.png", dpi=600, width=21, height=14.8, units="cm")


### velocity separation zone
vcur_dat <- read_csv("VCUR_sep_monthly test2.csv", lazy = F)  %>% 
  filter(Month>5) %>% filter(Month<11) %>% ungroup() %>%
  dplyr::group_by(Year) %>% summarise(VCUR = mean(VCUR_mean_Sep, na.rm=T),
                                      UCUR = mean(UCUR_mean_Sep, na.rm=T))
ggplot(vcur_dat, aes(Year, UCUR)) + geom_line() + theme_classic() + ylab("June-Oct U-Current (m/s)") +
  theme(axis.text=element_text(colour="black", size=12),
        axis.title =element_text(face="bold", size=14)) +
  #geom_errorbar(aes(ymin=EAC_Distance-Distance_SE,ymax=EAC_Distance+Distance_SE))+
  scale_x_continuous(breaks=seq(1982,2020,4))
ggsave("Annual winter UCUR Sep.png", dpi=600, width=21, height=14.8, units="cm")


ggplot(vcur_dat, aes(Year, VCUR)) + geom_line() + theme_classic() + ylab("June-Oct V-Current (m/s)") +
  theme(axis.text=element_text(colour="black", size=12),
        axis.title =element_text(face="bold", size=14)) +
  #geom_errorbar(aes(ymin=EAC_Distance-Distance_SE,ymax=EAC_Distance+Distance_SE))+
  scale_x_continuous(breaks=seq(1982,2020,4))
ggsave("Annual winter VCUR Sep.png", dpi=600, width=21, height=14.8, units="cm")
