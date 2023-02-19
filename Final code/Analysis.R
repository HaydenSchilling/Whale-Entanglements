library(tidyverse)
library(DHARMa)
library(gratia)
library(patchwork)
library(ggrepel)
full_long <- read_csv("Final Analysis Data.csv", lazy=F)
winter_monthly <- read_csv("DPI Data/Rope Days Winter Annual.csv") %>% select(-Date) %>% rename(Year = CalendarYear) %>%
  filter(Year != 2021)

full_long_plot <- full_long %>% left_join(winter_monthly) %>%
  select(Year, Rel_Abund, Total_Winter_RopeDays) %>% distinct(.keep_all = T) %>% drop_na

ggplot(full_long_plot, aes(x=Total_Winter_RopeDays/1000, y = Rel_Abund)) + geom_point() + geom_text_repel(aes(label=Year))+
  theme_classic() + theme(axis.text = element_text(colour="black", size=12),
                          axis.title = element_text(face="bold", size=14))+
  labs(y = "Estimated Whale Abundance", x = "Winter Fishing Effort\nRope Days ('000)")
ggsave("Rope Days v Abundance.png", dpi=600, units = "cm", height=14.8, width=21)

cor(full_long_plot$Rel_Abund, full_long_plot$Total_Winter_RopeDays)

full_long_small <- full_long %>% select(-1,-6,-7,-8,-11,16,-17,-18,-25)
#psych::pairs.panels(full_long_small)

plot(full_long$Rel_Abund ~ full_long$Total_Days_Effort) # can't have effort and abund in same model... Test to see which is better

fff <- full_long %>% drop_na(Total_Days_Effort)

# Key parameters are Month, VCUR, Prev_year_SOI, Distance, Dist_Coffs, Dist_Sydney
# also need entangelments and Rel_Abund,

modelling_data1 <- full_long %>% select(Rel_Abund, Month, Whales_Caught, VCUR_mean, VCUR_mean_Sep,
                                        dist_edge_Coffs, dist_edge_Sydney, prev_year_SOI, SST_sep_anom,
                                        SST_anom) # , Distance

psych::pairs.panels(modelling_data1) 
### VCUR_mean and dist_coffs 0.6 correlation, as is dists at coffs

modelling_data2 <- modelling_data1 %>% drop_na() %>% mutate(VCUR_mean = VCUR_mean*-1, VCUR_mean_Sep = VCUR_mean_Sep*-1)

#### GAM
library(mgcv)
# g0 <- gam(Whales_Caught ~  1, # Null Model
#           offset= log(Rel_Abund),
#           method="ML",
#           #ziformula=~1,
#           data=modelling_data2, family = "nb")
# 
# resids <- simulateResiduals(g0)
# plot(resids)
# AIC(g0) # 588.22
# 
# 
# g1 <- gam(Whales_Caught ~  s(Month, k = 5,  bs=c("cc")), # Just Month (cyclical)
#           offset= log(Rel_Abund),
#           method="ML",
#           #ziformula=~1,
#           data=modelling_data2, family = "nb")
# AIC(g1) # 449.86
# 
# 
# g2 <- gam(Whales_Caught ~  te(VCUR_mean, Month, k = 5, m = 1, bs=c("cr","cc")), # VCUR and month
#           offset= log(Rel_Abund),
#           method="ML",
#           #ziformula=~1,
#           data=modelling_data2, family = "nb")
# AIC(g2) # 450.86
# 
# draw(g2)
# 
# g3 <- gam(Whales_Caught ~  te(VCUR_mean_Sep, Month, k = 5, m = 1, bs=c("cr","cc")), # VCUR_sep and month
#           offset= log(Rel_Abund),
#           method="ML",
#           #ziformula=~1,
#           data=modelling_data2, family = "nb")
# AIC(g3) # 454.37
# 
# 
# g4 <- gam(Whales_Caught ~  s(Month, k = 5,  bs=c("cc")) + s(prev_year_SOI, bs="cr"), # month and SOI
#           offset= log(Rel_Abund),
#           method="ML",
#           #ziformula=~1,
#           data=modelling_data2, family = "nb")
# AIC(g4) # 445.80
# draw(g4)
# 
# g5 <- gam(Whales_Caught ~  te(dist_edge_Coffs, Month, k = 5, m = 1, bs=c("cr","cc")), # month and dist_coffs
#           offset= log(Rel_Abund),
#           method="ML",
#           #ziformula=~1,
#           data=modelling_data2, family = "nb")
# AIC(g5) # 451.67
# 
# g6 <- gam(Whales_Caught ~  te(dist_edge_Sydney, Month, k = 5, m = 1, bs=c("cr","cc")), # month and dist_sydney
#           offset= log(Rel_Abund),
#           method="ML",
#           #ziformula=~1,
#           data=modelling_data2, family = "nb")
# AIC(g6) # 455.08
# 
# #### from Here month and SOI are the best (g4)
# 
# g4 <- gam(Whales_Caught ~  s(Month, k = 5,  bs=c("cc")) + s(prev_year_SOI), # month and SOI
#           offset= log(Rel_Abund),
#           method="ML",
#           #ziformula=~1,
#           data=modelling_data2, family = "nb")
# AIC(g4) # 446.43
# summary(g4)
# gratia::draw(g4)
# 
# g41 <- gam(Whales_Caught ~  te(VCUR_mean, Month, k = 5, m = 1, bs=c("cr","cc")) + s(prev_year_SOI), # month and SOI
#           offset= log(Rel_Abund),
#           method="ML",
#           #ziformula=~1,
#           data=modelling_data2, family = "nb", select = T)
# AIC(g41) # 462.61
# draw(g41)
# 
# summary(g41)
# 
# anova(g4, g41 ,test="Chisq")
# 
# 
# g42 <- gam(Whales_Caught ~  te(dist_edge_Sydney, Month, k = 5, m = 1, bs=c("cr","cc")) + s(prev_year_SOI), # month and SOI
#            offset= log(Rel_Abund),
#            method="ML",
#            #ziformula=~1,
#            data=modelling_data2, family = "nb", select = T)
# AIC(g42) # 462.61
# draw(g42)
# 
# summary(g42)
# 
# anova(g4, g42 ,test="Chisq")


#### Hypothesis 1

# VCUR pushes humpbacks inshore
## Make better offset variable
modelling_data2$Log_Rel_Abund <- log(modelling_data2$Rel_Abund)


g1a_1 <- gam(Whales_Caught ~  te(VCUR_mean, Month, k = 12, bs=c("cr","cc")) +
             #te(dist_edge_Coffs, Month, k = 12, bs=c("cr","cc"))+ 
             s(prev_year_SOI, bs="cr", k=5)+ # month and SOI
           offset(Log_Rel_Abund),
           method="ML",
           #ziformula=~1,
           data=modelling_data2, family = "nb", select = T)
AIC(g1a_1) # 445.605

g1a_2 <- gam(Whales_Caught ~  #te(VCUR_mean, Month, k = 12, bs=c("cr","cc")) +
               te(dist_edge_Coffs, Month, k = 12, bs=c("cr","cc"))+ 
               s(prev_year_SOI, bs="cr", k=5)+ # month and SOI
               offset(Log_Rel_Abund),
             method="ML",
             #ziformula=~1,
             data=modelling_data2, family = "nb", select = T)
AIC(g1a_2) # 444.6933


g1a_1_REML <- gam(Whales_Caught ~  te(VCUR_mean, Month, k = 12, bs=c("cr","cc")) +
               #te(dist_edge_Coffs, Month, k = 12, bs=c("cr","cc"))+ 
               s(prev_year_SOI, bs="cr", k=5)+ # month and SOI
               offset(Log_Rel_Abund),
             method="REML",
             #ziformula=~1,
             data=modelling_data2, family = "nb", select = T)
summary(g1a_1_REML)

g1a_2_REML <- gam(Whales_Caught ~  #te(VCUR_mean, Month, k = 12, bs=c("cr","cc")) +
               te(dist_edge_Coffs, Month, k = 12, bs=c("cr","cc"))+ 
               s(prev_year_SOI, bs="cr", k=5)+ # month and SOI
               offset(Log_Rel_Abund),
             method="REML",
             #ziformula=~1,
             data=modelling_data2, family = "nb", select = T)
summary(g1a_2_REML) 



gratia::draw(g1a_1_REML, scales = "fixed", residuals = F, rug=T,
             nrow=1) & 
  scale_x_continuous(expand = c(0,0)) &  theme_classic() &
  scale_y_continuous(expand=c(0,0), breaks = c(-1,-0.5,0,0.5,2,4,6,8,10,12)) &
  ggtitle(label=NULL)&
  theme(legend.position = "bottom",
        axis.text = element_text(colour="black", size=10),
        axis.title = element_text(face="bold", size=12),
        axis.ticks = element_line(colour = "black"),
        legend.title = element_text(face="bold", size=12),
        panel.border = element_rect(colour = "black", fill=NA))#

#ggsave("Coffs VCUR plot effects.png", width=21, height=14.8, dpi=600, units="cm")
#ggsave("Coffs VCUR plot effects2.pdf", width=21, height=14.8, dpi=600, units="cm")


gratia::draw(g1a_2_REML, scales = "fixed", residuals = F, rug=T,
             nrow=1) & 
  scale_x_continuous(expand = c(0,0)) &  theme_classic() &
  scale_y_continuous(expand=c(0,0), breaks = c(-1,-0.5,0,0.5,2,4,6,8,10,12)) &
  ggtitle(label=NULL)&
  theme(legend.position = "bottom",
        axis.text = element_text(colour="black", size=10),
        axis.title = element_text(face="bold", size=12),
        axis.ticks = element_line(colour = "black"),
        legend.title = element_text(face="bold", size=12),
        panel.border = element_rect(colour = "black", fill=NA))#

#ggsave("Coffs Dist plot effects.png", width=21, height=14.8, dpi=600, units="cm")
#ggsave("Coffs Dist plot effects2.pdf", width=21, height=14.8, dpi=600, units="cm")



resids <- simulateResiduals(g1a_1_REML)
plot(resids)
testDispersion(g1a_1_REML)
concurvity(g1a_1_REML)


resids <- simulateResiduals(g1a_2_REML)
plot(resids)
testDispersion(g1a_2_REML)
concurvity(g1a_2_REML)


### prediction

### to get plot of certain month

#
#devtools::install_github(repo = "florianhartig/DHARMa", subdir = "DHARMa", 
#                         ref = "0.4.5-mgcViz", dependencies = T, build_vignettes = T)

### Partial prediction plots for each month
f_dat <- data.frame()

for( i in 1:12){
dec_d <- modelling_data2 %>% filter(Month == i)
dec <- median(dec_d$dist_edge_Coffs, na.rm=T)
dd <- data_slice(g1a_1_REML, var1 = "VCUR_mean", offset = max(modelling_data2$Log_Rel_Abund),
                 data=data.frame("SOI" = median(modelling_data2$prev_year_SOI),
                                    "Month" = i,
                                    "dist_edge_Coffs" = dec))

ff <- predict.gam(g1a_1_REML, newdata = dd, type="response", se.fit = T)
#plot(ff$fit)
#lines(ff$fit - ff$se.fit)
#lines(ff$fit + ff$se.fit)
dd$fit <- ff$fit
dd$se_fit <- ff$se.fit
f_dat <- bind_rows(f_dat, dd)

}

month_string <- as_labeller(c(`1` = "January", `2` = "February", `3` = "March",
                           `4` = "April", `5` = "May", `6` = "June",
                           `7` = "July", `8` = "August", `9` = "September",
                           `10` = "October", `11`= "November", `12` = "December"))

ggplot(f_dat, aes(x=VCUR_mean, y = fit)) + geom_line()+
  facet_wrap(~ Month, labeller = month_string) +
  geom_ribbon(aes(ymin=fit-se_fit, ymax=fit+se_fit), alpha = 0.5) +
  ylab("Predicted Whale Entanglements")+
  xlab("Mean Southward Current (m/s)")+
  theme_classic()+ theme(axis.title = element_text(face="bold", size=12),
                         axis.text = element_text(colour="black", size=10),
                         panel.border = element_rect(colour="black", fill=NA),
                         strip.text = element_text(size = 10))

ggsave("VCUR Coffs by month.png", width=21, height=14.8, units="cm", dpi=600)
ggsave("VCUR Coffs by month.pdf", width=21, height=14.8, units="cm", dpi=600)


### now distance from coast

c_dat <- data.frame()

for( i in 1:12){
  dec_d <- modelling_data2 %>% filter(Month == i)
  dec <- median(dec_d$VCUR_mean, na.rm=T)
  dd <- data_slice(g1a_2_REML, var1 = "dist_edge_Coffs", offset = max(modelling_data2$Log_Rel_Abund),
                   data=data.frame("SOI" = median(modelling_data2$prev_year_SOI),
                                   "Month" = i,
                                   "VCUR_mean" = dec))
  
  ff <- predict.gam(g1a_2_REML, newdata = dd, type="response", se.fit = T)
  #plot(ff$fit)
  #lines(ff$fit - ff$se.fit)
  #lines(ff$fit + ff$se.fit)
  dd$fit <- ff$fit
  dd$se_fit <- ff$se.fit
  c_dat <- bind_rows(c_dat, dd)
  
}

ggplot(c_dat, aes(x=dist_edge_Coffs, y = fit)) + geom_line()+
  facet_wrap(~Month, labeller = month_string) +
  ylab("Predicted Whale Entanglements")+
  xlab("Distance to EAC Edge (30.3° S; km)")+
  geom_ribbon(aes(ymin=fit-se_fit, ymax=fit+se_fit), alpha = 0.5) +
  theme_classic()+ theme(axis.title = element_text(face="bold", size=12),
                         axis.text = element_text(colour="black", size=10),
                         panel.border = element_rect(colour="black", fill=NA),
                         strip.text = element_text(size = 10))


ggsave("EAC Dist Coffs by month.png", width=21, height=14.8, units="cm", dpi=600)
ggsave("EAC Dist Coffs by month.pdf", width=21, height=14.8, units="cm", dpi=600)



### Sep Zone Models

g1b_1 <- gam(Whales_Caught ~  te(VCUR_mean_Sep, Month, k = 12, bs=c("cr","cc")) +
             #te(dist_edge_Sydney, Month, k=12, bs=c("cr","cc")) +
             # s(dist_edge_Sydney, bs = "cr")+
             s(prev_year_SOI, bs="cr", k=5)+ # month and SOI
             offset(Log_Rel_Abund),
           method="ML",
           #ziformula=~1,
           data=modelling_data2, family = "nb", select = T)
AIC(g1b_1) # 449.39.19

resids <- simulateResiduals(g1b_1)
plot(resids)


# g1b_2 <- gam(Whales_Caught ~  #te(VCUR_mean_Sep, Month, k = 12, bs=c("cr","cc")) +
#                te(dist_edge_Sydney, Month, k=12, bs=c("cr","cc")) +
#                # s(dist_edge_Sydney, bs = "cr")+
#                s(prev_year_SOI, bs="cr", k=5)+ # month and SOI
#                offset(Log_Rel_Abund),
#              method="ML",
#              #ziformula=~1,
#              data=modelling_data2, family = "nb", select = T)
# AIC(g1b_2) # 445.93
# 
# resids <- simulateResiduals(g1b_2)
# plot(resids)
# 
# concurvity(g1b_2)


g1b_1_REML <- gam(Whales_Caught ~  te(VCUR_mean_Sep, Month, k = 12, bs=c("cr","cc")) +
               #te(dist_edge_Sydney, Month, k=12, bs=c("cr","cc")) +
               # s(dist_edge_Sydney, bs = "cr")+
               s(prev_year_SOI, bs="cr", k=5)+ # month and SOI
               offset(Log_Rel_Abund),
             method="REML",
             #ziformula=~1,
             data=modelling_data2, family = "nb", select = T)
#AIC(g1b_2_REML) # 445.93

resids <- simulateResiduals(g1b_1_REML)
plot(resids)

concurvity(g1b_1_REML)

summary(g1b_1_REML)


gratia::draw(g1b_1_REML, scales = "fixed", residuals = F, rug=T,
             nrow=1) & 
  scale_x_continuous(expand = c(0,0)) &  theme_classic() &
  scale_y_continuous(expand=c(0,0), breaks = c(-0.5,0,0.5,2,4,6,8,10,12)) &
  ggtitle(label=NULL)&
  theme(legend.position = "bottom",
        axis.text = element_text(colour="black", size=10),
        axis.title = element_text(face="bold", size=12),
        axis.ticks = element_line(colour = "black"),
        legend.title = element_text(face="bold", size=12),
        panel.border = element_rect(colour = "black", fill=NA))#

ggsave("Sydney test plot effects.png", width=21, height=14.8, dpi=600, units="cm")
ggsave("Sydney test plot effects.pdf", width=21, height=14.8, dpi=600, units="cm")

hist(modelling_data2$dist_edge_Sydney)





f_dat2 <- data.frame()

for( i in 1:12){
  dec_d <- modelling_data2 %>% filter(Month == i)
  dec <- median(dec_d$dist_edge_Sydney, na.rm=T)
  dd <- data_slice(g1b_1_REML, var1 = "VCUR_mean_Sep", offset = max(modelling_data2$Log_Rel_Abund),
                   data=data.frame("SOI" = median(modelling_data2$prev_year_SOI),
                                   "Month" = i,
                                   "dist_edge_Sydney" = dec))

  ff <- predict.gam(g1b_1_REML, newdata = dd, type="response", se.fit = T)
  #plot(ff$fit)
  #lines(ff$fit - ff$se.fit)
  #lines(ff$fit + ff$se.fit)
  dd$fit <- ff$fit
  dd$se_fit <- ff$se.fit
  f_dat2 <- bind_rows(f_dat2, dd)

}

ggplot(f_dat2, aes(x=VCUR_mean_Sep, y = fit)) + geom_line()+
  facet_wrap(~Month, labeller = month_string) +
  geom_ribbon(aes(ymin=fit-se_fit, ymax=fit+se_fit), alpha = 0.5) +
  scale_x_continuous(breaks=c(0.5, 0.25, 0))+
  ylab("Predicted Whale Entanglements")+
  xlab("Mean Southward Current (m/s)")+
  theme_classic()+ theme(axis.title = element_text(face="bold", size=12),
                         axis.text = element_text(colour="black", size=10),
                         panel.border = element_rect(colour="black", fill=NA),
                         strip.text = element_text(size = 10))

ggsave("VCUR Sep Zone by month.png", width=21, height=14.8, units="cm", dpi=600)
ggsave("VCUR Sep Zone by month.pdf", width=21, height=14.8, units="cm", dpi=600)


# ### now distance from coast
# 
# c_dat2 <- data.frame()
# 
# for( i in 1:12){
#   dec_d <- modelling_data2 %>% filter(Month == i)
#   dec <- median(dec_d$VCUR_mean_Sep, na.rm=T)
#   dd <- data_slice(g1b_2_REML, var1 = "dist_edge_Sydney", offset = max(modelling_data2$Log_Rel_Abund),
#                    data=data.frame("SOI" = median(modelling_data2$prev_year_SOI),
#                                    "Month" = i,
#                                    "VCUR_mean_Sep" = dec))
#   
#   ff <- predict.gam(g1b_2_REML, newdata = dd, type="response", se.fit = T)
#   #plot(ff$fit)
#   #lines(ff$fit - ff$se.fit)
#   #lines(ff$fit + ff$se.fit)
#   dd$fit <- ff$fit
#   dd$se_fit <- ff$se.fit
#   c_dat2 <- bind_rows(c_dat2, dd)
#   
# }
# 
# ggplot(c_dat2, aes(x=dist_edge_Sydney, y = fit)) + geom_line()+
#   facet_wrap(~Month, labeller = month_string) +
#   ylab("Predicted Whale Entanglements")+
#   xlab("Distance to EAC Edge (33.8° S; km)")+
#   geom_ribbon(aes(ymin=fit-se_fit, ymax=fit+se_fit), alpha = 0.5) +
#   theme_classic()+ theme(axis.title = element_text(face="bold", size=12),
#                          axis.text = element_text(colour="black", size=10),
#                          panel.border = element_rect(colour="black", fill=NA))
# 
# 
# ggsave("EAC Dist Sep Zone by month.png", width=21, height=14.8, units="cm", dpi=600)
# ggsave("EAC Dist Sep Zone by month.pdf", width=21, height=14.8, units="cm", dpi=600)
# 
# 

