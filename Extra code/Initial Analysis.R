# initial data analysis

### Based on J Smith Bonito GAM

library(tidyverse)
library(DHARMa)

mydata <- read_csv("pre_data.csv")
mydata_long <- mydata %>% pivot_longer(`Month 1`:`Month 12`, names_to = "Month", values_to = "Whales_Caught") %>%
  mutate(Month = as.numeric(str_remove(Month, "Month ")))

library(mgcv)


## Now add 'fYear' to see if there's anything else worth looking at [*** I would see as the 'best' model]
mydata_long <- mydata_long %>% mutate(fYear = as.character(Year))

mydata5 <- mydata_long %>%
  group_by(Year, Month) %>%
  summarise(Monthly_Catch = mean(Whales_Caught))

### POISSON

# Just month
fit_js1p <- gam(Whales_Caught ~ s(Month, bs="cc"), data=mydata_long, family = "poisson")
summary(fit_js1p)  #expl. dev = 43.8 %

resids1 <- simulateResiduals(fit_js1p)
plot(resids1) # not great

mydata_long$pred_js1p <- fitted(fit_js1p)

pred_summ1 <- mydata_long %>%
  group_by(Year, Month) %>%
  summarise(Pred_Catch = mean(pred_js1p))

plot(mydata5$Monthly_Catch, type="l")
lines(pred_summ1$Pred_Catch, col="blue")  #does not get increasing trend


# with relative whale abundance
fit_js2p <- gam(Whales_Caught ~ s(Month, bs="cc") + s(Rel_Abund), data=mydata_long, family = "poisson")
summary(fit_js2p)  #expl. dev = 71.9 %

resids2 <- simulateResiduals(fit_js2p)
plot(resids2) # better

mydata_long$pred_js2p <- fitted(fit_js2p)

pred_summ2 <- mydata_long %>%
  group_by(Year, Month) %>%
  summarise(Pred_Catch = mean(pred_js2p))

plot(mydata5$Monthly_Catch, type="l")
lines(pred_summ2$Pred_Catch, col="blue")  #better

### Add fYear to see interannual variation
fit_js3p <- gam(Whales_Caught ~ s(Month, bs="cc") + s(Rel_Abund) + fYear, data=mydata_long, family = "poisson")
summary(fit_js3p)  #expl. dev = 75%
resids3 <- simulateResiduals(fit_js3p)
plot(resids3) # good


mydata_long$pred_js3p <- fitted(fit_js3p)

pred_summ3 <- mydata_long %>%
  group_by(Year, Month) %>%
  summarise(Pred_Catch = mean(pred_js3p))

plot(mydata5$Monthly_Catch, type="l")
lines(pred_summ3$Pred_Catch, col="blue")  #actually a better fit with fYear



## I have been comparing to mean monthly catches (which aggregates your data), so let's just look at fit
summary(fit_js1p)$dev.expl  #month-only 44%
summary(fit_js2p)$dev.expl  #month + abund 72%
summary(fit_js3p)$dev.expl  #month + fYear + effort 75%


### try to pull out residuals
mydata_long$pred_js2p_resids <- resid(fit_js2p)
mydata_long$pred_js2p_resids2 <- mydata_long$Whales_Caught - mydata_long$pred_js2p

plot(mydata_long$pred_js2p_resids, type = "l")
plot(mydata_long$pred_js2p_resids2, type = "l")


### Attempt to do an EAC analysis
EAC <- read_csv("EAC Shoreward Intrusion (Area & Distance) 1992 to 2018.csv")
EAC <- EAC %>% mutate(`...4` = NULL, `...5` = NULL,`...6` = NULL,
                      Date = lubridate::ym(`Date (yyyymm)`),
                      Year = lubridate::year(Date),
                      Month = lubridate::month(Date),
                      Distance = `Distance (km)`,
                      Area = `Area (km2)`)

full_long <- left_join(mydata_long, EAC)

### Add EAC Distance to see interannual variation
fit_js4p <- gam(Whales_Caught ~ s(Month, bs="cc") + s(Rel_Abund) + s(Distance) , data=full_long, family = "poisson")
summary(fit_js4p)  #expl. dev = 68%
resids4 <- simulateResiduals(fit_js4p)
plot(resids4) # good

# this bit not quite working
full_long$pred_js4p <- fitted(fit_js4p)

pred_summ4 <- full_long %>%
  group_by(Year, Month) %>%
  summarise(Pred_Catch = mean(pred_js4p))

plot(mydata5$Monthly_Catch, type="l")
lines(pred_summ3$Pred_Catch, col="red")  

### Add EAC Area to see interannual variation
fit_js4p <- gam(Whales_Caught ~ s(Month, bs="cc") + s(Rel_Abund) + s(Area) , data=full_long, family = "poisson")
summary(fit_js4p)  #expl. dev = 68%
resids4 <- simulateResiduals(fit_js4p)
plot(resids4) # good

# this bit not quite working
full_long$pred_js4p <- fitted(fit_js4p)

pred_summ4 <- full_long %>%
  group_by(Year, Month) %>%
  summarise(Pred_Catch = mean(pred_js4p))

plot(mydata5$Monthly_Catch, type="l")
lines(pred_summ3$Pred_Catch, col="red") 