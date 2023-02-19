# Effort Data Preparation

library(tidyverse)

mydata <- read_csv("19102021_ComC Results_raw.csv", lazy = F)

monthly_summary <- mydata %>% group_by(CalendarYear, MonthMM) %>%
  summarise(Total_Days_Effort = sum(SumDaysFishedScaled, na.rm=T)) %>% # Scaled is the good one to use
  mutate(Date = lubridate::dmy(paste0("15/",MonthMM,"/",CalendarYear))) %>%
  rename(Month = MonthMM, Year = CalendarYear)

ggplot(monthly_summary, aes(Date, y = Total_Days_Effort)) + geom_line()

### now do by zone
zone_monthly_summary <- mydata %>% group_by(Area, CalendarYear, MonthMM) %>%
  summarise(Total_Days_Effort = sum(SumDaysFishedScaled, na.rm=T)) %>% # Scaled is the good one to use
  mutate(Date = lubridate::dmy(paste0("15/",MonthMM,"/",CalendarYear))) %>%
  rename(Month = MonthMM, Year = CalendarYear)

ggplot(zone_monthly_summary, aes(Date, y = Total_Days_Effort)) + geom_line() +
  facet_wrap(~Area)

### now do by month
month_monthly_summary <- mydata %>% group_by(Area, MonthMM) %>%
  summarise(Total_Days_Effort = mean(SumDaysFishedScaled, na.rm=T),
            sd_E = sd(SumDaysFishedScaled, na.rm=T), n = n(),
            SE_E = sd_E/sqrt(n)) %>% # Scaled is the good one to use
 #mutate(Date = lubridate::dmy(paste0("15/",MonthMM,"/",CalendarYear))) %>%
  rename(Month = MonthMM)

### There is some seasonality
ggplot(month_monthly_summary, aes(Month, y = Total_Days_Effort)) + geom_line() +
  facet_wrap(~Area) + geom_errorbar(aes(ymax=Total_Days_Effort+SE_E, ymin = Total_Days_Effort-SE_E))



# Join to Entanglements
mydataE <- read_csv("Entanglements clean.csv", lazy = F)
mydataE_long <- mydataE %>% pivot_longer(`Month 1`:`Month 12`, names_to = "Month", values_to = "Whales_Caught") %>%
  mutate(Month = as.numeric(str_remove(Month, "Month ")))

full_dat <- left_join(mydataE_long, monthly_summary) %>% mutate(fYear = as.character(Year)) %>% select(-Date)


library(mgcv)
library(DHARMa)

### Add fYear to see interannual variation
#fit_js3p <- gam(Whales_Caught ~ s(Month, bs="cc") + s(Rel_Abund) + s(Total_Days_Effort), data=full_dat, family = "poisson")
#summary(fit_js3p)  #expl. dev = 75%
#resids3 <- simulateResiduals(fit_js3p)
#plot(resids3) # good
#
#plot(fit_js3p)



### Attempt to do an EAC analysis
EAC <- read_csv("EAC Shoreward Intrusion (Area & Distance) 1992 to 2018.csv")
EAC <- EAC %>% mutate(`...4` = NULL, `...5` = NULL,`...6` = NULL,
                      Date = lubridate::ym(`Date (yyyymm)`),
                      Year = lubridate::year(Date),
                      Month = lubridate::month(Date),
                      Distance = `Distance (km)`,
                      Area = `Area (km2)`)

full_long <- left_join(full_dat, EAC) %>% select(-Date)
sst_dat <- read_csv("SST_monthly test.csv")
full_long <- left_join(full_long, sst_dat)
sst2_dat <- read_csv("SST_sep_zone_monthly test.csv")
full_long <- left_join(full_long, sst2_dat)
vcur_dat <- read_csv("VCUR_monthly test02.csv")
full_long <- left_join(full_long, vcur_dat)

cor.test(full_long$Distance, full_long$SST_mean) # moderate but OK correlation
cor.test(full_long$SST_mean, full_long$SST_sep_mean) # moderate but OK correlation

cor.test(full_long$Distance, full_long$Area) # can't have both
cor.test(full_long$Distance, full_long$VCUR_mean) # OK
cor.test(full_long$SST_mean, full_long$VCUR_mean) # moderate but OK correlation


plot(full_long$Distance, full_long$Month)
plot(full_long$Distance, full_long$SST_mean)
plot(full_long$SST_mean, full_long$Month) # can't have both Month and SST


clim_dat <- full_long %>% group_by(Month) %>% summarise(Dist_clim = mean(Distance, na.rm=T),
                                                        SST_clim = mean(SST_mean, na.rm=T),
                                                        SST_sep_clim = mean(SST_sep_mean, na.rm=T))

full_long <- full_long %>% left_join(clim_dat) %>% mutate(SST_anom = SST_mean-SST_clim,
                                                          SST_sep_anom = SST_sep_mean-SST_sep_clim,
                                                          Dist_anom = (Distance - Dist_clim))
cor.test(full_long$Distance, full_long$Dist_anom) # OK
cor.test(full_long$SST_anom, full_long$SST_sep_anom) # OK

plot(full_long$Distance, full_long$Dist_anom)
plot(full_long$Month, full_long$SST_anom)

plot(full_long$Distance ~ full_long$Year)

hdat <- read_csv("Heatwave modelling Data.csv")
full_long <- left_join(full_long, hdat)
cor.test(full_long$total_intensity, full_long$total_HW)
cor.test(full_long$total_intensity, full_long$total_duration) # all heatwave metrics are correlated
cor.test(full_long$total_intensity, full_long$Distance)
cor.test(full_long$total_intensity, full_long$SST_mean)

### EAC distance BOLIN METHOD
bolin_dat <- read_csv("Final Coffs EAC Inner Edge Distance.csv")

ggplot(bolin_dat, aes(x=distedge)) + geom_histogram()+
  facet_wrap(~site)

bolin_dat <- bolin_dat %>% mutate(Month = lubridate::month(date),
                                  Year = lubridate::year(date))
bolin_monthly <- bolin_dat %>% group_by(site, Month, Year) %>%
  summarise(PC1 = mean(PC1, na.rm=T),
            dist_edge= mean(distedge, na.rm=T)) %>%
  pivot_wider(names_from = site, values_from = c(PC1, dist_edge))

cor.test(bolin_monthly$dist_edge_Sydney, bolin_monthly$dist_edge_Coffs)

full_long <- left_join(full_long, bolin_monthly)

soi <- read_csv("SOI data.csv")

full_long <- left_join(full_long, soi)

# This could be key.... to remove the seasonal changes in EAC (to look into further)
full_long2 <- full_long %>% filter(Month >5 & Month <11)

### Add fYear to see interannual variation
# 
# # Harmonic Function
# Harm <- function (theta, k = 4) {
#   X <- matrix(0, length(theta), 2 * k)
#   nam <- as.vector(outer(c("c", "s"), 1:k, paste, sep = ""))
#   dimnames(X) <- list(names(theta), nam)
#   m <- 0
#   for (j in 1:k) {
#     X[, (m <- m + 1)] <- cos(j * theta)
#     X[, (m <- m + 1)] <- sin(j * theta)
#   }
#   X
# }
# 
# full_long$HarmMonth <- (full_long$Month/12)*2*pi

cor.test(full_long$VCUR_mean, full_long$Distance)

Current_dat2 <- read_csv("VCUR_sep_monthly test02.csv", lazy = F)
full_long <- left_join(full_long, Current_dat2)
cor.test(full_long$VCUR_mean, full_long$VCUR_mean_Sep) # Current zones not correlated
cor.test(full_long$UCUR_mean, full_long$UCUR_mean_Sep)

write_csv(full_long, "Final Analysis Data.csv")

# library(glmmTMB)
# fit_js3pO <- glmmTMB(Whales_Caught ~  VCUR_mean:Harm(HarmMonth,k=1) + dist_edge_Coffs:Harm(HarmMonth,k=1),# +(Year), # VCUR seems to have an effect...
#                  offset= log(Rel_Abund),
#                  #ziformula=~1,
#                 data=full_long, family = "nbinom1")
# 
# #plot(effects::allEffects(fit_js3pO))
# AIC(fit_js3pO)
# summary(fit_js3pO)  
# resids3 <- simulateResiduals(fit_js3pO)
# plot(resids3) # not great
# plot(effects::allEffects(fit_js3pO))
# testDispersion(resids3)
# plot(fit_js3pO)
# 

cor.test(full_long$SOI, full_long$prev_May_Sept_SOI)

g1 <- gam(Whales_Caught ~  te(VCUR_mean, Month, k = 5, m = 1, bs=c("cr","cc"))+ s(prev_year_SOI), # VCUR seems to have an effect...
                     offset= log(Rel_Abund),
                     #ziformula=~1,
                     data=full_long, family = "nb")

resids <- simulateResiduals(g1)
plot(resids)

#install.packages("gratia")
library(gratia)
draw(g1) + scale_fill_viridis_c(option="magma")
#plot(g1)
summary(g1)


AIC(g1)


g2 <- gam(Whales_Caught ~  te(VCUR_mean, Month, k = 5, m = 1, bs=c("cr","cc")), # VCUR seems to have an effect...
          offset= log(Rel_Abund),
          #ziformula=~1,
          data=full_long, family = "nb")

AIC(g2)

g3 <- gam(Whales_Caught ~  s(VCUR_mean),# Month, k = 5, m = 1, bs=c("cr","cc")), # VCUR seems to have an effect...
          offset= log(Rel_Abund),
          #ziformula=~1,
          data=full_long, family = "nb")

AIC(g3)


g4 <- gam(Whales_Caught ~  s(Month, bs="cc"),# Month, k = 5, m = 1, bs=c("cr","cc")), # VCUR seems to have an effect...
          offset= log(Rel_Abund),
          #ziformula=~1,
          data=full_long, family = "nb")

AIC(g4)


DHARMa::testZeroInflation(fit_js3pO) # model is zero inflated 
plot(effects::allEffects(fit_js3pO))
performance::r2(fit_js3pO)
#install.packages("DHARMa")
#
#devtools::install_github(repo = "florianhartig/DHARMa", subdir = "DHARMa", 
#                         ref = "0.4.5-mgcViz", dependencies = T, build_vignettes = T)

plot(fit_js3pO)
gam.check(fit_js3pO)
mgcv.helper::vif.gam(fit_js3pO)
concurvity(fit_js3pO)

?str_split()
str_remove_all()

library(randomForest)
full_test <- drop_na(full_long) %>% select(c(4,8:19))

f1 <- randomForest(Whales_Caught ~SST_mean+Distance, data=full_test,importance = TRUE)
print(f1)
plot(f1)
importance(f1)

#install.packages("brms")

offset(offs_col)

offS <- full_long$Rel_Abund
library(brms)
m2 <- brm(bf(Whales_Caught ~  s(VCUR_mean) + s(UCUR_mean) + s(Distance)+ s(SST_anom)),
             #offset= log(offS),
          data = full_long, family = negbinomial(), cores = 4, seed = 17,
          iter = 4000, warmup = 1000, thin = 10, refresh = 0,
          control = list(adapt_delta = 0.99))
summary(m2)
plot(m2)
plot(conditional_smooths(m2))
launch_shinystan(m2)
pp_check(m2, ndraws = 100)
pp_check(m2, type = "ecdf_overlay")
performance::r2_bayes(m2)

check_brms <- function(model,             # brms model
                       integer = FALSE,   # integer response? (TRUE/FALSE)
                       plot = TRUE,       # make plot?
                       ...                # further arguments for DHARMa::plotResiduals 
) {
  
  mdata <- brms::standata(model)
  if (!"Y" %in% names(mdata))
    stop("Cannot extract the required information from this brms model")
  
  dharma.obj <- DHARMa::createDHARMa(
    simulatedResponse = t(brms::posterior_predict(model, nsamples = 1000)),
    observedResponse = mdata$Y, 
    fittedPredictedResponse = apply(
      t(brms::posterior_epred(model, nsamples = 1000, re.form = NA)),
      1,
      mean),
    integerResponse = integer)
  
  if (isTRUE(plot)) {
    plot(dharma.obj, ...)
  }
  
  invisible(dharma.obj)
  
}
check_brms(m2)
