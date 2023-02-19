# SOI Data exploration

library(tidyverse)

soi <- read_csv("SOI data.csv") %>% filter(Year >1990) %>%
  mutate(Date = lubridate::dmy(paste0("15/",Month,"/",Year)))

ggplot(soi, aes(x=Date, y = SOI)) + geom_line()


ggplot(soi, aes(x=Year, y = SOI)) + geom_line() +
  facet_wrap(~Month) + geom_vline(xintercept = c(2008,2009,2019), col="red")


soi$prev_July_Aug_SOI <- NA

for (i in (1:nrow(soi))){
  tdat <- soi %>% filter(Year == soi$Year[i]-1) %>%
    filter(Month >=7) %>% filter(Month <= 8)
  tt <- mean(tdat$SOI)
  soi$prev_July_Aug_SOI[i] <- tt
}



soi$prev_May_Sept_SOI <- NA

for (i in (1:nrow(soi))){
tdat <- soi %>% filter(Year == soi$Year[i]-1) %>%
  filter(Month >=5) %>% filter(Month <= 9)
tt <- mean(tdat$SOI)
soi$prev_May_Sept_SOI[i] <- tt
}

ggplot(soi, aes(x=Date, y = prev_May_Sept_SOI)) + geom_line()


soi$prev_year_SOI <- NA

for (i in (1:nrow(soi))){
  tdat <- soi %>% filter(Year == soi$Year[i]-1) #%>%
    #filter(Month >=5) %>% filter(Month <= 9)
  tt <- mean(tdat$SOI)
  soi$prev_year_SOI[i] <- tt
}

ggplot(soi, aes(x=Date, y = prev_year_SOI)) + geom_line()

cor.test(soi$prev_year_SOI, soi$prev_May_Sept)

soi <- soi %>% select(-Date)
write_csv(soi, "SOI data.csv")
