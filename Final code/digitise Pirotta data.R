# To Digitize old graphs (only .jpg format)

# # Get the software
# install.packages("digitize")
# #install.packages("bmp")
# # Load the software
# library(digitize)
# 
# # Run line below then click on 2 points on the x-axis followed 
# # by 2 points on y-axis for calibration
# cal <- ReadAndCal("Pirotta figure count data.jpg")
# 
# # Run line below then click on all points you are interested in 
# # then press Esc button
# GI <- DigitData(col = 'red') # GI for Gonad Index, change at will
# 
# # Run the calibration through the points
# data <- Calibrate(GI, cal, 2000, 2015, 0, 125)
# 
# # List the y values (eg: if you want to extract them)
# write.table(data$y)
# 
# # Plot it up!!! - This will do connected line graph
# plot(data$x, data$y, pch=20, col='black',
#      type = "o",
#      xlab ='Year',
#      ylab ='Whales')
# 
# write.csv(data, "Solander digitised data.csv", row.names = F)

data <- read.csv("Solander digitised data.csv")
#f1 <- nls(y ~ a*exp(r*x),data=data, start = list(a=0.00001, r = 0.08))

# using eqtn fit in excel due to convergence errors
data$f <- 2E-76*exp(0.0885*data$x)
data$g <- data$y - data$f



library(tidyverse)
data <- data %>% rename(Year = x, Whales = y, fitted = f, difference = g)
data <- data %>%   mutate(Year = round(Year), resids = difference/fitted)

SOI <- read_csv("SOI data.csv")
data <- data %>% inner_join(SOI) %>% distinct(Year, .keep_all = T)


ggplot(data, aes(SOI, resids)) + geom_point() + geom_smooth(method="lm")
ggplot(data, aes(Year, Whales)) + geom_point() + geom_smooth(method="lm")


write.csv(data, "Solander digitised data final.csv", row.names = F)

f2 <- glm(Whales ~ Year, family = gaussian(link="log"), data= data)
plot(f2)
summary(f2)

data$Predicted_lm <- predict(f2, newdata=data,type="response")

ggplot(data, aes(Year, Whales)) + geom_point()+
  geom_point(inherit.aes = F, aes(Year, Predicted_lm), col="red")
ggplot(data, aes(Year, Whales)) + geom_point() + geom_smooth(method="lm")

data$deviation = data$Whales - data$Predicted_lm

ggplot(data, aes(prev_year_SOI, deviation)) + geom_point() + geom_smooth(method="lm")
write.csv(data, "Solander digitised data final.csv", row.names = F)
