
istall.packages("forecast")
#import the libraries
library(forecast)
library(readxl)
#import the data set from excel
mydata <- read_excel("//ug.kth.se/dfs/home/a/a/aacv/appdata/xp.V2/Desktop/Hen2.xlsx")
View(mydata)

#convert the virus column in the excel to time series from week 25. 
#The frequency is set at 7, as the data is presented in every 1/7th interval
tsvirus <- ts(mydata2$`virus `, frequency = 7, start=c(25))

#plot the time series data using the autoplot function
autoplot(tsvirus)

#form auto.arima for the time series of the virus
arima1 <- auto.arima(tsvirus, trace= F, stepwise = F, approximation = F)
auto.arima(tsvirus , trace= F, stepwise = F, approximation = F)

#with the help this function, the ACF plot can be developed to 
#provide information on the lags 
checkresiduals(arima1)

#the forecast function creates a plot for the next 10 weeks(h=10)
farima1 <- forecast(arima1, h=10)

#plots the forecast plot
 plot(farima1)

