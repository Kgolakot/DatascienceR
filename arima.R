rm(list=ls()) 

setwd("/Users/krishnasaigolakoti/Downloads/R project")
ev<-read.csv("E_sales.csv")

library('tseries')

#create time series data and clean the data
sales<-log(ev$S)
t.sales<-ts(sales, start = c(2012,5), end = c(2018,12), frequency = 12)
t.sales<-tsclean(t.sales)

#plot the time series data and decompose the data
plot(t.sales)
dec.sales<-stl(t.sales, s.window = "periodic")
plot(dec.sales)

#plot the correlogram for acf and pacf, we intially see the acf decreasing slowly suggesting that our data is nonstationary
acf(t.sales)
pacf(t.sales)


#For ADF If p-value >0.05 we failed to reject null hypothesis which means the series is non-stationary
#For ADF if p-value<0.05 we failed to reject the alternative hypothesis which means the series is either staionary or trend-stationary
#KPSS test p>0.05 the null hypothesis of trend stationary is not rejected
#WE use both tests to confirm sationarity , the  series is I(1) which means we need to difference one time to obtain stationarity
adf.test(t.sales)
kpss.test(t.sales)

#difference the series and plot the acf again
#Do  the adf test to confirm stationarity
diff.sales<-diff(t.sales, differences = 1)
adf.test(diff.sales)
kpss.test(diff.sales)

#we see the correlation of the lag in acf decreasing suggesting the lag  order for  ma to be 1
#we do not see a significant correlation in pacf so the lag order for ar should be  0
plot(diff.sales)
acf(t.sales, main="Differenced sales series")
pacf(t.sales, main="Differenced sales series")

#auto.arima chooses  the  lag.order and differences automatically. The lag orders are determined through AIC criterion through stepwise regression
arima.fit<-auto.arima(t.sales)
summary(arima.fit)

#plot the forecast of the arima model
plot(forecast(arima.fit, 12))

#the correlation between the residulas within the blue line so there is no significaant correlation between lag orders which we need to consider
residuals<-arima.fit$residuals
acf(residuals)
tsdisplay(residuals(arima.fit), lag.max=45, main='(0,1,1) Model Residuals')

plot(arima.fit$fitted)

t.series<-ts(ev[,2], start = c(2012,5), end = c(2019,9), frequency = 12)
hold<-window(log(t.series), start = c(2019,1), end = c(2019,9), frequency = 12)

library("forecast")

#forecast the predictions
pred<-forecast(arima.fit,  h = 9)$mean
plot(log(t.series), main = " Electric Vehicle sales", xlab = "Months",ylab = "Electric sales", col="darkblue")
lines(pred, col = "red")

arima.pred<-forecast(arima.fit,  h =  9)$mean
#extract the test set for 2019 year
test.set<-log(ev[81:89,2])

#RMSE and MAE for forecast accuracy
library("Metrics")
rmse(test.set, pred)
mae(test.set, pred)





