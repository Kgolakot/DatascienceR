rm(list=ls()) 

set.seed(123)

setwd("/Users/krishnasaigolakoti/Downloads/R project")
ev<-read.csv("E_sales.csv")

library('tseries')

ev<-ev[,-c(9:11)]
ev<-ev[,-1]

log.dat<-log(ev)
log.dat[,2]<-ev[,2]
log.dat<-log.dat[-c(90:92),]

ts.dat<-ts(data = log.dat, start = c(2012,5), end = c(2018,12), freq=12)

plot(ts.dat)


#If p-value >0.05 we failed to reject null hypothesis which means the series is non-stationary
#if p-value<0.05 we failed to reject the alternative hypothesis which means the series is either staionary or trend-stationary
for(i in 1:ncol(ts.dat)){
  print(adf.test(ts.dat[,i]))
  print(names(ev[i]))
}

#KPSS test p>0.05 the null hypothesis of trend stationary is not rejected
for(i in 1:ncol(ts.dat)){
  print(kpss.test(ts.dat[,i]))
  print(names(ev[i]))
}

for(i in 1:ncol(ts.dat)){
  acf(ts.dat[,i])
  pacf(ts.dat[,i])
}

#If p-value >0.05 we failed to reject null hypothesis which means the series is non-stationary
#if p-value<0.05 we failed to reject the alternative hypothesis which means the series is either staionary or trend-stationary
for(i in 1:ncol(ts.dat)){
  print(adf.test(diff(ts.dat[,i])))
  print(names(ev[i]))
}

#KPSS test p>0.05 the null hypothesis of trend stationary is not rejected
for(i in 1:ncol(ts.dat)){
  print(kpss.test(diff(ts.dat[,i])))
  print(names(ev[i]))
}

for(i in 2:7){
  temp.dat<-cbind(ts.dat[,1], ts.dat[,i])
  print(VARselect(temp.dat))
  print(names(ev[i]))
}

lag.order<-c(10,3,10,4,10,10)

for(i in 2:7){
  temp.dat<-cbind(ts.dat[,1], ts.dat[,i])
  colnames(temp.dat)<-c(names(ev[1]), names(ev[i]))
  temp.mod<-VAR(temp.dat, lag.max=lag.order[i-1], type = "both", season = 12, ic = "AIC")
  print(causality(temp.mod, cause = names(ev[i])))
  print(names(ev[i]))
}

#multivariate for sales ~ gas charhes and electric charges
temp.dat<-cbind(ts.dat[,1], ts.dat[,2], ts.dat[,3])
colnames(temp.dat)<-c(names(ev[1]), names(ev[2]), names(ev[3]))
print(VARselect(temp.dat))
var.fit<-VAR(temp.dat, lag.max = 5, type="both", season = 12, ic = "AIC")
summary(var.fit)

#if p>0/05 we cannot reject null hypothesis
print(causality(var.fit, cause = 'GC'))
print(causality(var.fit, cause = 'EC'))

#plot the forecasts
plot(forecast(var.fit, 12))

#check the serial correlation
var.serial<-serial.test(var.fit, lags.pt = 12, type = "PT.asymptotic")
plot(var.serial, names = "GC")
print(var.serial)

#check for heteroschedasticity
var.arch <- arch.test(var.fit, lags.multi = 12, multivariate.only = TRUE)
print(var.arch)

#impulse response for electric charges
irf.EC <- irf(var.fit, impulse = "EC", response = "S", n.ahead = 40, boot = TRUE)
plot(irf.EC, ylab = "ouput", main = "Shock from Electric Charges")

#impulse response for gas charges
irf.GC <- irf(var.fit, impulse = "GC", response = "S", n.ahead = 40, boot = TRUE)
plot(irf.GC, ylab = "ouput", main = "Shock from GAS Charges")

#extraxt the test set and total set
t.series<-ts(ev[,1], start = c(2012,5), end = c(2019,9), frequency = 12)
hold<-log(ev[81:89,1])

#forecast the sales data for 9 months ahead
sales.pred<-lapply(predict(var.fit, n.ahead = 9)$fcst['S'],'[',,1)
sales.pred<-as.numeric(unlist(sales.pred))

#convert the predictions and test set to time series predictions
test.sales<-ts(hold, start = c(2019, 1), end = c(2019, 9), frequency = 12)
ts.pred<-ts(sales.pred, start = c(2019, 1), end = c(2019, 9), frequency = 12)

#plot the forecast and actual sales
plot(log(t.series), main = " Electric Vehicle sales", xlab = "Months",ylab = "Electric sales", col="darkblue")
lines(ts.pred, col = "red")

#calculate the rmse and mae
library("Metrics")
rmse(test.sales, ts.pred)
mae(test.sales, ts.pred)

#bivariate for sales Electric charges
#temp1.dat<-cbind(ts.dat[,1], ts.dat[,2])
#colnames(temp1.dat)<-c(names(ev[1]), names(ev[2]))
#var1.fit<-VAR(temp1.dat, lag.max = 5, type="both", season = 12, ic = "AIC")
#summary(var1.fit)

#print(causality(var1.fit, cause = 'EC'))

#plot(forecast(var1.fit, 12))

#check serial correlation for the residuals 
#var1.serial<-serial.test(var1.fit, lags.pt = 12, type = "PT.asymptotic")
#plot(var1.serial, names = "S")
#print(var1.serial)

#bivariate for sales vs Gas charges
#temp2.dat<-cbind(ts.dat[,1], ts.dat[,3])
#colnames(temp2.dat)<-c(names(ev[1]), names(ev[3]))
#var2.fit<-VAR(temp2.dat, lag.max = 3, type="both", season = 12, ic = "AIC")
#summary(var2.fit)

#print(causality(var2.fit, cause = 'GC'))

#plot(forecast(var2.fit, 12))

#check serial correlation for the residuals
#var2.serial<-serial.test(var2.fit, lags.pt = 12, type = "PT.asymptotic")
#plot(var2.serial, names = "S")
#print(var2.serial)








