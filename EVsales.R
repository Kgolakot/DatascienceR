rm(list=ls()) 
df1 <- read.csv('E_sales.csv')

df0<-df1[,-1]
######################################
 # SMOOTHING
#####################################
EV<-df1
plot(EV$T,EV$S, xlab = "Time", ylab = "Sales")
lines(EV$T[order(EV$T)],EV$S[order(EV$T)])

TSEV=ts(log(EV$S), start = c(2012,5), end = c(2019,9), frequency = 12)
TStest = window(TSEV, start = c(2019,1), end = c(2019,9), frequency = 12)
TStest
TSmodel = ts(log(EV$S), start = c(2012,5), end=c(2018,12), freq=12)
options(scipen = 999)
plot(TSmodel)
plot(TStest)
fit_decomposem = decompose(TSmodel,type = "multiplicative")
plot(fit_decomposem)
fit_decomposea = decompose(TSmodel,type = "additive")
plot(fit_decomposea)

library(forecast)
fit_ses = ses(TSmodel)
plot(fit_ses)
P1<-forecast(fit_ses,  h = 9)$mean
plot(TSEV, main = "Simple Exponential Smoothing", xlab = "Months", col="darkblue")
lines(P1, col = "red")
summary(fit_ses)
accuracy(TStest,P1)

fit_holt = holt(TSmodel)
plot(fit_holt)
P2<-forecast(fit_holt,  h = 9)$mean
plot(TSEV, main = "Holt's Model", xlab = "Months", col="darkblue")
lines(P2, col = "red")
summary(fit_holt)
accuracy(TStest,P2)

fit_hw=hw(TSmodel,seasonal = "multiplicative")
plot(fit_hw)
P3<-forecast(fit_hw,  h = 9)$mean
plot(TSEV, main = "Holt Winter's Model", xlab = "Months", col="darkblue")
lines(P3, col = "red")
summary(P3)
accuracy(TStest,P3)

fit_auto = ets(TSmodel,model = "MAM")
plot(forecast(fit_auto))
P4<-forecast(fit_auto,  h = 9)$mean
plot(TSEV, main = "Auto Smoothing", xlab = "Months", col="darkblue")
lines(P4, col = "red")
accuracy(TStest,P4)



#################################################
#           VAR AND ARIMA
#################################################
#install.packages('tseries')

ev<-df1

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


###############################################################
#                   NEURAL NETS
###############################################################

###############################################
tr_df <- df0[1:81,]
tst_df <-  df0[82:89,]
####################################
x<- log(tr_df$S)
train_ts <- ts(x,frequency = 3)

install.packages('nnfor') 
library(nnfor)

#############################################
#install.packages('nnet')
# library(nnet)
# install.packages('nnfor') 
# install.packages('rlist')
# library(nnfor)
# library(rlist)

###################################
install.packages('forecast')
library(forecast)
##################################
new_list <- log(tr_df$S)
list_trans <- log(df0$S)
new_ts <- ts(new_list,start=c(2012,5),end=c(2018,12),frequency=12)
model_mlp <- mlp(new_ts,reps=5,hd=7,lags=3)
ypred <- forecast(model_mlp,h=9)
accuracy(ypred$mean,log(tst_df$S)) 
plot(model_mlp)
TSEV <- ts(list_trans,start=c(2012,5),end=c(2019,9),frequency = 12)
plot(TSEV,main='Forecasts using Neural net')
lines(ypred$mean,col = 'red')
#####################################
#checking the optimum number of hidden layers
for (i in 2:7){
  new_list <- log(tr_df$S)
  list_trans <- log(df0$S)
  new_ts <- ts(new_list,start=c(2012,5),end=c(2018,12),frequency=12)
  model_mlp <- mlp(new_ts,reps=5,hd=i,lags=3)
  ypred <- forecast(model_mlp,h=9)
  print(accuracy(ypred$mean,log(tst_df$S)))
}






