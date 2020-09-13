library(nnfor)

rm(list=ls()) 

set.seed(123)

setwd("/Users/krishnasaigolakoti/Downloads/R project")
ev<-read.csv("E_sales.csv")

sales<-log(ev[,2])
sales<-sales[-90:92,]

ts.sales<-ts(sales, start=c(2012,5), end=c(2018,12), freq=12)
plot(ts.sales)

t.series<-ts(ev[,2], start = c(2012,5), end = c(2019,9), frequency = 12)


mlp.fit<-mlp(ts.sales, reps = "30")

pred<-forecast(mlp.fit, 9)$mean

plot(log(t.series), main = " Electric Vehicle sales", xlab = "Months", col="darkblue")
lines(pred, col = "red")

test.set<-log(ev[81:89,2])

plot()

plot(mlp.fit)


