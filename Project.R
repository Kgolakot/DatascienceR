rm(list=ls()) 

library(earth)
library(corrplot)
library(Hmisc)
library(mgcv)
library(glmnet)
library(car)

#intialize seed
set.seed(123)

#read the data
rdata <- read.csv('./Midterm_ElectricityDemand.csv', stringsAsFactors=FALSE)
sdata<-rdata[,-1]

#summary of the data
head(sdata)
summary(sdata)

#generate histogram plot for sales data to check the mean and variance
hist(sdata$SALES, prob=T, xlab='',
     main='Histogram of Sales',col='blue')  
lines(density(sdata$SALES,na.rm=T)) 
rug(sdata$SALES) 

#boxplot between reponse variable and year
boxplot(rdata$SALES~I(rdata$YEAR), main = "SALES vs YEAR",col="grey", xlab = "YEAR", 
        ylab = "SALES", varwidth = TRUE) 
rug(jitter(rdata$SALES),side=2)
abline(h=mean(rdataf$SALES,na.rm=T),lty=2) 

#boxplot between sales and months
boxplot(sdata$SALES~I(rdata$MONTH), main = "SALES vs MONTH",col="yellow", xlab = "YEAR", 
        ylab = "MONTH", varwidth = TRUE) 
rug(jitter(rdata$SALES),side=2)
abline(h=mean(rdata$SALES,na.rm=T),lty=2) 

# Plot Response with mean, mean + sd, median
plot(sdata$SALES,xlab='Sample', ylab='SALES') 
abline(h=mean(sdata$SALES,na.rm=T),lty=1) 
abline(h=mean(sdata$SALES,na.rm=T)+sd(sdata$SALES,na.rm=T),lty=2,lwd=2,col='blue') 
abline(h=median(sdata$SALES,na.rm=T),lty=3,lwd=2,col='red') 

#correlation matrix and plot
M<-cor(sdata)
corrplot(M, method="square")

#mars with 1 degree of freedom
mars.1D <- earth(SALES ~. , data=sdata, degree=1, penalty=2, pmethod="backward", nfold=5, ncross=5)
summary(mars.1D)

#summary mars
mars.1D$rss 
mars.1D$rsq 

#qq plot
residuals.mars <- c()
sales.predict <- predict(mars.1D,sdata,type="response")
residuals.mars <- (sdata$SALES-sales.predict)
library(car)
par(mfrow=c(1,1))
qqPlot(residuals.mars, main = "MARS: Residual Plot") 

#MARS error
mars1D.RMSE = sqrt(mean((sdata$SALES-sales.predict)^2))
mars1D.RMSE 
mars1D.MAE = mean(abs(sdata$SALES-sales.predict))
mars1D.MAE 

#mars with 2 degree of freedom
mars.2D <- earth(SALES ~. , data=sdata, degree=2, penalty=3, pmethod="backward", nfold=5, ncross=5)
summary(mars.2D)

#summary mars
mars.2D$rss 
mars.2D$rsq 

#qq plot
residuals2.mars <- c()
sales2.predict <- predict(mars.2D,sdata,type="response")
residuals2.mars <- (sdata$SALES-sales2.predict)
library(car)
par(mfrow=c(1,1))
qqPlot(residuals2.mars, main = "MARS: Residual Plot") 

# Plot predicted vs. residuals

plot(sdata$SALES, sales2.predict, pch="o", col='black',lty=5,  main="MARS: Actual vs. Predicted",
     xlab = "Actual SALES", ylab="Predicted SALES")
abline(0,1)

## Variable Selection

MARS.varimp <- evimp(mars.2D, trim=FALSE)
print(MARS.varimp)
plot(MARS.varimp)

plot(mars.2D)
plot.earth.models(mars.2D)

#MARS error
mars2D.RMSE = sqrt(mean((sdata$SALES-sales2.predict)^2))
mars2D.RMSE 
mars2D.MAE = mean(abs(sdata$SALES-sales2.predict))
mars2D.MAE 

#GAM model
attach(sdata)
sales_GAM <- gam(SALES ~ s(MONTH, bs="cr") 
                  + s(MDPT, bs="cr") + s(PRICE, bs="cr") 
                  + s(EMNT, bs="cr") + s(MXSPD, bs="cr")
                  + s(DP05, bs="cr") + s(MMNT, bs="cr") + s(UNEMPRATE, bs="cr") + s(MMXT, bs="cr") +s(GUST, bs = "cr"))

# Show distribution of residuals and response vs. fitted
par(mfrow=c(1,2))
gam.check(sales_GAM)

#update GAM model
sales_GAM2 <- update(sales_GAM, .~. -s(EMNT, bs='cr') -s(MXSPD, bs="cr") -s(DP05, bs="cr")
                      - s(MMNT, bs="cr") - s(UNEMPRATE, bs="cr") -s(MMXT, bs="cr") -s(GUST, bs="cr")
                      + EMNT + MXSPD + DP05 + MMNT + UNEMPRATE + MMXT + GUST)

#summary of sales GAM model
summary(sales_GAM2)
par(mfrow=c(1,1))
plot(sales_GAM2)

#QQ plot GAM
layout(matrix(c(1:1),1,1,byrow=TRUE))
residuals.gam <- c()
gam.predict <- predict(sales_GAM2, newdata = sdata, type="response")
residuals.gam <- (sdata$SALES-gam.predict)
qqPlot(residuals.gam,main = "GAM:Residual Plot") 

#Actual vs predicted GAM
plot(sdata$SALES, gam.predict, pch="o", col='black',lty=5,  main="GAM: Actual vs. Predicted",
     xlab = "Actual SALES", ylab="Predicted SALES")
abline(0,1)

#GAM RSME values
gam.insample.RMSE = sqrt(mean((sdata$SALES-gam.predict)^2))
gam.insample.RMSE 
gam.insample.MAE = mean(abs(sdata$SALES-gam.predict))
gam.insample.MAE 

#splitting the data
smp_size <- floor(0.75 * nrow(sdata))
train_ind <- sample(seq_len(nrow(sdata)), size = smp_size)

train <- sdata[train_ind, ]
test <- sdata[-train_ind, ]

#linear regression
lm.mod<-lm(SALES~., data = train)
lm.predict<-predict(lm.mod, newdata = test)
lm.rmse<-sqrt(mean((test$SALES - lm.predict)^2))




