#Clear workspace
rm(list=ls())

#load elemstat library for data
library(ElemStatLearn)

#set initial seed
set.seed(22)

#load SAheart data
data(SAheart)
df<-SAheart

#summary of the data
summary(df)

#change factor values to numeric
df[,5]<-sapply(df[,5], as.numeric)

#boxplots of predictors against responses
par(mfrow = c(3,3))
for(i in 1:9){
  boxplot(df[,i]~df$chd, data = df ,xlab = colnames(df[i]))
  abline(h = mean(df[,i]))
}

#randomly assign sampled data
index = sample(x = 1:nrow(df), size = 325)
df_train <- df[index,]
df_test <- df[-index,]

#load corrplot library to create a correlation plot
library(corrplot)
cor.df<-cor(df[,1:10])
corrplot(cor.df, method = 'circle')

# create a scatter plot for various predictors
mycol<-c("red","blue")
pairs(df, col = mycol[df$chd+1])

#fit logistic regression model
glm.fit<-glm(chd~., data = df_train, family = binomial)
summary(glm.fit)

#predict the responses for test data
predict_test<-predict(glm.fit, newdata = df_test, type = 'response')

#create different confusion matrix for various threshold
table(df_test$chd, predict_test>0.5)
table(df_test$chd, predict_test>0.55)
table(df_test$chd, predict_test>0.6)
table(df_test$chd, predict_test>0.65)
table(df_test$chd, predict_test>0.7)
table(df_test$chd, predict_test>0.75)

#prediction accuracy
log.pred<-ifelse(predict_test>0.5, 1,0 )
log.acc<-mean(df_test$chd == log.pred)

#load tree library
library(tree)

#fit a CART model to the chd response
cart.fit<-tree(chd~., data = df_train)
summary(cart.fit)

#plot the CART tree
plot(cart.fit)
text(cart.fit, pretty = 0)

#Run k fold cross validation for CART tree
cv.cart = cv.tree(cart.fit)
cv.cart
#plot CART deviance vs tree size
plot(cv.cart$size,cv.cart$dev,type='b')
plot(cv.cart)

#best tree size is equal to 4
best.cart<-prune.tree(cart.fit, best =4)
summary(best.cart)

#plot the pruned tree
plot(best.cart)
text(best.cart,pretty=0)

#get the training predictions and test predictions of the orginal CART model and pruned CART model
train.pred_tree<-predict(cart.fit, df_train)
test.pred_tree<-predict(cart.fit, df_test)
train.pred_tprune<-predict(best.cart, df_train)
test.pred_tprune<-predict(best.cart, df_test)

#create confusion matrix for the above predictions
table(df_train$chd, train.pred_tree>0.5)
table(df_train$chd, train.pred_tprune>0.5)
table(df_test$chd, test.pred_tree>0.5)
table(df_test$chd, test.pred_tprune>0.5)

#prediction accuracy
cart.pred<-ifelse(test.pred_tprune>0.5, 1 ,0 )
cart.acc<-mean(df_test$chd == log.pred)

library(randomForest) # Random Forest
library(gbm) # Boosting

#Create intial random forest with all predictors and intial tree size to be 400
bag.heart <- randomForest(chd ~ ., data=df_train, mtry = ncol(df_train) - 1, importance = TRUE, ntree=400)
plot(bag.heart, type='l', main='MSE by ntree for Bagging')
#The best tree size was determined to be 90
bag.heart <- randomForest(chd ~ ., data=df_train, mtry = ncol(df_train) - 1, importance = TRUE, ntree=90)
plot(bag.heart, type='l', main='MSE by ntree for Bagging')

#confusion matrix for bag model
bag.predict<-predict(bag.heart, newdata=df_test)
table(df_test$chd, bag.predict>0.5)

#prediction accuracy
bag.pred<-ifelse(bag.predict>0.5, 1 ,0 )
bag.acc<-mean(df_test$chd == bag.pred)

#varibale importnce of Bagging model
varImpPlot(bag.heart)
importance(bag.heart)

#Calculate Out of bag error for different m predictors
oob.mse <- c()
test.mse <-c()
for(i in 1:(ncol(df_train)-1)){
  rf.heart <- randomForest(chd~., data=df_train, mtry=i, importance=TRUE, ntree=90)
  oob.mse[i] <- rf.heart$mse[90]
  pred <- predict(rf.heart, newdata = df_test)
  test.mse[i] <- with(df_test, mean( (chd - pred)^2))
}

#plot thr oob mse and test mse for different trees with different m predictors
plot(rf.mse, main='Training Error by m', xlab='Number of Predictors', ylab='MSE')
matplot(1:9 , cbind(oob.mse,test.mse), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))

#the final tree is made with selecting 2 random predictors for every subtree
fin.heart <- randomForest(chd ~ ., data=df_train, mtry = 2, importance = TRUE, ntree=90)
plot(bag.heart, type='l', main='MSE by ntree for Random Forest')

#variable importance for Random forest
varImpPlot(fin.heart)
importance(fin.heart)

#predict the test data 
rf_predict<-predict(fin.heart, newdata=df_test)
bag_predict<-predict(bag.heart, newdata=df_test)

#confusion matrix for random forest
table(df_test$chd, rf_predict>0.5)

#prediction accuracy
rf.pred<-ifelse(rf_predict>0.5, 1 ,0 )
rf.acc<-mean(df_test$chd == rf.pred)

#load ada library
library(ada)

boost.heart<-ada(chd~., loss = "ada", iter=100, data=df_train)
boost.heart
summary(boost.heart)
plot(boost.heart, type='l', main='MSE pre iteration for Boosting')

#predict the test error for boost model
true.boost<-predict(boost.heart, newdata=df_test)

#confusion matrix for boosting
table(df_test$chd, true.boost)

#prediction accuracy
boost.acc<-mean(df_test$chd == true.boost)
