library(corrplot)
df<-read.csv("Univ.csv")
df1<-df

head(df)
summary(df)

#1
df1<-df
for (i in 3:5){
  df1[is.na(df[,i]), i] = median(df[,i], na.rm = TRUE)
}
for (i in 6:7){
  df1[is.na(df[,i]), i] = mean((df[,i]), na.rm = TRUE)
}
for (i in 8:9){
  df1[is.na(df[,i]), i] = median(df[,i], na.rm = TRUE)
}
for (i in 10:ncol(df)){
  df1[is.na(df[,i]), i] = mean((df[,i]), na.rm = TRUE)
}

#2
par(mfrow = c(3,4))
for (i in 4:15){
  boxplot(df1[,i], xlab = colnames(df1[i]))
  abline(h = mean(df[,i]))
}

#3
m<-cor(df1[,3:19])
corrplot(m, method = "number")

#4
Texas.df<-df1[grep("Texas|TX", df$University.name),]
private= length(grep("Yes", Texas.df$Private))
public= length(grep("No", Texas.df$Private))
x<-Texas.df[grep("Yes", Texas.df$Private),10:13]
y<-Texas.df[grep("No", Texas.df$Private),10:13]
par(mfrow = c(2,2))
for (i in 1:4){
  boxplot(x[,i],y[,i], xlab = colnames(df1[i+9]))
}

#5
F_Split<-function(df, P_train){
  index<-sample(x = 1:nrow(df), size = floor(P_train*nrow(df)))
  df_train <- df[index,]
  df_test <- df[-index,]
  blend<-list(df_train, df_test)
  return(blend)
}

df_list = F_Split(df1,0.8)
df.train<-df_list[[1]]
df.test<-df_list[[2]]

#6
F_CV<-function(df, f){
  index.list<-split(sample(x = 1:nrow(df)), 1:f)
  for (i in 1:f){
    fit<-lm(Grad.Rate ~ Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate + Room.Board + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend, data = df[-index.list[[i]],])
    fit.predict<-predict(fit, newdata = df[index.list[[i]],])
    fit.mse[i]<-mean((fit.predict - df[index.list[[i]], ncol(df)])^2)
  }   
  CV<-sum(fit.mse)/f   
  return(CV)
}

CV.error<-F_CV(df1, 5)

for (i in 1:ncol(x)){
  x.mean[i]<-mean(x[,i])
}

for (i in 1:ncol(y)){
  y.mean[i]<-mean(y[,i])
}

for (i in 1:ncol(x)){
  perc[i]<-x.mean[i]/y.mean[i]
}