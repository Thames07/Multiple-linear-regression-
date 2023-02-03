#Read file from Excel

library(xlsx)
data<-read.csv('mtcars.csv')
data1<-mtcars[,c('mpg','disp','hp','wt')]
View(data1)
#Correlation Test
#install.packages(“psych”)
library('psych')
corr.test(data1)
#Multiple Linear Regression Model

linear_model<-lm(mpg~disp+hp+wt,data=data1)
print(linear_model)
summary(linear_model)
#Check Normality
#Histogram
hist(linear_model$residuals,col=40)
install.packages('nortest')
library(nortest)
#Anderson – Darling Test
ad.test(linear_model$residuals)
#Regression Diagnostics
par(mfrow = c(2, 2))
plot(linear_model)
#Confidence Interval
confint(linear_model)

#Prediction
find<-data.frame(disp=150,hp=170,wt=3.089)
predict(linear_model,find)
