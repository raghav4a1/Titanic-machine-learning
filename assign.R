rm(list = ls(all=TRUE))
setwd("C:\\Users\\raghavvarma\\Desktop")

getwd()
tdata<-read.csv("train.csv")
str(tdata)
tdata<-tdata[,-c(1,4,9,12,11)]

str(tdata)
summary(tdata)

tdata$Pclass<-as.factor(tdata$Pclass)
tdata$Survived<-as.factor(tdata$Survived)
tdata$Sex<-as.factor(tdata$Sex)
sum(is.na(tdata))
library(DMwR)
tdata1<-centralImputation(tdata)
sum(is.na(tdata1))

str(tdata)
set.seed(110)
a<-sample(2,nrow(tdata),replace = TRUE,prob = c(0.7,0.3))
train<-tdata[a==1,]
validate<-tdata[a==2,]
logistic1<-glm(Survived~.,data=train,family = "binomial")
summary(logistic1)


library(MASS)
step1<-stepAIC(logistic1,direction="both")

summary(step1)

logistic2<-glm(Survived~.-Parch-Fare,data=train,family = "binomial")
summary(logistic2)

library(car)
vif(logistic2)
library(pROC)
predict1<-predict(logistic2,train ,type = 'response')
pred_class<-ifelse(predict1>0.5,1,0)
traintable<-table(pred_class,train$Survived)
accuracy1<-sum(diag(traintable))/sum(traintable)
print(accuracy1)

predict_val<-predict(logistic2,validate,type = "response")
preds_val<-ifelse(predict_val>0.5,1,0)
validtable<-table(validate$Survived,preds_val)
accuracy2<-sum(diag(validtable))/sum(validtable)
print(accuracy2)

#given test
testg<-read.csv("test.csv")
str(testg)
testg1<-testg[,-c(1,3,8,10,11)]
 str(testg1)
 sum(is.na(testg1))
 testg1<-centralImputation(testg1)
 testg1$Pclass<-as.factor(testg$Pclass)

testg<-as.data.frame(testg)

predict_val<-predict(logistic2,testg1,type = "response")
Survived1<-ifelse(predict_val>0.5,1,0)
testi<-cbind(testg$PassengerId,Survived1)
testimp<-as.data.frame(testi)
colnames(testimp)[which(names(testimp)=="V1")]<-"passenger id"
write.csv(testimp,"raghav_sample1.csv",row.names = F)

