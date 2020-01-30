library(dplyr)
library(kernlab)
library(data.table)
data(spam)
#str(spam[,1:5])
set.seed(3435)
trainIndicator<-rbinom(4601,size=1,prob=0.5)
table(trainIndicator)
trainSpam<-spam[trainIndicator==1,]
testSpam<-spam[trainIndicator==0,]
names(trainSpam)
head(trainSpam)
table(trainSpam$type)

plot(trainSpam$capitalAve~trainSpam$type)
plot(log(trainSpam$capitalAve)~trainSpam$type)
names(trainSpam[,1:4])

plot(log(trainSpam[,1:4]+1))

hClusterUpdated<-hclust(dist(t(log(trainSpam[,1:55]+1))))
plot(hClusterUpdated) 

trainSpam$numType<-as.numeric(trainSpam$type)-1
costFunction<-function(x,y) sum(x !=(y>0.5)) ## very interesting cost function and useful!!! Just count how many errors
#predicted in the logistic regression

cvError=rep(NA,55)
library(boot)
for (i in 1:55){
  lmFormula<-reformulate(names(trainSpam)[i],response = 'numType')
  glmFit<-glm(lmFormula,family = 'binomial',data = trainSpam)
  cvError[i]<-cv.glm(trainSpam,glmFit,costFunction,2)$delta[2]
}
names(trainSpam)[which.min(cvError)]

predictionModel<-glm(numType ~ charDollar, family = 'binomial',data = trainSpam)

predictionTest<-predict(predictionModel,testSpam)
predictedSpam<-rep('nonspam',dim(testSpam)[1])

predictedSpam[predictionModel$fitted>0.5]='spam'

table(predictedSpam,testSpam$type)

require(devtools)
devtools::install_github("slidify")

library(cacher)

