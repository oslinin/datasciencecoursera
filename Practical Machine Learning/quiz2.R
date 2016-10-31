library("MASS")
data(menarche)
plot(Menarche/Total~Age, data=menarche)
x=menarche$Menarche/menarche$Total
dat=data.frame(menarche, perc=x)
glm.out=glm(perc~Age, family=binomial(logit), data=dat)
lm(log(perc/(1-perc))~Age, data=dat)

glm.out = glm(cbind(Menarche, Total-Menarche)  ~ Age, family=binomial(logit), data=menarche)
lines(menarche$Age, glm.out$fitted, type="l", col="red")

glm.out = glm(round(cbind(perc, 1-perc))*100~Age, 
              family=binomial(logit), 
              data=dat)

library(Hmisc)
library(AppliedPredictiveModeling)
#data(concrete)
load("//Msad/root/NA/NY/users/slinino/My Documents/R/AppliedPredictiveModeling_1.1-6/AppliedPredictiveModeling/data/concrete.RData")
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

training$ss <- cut2(training[["FlyAsh"]],g=3)
training$st <- 1:nrow(training)
qplot(st, CompressiveStrength,colour=ss,data=training)
qplot(st, CompressiveStrength,data=training)


qplot(SuperPlasticizer  ,colour=education,data=training,geom="density")


library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
load("//Msad/root/NA/NY/users/slinino/My Documents/R/AppliedPredictiveModeling_1.1-6/AppliedPredictiveModeling/data/AlzheimerDisease.RData")
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
x=names(training); x=x[grepl("^IL", x, T)]
PC=prcomp(training[,x], scale=F)

PC$sdev=PC$sdev[order(PC$sdev, decreasing = T)]
cumsum(PC$sdev^2)/sum(PC$sdev^2)
preProc <- preProcess(training[,x],method="pca")



library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
#data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

preProc <- preProcess(training[,x],method="pca",pcaComp=6)
spamPC <- predict(preProc,training[,x])
modelFit <- train(training$diagnosis ~ .,method="glm",data=spamPC)
testPC <- predict(preProc,testing[,x])
hhpca=confusionMatrix(testing$diagnosis,predict(modelFit, testPC))


preProc <- preProcess(training[,x])
spamPC <- predict(preProc,training[,x])
modelFit <- train(training$diagnosis ~ .,method="glm",data=spamPC)
testPC <- predict(preProc,testing[,x])
hh=confusionMatrix(testing$diagnosis,predict(modelFit, testPC))