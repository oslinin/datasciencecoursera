#1 compare models.  Where they agree it's more accurate.
addpkg('gbm', '2.1.1')

source("G:/knitr.R")
library(ElemStatLearn)
#http://sux13.github.io/DataScienceSpCourseNotes/8_PREDMACHLEARN/Practical_Machine_Learning_Course_Notes.html#preprocessing-tutorial
data(vowel.train)
data(vowel.test)
set.seed(33833)
rf <- train(factor(y) ~ .,data=vowel.train,method="rf", prox=T) #random forest
gbm <- train(factor(y) ~ ., method="gbm",data=vowel.train,verbose=FALSE) #boosted
pred.rf <- predict(rf,vowel.test)
pred.gbm <- predict(gbm,vowel.test)

acc = function(x=pred.rf, dat=vowel.test)  mean(x==factor(dat$y))  
acc.rf = acc(pred.rf)
acc.gbm= acc(pred.gbm)
i = pred.rf==pred.gbm
acc.both=acc(x=pred.rf[i], dat=vowel.test[i,])
c(acc.rf, acc.gbm, acc.both)

#2 combine models, specifically stack predictors using random forest.
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
load("//Msad/root/NA/NY/users/slinino/My Documents/R/AlzheimerDisease.RData")
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433 )

rf <-  train(diagnosis ~ ., method="rf" ,data=training, prox=T) #random forest
gbm <- train(diagnosis ~ ., method="gbm",data=training, verbose=FALSE) #boosted
lda <- train(diagnosis ~ ., method="lda",data=training) #linear discriminant analysis

pred.rf  <- predict(rf, training)
pred.gbm <- predict(gbm,training)
pred.lda <- predict(lda,training)
combinedTrainData <- data.frame(glm.pred=pred.gbm,rf.pred =pred.rf,lda.pred=pred.lda, 
                                diagnosis=training$diagnosis)
comb.fit <- train(diagnosis ~.,method="rf",data=combinedTrainData, prox=TRUE)

pred.rf1 <- predict(rf,testing)
pred.gbm1 <- predict(gbm,testing)
pred.lda1 <- predict(lda,testing)
combinedTestData <- data.frame(glm.pred=pred.gbm1,rf.pred =pred.rf1,lda.pred=pred.lda1, 
                               diagnosis=testing$diagnosis)

combinedTestData$combined.pred = predict(comb.fit, combinedTestData)
sapply(combinedTestData, function(x) mean(x==combinedTestData$diagnosis))

#3 Regularized Regression - LASSO Regression
# regularized regression penalizes large coef. by minimizing PRSS
# Lasso PRSS is penalized by absolute value of coef * lambda
set.seed(3523)
addpkg('elasticnet', '1.1')
addpkg('lars', '1.2')

load("//Msad/root/NA/NY/users/slinino/My Documents/R/concrete.RData")
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233 )
lasso = train(CompressiveStrength ~ ., data=training, method="lasso")
x = as.matrix(subset(training, select=-c(CompressiveStrength)))
lasso1 = enet(x,training$CompressiveStrength, 0 )
plot(lasso1, use.color=T)

addpkg('lars', '1.2')
library("lars")

lasso3 = lars(x,training$CompressiveStrength, type="lasso", trace=T)
plot(lasso3)


#4 forecasting
#Fit a model using the bats() function in the forecast package 
#to the training time series. Then forecast this model for the 
#remaining time points. For how many of the testing points is 
#the true value within the 95% prediction interval bounds?

addpkg('forecast', '5.8')
addpkg('zoo', '1.7-12') # Used by forecast
addpkg('lattice', '0.20-33') # Used by zoo
addpkg('timeDate', '3012.100') # Used by forecast
addpkg('tseries', '0.10-34') # Used by forecast
addpkg('quadprog', '1.5-5') # Used by tseries
addpkg('fracdiff', '1.4-2') # Used by forecast
addpkg('Rcpp', '0.12.1-ms1') # Used by forecast
addpkg('colorspace', '1.2-4') # Used by forecast
addpkg('RcppArmadillo', '0.5.300.4') # Used by forecast
addpkg('forecast', '7.1')

library("forecast")
library(lubridate)
dat = read.csv("//Msad/root/NA/NY/users/slinino/My Documents/R/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
bat <- bats(tstrain)
res<-forecast(bat, 235)
res=as.data.frame(res)
res$yhat = testing$visitsTumblr
res$inbound = (res$yhat > res[["Lo 95"]]) & (res$yhat <= res[["Hi 95"]])
mean(res$inbound)

#5 SVM
set.seed(3523)
load("//Msad/root/NA/NY/users/slinino/My Documents/R/concrete.RData")
#data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(325)
addpkg('e1071', '1.6-5')
library("e1071")
ss = svm(CompressiveStrength ~ ., data=training)
res=predict(ss, testing)
sqrt(sum((testing$CompressiveStrength -  res)^2)/nrow(testing))