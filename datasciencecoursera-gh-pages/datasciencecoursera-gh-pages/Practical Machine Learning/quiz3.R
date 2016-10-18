
#Question 1
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
set.seed(125)
i=which(names(segmentationOriginal)=="Case")
dat<-split(segmentationOriginal[,-i], segmentationOriginal[[i]])
modFit <- train(Area~ .,data=dat[["train"]],method="treebag")

pre.dat=data.frame(TotalIntench2= c(23, 50,  57,  NA)*1000, 
                   FiberWidthCh1= c(10, 10,   8,   8), 
                   PerimStatusCh1=c(2 , NA,  NA,   2), 
                   VarIntenCh4=   c(NA,100, 100, 100)
                  )
predict(modFit, pre.dat)

#Question 3
load("//Msad/root/NA/NY/users/slinino/My Documents/R/olive_data/olive.rda")
olive = olive[,-1]
modFit <- train(Area~ .,data=olive,method="rpart")
newdata = as.data.frame(t(colMeans(olive)))
predict(modFit, newdata)


#Question 4
source("G:/knitr.R")
library("msversion")
addpkg('ElemStatLearn', '2012.04-0')
addpkg('e1071', '1.6-5')
library("e1071")
library("ElemStatLearn")
library(caret)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
modelFit <- train(chd ~ age+alcohol+obesity+tobacco+typea+ldl,method="glm",data=trainSA, family="binomial")
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
print("train")
missClass(trainSA$chd, as.numeric(predict(modelFit, trainSA)))
print("test")
missClass(testSA$chd, as.numeric(predict(modelFit, testSA)))

modelFit$finalModel



#Question 5
addpkg('randomForest', '4.6-12')
library("randomForest")
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
vowel.train$y=factor(vowel.train$y)
vowel.test$y=factor(vowel.test$y)
set.seed(33833)
modFit <- train(y~ .,data=vowel.train,method="rf",prox=TRUE)
varImp(modFit)

