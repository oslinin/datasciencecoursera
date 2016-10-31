## source("machine learning project prod.R")
#source("G:/knitr.R")
setwd("//msad/root/NA/NY/users/slinino/My Documents/R/datasciencecoursera-gh-pages/Practical Machine Learning/project")

#library("caret")
#library("mvbutils")

if (!exists("train"))if (file.exists("train.RData")) load("train.RData")
if (!exists("train")){
  train<<-read.csv("pml-training.csv",row.names=1, na.strings=c("NA", "", "#DIV/0!"))
  test<<-read.csv("pml-testing.csv",row.names=1, na.strings=c("NA", "", "#DIV/0!"))
  keep.cols=names(train) #columns to include in the model.
  save(train, test, keep.cols, file="train.RData")
} 

names(train)[sapply(train, class)=="factor"]

library(ggplot2)
qplot(raw_timestamp_part_1, raw_timestamp_part_2, data=train, 
      color=user_name, main = "Timestamp Columns")
qplot(raw_timestamp_part_1, raw_timestamp_part_2, color=user_name, 
      data=train[train$user_name=="charles",], main = "Timestamp Columns: one user")


qplot(raw_timestamp_part_1, cvtd_timestamp, data=train, color=user_name, main = "Timestamp Columns")
qplot(raw_timestamp_part_1, cvtd_timestamp, color=user_name, 
      data=train[train$user_name=="charles",], main = "Timestamp Columns: one user")


if (!exists("zero.vars")) if (file.exists("zero.var.RData")) load("zero.var.RData")
if (!exists("zero.vars")){
  zero.vars = nearZeroVar(train[,keep.cols], saveMetrics = T)
  drop.cols <- names(train[,keep.cols])[zero.vars$nzv]; 
  keep.cols=keep.cols%except%drop.cols
  save(zero.vars, drop.cols, keep.cols, file="zero.var.RData")
} 


if (!exists("preObj")) if (file.exists("preObj.RData")) load("preObj.RData")
if (!exists("preObj")){
  preObj <- preProcess(train[,keep.cols],method=c("knnImpute")) #,method=c(,"center","scale","nzv")
  trainp <- predict(preObj, train[,keep.cols])
  save(preObj, trainp, file="preObj.RData")
} 



if (!exists("modFit.rpart")) if (file.exists("modFit.rpart.RData")) load("modFit.rpart.RData")
if (!exists("modFit.rpart")){
  modFit.rpart <- train(classe~ .,data=trainp,method="rpart") 
  print(modFit.rpart$finalModel) #factors handled appropriately
  print(confusionMatrix(trainp$classe, predict(modFit.rpart,trainp))$overall['Accuracy'])
  save(modFit.rpart, file="modFit.rpart.RData")
} 


if (!exists("modFit.lda")) if (file.exists("modFit.lda.RData"))  load("modFit.lda.RData")
if (!exists("modFit.lda")){
  modFit.lda <- train(classe~ .,data=trainp,method="lda") 
  print(modFit.lda$finalModel) #factors handled appropriately
  print(confusionMatrix(trainp$classe, predict(modFit.lda,trainp))$overall['Accuracy'])
  save(modFit.lda, file="modFit.lda.RData")
}


fitControl <- trainControl(method = "cv", #cross validation
                           number = 100 # fold cross-validation so variance not too large
)
if (!exists("modFit.lda.cv")) if(file.exists("modFit.lda.cv.RData")) load("modFit.lda.cv.RData")
if (!exists("modFit.lda.cv")){
  modFit.lda.cv <- train(classe~ .,data=trainp,method="lda", trainControl=fitControl)
  print(modFit.lda.cv$finalModel) #factors handled appropriately
  print(confusionMatrix(trainp$classe, predict(modFit.lda.cv,trainp))$overall['Accuracy'])
  save(modFit.lda.cv, file="modFit.lda.cv.RData")
}  

  
  
  
if (!exists("preObj.pca")) if (file.exists("pca.RData")) load("pca.RData")
if (!exists("preObj.pca")){
  #calculate how many principal components we need
  trainp1 <- train[,keep.cols[6:(length(keep.cols)-1)]]
  preObj <- preProcess(trainp1,method=c("knnImpute","center","scale")) #,method=c()
  trainp1 <- predict(preObj, trainp1)
  PC=prcomp(trainp1,scale=T)
  n.pca=min(which(PC$importance[3,]>=.9))
  x=PC$sdev^2
  x=x[order(x,decreasing=T)]
  x2=cumsum(x)/sum(x)
  n.pca=min(which(x2>.9))
  print(paste(n.pca, " principal components capture 90% of the variance in the variables"))
  
  #fit an LDA model with PCA.
  preObj.pca <- preProcess(train[,keep.cols],method=c("knnImpute", "pca", "center", "scale"),
                           pcaComp=n.pca) 
  train.pca <- predict(preObj.pca, train[,keep.cols])
  modFit.lda.pca <- train(classe~ .,data=train.pca,method="lda") #missing values
  print(modFit.lda.pca$finalModel) #factors handled appropriately
  print(confusionMatrix(trainp$classe, predict(modFit.lda.pca,train.pca))$overall['Accuracy'])
  save(n.pca, preObj.pca, train.pca, modFit.lda.pca, file="pca.RData")
} 


if (!exists("combinedTrainData")) if (file.exists("ensemble.RData")) load("ensemble.RData")
if (!exists("combinedTrainData")){
  combinedTrainData <- data.frame(
    classe=train$classe, 
    lda=      predict(modFit.lda    ,trainp), 
    lda.pca = predict(modFit.lda.pca,train.pca), 
    rpart =   predict(modFit.rpart  ,trainp)
  )
  
  comb.fit <- train(classe ~.,method="rf",data=combinedTrainData, prox=F) #The ensemble model does not calibrate unless prox=F.
  combinedTrainData$combined.pred = predict(comb.fit, combinedTrainData)
  confusionMatrix(combinedTrainData$classe, combinedTrainData$combined.pred)$overall['Accuracy']
  sapply(combinedTrainData, function(x) mean(x==combinedTrainData$classe))
  save(combinedTrainData,comb.fit, file= "ensemble.RData")
}

if (!exists("ensemble.test")) if (file.exists("modFit.lda.test.RData"))  load("modFit.lda.test.RData")
if (!exists("ensemble.test")){
  # apply the transformation in preObj to test data: knnImpute, center, scale; subset to keep.cols
  keep.cols1 = keep.cols %except% "classe"
  testp <- predict(preObj, test[,keep.cols1]) 
  test.pca <- predict(preObj.pca, test[,keep.cols1])
  
  #run the model on test data.
  combinedTestData <- data.frame(
    lda=      predict(modFit.lda    ,testp), 
    lda.pca = predict(modFit.lda.pca,test.pca), 
    rpart =   predict(modFit.rpart  ,testp)
  )  
  ensemble.test <- predict(comb.fit, combinedTestData)
  print(ensemble.test)
  
  save(ensemble.test, file="modFit.lda.test.RData")
} 