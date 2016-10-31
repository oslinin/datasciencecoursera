source("G:/knitr.R") #ms add package calls
library(caret)
library(mvbutils)
library(doMC)
doMC::registerDoMC(cores=4)
setwd("//msad/root/NA/NY/users/slinino/My Documents/R/datasciencecoursera-gh-pages/Practical Machine Learning/project")

train<-read.csv("pml-training.csv",row.names=1, na.strings=c("NA", "", "#DIV/0!"))
test<-read.csv("pml-testing.csv",row.names=1, na.strings=c("NA", "", "#DIV/0!"))

keep.cols=names(train)
#save.image("workspace.RData")
#load("workspace.RData")
#works default maximise accuracy for factor(y)
trainp=train[,c("classe","pitch_belt","gyros_forearm_y")]
modFit <- train(classe~ .,data=trainp,method="lda")
confusionMatrix(trainp$classe, predict(modFit,trainp))$overall['Accuracy']

#####doesn't work##############
#for lda
modFit <- train(classe~ .,data=train,method="lda")
modFit <- train(classe~ .,data=train,method="lda", preProcess=c("nzv","knnImpute"))
#for rpart
modFit <- train(classe~ .,data=train,method="rpart") #missing values

modFit <- train(classe~ .,data=train,method="rpart", preProcess="knnImpute") #missing values
confusionMatrix(trainp$classe, predict(modFit,trainp))$overall['Accuracy']

preObj <- preProcess(train,method=c("knnImpute","nzv")) #,method=c(,"center","scale","nzv")
trainp <- predict(preObj, train)
modFit <- train(classe~ .,data=trainp,method="rpart") #missing values
#####end doesn't work##############
# use keep.cols variable to fix
zero.vars = nearZeroVar(train[,keep.cols], saveMetrics = T)
drop.cols <- names(train[,keep.cols])[zero.vars$nzv]; 
keep.cols=keep.cols%except%drop.cols


#create trainp
preObj <- preProcess(train[,keep.cols],method=c("knnImpute")) #,method=c(,"center","scale","nzv")
trainp <- predict(preObj, train[,keep.cols])

#test trainp
modFit.lda <- train(classe~ .,data=trainp,method="lda") #missing values
modFit.lda$finalModel #factors handled appropriately
confusionMatrix(trainp$classe, predict(modFit.lda,trainp))$overall['Accuracy']


#create trainpca
trainp1 <- train[,keep.cols[6:(length(keep.cols)-1)]]
preObj <- preProcess(trainp1,method=c("knnImpute")) #,method=c(,"center","scale","nzv")
trainp1 <- predict(preObj, trainp1)
PC=prcomp(trainp1,scale=T)
n.pca=min(which(PC$importance[3,]>=.9))
x=PC$sdev^2
x=x[order(x,decreasing=T)]
x2=cumsum(x)/sum(x)
n.pca=min(which(x2>.9))


preObj.pca <- preProcess(train[,keep.cols],method=c("knnImpute", "pca"), pcaComp=30) #,method=c(,"center","scale","nzv")
train.pca <- predict(preObj.pca, train[,keep.cols])

# test trainpca
modFit.lda.pca <- train(classe~ .,data=train.pca,method="lda") #missing values
modFit.lda.pca$finalModel #factors handled appropriately
confusionMatrix(trainp$classe, predict(modFit.lda.pca,train.pca))$overall['Accuracy']
# 
# #still not bad, will try with model mixing.
# 
# modFit.rpart <- train(classe~ .,data=trainp,method="rpart") #missing values
# modFit.rpart$finalModel
# confusionMatrix(trainp$classe, predict(modFit.rpart,trainp))$overall['Accuracy']
# 
# modFit.nb <- train(classe~ .,data=trainp,method="nb") #missing values
# modFit.nb$finalModel
# confusionMatrix(trainp$classe, predict(modFit.nb,trainp))$overall['Accuracy']

fitControl <- trainControl(method = "cv", #cross validation
                           #number = 1, #100 fold cross-validation so variance not too large
                           allowParallel = T #parallel processing
)

#library(data.table)
tests=list(test=c("lda", "rpart", "rf", "gbm", "lasso1", "lasso2"), 
           pca=c("nonPCA", "PCA"), 
           cv=c( "cv"))
tests=expand.grid(tests)

res=list()
res2=list()
res3=c()
i=1
for (i in 1:nrow(tests)){
  print(i)
  pca=ifelse(tests[i, "pca"]=="nonPCA", 0, 1)
  cv=ifelse(tests[i, "cv"]=="noncv", 0, 1)
  testsi=as.character(tests[i, "test"])
  if (pca) { data1=train.pca  } else {data1=trainp}
 # data1<-data1[x,]
  #if (cv){fitControl1 <-fitControl} else {
  fitControl1=fitControl
  if (testsi=="rpart") {
    try({res[[i]] <- train(classe ~ ., method="rpart",data=data1)})
  } else if (testsi=="lasso1") {
    try({res[[i]] <- train(classe ~ ., method="lasso",data=data1, lambda=5, phi=1,trainControl=fitControl1)})
  } else if (testsi=="lasso2") {
    try({res[[i]] <- train(classe ~ ., method="lasso",data=data1, lambda=5, phi=0,trainControl=fitControl1)})
  } else if (testsi=="gbm") {
    try({res[[i]] <- train(classe ~ ., method="gbm",  data=data1, verbose=F,      trainControl=fitControl1) })
  } else if (testsi=="rf") {
    try({res[[i]] <- train(classe ~ ., method="rf",   data=data1, prox=T,         trainControl=fitControl1) })
  } else {
    try({res[[i]] <- train(classe ~ ., method=testsi, data=data1,                 trainControl=fitControl1)})
  }
  res2[[i]]<-  predict(res[[i]], data1)
  res3[i] <- confusionMatrix(data1$classe, res2[[i]])$overall['Accuracy']
}
save(res, res2, res3, tests, file="workspace.RData")


resx=list()
res2x=list()
res3x=c()
i=1
for (i in 1:nrow(tests)){
  print(i)
  pca=ifelse(tests[i, "pca"]=="nonPCA", 0, 1)
  cv=ifelse(tests[i, "cv"]=="noncv", 0, 1)
  testsi=as.character(tests[i, "test"])
  if (pca) { data1=train.pca  } else {data1=trainp}
  # data1<-data1[x,]
  #if (cv){fitControl1 <-fitControl} else {
  fitControl1=fitControl
  if (testsi=="rpart") {
    try({resx[[i]] <- train(classe ~ ., method="rpart",data=data1)})
  } else if (testsi=="lasso1") {
    try({resx[[i]] <- train(classe ~ ., method="lasso",data=data1, lambda=5, phi=1)})
  } else if (testsi=="lasso2") {
    try({resx[[i]] <- train(classe ~ ., method="lasso",data=data1, lambda=5, phi=0)})
  } else if (testsi=="gbm") {
    try({resx[[i]] <- train(classe ~ ., method="gbm",  data=data1, verbose=F) })
  } else if (testsi=="rf") {
    try({resx[[i]] <- train(classe ~ ., method="rf",   data=data1, prox=T) })
  } else {
    try({resx[[i]] <- train(classe ~ ., method=testsi, data=data1)})
  }
  res2x[[i]]<-  predict(resx[[i]], data1)
  res3x[i] <- confusionMatrix(data1$classe, res2x[[i]])$overall['Accuracy']
}
save(resx, res2x, res3x, tests, file="workspacex.RData")








combinedTestData <- as.data.frame(res2)
combinedTestData$classe=train$classe
comb.fit <- train(classe ~.,method="rf",data=combinedTrainData, prox=TRUE)
combinedTestData$combined.pred = predict(comb.fit, combinedTestData)
confusionMatrix(data1$classe, combinedTestData$combined.pred)$overall['Accuracy']
sapply(combinedTestData, function(x) mean(x==combinedTestData$classe))

#handle missing
# remove NAs
# remove columns that are NA
# na.columns = sapply(train, function(x) sum(is.na(x)))
# na.columns = names(na.columns)[na.columns>19000]
# keep.cols=names(train)%except%na.columns
# sapply(train[,keep.cols], summary) #many factors have 19216 NA's
# modFit <- train(classe~ .,data=train[,keep.cols],method="rpart") #missing values


keep.cols = keep.cols %except% "cvtd_timestamp"

#factor variables
names(train)[sapply(train, class)=="factor"]
train$cvtd_timestamp=strptime(as.character(train$cvtd_timestamp), "%d/%m/%Y %H:%M")
names(train)[sapply(train, class)=="factor"]

dummies <- dummyVars(classe~., data=train)
trainp = predict(dummies,newdata=train)

keep.cols=keep.cols %except% "cvtd_timestamp"

dummies <- dummyVars(classe~., data=train[,keep.cols])
trainp = predict(dummies,newdata=train[,keep.cols])
#preObj <- preProcess(trainp,method=c("center","scale")) #,method=c(,"center","scale","nzv""knnImpute")
#trainp <- predict(preObj, trainp)
modFit <- train(classe~ .,data=data.frame(trainp, classe=train$classe),method="rpart") #missing values
confusionMatrix(train$classe, predict(modFit,trainp))$overall['Accuracy']



#what to do with timestamp?
#raw_timestamp_part_2
qplot(raw_timestamp_part_1, raw_timestamp_part_2, data=train, color=user_name)
qplot(raw_timestamp_part_1, raw_timestamp_part_2, color=user_name, 
      data=train[train$user_name=="charles",])
#cvtd_timestamp
qplot(raw_timestamp_part_1, cvtd_timestamp, color=user_name, data=train)
qplot(raw_timestamp_part_1, cvtd_timestamp, color=c(new_window), 
      data=train, facets=user_name~.)
qplot(raw_timestamp_part_1, cvtd_timestamp, color=new_window, 
      data=train[train$user_name=="eurico",])

qplot(cvtd_timestamp, classe, color = user_name,data=train)
qplot(cvtd_timestamp, classe, data=train[train$user_name=="adelmo",])
qplot(cvtd_timestamp, classe, data=train[train$user_name=="eurico",])

#only timestamp
keep.cols1 = c("classe", "cvtd_timestamp")
modFit.justtimestamp <- train(classe~ .,data=train[,keep.cols1],method="rpart") #missing values
confusionMatrix(trainp$classe, predict(modFit.justtimestamp,trainp[,keep.cols1]))$overall['Accuracy']
 
#remove timestamp
require(mvbutils)
keep.cols1 = keep.cols %except%"cvtd_timestamp"
modFit.notimestamp <- train(classe~ .,data=trainp[,keep.cols1],method="rpart") #missing values
confusionMatrix(trainp$classe, predict(modFit.notimestamp,trainp[,keep.cols1]))$overall['Accuracy']


#try random forest
keep.cols1 = keep.cols %except%"cvtd_timestamp"
modFit.notimestamp <- train(classe~ .,data=trainp[,keep.cols1],method="rf") #missing values
confusionMatrix(trainp$classe, predict(modFit.notimestamp,trainp[,keep.cols1]))$overall['Accuracy']


#accuracy = 58

#variable selection.  plot correlations


#reduce out of sample variace with cross validation
fitControl <- trainControl(method = "cv", #cross validation
                           number = 100, #100 fold cross-validation so variance not too large
                           allowParallel = T #parallel processing
)
modFit <- train(classe~ .,data=trainp,method="rpart",trControl =fitControl) #missing values
confusionMatrix(trainp$classe, predict(modFit,trainp))
#accuracy = 58

#try to reduce number of factors through PCA (plot?)
preObj <- preProcess(train[,keep.cols%except%c("classe")],method=c("knnImpute")) #,method=c(,"center","scale","nzv")
trainp <- train[,keep.cols[6:(length(keep.cols)-1)]]
preObj <- preProcess(trainp,method=c("knnImpute")) #,method=c(,"center","scale","nzv")
trainp <- predict(preObj, trainp)
PC=prcomp(trainp,scale=T)
n.pca=min(which(PC$importance[3,]>=.9))
x=PC$sdev^2
x=x[order(x,decreasing=T)]
x2=cumsum(x)/sum(x)
n.pca=min(which(x2>.9))
n.pca
preObj <- preProcess(train[,keep.cols],method=c("knnImpute", "pca"), pcaComp=n.pca) #,method=c(,"center","scale","nzv")
trainp <- predict(preObj, train[,keep.cols])
modFit <- train(classe~ .,data=trainp,method="rpart",trControl =fitControl) #missing values
confusionMatrix(trainp$classe, predict(modFit,trainp))
#reduces accuracy too much, maybe regularized regression?

#regularized (ridge, lasso)

#convert username to dummy variable
dummies <- dummyVars(classe~., data=train[,keep.cols%except%"classe"])
predict(dummies,newdata=trainp[,keep.cols%except%"classe"])

#try to label the exercise



#try random forests
rf <- train(classe~ .,data=trainp,method="rf",prox=TRUE) #missing values
rf.CV <- train(classe~ .,data=trainp,method="rf",prox=TRUE,trControl =fitControl) #missing values
gbm <- train(classe ~ ., method="gbm",data=trainp, verbose=FALSE) #boosting with trees
gbm.CV <- train(classe ~ ., method="gbm",data=trainp, verbose=FALSE,trControl =fitControl) #boosted
lasso = train(classe ~ ., method="lasso",data=trainp, lambda=5)
rf <- train(classe~ .,data=trainp,method="lda") #linear discriminant
rf.CV <- train(classe~ .,data=trainp,method="lda",trControl =fitControl) #linear discriminant
lasso.relaxed = train(classe ~ ., method="lasso",data=trainp, lambda=5, phi=1)  #corresponds to the regular Lasso solutions
lasso.relaxed = train(classe ~ ., method="lasso",data=trainp, lambda=5, phi=0) #phi=0 computes the OLS estimates on the set of variables selected by the Lasso
lasso.CV = train(classe ~ ., method="lasso",data=trainp, lambda=5,  phi=0, trControl =fitControl) 


modFit <- train(classe~ .,data=train[,c("classe","pitch_belt","gyros_forearm_y")],method="rpart")
modFit <- train(train$classe~ .,data=train[,c("pitch_belt","gyros_forearm_y")],method="rpart") #doesn't work
modFit <- train(classe~ .,data=train[,keep.cols],preProcess=c("knnImpute"),method="rpart" ) 


#diagnostic plots



load("workspace.RData")


#variable selection
zero.vars = nearZeroVar(train, saveMetrics = T)
keep.cols <- names(train)[!zero.vars$nzv]
names(train)[sapply(train, class)=="factor"]

require(mvbutils)

keep.cols = keep.cols %except% c("user_name", "new_window")

## lots of predictors, use PCA







save.image("workspace.RData")
modFit <- train(classe~ .,data=trainp,method="rpart") #,trControl =fitControl)
#modFit$finalModel
confusionMatrix(trainp$classe, predict(modFit,trainp))

testPC <- predict(preProc,test)
hhpca=confusionMatrix(train$classe,predict(modFit, testPC))


stopCluster(cluster)


