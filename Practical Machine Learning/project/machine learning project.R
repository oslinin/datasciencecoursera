library(kernlab); data(spam); set.seed(333)
# create train and test sets
inTrain <- createDataPartition(y=spam$type,p=0.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
# create preprocess object
preProc <- preProcess(log10(training[,-58]+1),method="pca",pcaComp=2)
# calculate PCs for training data
trainPC <- predict(preProc,log10(training[,-58]+1))
# run model on outcome and principle components
modelFit <- train(training$type ~ .,method="glm",data=trainPC)
# calculate PCs for test data
testPC <- predict(preProc,log10(testing[,-58]+1))
# compare results
confusionMatrix(testing$type,predict(modelFit,testPC))

#html file with analysis < 5 figures
#build model
#cross validation (createFolds(); createResample(); creatTimeSlices() OR trainControl(method=cv))
#expected out of sample error
#apply to test.

#cross validation: needed for unbiased measurement of out of sample accuracy
   # random, kfold (larger k=less bias, more variance; more k more df more variance),leave one out
#impute missing values: careful
#center, scale, normalize: boxCox
#nearzerovar, PCA (spam::prcomp or caret::preProcess(pcaComp))
#cross validation: 
#trees, 
#model based: lm, glm, linear discriminant
#bagging: bootstrap and average.  keep bias but reduce variance.  Helps with coarse trees.
#boosting: run simple (or complex) classifiers, overweight missed obs for next iter. and avg.
#random forests: build trees with random variates and samples at each step.
#ensemble methods

#model selection
#accuracy mean(rule1(spam$capitalAve)==spam$type), concordance

## http://sux13.github.io/DataScienceSpCourseNotes/8_PREDMACHLEARN/Practical_Machine_Learning_Course_Notes.html
source("G:/knitr.R")
library(iterators)
library(foreach)
library("doMC")
library(caret)
library(mvbutils)
library("RANN") #missing values
library("e1071") 
library("doParallel")
library(dplyr)

setwd("//msad/root/NA/NY/users/slinino/My Documents/R/datasciencecoursera-gh-pages/Practical Machine Learning/project")
train<-read.csv("pml-training.csv",row.names=1, na.strings=c("NA", "", "#DIV/0!"))
test<-read.csv("pml-testing.csv",row.names=1, na.strings=c("NA", "", "#DIV/0!"))
#because data comes split, no need for 
#inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
#training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

#predict classe variable

#http://sux13.github.io/DataScienceSpCourseNotes/8_PREDMACHLEARN/Practical_Machine_Learning_Course_Notes.html#multicore-parallel-processing
#http://ark.intel.com/products/75043/Intel-Core-i5-4570-Processor-6M-Cache-up-to-3_60-GHz
doMC::registerDoMC(cores=4)
#https://github.com/lgreski/datasciencectacontent/blob/master/markdown/pml-randomForestPerformance.md

cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

table(train$user_name)
fitControl <- trainControl(method = "cv", #cross validation
                           number = 100, #100 fold cross-validation so variance not too large
                           allowParallel = TRUE #parallel processing
                           )

#use PCA rather than nearZeroVar because all numerical values
head(train)

# 1. drop columns: names of participants (add back later and see if performance improves.)
# remove the Y from the dataset, as well as any other stuff.
# to avoid taking Y out during preprocessing.



##questions about some variables
## make sure all factor variables can be converted to dummies. 
##cvtd_timestamp is redundant to raw_timestamp_part_1
names(train)[sapply(train, class)=="factor"]
x=strptime(as.character(train$cvtd_timestamp), "%d/%m/%Y %H:%M")
plot(x,train$raw_timestamp_part_1 )
plot(x,train$raw_timestamp_part_2 )
train$cvtd_timestamp=NULL
table(train$user_name, train$new_window)

x=train$raw_timestamp_part_1; x=(x-mean(x))/sd(x)
qplot(x , new_window, color = user_name, data=train)
qplot(raw_timestamp_part_1 , new_window,color=classe, data=subset(train, user_name=="eurico"))
qplot(raw_timestamp_part_1, user_name, data=train )

qplot(user_name, raw_timestamp_part_1, data=train, geom=c("boxplot","jitter"))
qplot(user_name, num_window, color=c(classe), data=train, geom=c("boxplot","jitter"))

qplot(num_window, raw_timestamp_part_1,data=train, color=user_name)


p <- ggplot(train, aes(num_window, raw_timestamp_part_1,color=user_name))
p + geom_point(shape=1)
#p + facet_grid(user_name ~ .)
p


qplot(num_window, raw_timestamp_part_1,data=train[train$user_name%in%c("adelmo"),], color=user_name)
qplot(num_window, raw_timestamp_part_1,data=train[train$user_name%in%c("carlitos"),], color=user_name)

x <- subset(train, num_window>750 & user_name=="carlitos")
qplot(num_window, raw_timestamp_part_1,data=x, color=user_name) # num_window is categorical?
qplot(raw_timestamp_part_1, raw_timestamp_part_2,data=x, color=num_window)
summary(train$raw_timestamp_part_1, by=train$user_name)

train %>% group_by(user_name)%>%subset(select(raw_timestamp_part_1))%>%summary()

x = split(train$raw_timestamp_part_1, train$user_name)
x2=sapply(x, range)
p<-function(...) format(...,big.mark=",", trim=TRUE, digits = 2, scientific = FALSE)

#classe is the Y variable.  we need it for 
which(names(train)=="classe")
x=train[train$user_name]
# can we use username to train
all(as.character(test$user_name)%in%train$user_name)
# Although this will not work in general, we may get better performance
names(train)[sapply(train, class)=="factor"]
# 2. remove zero var variables  http://sux13.github.io/DataScienceSpCourseNotes/8_PREDMACHLEARN/Practical_Machine_Learning_Course_Notes.html#removing-zero-covariates
# 50 variables removed, e.g. skewness_yaw_dumbbell
summary(train$magnet_forearm_z)
zero.vars = nearZeroVar(train, saveMetrics = T)
keep.cols <- names(train)[!zero.vars$nzv]

dummies <- dummyVars(classe~., data=train[,keep.cols])
head(predict(dummies,newdata=train[,keep.cols]))
head(predict(dummies,newdata=test[,keep.cols%except%"classe"]))

summary(train$user_name, as.numeric(train$raw_timestamp_part_1))
p1 <- qplot(user_name,num_window, data=train,geom=c("boxplot"))
p1

# 3. impute missing values
# create preProcess object for all predictors
preObj <- preProcess(train2,method="knnImpute") #,method=c("center","scale")
# normalize training set
train3 <- predict(preObj, train2)

modFit <- train(Y~ .,data=train3,method="glm")
modFit$finalModel

stopCluster(cluster)