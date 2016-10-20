## http://sux13.github.io/DataScienceSpCourseNotes/8_PREDMACHLEARN/Practical_Machine_Learning_Course_Notes.html
source("G:/knitr.R")
setwd("//msad/root/NA/NY/users/slinino/My Documents/R/datasciencecoursera-gh-pages/datasciencecoursera-gh-pages/Practical Machine Learning/project")
train<-read.csv("pml-training.csv")
test<-read.csv("pml-testing.csv")
#because data comes split, no need for 
#inTrain <- createDataPartition(y=Wage$wage,p=0.7, list=FALSE)
#training <- Wage[inTrain,]; testing <- Wage[-inTrain,]

#predict classe variable

#http://sux13.github.io/DataScienceSpCourseNotes/8_PREDMACHLEARN/Practical_Machine_Learning_Course_Notes.html#multicore-parallel-processing
doMC::registerDoMC(cores=4)

#use PCA rather than nearZeroVar because all numerical values
head(train)

# 1. drop columns: names of participants (add back later and see if performance improves.)
# remove the Y from the dataset, as well as any other stuff.
# to avoid taking Y out during preprocessing.
y.index = which(names(train2)=="classe")
Y = train[[y.index]]
train = train[,-y.index]

# 2. remove zero var variables  http://sux13.github.io/DataScienceSpCourseNotes/8_PREDMACHLEARN/Practical_Machine_Learning_Course_Notes.html#removing-zero-covariates
zero.vars = nearZeroVar(train, saveMetrics = T)
train2 <- train[, which(names(train) %in% names(train)[zero.vars$nzv]) ]

# 3. impute missing values
i = which(names(train2)=="classe")
preObj <- preProcess(train2,method="knnImpute") #,method=c("center","scale")

modFit <- train(classe~ .,data=train2,method="glm")
modFit$finalModel

#html file with analysis < 5 figures
#build model
#cross validation
#expected out of sample error
#apply to test.

#cross validation
#impute missing values: careful
#nearzerovar, PCA
#cross validation
#trees, 
#model based: lm, glm, linear discriminant
#bagging: bootstrap and average.  keep bias but reduce variance.  Helps with coarse trees.
#boosting: run simple (or complex) classifiers, overweight missed obs for next iter. and avg.
#random forests: build trees with random variates and samples at each step.


#model selection