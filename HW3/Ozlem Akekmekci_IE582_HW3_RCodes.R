## *Ozlem Akekmekci*
### *December 6,2021*


## **TASK 1 - On the use of distance information for UwaveGesture Recognition Task **

##A

trainX = as.matrix(read.table("uWaveGestureLibrary_X_TRAIN"))
trainY = as.matrix(read.table("uWaveGestureLibrary_Y_TRAIN"))
trainZ = as.matrix(read.table("uWaveGestureLibrary_Z_TRAIN"))
testX = as.matrix(read.table("uWaveGestureLibrary_X_TEST"))
testY = as.matrix(read.table("uWaveGestureLibrary_Y_TEST"))
testZ = as.matrix(read.table("uWaveGestureLibrary_Z_TEST"))

train = matrix(0, 896, 946) #concetaneted as in HW2 - train
train[,1:316] = trainX[,1:316]
train[,317:631] = trainY[,2:316]
train[,632:946] = trainZ[,2:316]

test = matrix(0, 3582, 946) #concetaneted as in HW2 - test
test[,1:316] = testX[,1:316]
test[,317:631] = testY[,2:316]
test[,632:946] = testZ[,2:316]

library(TunePareto) #for generateCVRuns
library(FastKNN) #for knn_test_function
library(data.table)

testLength = nrow(test) #will be used while generating distance matrices afterwards.
trainLength = nrow(train)

set.seed(80)
trainClass = as.matrix(train[,1]) #since first columns consist the class information.
testClass = as.matrix(test[,1])
kLev = c(1:10) #number of neighbors
folds = 10    #10-fold CV, as given in the question.
#number of replications is set to be 1 for simplicity.

euclideanTrain = matrix(rep(0,trainLength^2),trainLength)  #euclidean distance matrix
for(i in 1:trainLength)
  for(j in 1:trainLength)
    euclideanTrain[j,i] = sqrt(sum((train[i,]-train[j,])^2))


manhattanTrain = matrix(rep(0,trainLength^2),trainLength)  #manhattan distance matrix
for(i in 1:trainLength)
  for(j in 1:trainLength)
    manhattanTrain[j,i] = sum(abs(train[i,]-train[j,]))

partitions = generateCVRuns(trainClass, ntimes = 1, nfold = 10, leaveOneOut = FALSE, stratified = FALSE)


i=1 #since number of replications is set to be 1.

cvEuc=data.table()  #blank data table
for(j in 1:folds)
{
  testIndices=partitions[[i]][[j]] #by using the partitions generated above.
  cvTrain=train[-testIndices,] #creating CV test and training data
  cvTest=train[testIndices,]
  
  for(k in kLev)
  {
    knnEuc=knn_test_function(cvTrain,cvTest, euclideanTrain[testIndices,-testIndices],trainClass[-testIndices],k=k)
    cvEuc=rbind(cvEuc,data.table(replication=i,fold=j,method='knn',kLev=k,testID=testIndices, predictions=as.numeric(as.character(knnEuc)), real=trainClass[testIndices]))
  }
}


cvEuc[,list(accuracy=mean(predictions==real)),by=kLev]

cvMan=data.table()
for(j in 1:folds)
{
  testIndices=partitions[[i]][[j]]
  
  cvTrain=train[-testIndices,]        
  cvTest=train[testIndices,]
  
  for(k in kLev)
  {
    knnMan=knn_test_function(cvTrain,cvTest, manhattanTrain[testIndices,-testIndices],trainClass[-testIndices],k=k)
    cvMan=rbind(cvMan,data.table(replication=i,fold=j,Method='knn',kLev=k,testID=testIndices, predictions=as.numeric(as.character(knnMan)), real=trainClass[testIndices]))
  }
}

cvMan[,list(accuracy=mean(predictions==real)),by=kLev]


##B

#Euclidean, k=4,

euclideanDistance = matrix(rep(0,testLength*trainLength),ncol=trainLength)  #Euclidean distance matrix for the distances between test and training data

for(i in 1:trainLength) 
  for(j in 1:testLength)
    euclideanDistance[j,i] = sqrt(sum((train[i,]-test[j,])^2))


euclideanPred = as.numeric(knn_test_function(train,test,euclideanDistance,trainClass,k=4)) #predictions
euclideanAccur= sum(euclideanPred==testClass)/testLength 
euclideanAccur #accuracy for Euclidean model

table(euclideanPred,testClass) #confusion matrix for Euclidean model

timeEuclidean = system.time(knn_test_function(train,test,euclideanDistance,trainClass,k=4))
timeEuclidean #runtime for the Euclidean model


#Manhattan, k=5,

manhattanDistance = matrix(rep(0,testLength*trainLength),ncol=trainLength)  #Manhattan distance matrix for the distances between test and training data

for(i in 1:trainLength)
  for(j in 1:testLength)
    manhattanDistance[j,i] = sum(abs(train[i,]-test[j,]))

manhattanPred = as.numeric(knn_test_function(train,test,manhattanDistance,trainClass,k=5))
manhattanAccur = sum(manhattanPred==testClass)/testLength
manhattanAccur #accuracy for the Manhattan model

table(manhattanPred,testClass) #confusion matrix for Manhattan model

timeManhattan = system.time(knn_test_function(train,test,manhattanDistance,trainClass,k=5))
timeManhattan #runtime for the Manhattan model

##C - Comments



## **TASK 2 - Linear models on alternative representations of the data **

library("caTools") #for logistic regression
library(data.table)
library(glmnet)
library(glmpathcr)

##A

classificationTrain = as.data.table(copy(train))
classificationTrain [,three:=as.numeric(V1==3)]
classificationTrain [,V1:=NULL]

classificationTest = as.data.table(copy(test))
classificationTest [,three:=as.numeric(V1==3)]
classificationTest [,V1:=NULL]

ratio = sum(classificationTrain$three)/nrow(classificationTrain) #ratio of the class3 instances in the training data
ratio

trainn = as.data.table(train)
testt=as.data.table(test)

model = glm(three~., classificationTrain, family="binomial") 
probabilities =  predict(model,classificationTest, type = "response")
predicted = ifelse(probabilities>ratio, "1", "0")

table(classificationTest$three, predicted)  #confusion matrix

mean(predicted==classificationTest$three)   #accuracy

##B

set.seed(60)
x = model.matrix(three~., classificationTrain)
lasso = cv.glmnet(x, classificationTrain$three, family="binomial", alpha=1, nfolds=10, type.measure="deviance") #alpha=1 due to Lasso

plot(lasso)
lasso$lambda.min   
lasso$lambda.1se  
lasso

#final model and prediction on the test data with lambdamin
lassoMin = glmnet(x, classificationTrain$three, alpha=1, family="binomial", lambda=lasso$lambda.min)
test = model.matrix(three~., classificationTest)
probabilities2 = predict(lassoMin, test)
predicted2 = ifelse(probabilities2>ratio, "1", "0")

table(classificationTest$three, predicted2)  #confusion matrix

coef1=coef(lassoMin, lasso$lambda.min)
coef1[coef1[,1]!=0,] #to show only the nonzero coefficients

mean(predicted2==classificationTest$three) ##accuracy

#final model and prediction on the test data with lambda1se
lasso1se = glmnet(x, classificationTrain$three, alpha=1, family="binomial", lambda=lasso$lambda.1se)
test = model.matrix(three~., classificationTest)
probabilities3 = predict(lasso1se, test)
predicted3 = ifelse(probabilities3>ratio, "1", "0")

table(classificationTest$three, predicted3)  #confusion matrix

coef2 = coef(lasso1se, s=lasso$lambda.1se)
coef2[coef2[,1]!=0,] #to see only the nonzero coefficients

mean(predicted3==classificationTest$three) ##accuracy


##C

eucTrain = matrix(0,trainLength,trainLength)
for(i in 1:trainLength)
  for(j in 1:trainLength)
    eucTrain[j,i] = sqrt(sum((train[i,]-train[j,])^2))


lassoC = glmnet(eucTrain, classificationTrain$three, alpha=1, family="binomial", lambda=lasso$lambda.min)


eucTest = matrix(0, nrow=testLength, ncol=trainLength)

for(i in 1:trainLength)
  for(j in 1:testLength)
    eucTest[j,i] = sqrt(sum((train[i,]-test[j,])^2))


probabilities4 = predict(lassoC, eucTest)
predicted4 = ifelse(probabilities2>ratio, "1", "0")
table(classificationTest$three, predicted4)  #confusion matrix


mean(predicted4==classificationTest$three) ##accuracy

coef3=coef(lassoC, lasso$lambda.min)
coef3[coef3[,1]!=0,] #to show only the nonzero coefficients

##D - Comments
