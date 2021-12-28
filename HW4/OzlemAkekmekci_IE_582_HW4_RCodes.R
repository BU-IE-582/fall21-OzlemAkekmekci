library(caret)
library(TunePareto)
library(data.table)
library(FastKNN)
library(glmnet)

musk = read.csv("Musk1.csv", header = F)
colnames(musk)[1:2] = c("Label","Bag")
head(musk)

musk_no_label = musk[,-(1:2)]
head(musk_no_label)
K = 10 #number of clusters
musk_k_means = kmeans(musk_no_label,K) #Hartigan-Wong KMeans
musk_k_means$cluster

n_musk = nrow(musk)
n_bag = max(musk$Bag)
S = matrix(0, nrow = n_bag, ncol = K+1)
for(i in 1:n_musk)
{
  S[musk$Bag[i],musk_k_means$cluster[i]]=1
  S[musk$Bag[i],K+1]=musk$Label[i]
}

set.seed(20)     #create test and training data
S = as.data.frame(S)
colnames(S)[11] = c("label")
testindices = createDataPartition(S$label, p = .3,
                                  list = FALSE,
                                  times = 1)
train = S[-testindices,]
test = S[testindices,]

xtrain1 = model.matrix(label~., train) #Lasso for Hartigan-Wong Kmeans
xtest1 = model.matrix(label~., test)

lasso1 = cv.glmnet(xtrain1, train$label, family = "binomial", alpha=1, nfolds=10)
lassopredict1 = predict(lasso1, xtest1, type = "class", s="lambda.min")

accuracy1 = sum(lassopredict1 == test$label)/nrow(test)
accuracy1    #accuracy for Lasso of Hartigan-Wong Kmeans

testLength = nrow(test) #will be used while generating distance matrices afterwards.
trainLength = nrow(train)

trainClass = as.matrix(train[,11]) #since these columns consist the label information.
testClass = as.matrix(test[,11])

kLev = c(1:10) #number of neighbors
folds = 10  #10-fold CV, as given in the question.
#number of replications is set to be 1 for simplicity.


euclideanTrain1 = matrix(rep(0,trainLength^2),trainLength)  #euclidean distance matrix
for(i in 1:trainLength)
  for(j in 1:trainLength)
    euclideanTrain1[j,i] = sqrt(sum((train[i,]-train[j,])^2))


partitions1 = generateCVRuns(trainClass, ntimes = 1, nfold = 10, leaveOneOut = FALSE, stratified = FALSE)

i=1 #since number of replications is set to be 1.

cvEuc1=data.table()  #blank data table
for(j in 1:folds)
{
  testIndices=partitions1[[i]][[j]] #by using the partitions generated above.
  cvTrain=train[-testIndices,] #creating CV test and training data
  cvTest=train[testIndices,]
  
  for(k in kLev)
  {
    knnEuc1=knn_test_function(cvTrain,cvTest, euclideanTrain1[testIndices,-testIndices],trainClass[-testIndices],k=k)
    cvEuc1=rbind(cvEuc1,data.table(replication=i,fold=j,method='knn',kLev=k,testID=testIndices, predictions=as.numeric(as.character(knnEuc1)), real=trainClass[testIndices]))
  }
}

accu2 = cvEuc1[,list(accuracy2=mean(predictions==real)),by=kLev]  #accuracies for kNN of Hartigan-Wong Kmeans

accuracy2=max(accu2$accuracy2)

musk_k_means2 = kmeans(musk_no_label,K, algorithm = "MacQueen") #MacQueen KMeans
musk_k_means2$cluster

S2 = matrix(0, nrow = n_bag, ncol = K+1)
for(i in 1:n_musk)
{
  S2[musk$Bag[i],musk_k_means2$cluster[i]]=1
  S2[musk$Bag[i],K+1]=musk$Label[i]
}

set.seed(20)     #create test and training data
S2 = as.data.frame(S2)
colnames(S2)[11] = c("label")
testindices2 = createDataPartition(S2$label, p = .3,
                                   list = FALSE,
                                   times = 1)
train2 = S2[-testindices2,]
test2 = S2[testindices2,]

xtrain2 = model.matrix(label~., train2) #Lasso for MacQueen kMeans
xtest2 = model.matrix(label~., test2)

lasso2 = cv.glmnet(xtrain2, train2$label, family = "binomial", alpha=1, nfolds=10)
lassopredict2 = predict(lasso2, xtest2, type = "class", s="lambda.min")

accuracy3 = sum(lassopredict2 == test2$label)/nrow(test2)
accuracy3    #accuracy for Lasso of MacQueen kMeans

testLength2 = nrow(test2) #will be used while generating distance matrices afterwards.
trainLength2 = nrow(train2)

trainClass2 = as.matrix(train2[,11]) #since these columns consist the label information.
testClass2 = as.matrix(test2[,11])

#number of kLev, folds and repetitions remains same. 

euclideanTrain2 = matrix(rep(0,trainLength2^2),trainLength2)  #euclidean distance matrix
for(i in 1:trainLength2)
  for(j in 1:trainLength2)
    euclideanTrain2[j,i] = sqrt(sum((train2[i,]-train2[j,])^2))


partitions2 = generateCVRuns(trainClass2, ntimes = 1, nfold = 10, leaveOneOut = FALSE, stratified = FALSE)

i=1 #since number of replications is set to be 1.

cvEuc2=data.table()  #blank data table
for(j in 1:folds)
{
  testIndices2=partitions2[[i]][[j]] #by using the partitions generated above.
  cvTrain2=train[-testIndices2,] #creating CV test and training data
  cvTest2=train[testIndices2,]
  
  for(k in kLev)
  {
    knnEuc2=knn_test_function(cvTrain2,cvTest2, euclideanTrain2[testIndices2,-testIndices2],trainClass2[-testIndices2],k=k)
    cvEuc2=rbind(cvEuc2,data.table(replication=i,fold=j,method='knn',kLev=k,testID=testIndices2, predictions2=as.numeric(as.character(knnEuc2)), real=trainClass2[testIndices2]))
  }
}

cvEuc2[,list(accuracy4=mean(predictions2==real)),by=kLev]  #accuracies for kNN of MacQueen kMeans
accu4 = cvEuc2[,list(accuracy4=mean(predictions2==real)),by=kLev]

accuracy4 = max(accu4$accuracy4)

accuracytable = matrix(0, nrow=1, ncol=4)
colnames(accuracytable)[1:4] = c("Lasso.HW", "KNN.HW", "Lasso.MQ", "KNN.MQ")
accuracytable[1:4] = cbind(accuracy1, accuracy2, accuracy3, accuracy4)
accuracytable

