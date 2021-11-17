#TASK1 - Dimensionality Reduction

dat = read.csv('IE582_HW2data.csv',header=T)
dat[dat$class=="a",3] = 1 #assign 1 to type a
dat[dat$class=="b",3] = 2 #assign 2 to type b


lev=as.numeric(dat[,3]) #transform char to numeric
plot(dat[,1],dat[,2],col=lev, pch=lev,
     xlab=names(dat)[1],ylab=names(dat)[2]) 
legend("topleft",paste("Class",unique(dat[,3])),
       col=unique(lev), pch= unique(lev)) 

#A
dat[,3] = lev #to convert 3rd column to numeric before applying pca
pca = princomp(dat[,1:2], cor=T)
summary(pca, loadings=T)

plot(pca$scores[,1], col = lev, main="PCA")

#B


dist_dat = dist(dat[,1:2], method="euclidean")
mds_dat = cmdscale(dist_dat, eig=TRUE, k=1)
plot(mds_dat$points, col=lev, main="MDS with Euclidean Distance")

dist_dat2 = dist(dat[,1:2], method="manhattan")
mds_dat2 = cmdscale(dist_dat2, eig=TRUE, k=1)
plot(mds_dat2$points, col=lev, main="MDS with Manhattan Distance")

#C Comments
#D 

newdat = matrix(0, 198, 6) #generate new matrix for 6 variables
dat[,3] = as.numeric(dat[,3])
datmat = as.matrix(dat)

newdat[,1:3] = datmat[,1:3]

newdat[,4] = newdat[,1]^2 #generate (x_1)^2
newdat[,5] = newdat[,2]^2 #generate (x_2)^2
newdat[,6] = newdat[,1]*newdat[,2] #generate (x_1)*(x_2)

pcanew = princomp(newdat, cor=T)
summary(pcanew, loadings=T)


#TASK2 - Reconstructing Turkey Map

mesafe <- read.csv("ilmesafeprep.csv")
mesafemat = as.matrix(mesafe)

mesafemat[lower.tri(mesafemat)] = t(mesafemat)[lower.tri(t(mesafemat))]

ilmesafe = matrix(rep(0,81*81),81)
ilmesafe[lower.tri(mesafemat)] = t(mesafemat)[lower.tri(t(mesafemat))]
ilmesafe[upper.tri(mesafemat)] = mesafemat[upper.tri(mesafemat)]


ilmesafe = as.data.frame(ilmesafe)
colnames(ilmesafe) <- colnames(mesafemat)


il.location = cmdscale(ilmesafe, k=2)    
plot(il.location, type="n", xlab="", ylab="", main ="Turkey MDS Map")    
text(il.location,labels=names(ilmesafe), cex=0.6)


#TASK3 - Dimensionality reduction for time series data

#A

trainXX = read.table("uWaveGestureLibrary_X_TRAIN")
trainYY = read.table("uWaveGestureLibrary_Y_TRAIN")
trainZZ = read.table("uWaveGestureLibrary_Z_TRAIN")


trainX = as.matrix(trainXX[,-1])
trainY = as.matrix(trainYY[,-1])
trainZ = as.matrix(trainZZ[,-1])


velocityX = matrix(0, 896, 315)
velocityY = matrix(0, 896, 315)
velocityZ = matrix(0, 896, 315)
axisX = matrix(0, 896, 315)
axisY = matrix(0, 896, 315)
axisZ = matrix(0, 896, 315)

for (i in 1:896)
{
  velocityX[i,] = cumsum(trainX[i,])
  velocityY[i,] = cumsum(trainY[i,])
  velocityZ[i,] = cumsum(trainZ[i,])
  axisX[i,] = cumsum(velocityX[i,])
  axisY[i,] = cumsum(velocityY[i,])
  axisZ[i,] = cumsum(velocityZ[i,])
}

g = list()
for (i in 1:8)
{
  g[[i]] = which (trainXX[,1]==i)[[1]]
}

install.packages("scatterplot3d")
library(scatterplot3d)

for (i in 1:8)
{
  scatterplot3d(axisX[g[[i]],], axisY[g[[i]],], axisZ[g[[i]],], main = c("Gesture",i), xlab="X", ylab="Y", zlab = "Z")
}


#B

concXYZ = matrix(0, 896, 946)
trainXX_matrix = as.matrix(trainXX)

concXYZ[,1:316] = trainXX_matrix [,1:316]
concXYZ[,317:631] = trainY[,1:315]
concXYZ[,632:946] = trainZ[,1:315]


library(dplyr)
concXYZ1 = as.data.frame(concXYZ)

class = list()
for(i in 1:8)
{
  class[[i]] = filter(concXYZ1, V1 == i)
  class[[i]] = class[[i]][,-1]
}



pcalist = list()

for(i in 1:8)
{
  pcalist[[i]]=princomp(t(class[[i]]), cor=T)
}

summaries = list()
for(i in 1:8)
{
  summaries[[i]]=summary(pcalist[[i]])plotlist = list()
for (i in 1:8)
{
  plotlist[[i]]=
    {
plot(pcalist[[i]]$scores[,1], type="l", col="red", main=c("Eigenvectors for class",i), ylab="")
lines(pcalist[[i]]$scores[,2], col="blue")
legend("topleft", legend=c("Eigenvector 1", "Eigenvector 2"), col=c("red", "blue"), lty = 1, cex=0.4)
    }
}
}

