#Ozlem_Akekmekci_IE582_HW1

#TASK1 - Curse of dimensionality and effect of sample size#


n = 1000    #Since we will use larger sample sizes afterwards, I defined such "n".
d_list = list()
for (i in 1:15)  
  d_list[[i]] = matrix(runif(n*i,-1,1),i,n)   

#PartA

d_dist = list()
for (i in 1:15) 
{
  d_dist[[i]] = rep(0,n)
  for(j in 1:n)
    d_dist[[i]][j] = sqrt(sum((d_list[[i]][,j]-0)^2))
}

less_than_1 = rep(0,15)

for (i in 1:15) 
  less_than_1[i] = sum(d_dist[[i]]<=1)

#PartB

d2_pi <- (less_than_1[2]/n)*4
d3_pi <- (less_than_1[3]/n)*6

#PartC

pi_approximator = function(n)
{
  d_list = list()
  for (i in 1:15)
    d_list[[i]] = matrix(runif(n*i,-1,1),i,n)
  
  
  
  d_dist = list()
  for (i in 1:15) 
  {
    d_dist[[i]] = rep(0,n)
    for(j in 1:n)
      d_dist[[i]][j] = sqrt(sum((d_list[[i]][,j]-0)^2))
  }
  
  less_than_1 = rep(0,15)
  
  for (i in 1:15) 
    less_than_1[i] = sum(d_dist[[i]]<=1)
  
  
  
  d2_pi = (less_than_1[2]/n)*4
  d3_pi = (less_than_1[3]/n)*6
  print(c("For sample size: ", n))
  print(c("Pi Approximation in 2D: ",d2_pi))
  print(c("Pi Approximation in 3D: ",d3_pi))
  c(d2_pi,d3_pi)
}

results = matrix(rep(0,6*3),6)
results[,1] = c(1000,5000,10000,25000,50000,100000)

for(i in 1:6)
  results[i,2:3] = pi_approximator(results[i,1])

plot(results[,1],results[,2], col = "blue", pch = 16, ylim = c(3,3.3), main= 'Pi estimations for D=2 and D=3', xlab= "Sample Size", ylab = "Pi Estimation")

points(results[,1],results[,3], col = "red", pch = 17)
lines(results[,1],rep(pi,6), col = "green", )



#PartD


d_list = list()
for (i in 1:15)
  d_list[[i]] = matrix(runif(1000*i,-1,1),i,1000) #above simulation is repeated

test_instances = list()
for (i in 1:15)
  test_instances[[i]] = matrix(runif(100*i,-1,1),i,100) #generated additional 100 test instances for each D.

distance_matrices = list()

for (i in 1:15)
  distance_matrices[[i]] = matrix(rep(0,100^2),100)


dist_fnc = function(x,y) # to find euclidean distance between given two points
{
  sqrt(sum((x-y)^2))
}

for(i in 1:15)
{  for(j in 1:100)
{
  for(k in 1:100)
    distance_matrices[[i]][j,k] = dist_fnc(test_instances[[i]][,j],test_instances[[i]][,k])
  
  distance_matrices[[i]][j,j] = 1000000 #self-distance set to be very large to eliminate it.
}
}

nearest_neighbor_dist = list()

for(i in 1:15)
{
  nearest_neighbor_dist[[i]] = rep(0,100)
  for(j in 1:100)
    nearest_neighbor_dist[[i]][j] = min(distance_matrices[[i]][j,])
}

avg_dists = rep(0,15)

for(i in 1:15)
  avg_dists[i] = mean(nearest_neighbor_dist[[i]])

plot(1:15,avg_dists, main = 'Average distance from the test instances to their nearest neighbors' , xlab= "Average minimum distance", ylab= "Dimension")






# TASK2 - Practicing data manipulation skills on images

#PartA

library(jpeg)
photo <- readJPEG("IE582Photo.jpg", native = FALSE)

dim(photo)

plot(1:512, 1:512, ann=FALSE)
rasterImage(photo,
            0, 0, 512, 512,
            interpolate=FALSE)

#PartB

par(mfrow=c(1,3))
redChannel <- photo
redChannel[,,2:3]=0
plot(0:512, 0:512, type="n")
rasterImage(redChannel,
            0, 0, 512, 512,
            interpolate=FALSE)

greenChannel <- photo
greenChannel[,,c(1,3)]=0
plot(0:512, 0:512, type="n")
rasterImage(greenChannel,
            0, 0, 512, 512,
            interpolate=FALSE)

blueChannel <- photo
blueChannel[,,1:2]=0
plot(0:512, 0:512, type="n")
rasterImage(blueChannel,
            0, 0, 512, 512,
            interpolate=FALSE)

#PartC

plot(colMeans(redChannel)[,1], type="l", main= 'Avg of the columns for each channel', col="red", xlab="", ylab="")
lines(colMeans(greenChannel)[,2], col="green")
lines(colMeans(blueChannel)[,3], col="blue")

#PartD

redhalf1 <- redChannel[1:512, 1:256,]
redhalf2 <- redChannel[1:512, 257:512,]
redSubtract <- redhalf1 - redhalf2
redSubtract[redSubtract<0] <- 0 #negative pixel values are converted to zero.

greenhalf1 <- greenChannel[1:512, 1:256,]
greenhalf2 <- greenChannel[1:512, 257:512,]
greenSubtract <- greenhalf1 - greenhalf2
greenSubtract[greenSubtract<0] <- 0


bluehalf1 <- blueChannel[1:512, 1:256,]
bluehalf2 <- blueChannel[1:512, 257:512,]
blueSubtract <- bluehalf1 - bluehalf2
blueSubtract[blueSubtract<0] <- 0

newPhoto <- redSubtract + greenSubtract + blueSubtract

plot(0:512, 0:512, type="n")
rasterImage(newPhoto,
            0, 0, 256, 512,
            interpolate=FALSE)

par(mfrow=c(1,3))
plot(0:512, 0:512, type="n")
rasterImage(redSubtract,
            0, 0, 256, 512,
            interpolate=FALSE)

plot(0:512, 0:512, type="n")
rasterImage(greenSubtract,
            0, 0, 256, 512,
            interpolate=FALSE)

plot(0:512, 0:512, type="n")
rasterImage(blueSubtract,
            0, 0, 256, 512,
            interpolate=FALSE)

#PartE

redNoise <- matrix(runif(512*512,0,0.1), 512, 512)
greenNoise <- matrix(runif(512*512,0,0.1), 512, 512)
blueNoise <- matrix(runif(512*512,0,0.1), 512, 512)

noisedRed <- redChannel[1:512, 1:512, 1] + redNoise
noisedGreen <- greenChannel[1:512, 1:512, 2] + greenNoise
noisedBlue <- blueChannel[1:512, 1:512, 3] + blueNoise

noisedRed = (noisedRed - min(noisedRed)) / (max(noisedRed)-min(noisedRed))
noisedGreen = (noisedGreen- min(noisedGreen)) / (max(noisedGreen)-min(noisedGreen))
noisedBlue = (noisedBlue - min(noisedBlue)) / (max(noisedBlue)-min(noisedBlue))


noisedPhoto = photo
noisedPhoto[,,1] = noisedRed
noisedPhoto[,,2] = noisedGreen
noisedPhoto[,,3] = noisedBlue


plot(0:512, 0:512, type="n")
rasterImage(noisedPhoto,
            0, 0, 512, 512,
            interpolate=FALSE)


par(mfrow=c(1,3))
redNoised <- noisedPhoto
redNoised[,,2:3]=0
plot(0:512, 0:512, type="n")
rasterImage(redNoised,
            0, 0, 512, 512,
            interpolate=FALSE)

greenNoised <- noisedPhoto
greenNoised[,,c(1,3)]=0
plot(0:512, 0:512, type="n")
rasterImage(greenNoised,
            0, 0, 512, 512,
            interpolate=FALSE)

blueNoised <- noisedPhoto
blueNoised[,,1:2]=0
plot(0:512, 0:512, type="n")
rasterImage(blueNoised,
            0, 0, 512, 512,
            interpolate=FALSE)







