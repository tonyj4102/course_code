movies = read.table("movieLens.txt", header=FALSE, sep="|", quote="\"") #so read in properly
colnames(movies) = c("ID", "Title", "ReleaseDate","VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

#remove variables not to be used

movies$ID=NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB= NULL
movies = unique(movies)
str(movies)

table(movies)

distance = dist(movies[2:20], method="euclidean") ##compute distances on genre variable
clusterMovies = hclust(distance, method="ward" )  ##ward - cares about distance and variance
plot(clusterMovies)

#need to assign clusters to groups
# number of clusters of movies # choose 10 clusters/genre groups

clusterGroups=cutree(clusterMovies, k=10)# label each datapoints by cluster
tapply(movies$Action, clusterGroups, mean) #% of movies in each cluster that belong in the cluster

#% of action in each cluster

#what cluster is men in black in?
subset(movies, Title=="Men in Black (1997)")

#men is black in 257 row
#which cluster

clusterGroups[257]
# 257 
# 2 (cluster #2-action , adventure sci fi cluster)

cluster2=subset(movies, clusterGroups == 2)

#movies like men in black
cluster2$Title[1:10]

table(cluster2)

clusterGroups=cutree(clusterMovies, k=2)# label each datapoints by cluster
tapply(movies$Action, clusterGroups, mean) #% of movies in each cluster that belong in the cluster

#% of action in each cluster

cluster2=subset(movies, clusterGroups == 2)
head(cluster2)
cluster2$Title[1:10]
cluster3=cluster2
cluster3$Title=NULL
colSums(cluster3)


##HW

##HW  

dailykos = read.csv("dailykos.csv")   

kosDist = dist(dailykos, method="euclidean")

#create cluster

kosHierClust = hclust(kosDist, method="ward")

plot(kosHierClust)

#define size of cluster

clusterGroupsA=cutree(kosHierClust, k=7)

# create a dataset for each cluster

cluster1a=subset(dailykos, clusterGroupsA == 1)
cluster2a=subset(dailykos, clusterGroupsA == 2)
cluster3a=subset(dailykos, clusterGroupsA == 3)
cluster4a=subset(dailykos, clusterGroupsA == 4)
cluster5a=subset(dailykos, clusterGroupsA == 5)
cluster6a=subset(dailykos, clusterGroupsA == 6)
cluster7a=subset(dailykos, clusterGroupsA == 7)

OR
HierCluster = split(dailykos, hierGroups)
#Then cluster 1 can be accessed by typing HierCluster[[1]], cluster 2 can be accessed by typing HierCluster[[2]]

head(cluster1a)
str(cluster1a)

#top 6 words in each cluster
#This computes the mean frequency values of each of the words in cluster 1, and then outputs the 6 words that occur the most frequently.
tail(sort(colMeans(cluster4a)))
state republican       poll   democrat      kerry       bush 
0.7575039  0.7590837  0.9036335  0.9194313  1.0624013  1.7053712 
#most frequent word in terms of average value
tail(sort(colMeans(cluster7a)))



#1 Specify number of clusters
k = 7  # example by different colors based on the image

#2--random group
#3--compute mean
#4--re-assign 
#5-- update mean
#6--repeat 4 and 5 until no improvement is made

# Run k-means
set.seed(1000)

dailykos = read.csv("dailykos.csv")
KmeansCluster = kmeans(dailykos, centers=7)

KmeansCluster1 = subset(dailykos, KmeansCluster$cluster == 1)

KmeansCluster2 = subset(dailykos, KmeansCluster$cluster == 2)

KmeansCluster3 = subset(dailykos, KmeansCluster$cluster == 3)

KmeansCluster4 = subset(dailykos, KmeansCluster$cluster == 4)

KmeansCluster5 = subset(dailykos, KmeansCluster$cluster == 5)

KmeansCluster6 = subset(dailykos, KmeansCluster$cluster == 6)

KmeansCluster7 = subset(dailykos, KmeansCluster$cluster == 7)

tail(sort(colMeans(KmeansCluster6)))

#$ cluster     : int [1:365636] 3 3 3 3 3 3 3 3 3 3 ...
#$ centers     : num [1:5, 1] 0.4818 0.1062 0.0196 0.3094 0.1842  #mean intensity value ##grey scale color of each 
#$ size        : int [1:5] 20556 101085 133162 31555 79278

# Extract clusters
healthyClusters = KMC$cluster
KMC$centers[2]   #mean intensity value of the 2nd cluster

# Plot the image with the clusters
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))

image(healthyClusters, axes = FALSE, col=rainbow(k))


HW#2

airlines=read.csv("AirlinesCluster.csv")
str(airlines)
colSums(airlines)

#normalizing data
install.packages("caret")
library(caret)
#create a normalized dataframe
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

airlinesNorm$Balance[which.max(airlinesNorm$Balance)]
airlinesNorm$QualMiles[which.max(airlinesNorm$QualMiles)]
airlinesNorm$BonusMiles[which.max(airlinesNorm$BonusMiles)]
airlinesNorm$BonusTrans[which.max(airlinesNorm$BonusTrans)]
airlinesNorm$FlightMiles[which.max(airlinesNorm$FlightMiles)]
airlinesNorm$FlightTrans[which.max(airlinesNorm$FlightTrans)]
airlinesNorm$DaysSinceEnroll[which.max(airlinesNorm$DaysSinceEnroll)]

# Change the data type to matrix #1
airlinesNormMatrix = as.matrix(airlinesNorm)
str(airlinesNormMatrix)

# Turn matrix into a vector  #2
airlinesNormVector = as.vector(airlinesNormMatrix)
str(airlinesNormVector)

#num [1:2500] 0.0991 0.0991 0.1034 0.1034 0.1034 ...

airlinesNormVector = as.vector(airlinesNorm)
str(airlinesNormVector)


# Compute distances
distance = dist(airlinesNorm, method = "euclidean")

##HIERACHICAL CLUSTERS
clusterIntensity = hclust(distance, method="ward") #MINMUM VARIANCE TRYING TO FIND COMPACT CLUSTERS
plot(clusterIntensity)

airlinesClusters = cutree(clusterIntensity, k = 5) #ASSIGNING TO A CLUSTER

# create a dataset for each cluster

cluster1aa=subset(dailykos, airlinesClusters == 1)
cluster2aa=subset(dailykos, airlinesClusters == 2)
cluster3aa=subset(dailykos, airlinesClusters == 3)
cluster4aa=subset(dailykos, airlinesClusters == 4)
cluster5aa=subset(dailykos, airlinesClusters == 5)

tapply(airlines$Balance, airlinesClusters, mean)
tapply(airlines$QualMiles, airlinesClusters, mean)
tapply(airlines$BonusMiles, airlinesClusters, mean)
tapply(airlines$BonusTrans, airlinesClusters, mean)
tapply(airlines$FlightMiles, airlinesClusters, mean)
tapply(airlines$FlightTrans, airlinesClusters, mean)
tapply(airlines$DaysSinceEnroll, airlinesClusters, mean)

set.seed(88)

KmeansCluster = kmeans(airlinesNorm, centers=5,iter.max = 1000)

cluster1ab=subset(airlinesNorm, KmeansCluster$cluster == 1)
cluster2ab=subset(airlinesNorm, KmeansCluster$cluster == 2)
cluster3ab=subset(airlinesNorm, KmeansCluster$cluster == 3)
cluster4ab=subset(airlinesNorm, KmeansCluster$cluster == 4)
cluster5ab=subset(airlinesNorm,KmeansCluster$cluster == 5)


#

stocks=read.csv("StocksCluster.csv")

str(stocks)
table(stocks$PositiveDec)
6324/(6324+5256)
cor(stocks)
mean(stocks$ReturnJan)  
mean(stocks$ReturnFeb)  
     mean(stocks$ReturnMar)
          mean(stocks$ReturnApr)
               mean(stocks$ReturnMay)
                    mean(stocks$ReturnJune)
                         mean(stocks$ReturnJuly)
                              mean(stocks$ReturnAug)
                                   mean(stocks$ReturnSep)  
                                        mean(stocks$ReturnOct)  
                                             mean(stocks$ReturnNov) 



install.packages("caTools")
library(caTools)

set.seed(144)

spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain = subset(stocks, spl == TRUE)

stocksTest = subset(stocks, spl == FALSE)

StocksModel = glm(PositiveDec ~ ., data=stocksTrain, family=binomial)
summary(StocksModel)
predictTrain = predict(StocksModel, data=stocksTrain, type="response") 
predictTest = predict(StocksModel, newdata=stocksTest, type="response") 
# give probabilities
table(stocksTrain$PositiveDec, predictTrain > 0.5)
table(stocksTest$PositiveDec, predictTest > 0.5)

(3640+990)/(3640+2689+787+990)
(417+1553)/(417+1553+1160+344)


table(stocksTest$PositiveDec)
1897/(1897+1577)

#CLUSTER STOCKS

limitedTrain = stocksTrain

limitedTrain$PositiveDec = NULL

limitedTest = stocksTest

limitedTest$PositiveDec = NULL

library(caret)

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)

mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)


set.seed(144)

km = kmeans(normTrain, centers=3)

cluster1r=subset(normTrain, km$cluster == 1)
cluster2r=subset(normTrain, km$cluster == 2)
cluster3r=subset(normTrain, km$cluster == 3)

library(flexclust)

km.kcca = as.kcca(km, normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)

cluster1v=subset(normTest, clusterTest == 1)
cluster2v=subset(normTest, clusterTest == 2)
cluster3v=subset(normTest, clusterTest == 3)
