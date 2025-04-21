# Unit 6 - Recitation

# Video 2

flower = read.csv("flower.csv", header=FALSE)  # no headers
str(flower)

#50x50 pixel matrix

#$ V45: num  0.0259 0.0259 0.0259 0.0388 0.0474 ...
#$ V46: num  0.0259 0.0172 0.0172 0.0259 0.0345 ...

# Change the data type to matrix #1
flowerMatrix = as.matrix(flower)
str(flowerMatrix)

# Turn matrix into a vector  #2
flowerVector = as.vector(flowerMatrix)
str(flowerVector)

#num [1:2500] 0.0991 0.0991 0.1034 0.1034 0.1034 ...

flowerVector2 = as.vector(flower)
str(flowerVector2)


# Compute distances
distance = dist(flowerVector, method = "euclidean")

##HIERACHICAL CLUSTERS
clusterIntensity = hclust(distance, method="ward") #MINMUM VARIANCE TRYING TO FIND COMPACT CLUSTERS

# Plot the dendrogram
plot(clusterIntensity)

# Select 3 clusters
rect.hclust(clusterIntensity, k = 3, border = "red") # SEE BORDERS FOR CLUSTERS

flowerClusters = cutree(clusterIntensity, k = 3) #ASSIGNING TO A CLUSTER
flowerClusters

# Find mean intensity values
tapply(flowerVector, flowerClusters, mean) #GROUP TO FLOWER VECTOR ACCORDIG TO FLOWER CLUSTER

# Plot the image and the clusters
dim(flowerClusters) = c(50,50)    #NEED TO CONVERT VECTOR TO MATRIX 50X50 PCITURE
image(flowerClusters, axes = FALSE)

# Original image
image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))



# Video 4

# Let's try this with an MRI image of the brain

healthy = read.csv("healthy.csv", header=FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)




#num [1:566, 1:646] 0.00427 0.00855 0.01282 0.01282 0.01282 ...
#- attr(*, "dimnames")=List of 2

# Plot image
image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

# Hierarchial clustering  #ISOLATE THE COLORS

healthyVector = as.vector(healthyMatrix)
distance = dist(healthyVector, method = "euclidean")

# We have an error - why?
str(healthyVector)

#566X646 MATRIX
n=566*646
(n*(n-1))/2
[1] 66844659430

# Video 5

#1 Specify number of clusters
k = 5  # example by different colors based on the image

#2--random group
#3--compute mean
#4--re-assign 
#5-- update mean
#6--repeat 4 and 5 until no improvement is made

# Run k-means
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

#$ cluster     : int [1:365636] 3 3 3 3 3 3 3 3 3 3 ...
#$ centers     : num [1:5, 1] 0.4818 0.1062 0.0196 0.3094 0.1842  #mean intensity value ##grey scale color of each 
#$ size        : int [1:5] 20556 101085 133162 31555 79278

# Extract clusters
healthyClusters = KMC$cluster
KMC$centers[2]   #mean intensity value of the 2nd cluster

# Plot the image with the clusters
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))

image(healthyClusters, axes = FALSE, col=rainbow(k))



# Video 6

# Apply to a test image
 
tumor = read.csv("tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

# Apply K-means cluster results from before to new image, using the flexclust package


install.packages("flexclust")
library(flexclust)  #contains object class (kcca)k-centroids cluster analysis-convert of class kca to use the predict function on the test set vector 

KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector) #cluster pixels in tumor vector using the predict function

#Assigns 1-5 to 

# Visualize the clusters
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))

image(tumorClusters, axes = FALSE, col=rainbow(k))

