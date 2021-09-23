# 
#1. Download any two datasets of your choice, read it and perform the following tasks
#(a) Visualise the data.
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)

# one dataset
data("ToothGrowth")
head(ToothGrowth)
View(ToothGrowth)
# two dataset
data("PlantGrowth")
head(PlantGrowth)
View(PlantGrowth)



#(b) Perform K-Means Clustering selecting the best value of k and taking Euclidean distance
#as similarity measure. Check your algorithm with the following three conditions
#, visulaise the clusters and note the difference
#i. Maximum number of iterations
#ii. Cluster centroid remains unchanged
#iii. Highest quality of cluster is reached.

# i am trying with ToothGrowth data, here we drop column 2 supp as we are doing clustering problem
MyToothGrowth=select(ToothGrowth,c(1,3))
# omit any missing values
MyToothGrowth <- na.omit(MyToothGrowth)
# As we don’t want the clustering algorithm to depend to an arbitrary variable unit, we start by scaling/standardizing the data using the R function
MyToothGrowthscaled <- scale(MyToothGrowth)
head(MyToothGrowthscaled)
plot(MyToothGrowthscaled)
# here we see about 3 clusters if we visualize with plain eyes, so we may go with k=3 or may be we can run some more logic to get k using elbow method.
fitK <- kmeans(MyToothGrowthscaled,3)

# fitK
#K-means clustering with 3 clusters of sizes 23, 20, 17
#
#Cluster means:
#          len      dose
#1  0.09999948 -0.368730
#2  0.95259072  1.325124
#3 -1.25598838 -1.060099
#
#Clustering vector:
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 
# 3  3  3  3  3  3  3  3  3  3  1  1  1  1  1  1  1  1  1  1  2  2  2  2  2  2  2  2  2  2  3  1  1 
#34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 
# 3  3  3  3  3  1  3  1  1  1  1  1  1  1  1  1  1  2  2  2  2  2  2  2  2  2  2 
#
#Within cluster sum of squares by cluster:
#[1] 8.280169 4.625374 2.547330
# (between_SS / total_SS =  86.9 %)
#
#Available components:
#
#[1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss" "betweenss"   
#[7] "size"         "iter"         "ifault"      



# K means clustering
# extract cluster membership
str(fitK)
# List of 9
# $ cluster     : Named int [1:60] 3 3 3 3 3 3 3 3 3 3 ...
#  ..- attr(*, "names")= chr [1:60] "1" "2" "3" "4" ...
# $ centers     : num [1:3, 1:2] 0.1 0.953 -1.256 -0.369 1.325 ...
#  ..- attr(*, "dimnames")=List of 2
#  .. ..$ : chr [1:3] "1" "2" "3"
#  .. ..$ : chr [1:2] "len" "dose"
# $ totss       : num 118
# $ withinss    : num [1:3] 8.28 4.63 2.55
# $ tot.withinss: num 15.5
# $ betweenss   : num 103
# $ size        : int [1:3] 23 20 17
# $ iter        : int 4
# $ ifault      : int 0
# - attr(*, "class")= chr "kmeans"

plot(MyToothGrowth, col=fitK$cluster)
 

 ## CHOOSING K by iterative process.
k <- list()
for(i in 1:10){
  k[[i]] <- kmeans(MyToothGrowthscaled[,1:2], i)
}

k

# we see 10 iteration with every iteration between_SS / total_SS slightly increasing finally after 10th loop we are getting (between_SS / total_SS =  98.5 %)

betweenss_totss <- list()
for(i in 1:10){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}

plot(1:10, betweenss_totss, type = "b", 
     ylab = "Between SS / Total SS", xlab = "Clusters (k)")

# we get a graph where the elbow is at 3

# so clearly cluster count is 3.
for(i in 1:3){
  plot(MyToothGrowth, col = k[[i]]$cluster)
}
 
# HIERACHICAL CLUSTERING ----
d <- dist(MyToothGrowthscaled[, 1:2])
fitH <- hclust(d, "ward.D2")
plot(fitH) 

rect.hclust(fitH, k = 3, border = "red") 
clusters <- cutree(fitH, k = 3) 
plot(MyToothGrowth, col = clusters) 

# Model based clustering
library(mclust)
fitM <- Mclust(MyToothGrowthscaled)
plot(fitM)

# DENSITY-BASED CLUSTERING ----
library(dbscan)
kNNdistplot(MyToothGrowthscaled, k = 3)
abline(h = 0.7, col = "red", lty = 2)
fitD <- dbscan(MyToothGrowthscaled, eps = 0.7, minPts = 5)
fitD
plot(MyToothGrowth, col = fitD$cluster)

# we will get some thing like below 
#DBSCAN clustering for 60 objects.
#Parameters: eps = 0.7, minPts = 5
#The clustering contains 3 cluster(s) and 0 noise points.
#
# 1  2  3 
#20 20 20 

#Available fields: cluster, eps, minPts



#(c) Repeat the Q.2 taking Manhattan distance as similarity measure and note the difference
#between the clusters as compared to that found in Q. 2.
library(amap)
fitK <- Kmeans(MyToothGrowth,3,method="euclidean")
str(fitK)
plot(MyToothGrowth, col=fitK$cluster, pch = 19 )

fitK <- Kmeans(MyToothGrowth,3,method="manhattan")
str(fitK)
plot(MyToothGrowth, col=fitK$cluster,  pch = 19 )
