#1a
library(datasets)
barplot(iris$Petal.Length)

#1b
library("RColorBrewer")
barplot(iris$Sepal.Length, col=brewer.pal(3,"Set1"))
barplot(table(iris$Species, iris$Petal.Length), col=brewer.pal(3,"Set1"))

#1c
data(iris)
par(mfrow=c(2,2))
boxplot(iris$Sepal.Length,col="red")
boxplot(iris$Sepal.Length~iris$Species,col="red")
boxplot(iris$Sepal.Length~iris$Species,col=heat.colors(3))
boxplot(iris$Sepal.Length~iris$Species,col=topo.colors(3))
