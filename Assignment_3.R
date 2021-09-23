#1
library(datasets)
barplot(iris$Petal.Length)

#2
library("RColorBrewer")
barplot(iris$Sepal.Length, col=brewer.pal(3,"Set1"))
barplot(table(iris$Species, iris$Petal.Length), col=brewer.pal(3,"Set1"))
