#1.Perform the following tasks:
#  (a) Import mtcars dataset.
#  (b) Visualize your data using scatter plots
#  (c) Compute the Correlation between mpg and wt variables.

mtcars
head(mtcars)
library(ggplot2)
#Creating a Basic Scatterplot
ggplot(mtcars, aes(wt, y=mpg)) + geom_point()
#Add names to x and y axis
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() + scale_x_continuous("Weight of Car") + scale_y_continuous("Miles Per Gallon")

#Caluclate the correlation coefficient
cor(mtcars$wt, mtcars$mpg)


#2
#Perform the following:
#  (a) Define a normalized vector P.
#  (b) Define a normalized vector Q.
#  (c) Combine P and Q as matrix object. Please note that when defining a matrix from
#  vectors, the vectors should be combined as rows.
#  (d) Compute the Euclidean Distance with default parameters


# define a probability density function P
P <- 1:10/sum(1:10)
# define a probability density function Q
Q <- 20:29/sum(20:29)

# combine P and Q as matrix object
x <- rbind(P,Q)

library(philentropy)

# compute the Euclidean Distance with default parameters
distance(x, method = "euclidean")

# euclidean 
# 0.1280713 

# 3. Compute Manhattan distance and cosine similarity after performing (a),(b),(c) steps of
# Q.2.
distance(x, method = "manhattan")
# manhattan 
# 0.3525046 

# 4.
# (a) Import the Iris dataset (https://archive.ics.uci.edu/ml/datasets/iris).
# (b) As you may know, this dataset contains three kinds of flowers: Iris-Setosa, Iris-
#  Versicolor, and Iris-Virginica, having the following four features: sepal length, sepal
# width, petal length, petal width. choose only two features: petal length, petal width,
# and plot the data points in a 2-D space where the x-axis and the y-axis represent the
# petal length and the petal width, respectively.
# (c) Compute the similarity measure between Iris-Setosa and Iris-Versicolor; and Iris-
#   Versicolor and Iris-Virginica; and Iris-Setosa and Iris-Virginica considering only two
# features petal length and petal width

# 4a)
iris_df <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header=FALSE,
                    col.names=
                      c("sepal.length","sepal.width","petal.length","petal.width","species"))
iris_df

# Question 4(b)
plot(petal.length ~ petal.width, data=iris_df)
plot(petal.length ~ petal.width, data=iris_df,
     main="Petal Length vs. Petal Width\nIris Dataset",
     xlab="Petal Length", ylab="Petal Width",
     pch=20, col="blue",
     ylim=c(0,7), xlim=c(0,2.5))

plot(1:25, pch=1:25)

color1 <- c("red","green")
plot(petal.length ~ petal.width, data=iris_df, 
     main="Petal Length vs. Petal Width\nIris Dataset",
     xlab="Petal Length", ylab="Petal Width", 
     pch=20, col=color1,
     ylim=c(0,7), xlim=c(0,2.5))

petalLength_setosa <- iris_df$petal.length[iris_df$species =="Iris-setosa"]
petalLength_versicolor <- iris_df$petal.length[iris_df$species =="Iris-versicolor"]
petalLength_virginica <- iris_df$petal.length[iris_df$species=="Iris-virginica"]

petalWidth_setosa <- iris_df$petal.width[iris_df$species == "Iris-setosa"]
petalWidth_versicolor <- iris_df$petal.width[iris_df$species == "Iris-versicolor"]
petalWidth_virginica <- iris_df$petal.width[iris_df$species == "Iris-virginica"]

plot(petalLength_setosa ~ petalWidth_setosa, data=iris_df,
     xlab="Petal Length",
     ylab=" Petal Width",
     main =" Petal Length vs Petal width (setosa)",
     col = color2,
     cex = 1,
     pch = 16)

plot(petalLength_versicolor ~ petalWidth_versicolor, data=iris_df,
     xlab="Petal Length",
     ylab=" Petal Width",
     main =" Petal Length vs Petal width (setosa)",
     col = color2,
     cex = 1,
     pch = 16)


plot(petalLength_virginica ~ petalWidth_virginica, data=iris_df,
     xlab="Petal Length",
     ylab=" Petal Width",
     main =" Petal Length vs Petal width (setosa)",
     col = color2,
     cex = 1,
     pch = 16)

# Question 4(c)
l <- list(petalLength_setosa, petalLength_versicolor)
simil(l, method="cosine")

l <- list(petalLength_versicolor, petalLength_virginica)
simil(l, method="cosine")

l <- list(petalLength_setosa, petalLength_virginica)
simil(l, method="cosine")


l <- list(petalWidth_setosa, petalWidth_versicolor)
simil(l, method="cosine")

l <- list(petalWidth_versicolor, petalWidth_virginica)
simil(l, method="cosine")

l <- list(petalWidth_setosa, petalWidth_virginica)
simil(l, method="cosine")
