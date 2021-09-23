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


#1d
plot(x=iris$Petal.Length,y=iris$Species)

#2a
PAICOL <- read.csv("PAICOL.csv")
View(PAICOL)
PAICOL$DATE=as.Date(PAICOL$DATE,origin=PAICOL$DATE[1])
summary(PAICOL)
library(ggplot2)
ggplot(data = PAICOL,aes(DATE,LEVEL))+geom_line()

#Q2(b): Create the scatter plot of "RAIN" against "LEVEL"
ggplot(data = PAICOL,aes(RAIN,LEVEL)) +geom_point()

#Q2(c):  Create a plot of the RAIN and LEVEL.
ggplot(PAICOL,aes(DATE,LEVEL))+geom_line(aes(color="LEVEL"))+geom_line(data=PAICOL,aes(DATE,RAIN,color="RAIN"))


#Q2(d): Find and plot circles on the LEVEL plot at the maximum and minimum value



PAICOL$DATE=as.Date(PAICOL$DATE,origin=PAICOL$DATE[1])
summary(PAICOL)

data_max<- PAICOL[which.max(PAICOL$LEVEL),]
data_min<-PAICOL[which.min(PAICOL$LEVEL),]

ggplot(data = PAICOL, aes(x = DATE, y = LEVEL)) +
geom_line() +
geom_point(aes(x = data_max$DATE, y = data_max$LEVEL), shape = 1, size = 3, color = "red") + 
geom_point(aes(x = data_min$DATE, y = data_min$LEVEL), shape = 1, size = 3, color = "blue") + 
scale_shape_discrete(solid = FALSE) +
labs(title = "Daily levels of a river") +
    theme_bw()


#Q2(e):  Plot the LEVEL for the year “2001.”

PAICOL$YEAR = as.numeric(format(PAICOL$DATE, "%Y"))
index_2001=PAICOL$YEAR==2001
ggplot(PAICOL[index_2001,],aes(DATE,LEVEL))+geom_line(aes(color="LEVEL"))


#Q3: Simulate a simple dataset of 10 points.Create a heat map to visualize relationships between 
#the variables. A heat map is agraphical representation of data where the data values are
#represented as colours.


#set.seed(123)                                                     # Set seed for reproducibility
data <- matrix(rnorm(100, 0, 10), nrow = 10, ncol = 10)           # Create example data
colnames(data) <- paste0("col", 1:10)                             # Column names
rownames(data) <- paste0("row", 1:10)   
View (data)
heatmap(data, Rowv = NA, Colv = NA)

#Q4
x <- seq(0,100,by = 1)  
y <- dbinom(x,50,0.5)  
plot(x,y)  
