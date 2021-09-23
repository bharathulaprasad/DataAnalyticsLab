# 1a)In R a simple bar graph can be used to model the probability distribution function.
# Take a random vector and probability associated with it and plot it with a bar graph.

set.seed(124)
norm <- rnorm(100)
x <- norm[1:10]
y = dnorm(x, mean(x), sd(x))
dist <- data.frame(x,y) 

library(ggplot2)
ggplot(dist, aes(x,y)) +
 geom_bar(stat="identity", width=0.05) + 
  theme_light()
  
#2a)Draw an estimated density curve for the logarithms of the values.
library(MASS)
plot(density(Animals$body))
logbody <- log(Animals$body)
plot(density(logbody))  


#2b)Determine the mean and standard deviation of log(Animals$body). Overlay the es-
# timated density with the theoretical density for a normal distribution with the mean
#and standard deviation just obtained.
av <- mean(logbody)
sdev <- sd(logbody)
av
sdev
xval <- pretty(c(av - 3 * sdev, av + 3 * sdev), 50)
lines(xval, dnorm(xval, mean = av, sd = sdev))


#3 Take a random sample from the normal distribution, and plot the estimated density function.
# Now take repeated samples of size 4, calculate the mean for each such sample, and
# plot the density. Repeat the above: taking samples of size 9, and of size 25.
y <- rnorm(100)
plot(density(y), type = "l")
#Now take repeated samples of size 4
av <- numeric(100)
for (i in 1:100) {
  av[i] <- mean(rnorm(4))
}
lines(density(av), col = "red")
#Now take repeated samples of size 9
av <- numeric(100)
for (i in 1:100) {
  av[i] <- mean(rnorm(9))
}
lines(density(av), col = "green")
#Now take repeated samples of size 25
av <- numeric(100)
for (i in 1:100) {
  av[i] <- mean(rnorm(25))
}
lines(density(av), col = "blue")
