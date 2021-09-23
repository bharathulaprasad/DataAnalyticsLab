# Data Preprocessing

# Importing the dataset
knitr::opts_chunk$set(echo = TRUE, results='asis')
dataset = read.csv('Dataset.csv')
library(hwriter)
cat(hwrite(dataset, border = 1, table.frame='void', width='600px', table.style='padding: 100px', row.names=FALSE, row.style=list('font-weight:bold')))

# Taking care of missing data
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Age)
dataset$Salary = ifelse(is.na(dataset$Salary),
                        ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)),
                        dataset$Salary)
library(hwriter)
cat(hwrite(dataset, border = 1, table.frame='void', width='600px', table.style='padding: 100px', row.names=FALSE, row.style=list('font-weight:bold')))
# Encoding categorical data
dataset$Country = factor(dataset$Country,
                         levels = c('France', 'Spain', 'Germany'),
                         labels = c(1, 2, 3))
dataset$Purchased = factor(dataset$Purchased,
                           levels = c('No', 'Yes'),
                           labels = c(0, 1))
cat(hwrite(dataset, border = 1, table.frame='void', width='600px', table.style='padding: 100px', row.names=FALSE, row.style=list('font-weight:bold')))
# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(144)
split <- sample.split(dataset$Purchased, SplitRatio = 0.8)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)
cat(hwrite(training_set, border = 1, table.frame='void', width='600px', table.style='padding: 100px', row.names=FALSE, row.style=list('font-weight:bold')))
cat(hwrite(test_set, border = 1, table.frame='void', width='600px', table.style='padding: 100px', row.names=FALSE, row.style=list('font-weight:bold')))
# Feature Scaling
training_set[, 2:3] <- scale(training_set[, 2:3])
test_set[, 2:3] <- scale(test_set[, 2:3])
cat(hwrite(training_set, border = 1, table.frame='void', width='600px', table.style='padding: 100px', row.names=FALSE, row.style=list('font-weight:bold')))
cat(hwrite(test_set, border = 1, table.frame='void', width='600px', table.style='padding: 100px', row.names=FALSE, row.style=list('font-weight:bold')))
