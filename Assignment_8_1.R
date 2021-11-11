#OVERALL STRATEGY
#A SMALL TRAINING SET (here Iris dataset)
#TRAIN A SIMPLE MODEL ON SMALL TRAINING SET (here decision tree and knn)
#GATHER A LARGE SET OF UNLABELLED DATA (here using the test split set for convenience)
#IF MORE TRAINING DATA IS NEEDED MAKE PREDICTION ON UNLABELLED DATA (For convenience we treated test split as unlabelled data and made predictions on it)
#LABEL THE MOST UNCERTAIN EXAMPLE (We stop here after finding the most uncertain example)
#ADD LABELLED EXAMPLE TO TRAINING SET AND RETRAIN THE MODEL
#DATASET 1 -> IRIS DATASET
#-----------------------------------------------------------------------------------------#
df <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data",
               header = FALSE)#IMPORT IRIS DATA
colnames(df) <- c('Sepal.Length','Sepal.Width','Petal.Length','Petal.Width','Species')#RENAMING COLUMNS
df$Species <- as.numeric(as.factor(df$Species))
################################################
######################################################
#DIVIDE INTO TRAIN AND TEST
sample <- sample.int(n = nrow(df), size = floor(.75*nrow(df)), replace = F)
train <- df[sample, ]
test  <- df[-sample, ]
####################################################
library(caTools)
library(ROCR)
library(stats4) #Load package stats
library(splines) #Load package splines
#To load package VGAM, need to load package stats4 and splines.
library(VGAM) #Load package VGAM
#logistic_model <- vglm(train$Species ~ train$Sepal.Length + train$Sepal.Width + train$Petal.Length + train$Petal.Width, 
#data = train, 
#family = "multinomial")
#Perform classification
#logistic_probabilities<- predict(logistic_model, data = test, type="response")
#logistic_predictions <- apply(logistic_probabilities, 1, which.max)
#logistic_predictions
###########################
#DECISION TREE MODEL
library(rpart)
dt_fit <- rpart(train$Species~., data = train, method = 'class')
dt_predict <-list(predict(dt_fit, test, type = 'class'))
dt_predict
#########################
#K NEAREST NEIGHBOUS MODEL
library(class)
knn_pred <- knn(train = train, test = test,cl = train$Species, k=10)
commitee <- list("DecisionTree", "knn")
for (p in commitee) {
  print(p)
}
dt_knn <- cbind(data.frame(dt_predict), data.frame(knn_pred))
colnames(dt_knn) <- c("DecisionTree","knn")
dt_knn
####################################################
#ENTROPY AS A MESAURE OF UNCERTAINTY
library("entropy")
vote_entropy <- function(x, type='class', entropy_method='ML') {
  it <- do.call(itertools2::izip, x)
  disagreement <- sapply(it, function(obs) {
    entropy(table(unlist(obs)), method=entropy_method)
  })
  disagreement
}
##ABOVE FUNCTION IMPLEMENTS Query By Committee approach where we use a committee
#of models (in our case its decision tree and knn)
#for selecting unseen examples we get predictions from all models for all examples
#and look for examples where these models largely disagree and should be included in the training data
###################################################
xy <-vote_entropy(dt_knn)
xy
#looking at below result and figure data points which has highest disagreement value and hence should be included in training data
