rm(list = ls())

library(kknn)
library(class) 
library(e1071)
library(corrplot)
library(ggplot2)
library(randomForest)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(caret)

file <- file.choose()

dataset <- read.csv(file, na.strings = '?')
View(dataset)

#Identify missing values
naValues <- is.na(dataset)
colSums(naValues)



# gets data type of each column
sapply(dataset, class)

# gets a generic summary of the dataset
summary(dataset)

# data analysis
replyTable <- table(dataset$Is.Tweet.Reply)
replyLabels <- paste(names(replyTable), "\n", replyTable, sep="")
pie(replyTable, labels=replyLabels, main="Distribution of Tweets that are Replies vs Not Replies")

ggplot(dataset, aes(x=X..of.Likes)) + geom_density()
plot(dataset$X..of.Likes, dataset$X..of.Retweets, xlab="Likes", ylab="Retweets", pch=19)
# analysis top hashtags here

# convert factors to numerics
dataset$Is.Tweet.Reply <- ifelse(dataset$Is.Tweet.Reply == 'True', 1, 0)

# correlation between the variables
correlations <- cor(dataset[,-c(1,2,6,7, 8,9, 13)])
corrplot(correlations, method="circle")


# we want to update the dataset to take into account columns that matter in our prediction
x_data <- subset(dataset, select = -c(User.ID, Username, Tweet.Text, Tweet.Created.At, Day.Of.Week, Tweet.HashTags, X..of.Likes))
y_data <- subset(dataset, select = c(X..of.Likes))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

# we do not want to normalize TweetId
normalized_dataset <- as.data.frame(lapply(x_data[,-c(4)], normalize))

### We want to predict whether the number of likes in the next COVID-19 tweet

# Split data into training and testing
# 70% training
# 30% testing
index <- sort(sample(nrow(x_data), as.integer(0.7 * nrow(x_data))))
x_train <- normalized_dataset[index,]
x_test <- normalized_dataset[-index,]

y_train <- y_data[index,]
y_test <- y_data[-index,]

# Linear Regression
linearRegression <- lm(y_train~., data = x_train)
linearRegressionPredict <- predict(linearRegression, x_test, type="response")
plot(linearRegressionPredict, y_test, xlab='Prediction', ylab='Actual', main='Linear Regression')
linearRegressionR2 <- summary(linearRegression)$r.squared
linearRegressionStandardError <- summary(linearRegression)$sigma
summary(linearRegression)

# KNN
likesPredictionKNN <- knn(train=x_train, test=x_test, cl=y_train, k=15)
plot(y_test, likesPredictionKNN, xlab='Actual', ylab='Prediction', main='KNN Regression for Predicting Likes')
summary(lm(y_test~likesPredictionKNN))$r.square
likesPredictionKNN.score

# CART
str(x_train)
cartModel <- rpart(y_train~., 
                   data=x_train, method="anova")
rpart.plot(cartModel)


 # Random Forest
randomForestFit <- randomForest(y_train~., data=data.frame(x_train))
randomForestFit
importance(randomForestFit)
varImpPlot(randomForestFit)
randomForestPrediction <- predict(randomForestFit, x_test)
plot(randomForestPrediction, y_test, xlab='Prediction', ylab='Actual')
 # check results
 # get accuracy and error rate for random forest

