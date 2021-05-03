rm(list = ls())

library(kknn)
library(class) 
library(e1071)
library(corrplot)
library(ggplot2)
library(randomForest)
library(neuralnet)

file <- file.choose()

dataset <- read.csv(file, na.strings = '?')
View(dataset)

#Identify missing values
naValues <- is.na(dataset)
colSums(naValues)

# convert factors to numerics
dataset$Is.Tweet.Reply <- ifelse(dataset$Is.Tweet.Reply == 'True', 1, 0)

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

# correlation between the variables
correlations <- cor(dataset[,-c(1,2,3,6,7, 8, 12)])
corrplot(correlations, method="circle")

# we want to update the dataset to take into account columns that matter in our prediction
x_data <- subset(dataset, select = -c(X, User.ID, Username, Tweet.Text, Tweet.Created.At, Language))


# updating Is Tweet Reply to 1 or 0, 1 is True, 0 is False
x_data[,c(1,2,4)] <- scale(x_data[,c(1,2,4)])

x_data$X..of.Likes <- factor(x_data$X..of.Likes, order=TRUE)

str(x_data)
### We want to predict whether the number of likes in the next COVID-19 tweet

# Split data into training and testing
# 70% training
# 30% testing
index <- sort(sample(nrow(x_data), as.integer(0.7 * nrow(x_data))))
x_train <- x_data[index,]
x_test <- x_data[-index,]

# Linear Regression
linearRegression <- glm(X..of.Likes~., data = x_train[,-c(3)])
linearRegressionPredict <- predict(linearRegression, x_test, type="response")
#change to a fancier plot
plot(x_test$X..of.Likes, linearRegressionPredict, xlab='Actual', ylab='Prediction', main='Linear Regression')
summary(linearRegression)
# get accuracy and error rate for linear regression

# KNN

# Decision Tree

# Random Forest
randomForestFit <- randomForest(X..of.Likes~., data=x_train[,-c(3)], importance=TRUE, ntree=1000)
importance(randomForestFit)
varImpPlot(randomForestFit)
randomForestPrediction <- predict(randomForestFit, x_test[,-c(4)])
plot(randomForestFit)
# get accuracy and error rate for random forest

# Neural Net
neuralNetModel <- neuralnet(X..of.Likes~., x_train[,-c(3)], hidden = 5 , threshold = 0.01, rep=10)
plot(neuralNetModel)
