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
library(dplyr)
library(stringr)
library(neuralnet)


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
datasetAnalysis <- dataset
datasetAnalysis$X..of.Followers <- ifelse(dataset$X..of.Followers < 1000, "1000 >", 
                                          ifelse(dataset$X..of.Followers > 10000, "10000 <", "1000 - 10000"))
datasetAnalysis$X..of.Friends <- ifelse(dataset$X..of.Friends < 1000, "1000 >", 
                                        ifelse(dataset$X..of.Friends > 10000, "10000 < ", "1000 - 10000"))

# Comparing the Number of Retweets to Original Tweets
retweetTable <- table(dataset$Is.Retweet)
retweetPct <- round(retweetTable/sum(retweetTable) * 100)
retweetLabels <- paste(retweetPct, "%", sep="")
pie(retweetTable, labels=retweetLabels, main="Distribution of Tweets", col=topo.colors(length(retweetTable)))
legend("topright", c("Not a Retweet", "Retweets"), fill=topo.colors(length(retweetTable)))

# Desnity graph of Number of LIkes
ggplot(dataset, aes(x=X..of.Likes)) + geom_density(fill='lightblue') +
  geom_vline(aes(xintercept=mean(X..of.Likes)), color='red', linetype='dashed', size=1) + 
  theme_minimal() +
  labs(title='Density of Likes', x="# of Likes", y='Density')
  

# Comparing Likes to Retweets
ggplot(dataset, aes(x=X..of.Likes, y=X..of.Retweets)) + geom_point() + 
  geom_smooth() + 
  theme_minimal() +
  labs(title='Likes vs Retweets', x="# of Likes", y="# of Retweets")

# Comparing # of Followers
followersTable <- table(datasetAnalysis$X..of.Followers)
followersPct <- round(followersTable/sum(followersTable) * 100)
followerLabels <- paste(followersPct, "%", sep="")
pie(followersTable, labels=followerLabels, main="Distrubtion of Followers", col=topo.colors(length(followersTable)))
legend("topright", c("Between 1,000 to 10,000", "Less than 1,000", "More than 10,000"), fill=topo.colors(length(followersTable)))

# Comparing # of Following
followingTable <- table(datasetAnalysis$X..of.Friends)
followingPct <- round(followingTable/sum(followingTable) * 100)
followingLabels <- paste(followingPct, "%", sep="")
pie(followingTable, labels=followingLabels, main="Distrubtion of Followings", col=topo.colors(length(followersTable)))
legend("topright", c("Between 1,000 to 10,000", "Less than 1,000", "More than 10,000"), fill=topo.colors(length(followingTable)))


# Compare the Number of Tweets a User posts to their Followers
ggplot(dataset, aes(x = X..of.Tweets, y = X..of.Followers)) + geom_point() +
  geom_smooth() +
  theme_minimal() +
  labs(title='Tweets vs Followers', x="# of Tweets", y="# of Followers")

# analysis top hashtag here
hashtags <- dataset$Tweet.HashTags
hashTagList <- list()
for(i in hashtags){
  currentHashtag <-unlist(str_extract_all(i, "(\\w+)"))
  for(element in currentHashtag) {
    hashTagList <- c(hashTagList, element)
  }
}
uniqueTags <- unique(na.omit(hashTagList))
uniqueTags[which.max(tabulate((match(hashTagList, uniqueTags))))]

# convert factors to numerics
dataset$Is.Retweet <- ifelse(dataset$Is.Retweet == TRUE, 1, 0)

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
ggplot(NULL,aes(x=linearRegressionPredict, y=y_test)) + geom_point() +
  geom_smooth() + 
  theme_minimal() +
  labs(title="Linear Regression", x="Prediction", y='Actual')
linearRegressionSummary <- summary(linearRegression)
linearRegressionR2 <- linearRegressionSummary$r.squared
linearRegressionStandardError <- linearRegressionSummary$sigma

# KNN
likesPredictionKNN <- knn(train=x_train, test=x_test, cl=y_train, k=15)
plot(y_test,likesPredictionKNN, xlab='Actual', ylab='Prediction', main='KNN Regression for Predicting Likes')
knnSummary <- summary(lm(y_test~likesPredictionKNN))
knnR2 <- knnSummary$r.squared
knnStandardError <- knnSummary$sigma

# CART
cartModel <- rpart(y_train~., data=x_train, method="anova")
rpart.plot(cartModel)
cartPredict <- predict(cartModel, x_test)
ggplot(NULL,aes(x=cartPredict, y=y_test)) + geom_point() +
  geom_smooth() + 
  theme_minimal() +
  labs(title="CART", x="Prediction", y='Actual')
cartSummary <- summary(lm(y_test~cartPredict))
cartR2 <- cartSummary$r.squared
cartStandardError <- cartSummary$sigma

# Random Forest
randomForestFit <- randomForest(y_train~., data=data.frame(x_train))
importance(randomForestFit)
varImpPlot(randomForestFit)
randomForestPrediction <- predict(randomForestFit, x_test)
ggplot(NULL,aes(x=randomForestPrediction, y=y_test)) + geom_point() +
  geom_smooth() + 
  theme_minimal() +
  labs(title="Random Forest", x="Prediction", y='Actual')
randomForestSummary <- summary(lm(y_test~randomForestPrediction))
randomForestR2 <- randomForestSummary$r.squared
randomForestStandardError <- randomForestSummary$sigma

# Naive Bayes 
naiveBayesModel <- naiveBayes(y_train~., x_train)
naiveBayesPrediction <- predict(naiveBayesModel, x_test)
plot(y_test, naiveBayesPrediction, xlab='Actual', ylab='Prediction', main='Naive Bayes')
naiveBayesSummary <- summary(lm(y_test~naiveBayesPrediction))
naiveBayesR2 <- naiveBayesSummary$r.squared
naiveBayesStandardError <- naiveBayesSummary$sigma

# SVM
svmModel <- svm(y_train~., data=x_train)
svmPrediction <- predict(svmModel,x_test)
ggplot(NULL,aes(x=svmPrediction, y=y_test)) + geom_point() +
  geom_smooth() + 
  theme_minimal() +
  labs(title="SVM", x="Prediction", y='Actual')
svmSummary <- summary(lm(y_test~svmPrediction))
svmR2 <- svmSummary$r.squared
svmStandardError <- svmSummary$sigma

# Create a Table to Compare R2 Scores and Standard Errors
comparsionTable <- data.frame(
  Model = c('Linear Regression', 'KNN', 'CART', 'RandomForest', 'Naive Bayes', 'SVM'),
  R2Score = c(linearRegressionR2, knnR2, cartR2, randomForestR2, naiveBayesR2, svmR2),
  StandardError = c(linearRegressionStandardError, knnStandardError, cartStandardError, randomForestStandardError, naiveBayesStandardError, svmStandardError)
)
comparsionTable

# knowing that KNN and CART have high R2 scores, we can look at CART to see what variable is being used
cartModel # we can see that it is only using # of Retweets

# remove everything from the environment
rm(list = ls())
