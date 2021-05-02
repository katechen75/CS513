rm(list = ls())

library(kknn)
library(class) 
library(e1071)

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
languageTable <- table(dataset$Language)
languageLabels <- paste(names(languageTable), languageTable, sep=" ")
pie(languageTable, col = rainbow(length(languageTable)), main="Distribution of Tweets Via Language Written In")
legend("bottomright", legend=languageLabels, bty="n", fill=rainbow(length(languageTable)))

replyTable <- table(dataset$Is.Tweet.Reply)
replyLabels <- paste(names(replyTable), "\n", replyTable, sep="")
pie(replyTable, labels=replyLabels, main="Distribution of Tweets that are Replies vs Not Replies")

# ADD SOME MORE ANALYSIS HERE

# we want to update the dataset to take into account columns that matter in our prediction
updatedDF <- subset(dataset, select = -c(X, User.ID, Username, Tweet.Text))
# updating Is Tweet Reply to 1 or 0, 1 is True, 0 is False
updatedDF$Is.Tweet.Reply <- ifelse(updatedDF$Is.Tweet.Reply == 'True', 1, 0)
View(updatedDF)

### We want to predict whether the next tweet about COVID-19 is a Reply to another tweet, or an original tweet
### Classification problem

##Define max-min normalization function for KNN
normalization <-function(x,minx,maxx) {
  z<-((x-minx)/(maxx-minx))
  return(z) 
}

# normalized dataset 
dataset_normalized <- as.data.frame(         
  cbind(
        X..of.Followers=normalization(updatedDF[,c(1)],min(updatedDF[,c(1)]),max(updatedDF[,c(1)])),
        X..of.Friends=normalization(updatedDF[,c(2)],min(updatedDF[,c(2)]),max(updatedDF[,c(2)])),
        Tweet.ID=updatedDF[,c(3)],
        Tweet.Created.At=updatedDF[,c(4)],
        X..of.Retweets=normalization(updatedDF[,c(5)],min(updatedDF[,c(5)]),max(updatedDF[,c(5)])),
        X..of.Likes=normalization(updatedDF[,c(6)],min(updatedDF[,c(6)]),max(updatedDF[,c(6)])),
        Is.Tweet.Reply=updatedDF[,c(7)],
        Language=updatedDF[,c(8)]
    )
)

# Split data into training and testing
# 70% training
# 30% testing
index <- sort(sample(nrow(updatedDF), as.integer(0.7 * nrow(updatedDF))))

trainingData_normalized <- dataset_normalized[index,]
testingData_normalized <- dataset_normalized[-index,]

### KNN IS STILL IN PROGRESS
knn_prediction <- kknn(formula=X..of.Likes~., trainingData_normalized[,-c(3,4)], testingData_normalized[,-c(3,4)], k=5, kernel = "triangular")
fit_knnNorm <- fitted(knn_prediction)
tb<-table(Actual=testingData_normalized$X..of.Likes, Fitted=fit_knnNorm)
tb
# check error rate
accuracy <- (sum(diag(tb)))/sum(tb)
accuracy
wrong <- (testingData_normalized$X..of.Likes != fit_knnNorm)
rate <- sum(wrong) / length(wrong)
rate
plot(fit_knnNorm)

## Naive Bayes implementation
nBayes <- naiveBayes(Is.Tweet.Reply~., trainingData_normalized[,-c(3,4)])
predictionClass <-predict(nBayes, testingData_normalized[,-c(3,4)])

## Results of Naive Bayes Implementation
tb1 <- table(Class=testingData_normalized$Is.Tweet.Reply, predictionClass)
prop.table(table(Class=testingData_normalized$Is.Tweet.Reply, predictionClass))

NB_accuracy <- ((sum(diag(tb1))))/sum(tb1)
NB_accuracy
NB_wrong<-sum(predictionClass!=testingData_normalized$Is.Tweet.Reply)
NB_wrong
NB_error_rate<-NB_wrong/length(predictionClass)
NB_error_rate
