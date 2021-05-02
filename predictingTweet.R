
rm(list = ls())

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

# Split data into training and testing
# 70% training
# 30% testing
index <- sort(sample(nrow(dataset), as.integer(0.7 * nrow(dataset))))
trainingData <- dataset[index,]
testingData <- dataset[-index,]


rm(list = ls())