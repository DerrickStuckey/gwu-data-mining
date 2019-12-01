library(tidyverse)
library(FNN)
library(caret)

# check out our data (included with 'tidyverse' package)
diamonds

# training/test/validation split
set.seed(12345)
train.proportion <- 0.6
test.proportion <- 0.2
validation.proportion <- 0.2

# pull out the training data
train.index <- sample(1:nrow(diamonds), nrow(diamonds)*train.proportion)
train.data <- diamonds[train.index,]

# select test and validation from what's left over
holdout.data <- diamonds[-train.index,]
test.index <- sample(1:nrow(holdout.data), nrow(diamonds)*test.proportion)
test.data <- holdout.data[test.index,]
validation.data <- holdout.data[-test.index,]

# set up dataframes to hold normalized values
train.data.norm <- train.data
validation.data.norm <- validation.data
test.data.norm <- test.data

# select all numeric variables
head(train.data)
numeric.vars <- c("carat","depth","table","x","y","z")

# normalize numeric variables
# using preProcess() from caret package
normalizer <- preProcess(train.data[,numeric.vars],
                         method = c("center", "scale"))
train.data.norm[,numeric.vars] <- 
  predict(normalizer, train.data[,numeric.vars])
validation.data.norm[,numeric.vars] <- 
  predict(normalizer, validation.data[,numeric.vars])
test.data.norm[,numeric.vars] <- 
  predict(normalizer, test.data[,numeric.vars])
# why do we normalize the test and validation data using the training data normalizer?

# what did this normalization actually do?
mean(train.data$carat)
sd(train.data$carat)

mean(train.data.norm$carat)
sd(train.data.norm$carat)

# predict cut with a k-nearest neighbors model using
# all the numerical predictors available
cut.knn.3 <- knn(train = train.data.norm[,numeric.vars],
                   test = validation.data.norm[,numeric.vars],
                   cl = train.data.norm$cut,
                   k=3)

# row.names(train.data)[attr(cut.knn.3, "nn.index")]

# look at accuracy for each class
confusionMatrix(cut.knn.3, validation.data.norm$cut)



