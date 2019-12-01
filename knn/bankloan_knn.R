# install.packages("caret")
library(caret)
library(tidyverse)

# from "Data Mining for Business Analytics"
# https://www.dataminingbook.com/book/r-edition
bankdata <- read_csv("./data/UniversalBank.csv")
bankdata
View(bankdata)

# clean up some column types
# doesn't change any results, just easier to read
bankdata <- read_csv("./data/UniversalBank.csv",
                     col_types = cols(
                       ID = col_double(),
                       Age = col_double(),
                       Experience = col_double(),
                       Income = col_double(),
                       `ZIP Code` = col_factor(),
                       Family = col_double(),
                       CCAvg = col_double(),
                       Education = col_double(),
                       Mortgage = col_double(),
                       `Personal Loan` = col_logical(),
                       `Securities Account` = col_logical(),
                       `CD Account` = col_logical(),
                       Online = col_logical(),
                       CreditCard = col_logical()
                     ))
View(bankdata)
bankdata$Loan.Status <- "Rejects"
bankdata$Loan.Status[bankdata$`Personal Loan`] <- "Accepts"
bankdata$Loan.Status <- as.factor(bankdata$Loan.Status)

# training/test/validation split
set.seed(12345)
train.proportion <- 0.6
test.proportion <- 0.2
validation.proportion <- 0.2

# pull out the training data
train.index <- sample(1:nrow(bankdata), nrow(bankdata)*train.proportion)
train.data <- bankdata[train.index,]

# select test and validation from what's left over
holdout.data <- bankdata[-train.index,]
test.index <- sample(1:nrow(holdout.data), nrow(bankdata)*test.proportion)
test.data <- holdout.data[test.index,]
validation.data <- holdout.data[-test.index,]

# set up dataframes to hold normalized values
train.data.norm <- train.data
validation.data.norm <- validation.data
test.data.norm <- test.data

# select a subset of variables
head(train.data)
selected.vars <- c("Age","Experience","Income","Education")

# normalize numeric variables
# using preProcess() from caret package
normalizer <- preProcess(train.data[,selected.vars],
                         method = c("center", "scale"))
train.data.norm[,selected.vars] <- 
  predict(normalizer, train.data[,selected.vars])
validation.data.norm[,selected.vars] <- 
  predict(normalizer, validation.data[,selected.vars])
test.data.norm[,selected.vars] <- 
  predict(normalizer, test.data[,selected.vars])
# why do we normalize the test and validation data using the training data normalizer?

# what did this normalization actually do?
mean(train.data$Age)
sd(train.data$Age)

mean(train.data.norm$Age)
sd(train.data.norm$Age)

# predict Loan Status with a k-nearest neighbors model using
# all the numerical predictors available
loan.knn.3.preds <- knn(train = train.data.norm[,selected.vars],
                 test = validation.data.norm[,selected.vars],
                 cl = train.data.norm$Loan.Status,
                 k=3)

summary(loan.knn.3.preds)
summary(validation.data.norm$Loan.Status)

confusionMatrix(loan.knn.3.preds, validation.data.norm$Loan.Status)


# TODO now try over-sampling the target class "Accepts"
