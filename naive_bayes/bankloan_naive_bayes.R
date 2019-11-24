# install.packages("caret")
# install.packages("gains")
# install.packages("e1071")

library(caret)
library(gains)
library(e1071)

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
                       `ZIP Code` = col_double(),
                       Family = col_factor(),
                       CCAvg = col_double(),
                       Education = col_factor(),
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

# chech the sizes of each split
dim(train.data)
dim(test.data)
dim(validation.data)

# TODO add heatmap of correlations, other visualizations

# try with just a few variables
bd.nb <- naiveBayes(Loan.Status ~ `Securities Account` + `CD Account` + CreditCard + Online,
                    data=train.data)

bd.nb

# manually verify the conditional probabilities
train.data %>%
  group_by(Loan.Status) %>%
  summarise(
    securities.pct = mean(`Securities Account`),
    cd.account.pct = mean(`CD Account`),
    creditcard.pct = mean(CreditCard),
    online.pct = mean(Online)
  )

# score the validation data
test.probabilities.1 <- predict(bd.nb, newdata = test.data, type = "raw")
summary(test.probabilities.1)




