library(tidyverse)

# check out our data
diamonds

# TODO some visualizations / data exploration

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

# train a basic linear model to predict price using all other variables as predictors
dlm <- lm(data = train.data, price ~ .)
summary(dlm)




