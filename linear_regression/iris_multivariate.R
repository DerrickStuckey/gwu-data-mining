library(tidyverse)

# for 'accuracy' function
library(forecast)

# 'iris' dataset included with base R
iris

head(iris)
dim(iris)

set.seed(12345)
train.proportion <- 0.75

train.idx <- sample(1:nrow(iris), nrow(iris)*train.proportion)
train.idx

# train/test split
train.data <- iris[train.idx,]
test.data <- iris[-train.idx,]
dim(train.data)
dim(test.data)

summary(iris$Petal.Length)

# train a linear model on 'cyl' only
first.lm <- lm(mpg ~ cyl, data = train.data)
summary(first.lm)

# train a linear model on 'cyl' and 'disp'
second.lm <- lm(mpg ~ cyl + disp, data = train.data)
summary(second.lm)

# train a linear model on all available predictors
full.lm <- lm(mpg ~ ., data = train.data)
summary(full.lm)

# evaluate the model's performance against the training data
preds.train <- predict(full.lm, newdata = train.data)
accuracy(preds.train, train.data$mpg)

# evaluate the model's performance against the test data
preds.test <- predict(full.lm, newdata = test.data)
accuracy(preds.test, test.data$mpg)

# plot actual vs predicted values for the training data
ggplot() +
  geom_point(mapping = aes(x=preds.train, y=train.data$mpg)) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")

# plot actual vs predicted values for the test data
ggplot() +
  geom_point(mapping = aes(x=preds.test, y=test.data$mpg)) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")

# distribution of residuals for training data
ggplot() +
  geom_histogram(mapping = aes(train.data$mpg - preds.train), bins=10)


