library(tidyverse)

# for 'accuracy' function
library(forecast)

# 'mtcars' dataset included with base R
mtcars

head(mtcars)
dim(mtcars)

set.seed(12345)
train.proportion <- 0.67

train.idx <- sample(1:nrow(mtcars), nrow(mtcars)*train.proportion)
train.idx

# train/test split
train.data <- mtcars[train.idx,]
test.data <- mtcars[-train.idx,]
dim(train.data)
dim(test.data)

summary(mtcars$mpg)

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
  geom_point(mapping = aes(x=preds.train, y=train.data$mpg))

# add a line that represents perfect predictions (y=x)
ggplot() +
  geom_point(mapping = aes(x=preds.train, y=train.data$mpg)) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")

# plot actual vs predicted values for the test data
ggplot() +
  geom_point(mapping = aes(x=preds.test, y=test.data$mpg)) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")

# distribution of residuals for training data
ggplot() +
  geom_histogram(mapping = aes(train.data$mpg - preds.train), bins=8)

# distribution of residuals for test data
ggplot() +
  geom_histogram(mapping = aes(test.data$mpg - preds.test), bins=8)



# plot actual vs predicted values for the train and test data
train.data$split = "train"
train.data$prediction = preds.train
test.data$split = "test"
test.data$prediction <- preds.test
combined.data <- rbind(train.data, test.data)
ggplot(combined.data) +
  geom_point(mapping = aes(x=prediction, y=mpg, col=split)) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="black")



