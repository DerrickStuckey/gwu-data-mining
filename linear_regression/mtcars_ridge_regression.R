# for linearRidge function
library(ridge)

library(tidyverse)

# load included "mtcars" dataset
data(mtcars)

head(mtcars)
dim(mtcars)

set.seed(12345)
train.proportion <- 0.75

train.idx <- sample(1:nrow(mtcars), nrow(mtcars)*train.proportion)
train.idx

# train/test split
train.data <- mtcars[train.idx,]
test.data <- mtcars[-train.idx,]

# run ridge regression (from "ridge" package) 
# with lambda (coefficient penalty parameter) = 0.1
ridge.lm <- linearRidge(mpg ~ ., data=train.data, lambda = 0.1)
summary(ridge.lm)
# "Scaled estimate" - why is this necessary?

# with lambda (coefficient penalty parameter) = 0.2
ridge.lm.2 <- linearRidge(mpg ~ ., data=train.data, lambda = 0.2)
summary(ridge.lm.2)

# what is different?
head(ridge.lm$coef)
head(ridge.lm.2$coef)

# which model has bigger parameters on average?
sum(abs(ridge.lm$coef))
sum(abs(ridge.lm.2$coef))

# the total "penalty" terms for each version:
sum(ridge.lm$coef^2)
sum(ridge.lm.2$coef^2)

# what about lambda = 1? 100? 0?

# train a basic linear model
basic.lm <- lm(mpg ~ ., data=train.data)
summary(basic.lm)

# compare predictions on test data
# basic linear model vs ridge regression with lambda = 0.1
basic.preds <- predict(basic.lm, newdata = test.data)
ridge.preds <- predict(ridge.lm, newdata = test.data)

# which predictions are more "conservative"?
summary(basic.preds)
summary(ridge.preds)
sd(basic.preds)
sd(ridge.preds)

# which are actually better?
ggplot() + 
  geom_point(mapping = aes(x=basic.preds, y=test.data$mpg)) + 
  ggtitle("Basic Linear Regression") + 
  xlim(10,35) + 
  geom_abline(mapping = aes(intercept=0,slope=1), col="red")
ggplot() + 
  geom_point(mapping = aes(x=ridge.preds, y=test.data$mpg)) + 
  ggtitle("Ridge Regression") + 
  xlim(10,35) + 
  geom_abline(mapping = aes(intercept=0,slope=1), col="red")

library(forecast)
accuracy(basic.preds, test.data$mpg)
accuracy(as.vector(ridge.preds), test.data$mpg)

