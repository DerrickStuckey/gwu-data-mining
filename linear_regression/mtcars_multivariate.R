library(tidyverse)

# for 'accuracy' function
library(forecast)

# 'mtcars' dataset included with base R
head(mtcars)
# variables: (described in https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html)
# mpg	  Miles/(US) gallon
# cyl	  Number of cylinders
# disp	Displacement (cu.in.)
# hp	  Gross horsepower
# drat	Rear axle ratio
# wt	  Weight (1000 lbs)
# qsec	1/4 mile time
# vs	  Engine (0 = V-shaped, 1 = straight)
# am	  Transmission (0 = automatic, 1 = manual)
# gear	Number of forward gears
# carb	Number of carburetors

dim(mtcars)

set.seed(12345)
# set.seed(123456)
train.proportion <- 0.67

train.idx <- sample(1:nrow(mtcars), nrow(mtcars)*train.proportion)
train.idx

# train/test split
train.data <- mtcars[train.idx,]
test.data <- mtcars[-train.idx,]
dim(train.data)
dim(test.data)

summary(mtcars$mpg)

# train a linear model on 'hp' only
ggplot(mtcars) +
  geom_point(mapping = aes(x=hp, y=mpg))
hp.lm <- lm(mpg ~ hp, data = train.data)
summary(hp.lm)

# train a linear model on 'cyl' only
ggplot(mtcars) +
  geom_point(mapping = aes(x=cyl, y=mpg))
cyl.lm <- lm(mpg ~ cyl, data = train.data)
summary(cyl.lm)

# train a linear model on 'cyl' and 'hp'
hp.cyl.lm <- lm(mpg ~ cyl + hp, data = train.data)
summary(hp.cyl.lm)
# what happened to our coefficients?

# train a linear model on all available predictors
full.lm <- lm(mpg ~ ., data = train.data)
summary(full.lm)

# plot actual vs predicted values for the training data
preds.train.full <- predict(full.lm, newdata = train.data)
ggplot() +
  geom_point(mapping = aes(x=preds.train.full, y=train.data$mpg))

# add a line that represents perfect predictions (y=x)
ggplot() +
  geom_point(mapping = aes(x=preds.train.full, y=train.data$mpg)) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")

# plot actual vs predicted values for the test data
preds.test.full <- predict(full.lm, newdata = test.data)
ggplot() +
  geom_point(mapping = aes(x=preds.test.full, y=test.data$mpg)) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")

# distribution of residuals for training data
ggplot() +
  geom_histogram(mapping = aes(train.data$mpg - preds.train.full), bins=8)

# distribution of residuals for test data
ggplot() +
  geom_histogram(mapping = aes(test.data$mpg - preds.test.full), bins=8)

# plot actual vs predicted values for the train and test data together
train.data$split = "train"
train.data$prediction = preds.train.full
test.data$split = "test"
test.data$prediction <- preds.test.full
combined.data <- rbind(train.data, test.data)
ggplot(combined.data) +
  geom_point(mapping = aes(x=prediction, y=mpg, col=split)) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="black")


# evaluate the full model's performance against the training data
preds.train.full <- predict(full.lm, newdata = train.data)
accuracy(preds.train.full, train.data$mpg)
# full model train RMSE: 

# evaluate the model's performance against the test data
preds.test.full <- predict(full.lm, newdata = test.data)
accuracy(preds.test.full, test.data$mpg)
# full model test RMSE: 

# compare with the very first model we tried
# (using only hp as a predictor)
preds.train.hp <- predict(hp.lm, newdata = train.data)
accuracy(preds.train.hp, train.data$mpg)
# hp model train RMSE: 

preds.test.hp <- predict(hp.lm, newdata = test.data)
accuracy(preds.test.hp, test.data$mpg)
# hp model test RMSE: 

# which model is better?
# how sure are we?


