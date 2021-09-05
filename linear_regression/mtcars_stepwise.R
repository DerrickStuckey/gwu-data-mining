library(tidyverse)

# 'mtcars' dataset included with base R
mtcars

head(mtcars)
dim(mtcars)

set.seed(12345)
train.proportion <- 0.75

train.idx <- sample(1:nrow(mtcars), nrow(mtcars)*train.proportion)
train.idx

# train/test split
train.data <- mtcars[train.idx,]
test.data <- mtcars[-train.idx,]
dim(train.data)
dim(test.data)

summary(mtcars$mpg)

# train a linear model on all available predictors
full.lm <- lm(mpg ~ ., data = train.data)
summary(full.lm)
# R-squared = 
# Adj R-squared = 

# start with no predictors, add the most useful predictors
# through forwards stepwise regression
step.lm.back <- step(full.lm, direction = "backward")
summary(step.lm.back)
# R-squared = 
# Adj R-squared = 

# start with all predictors, drop the less-useful predictors 
# through backwards stepwise regression
base.lm <- lm(mpg ~ 1, data=train.data)
summary(base.lm)
step.lm.for <- step(base.lm, direction = "forward",
                    scope=list(lower=base.lm, upper=full.lm))
summary(step.lm.for)
# R-squared = 
# Adj R-squared = 

# evaluate each against the test set
preds.full.lm <- predict(full.lm, newdata = test.data)
preds.step.lm <- predict(step.lm.back, newdata = test.data)

accuracy(preds.full.lm, test.data$mpg)
accuracy(preds.step.lm, test.data$mpg)

# which is better?
