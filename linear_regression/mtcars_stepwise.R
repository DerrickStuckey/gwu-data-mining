library(tidyverse)
library(forecast)

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

# start with all predictors, drop the less-useful predictors 
# through backwards stepwise regression
step.lm.back <- step(full.lm, direction = "backward")
summary(step.lm.back)
# R-squared = 
# Adj R-squared = 

# start with no predictors, add the most useful predictors
# through forwards stepwise regression
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


## Interaction terms and polynomial terms

# add an interaction term for cylinders * weight
inter.lm.1 <- lm(mpg ~ . + cyl*wt, data = train.data)
summary(inter.lm.1)

# add a polynomial term for cylinders^2
poly.lm.1 <- lm(mpg ~ . - cyl + poly(cyl,2), data = train.data)
summary(poly.lm.1)

# add interaction terms for cyl with every other variable
inter.lm.2 <- lm(mpg ~ . + cyl*., data = train.data)
summary(inter.lm.2)

# add all possible interaction terms of degree 2
inter.lm.full <- lm(mpg ~ .^2, data=train.data)
summary(inter.lm.full)

# let stepwise regression try to choose the best features out of all the above
inter.lm.step <- step(base.lm, direction = "forward",
                      scope=list(lower=base.lm, upper=inter.lm.full))
summary(inter.lm.step)

# evaluate each against the test dataset
preds.inter.lm.1 <- predict(inter.lm.1, newdata = test.data)
accuracy(preds.inter.lm.1, test.data$mpg)

preds.poly.lm.1 <- predict(poly.lm.1, newdata = test.data)
accuracy(preds.poly.lm.1, test.data$mpg)

preds.inter.lm.2 <- predict(inter.lm.2, newdata = test.data)
accuracy(preds.inter.lm.2, test.data$mpg)

preds.inter.lm.step <- predict(inter.lm.step, newdata = test.data)
accuracy(preds.inter.lm.step, test.data$mpg)
