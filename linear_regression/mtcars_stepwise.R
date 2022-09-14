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

# add a polynomial term for hp^2
# why might this be useful?
ggplot(train.data) +
  geom_point(mapping = aes(x=hp, y=mpg))
hp.poly.lm <- lm(mpg ~ poly(hp,2), data = train.data)
summary(hp.poly.lm)

# plot the relationship between mpg and hp with polynomial model
ggplot(train.data, mapping = aes(x=hp, y=mpg)) +
  geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE), colour="blue")
# any problems you can envision with this approach?


# add the polynomial term to the full model
full.hp.poly.lm <- lm(mpg ~ . - hp + poly(hp,2), data = train.data)
summary(full.hp.poly.lm)

# add an interaction term for cylinders * weight to the full model
inter.lm.1 <- lm(mpg ~ . + cyl*wt, data = train.data)
summary(inter.lm.1)

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





## appendix

# evaluate each model against the test dataset
preds.inter.lm.1 <- predict(inter.lm.1, newdata = test.data)
accuracy(preds.inter.lm.1, test.data$mpg)

preds.full.hp.poly.lm <- predict(full.hp.poly.lm, newdata = test.data)
accuracy(preds.full.hp.poly.lm, test.data$mpg)

preds.inter.lm.2 <- predict(inter.lm.2, newdata = test.data)
accuracy(preds.inter.lm.2, test.data$mpg)

preds.inter.lm.step <- predict(inter.lm.step, newdata = test.data)
accuracy(preds.inter.lm.step, test.data$mpg)

# evaluate each against the training dataset for comparison
train.preds.full.lm <- predict(full.lm, newdata = train.data)
accuracy(train.preds.full.lm, train.data$mpg)

train.preds.step.lm.back <- predict(step.lm.back, newdata = train.data)
accuracy(train.preds.step.lm.back, train.data$mpg)

train.preds.inter.lm.1 <- predict(inter.lm.1, newdata = train.data)
accuracy(train.preds.inter.lm.1, train.data$mpg)

train.preds.full.hp.poly.lm <- predict(full.hp.poly.lm, newdata = train.data)
accuracy(train.preds.full.hp.poly.lm, train.data$mpg)

train.preds.inter.lm.2 <- predict(inter.lm.2, newdata = train.data)
accuracy(train.preds.inter.lm.2, train.data$mpg)

train.preds.inter.lm.step <- predict(inter.lm.step, newdata = train.data)
accuracy(train.preds.inter.lm.step, train.data$mpg)


# plot train and test with hp polynomial curve fit
mtcars2 <- mtcars
mtcars2$partition <- "test"
mtcars2$partition[train.idx] <- "train"

ggplot(mtcars2, mapping = aes(x=hp, y=mpg, col=partition)) +
  geom_point() + 
  stat_smooth(method="lm", se=TRUE, fill=NA,
              formula=y ~ poly(x, 2, raw=TRUE))

# compare performance of hp only model with poly(hp,2) model
lm.hp <- lm(mpg ~ hp, data=train.data)
summary(lm.hp)

train.preds.hp <- predict(lm.hp, newdata = train.data)
accuracy(train.preds.hp, train.data$mpg)

test.preds.hp <- predict(lm.hp, newdata = test.data)
accuracy(test.preds.hp, test.data$mpg)

lm.hp.2 <- lm(mpg ~ poly(hp,2), data=train.data)
summary(lm.hp.2)

train.preds.hp.2 <- predict(lm.hp.2, newdata = train.data)
accuracy(train.preds.hp.2, train.data$mpg)

test.preds.hp.2 <- predict(lm.hp.2, newdata = test.data)
accuracy(test.preds.hp.2, test.data$mpg)

