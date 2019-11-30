# 'mtcars' dataset included with base R
mtcars

head(mtcars)
dim(mtcars)

sample(1:10,5)

train.proportion <- 0.75
train.idx <- sample(1:nrow(mtcars), nrow(mtcars)*train.proportion)
train.idx

train.data <- mtcars[train.idx,]
test.data <- mtcars[-train.idx,]

nrow(train.data)
nrow(test.data)

full.lm <- lm(mpg ~ ., data = train.data)
summary(full.lm)

step.lm <- step(full.lm, direction = "backward")
summary(step.lm)

test.data$preds.full.lm <- predict(full.lm, newdata = test.data)
test.data$preds.step.lm <- predict(step.lm, newdata = test.data)

cor(test.data$preds.full.lm, test.data$mpg) ^ 2
cor(test.data$preds.step.lm, test.data$mpg) ^ 2

# WTF! stepwise regression fails again vs. a model using all predictors!


