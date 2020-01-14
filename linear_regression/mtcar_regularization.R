# install.packages("glmnet")
library(glmnet)

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

summary(mtcars$mpg)

# TODO probably should normalize predictors before running ridge regression

# glmnet requires a predictor matrix and target variable vector
train.predictors <- train.data[,-1]
head(train.predictors)
train.predictor.matrix <- as.matrix(train.predictors)

train.target <- train.data$mpg

# train ridge regression with a lambda (penalty parameter) of 0.1
lambda <- 0.1
ridge.lm <- glmnet(x = train.predictor.matrix, 
                    y = train.target, 
                    alpha = 0, lambda = lambda)
ridge.lm$beta

# train a basic linear model on the same data
basic.lm <- lm(mpg ~ ., data=train.data)
summary(basic.lm)
basic.lm$coefficients

# compare a few coefficients
basic.lm$coefficients["wt"]
ridge.lm$beta["wt",]

basic.lm$coefficients["cyl"]
ridge.lm$beta["cyl",]

# compare predictions on test data
basic.preds <- predict(basic.lm, newdata = test.data)
test.predictor.matrix <- as.matrix(test.data[,-1])
ridge.preds <- predict(ridge.lm, newx = test.predictor.matrix)

# which predictions are more "conservative"?
summary(basic.preds)
summary(ridge.preds)
sd(basic.preds)
sd(ridge.preds)

# which are actually better?
ggplot() + 
  geom_point(mapping = aes(x=basic.preds, y=test.data$mpg)) + 
  ggtitle("Basic Linear Regression")
ggplot() + 
  geom_point(mapping = aes(x=ridge.preds, y=test.data$mpg)) + 
  ggtitle("Ridge Regression")

library(forecast)
accuracy(basic.preds, test.data$mpg)
accuracy(as.vector(ridge.preds), test.data$mpg)

# TODO try lasso regression too
