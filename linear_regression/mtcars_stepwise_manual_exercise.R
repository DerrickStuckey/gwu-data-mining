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

## train a linear model on each predictor individually
predictors <- names(mtcars)
predictors <- predictors[predictors!="mpg"]
predictors
for (predictor in predictors) {
  formula <- paste0("mpg ~ ",predictor)
  lm <- lm(formula, data = train.data)
  # print(summary(lm))
  print(paste0("formula  : ",formula))
  print(paste0("r-squared: ",summary(lm)$r.squared))
  print(paste0("adj r-sq : ",summary(lm)$adj.r.squared))
  print("")
}

# which is the best single predictor of 'mpg'? (highest adjusted R-squared)
# what is the adjusted R-squared for that model?

## choose the single best predictor, 
## and try adding all the otherpredictors to it 1 by 1
predictors <- predictors[predictors!="<your predictor>"]
predictors
for (predictor in predictors) {
  formula <- paste0("mpg ~ <your predictor> + ",predictor)
  lm <- lm(formula, data = train.data)
  print(paste0("formula  : ",formula))
  print(paste0("r-squared: ",summary(lm)$r.squared))
  print(paste0("adj r-sq : ",summary(lm)$adj.r.squared))
  print("")
}

# add more predictors to your formula 1 by 1 
# until adjusted R-squared ceases to improve
# (does basic R-squared ever stop improving?)


## evaluate each model against the test data
library(Metrics)
formulas <- c("mpg ~ <1st predictor>",
              "mpg ~ <1st predictor> + <2nd predictor>",
              "mpg ~ <1st predictor> + <2nd predictor> + <3rd predictor>",
              "...")
for (formula in formulas) {
  lm <- lm(formula, data = train.data)
  # calculate test performance
  test.preds <- predict(lm, newdata = test.data)
  test.rmse <- rmse(test.preds, test.data$mpg)
  print(paste0("formula  : ",formula))
  print(paste0("test rmse: ",test.rmse))
  print("")
}

# which model performs best (lowest RMSE) against the test data?
# is this what you expected?

