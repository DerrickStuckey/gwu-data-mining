# 'mtcars' dataset included with base R
mtcars

head(mtcars)
dim(mtcars)

set.seed(12345)
train.proportion <- 0.75

# arrays to store the R-squared scores for each run
full.scores <- c()
step.scores <- c()

# run the regressions 100 times, each with a different train/test split
for (i in 1:100) {
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
  
  # append the R-squared result for each model to the result arrays
  full.scores <- c(full.scores, cor(test.data$preds.full.lm, test.data$mpg) ^ 2)
  step.scores <- c(step.scores, cor(test.data$preds.step.lm, test.data$mpg) ^ 2)
}

# compare the results
mean(full.scores)
mean(step.scores)

median(full.scores)
median(step.scores)
