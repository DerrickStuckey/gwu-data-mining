# the 'iris' dataset is included with base R
head(iris)
dim(iris)

# good visual images of the variables:
# https://www.math.umd.edu/~petersd/666/html/iris_pca.html
# https://www.snaplogic.com/machine-learning-showcase/iris-flower-classification

# training / test split (no validation set this time)
set.seed(12345)
train.proportion <- 0.67
test.proportion <- 0.33

num.trials <- 100

# arrays to hold results for each model
full.scores <- c()
step.scores <- c()
step.inter.scores <- c()
petal.length.scores <- c()

for (i in 1:num.trials) {
 
  # train/test split
  train.index <- sample(1:nrow(iris), nrow(iris)*train.proportion)
  train.data <- iris[train.index,]
  test.data <- iris[-train.index,]
  dim(train.data)
  dim(test.data)
  
  # train a linear model for Petal Length against each predictor individually
  
  # Petal Length as predictor
  petal.length.lm <- lm(data = train.data, Sepal.Length ~ Petal.Length)
  summary(petal.length.lm)
  
  # All available original predictors
  full.lm <- lm(data=train.data, Sepal.Length ~ .)
  summary(full.lm)
  
  # backward stepwise regression
  step.lm.backward <- step(full.lm, direction = "backward")
  summary(step.lm.backward)
  
  # add an interaction term
  inter.lm <- lm(data=train.data, Sepal.Length ~ . + Sepal.Width*Species + 
                   Petal.Length*Species + Petal.Width*Species)
  summary(inter.lm)
  
  # run stepwise regression again
  step.lm.backward.inter <- step(inter.lm, direction="backward")
  summary(step.lm.backward.inter)
  
  # test each model
  test.data$preds.full.lm <- predict(full.lm, newdata=test.data)
  test.data$preds.step.lm <- predict(step.lm.backward, newdata=test.data)
  test.data$preds.step.inter.lm <- predict(step.lm.backward.inter, newdata = test.data)
  test.data$preds.petal.length.lm <- predict(petal.length.lm, newdata = test.data)
  
  # R-squared for the test results
  full.scores <- c(full.scores, cor(test.data$preds.full.lm, test.data$Sepal.Length)^2)
  step.scores <- c(step.scores, cor(test.data$preds.step.lm, test.data$Sepal.Length)^2)
  step.inter.scores <- c(step.inter.scores, cor(test.data$preds.step.inter.lm, test.data$Sepal.Length)^2)
  petel.length.scores <- c(petal.length.scores, cor(test.data$preds.petal.length.lm, test.data$Sepal.Length)^2)
}

mean(full.scores)
mean(step.scores)
mean(step.inter.scores)
mean(petel.length.scores)

# Full Linear model still wins!

