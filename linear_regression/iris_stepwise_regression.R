library(stats) # for step() function

# the 'iris' dataset is included with base R
head(iris)
dim(iris)

# good visual images of the variables:
# https://www.math.umd.edu/~petersd/666/html/iris_pca.html
# https://www.snaplogic.com/machine-learning-showcase/iris-flower-classification

# training / test split (no final test set this time)
set.seed(12345)
train.proportion <- 0.67
test.proportion <- 0.33

train.index <- sample(1:nrow(iris), nrow(iris)*train.proportion)
train.data <- iris[train.index,]
test.data <- iris[-train.index,]
dim(train.data)
dim(test.data)

### data exploration ###

# look at the full set of relationships between variables
# install.packages("GGally")
library(GGally)
ggpairs(subset(train.data,select=c(Sepal.Width, Sepal.Length, Petal.Length, Petal.Width)))

# boxplot of Sepal Length vs Species
ggplot(data=iris) +
  geom_boxplot(mapping = aes(x=Species, y=Sepal.Length))

# plot of Sepal Length vs Petal Length for different species
ggplot(data=train.data) + 
  geom_point(mapping = aes(x=Petal.Length, y=Sepal.Length, col=Species))

# plot of Sepal Length vs Petal Width for different species
ggplot(data=train.data) + 
  geom_point(mapping = aes(x=Petal.Width, y=Sepal.Length, col=Species))

# plot of Sepal Length vs Sepal Width for different species
ggplot(data=train.data) + 
  geom_point(mapping = aes(x=Sepal.Width, y=Sepal.Length, col=Species))

ggplot(data=train.data) + 
  geom_point(mapping = aes(x=Sepal.Width, y=Sepal.Length))

# note: there appear to be different relationships between these variables 
# for different species


### Train some models ###

# train a linear model for Petal Length against each predictor individually

# Petal Length as predictor
petal.length.lm <- lm(data = train.data, Sepal.Length ~ Petal.Length)
summary(petal.length.lm)

# Petal Width as predictor
petal.width.lm <- lm(data = train.data, Sepal.Length ~ Petal.Width)
summary(petal.width.lm)

# Sepal Width as predictor
sepal.width.lm <- lm(data = train.data, Sepal.Length ~ Sepal.Width)
summary(sepal.width.lm)

# Species 
species.lm <- lm(data = train.data, Sepal.Length ~ Species)
summary(species.lm)

  # aside: what does a linear model with only categorical predictors actually do?
  species.lm.preds <- predict(species.lm, newdata = train.data)
  head(species.lm.preds)
  table(species.lm.preds)
  
  library(tidyverse)
  train.data %>%
    group_by(Species) %>%
    summarise(avg_length=mean(Sepal.Length))

# Throw everything in together
full.lm <- lm(data=train.data, Sepal.Length ~ .)
summary(full.lm)

# backward stepwise regression
step.lm.backward <- step(full.lm, direction = "backward")
summary(step.lm.backward)

# what gets dropped?
# is this what you would expect?

# forward stepwise regression
dummy.lm <- lm(data=train.data, Sepal.Length ~ 1)
step.lm.forward <- step(dummy.lm, direction = "forward",
                        scope=list(lower=dummy.lm, upper=full.lm))
summary(step.lm.forward)

# any difference in predictors used?
# any difference in coefficients?

# pick the 2 "best" predictors (at least according to stepwise regression)
step.lm.forward.2 <- step(dummy.lm, direction = "forward",
                        scope=list(lower=dummy.lm, upper=full.lm),
                        steps=2)
summary(step.lm.forward.2)

### Feature Engineering ###

# add an interaction term
inter.lm.1 <- lm(data=train.data, Sepal.Length ~ . + Sepal.Width*Species)
summary(inter.lm.1)
# why isn't there a Sepal.Width:Speciessetosa term?

# add a bunch of interaction terms
inter.lm <- lm(data=train.data, Sepal.Length ~ . + Sepal.Width*Species + 
                 Petal.Length*Species + Petal.Width*Species + 
                 Petal.Width * Sepal.Width + Sepal.Width * Petal.Length + 
                 Petal.Length * Petal.Width)
summary(inter.lm)

# run stepwise regression again
step.lm.backward.inter <- step(inter.lm, direction="backward")
summary(step.lm.backward.inter)

# what looks like the best model?


### Model Testing ###

# produce predictions for the test set for each model
test.data$preds.full.lm <- predict(full.lm, newdata=test.data)
test.data$preds.step.lm <- predict(step.lm.backward, newdata=test.data)
test.data$preds.inter.lm <- predict(inter.lm, newdata=test.data)
test.data$preds.step.inter.lm <- predict(step.lm.backward.inter, newdata = test.data)
test.data$preds.petal.length.lm <- predict(petal.length.lm, newdata = test.data)


# plot actual values vs. predictions for each model
ggplot(data=test.data) + 
  geom_point(mapping = aes(x=preds.full.lm, y=Sepal.Length)) + 
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  ggtitle("Full Linear Model")

ggplot(data=test.data) + 
  geom_point(mapping = aes(x=preds.step.lm, y=Sepal.Length)) + 
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  ggtitle("Stepwise")

ggplot(data=test.data) + 
  geom_point(mapping = aes(x=preds.inter.lm, y=Sepal.Length)) + 
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  ggtitle("Full Interaction Terms")

ggplot(data=test.data) + 
  geom_point(mapping = aes(x=preds.step.inter.lm, y=Sepal.Length)) + 
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  ggtitle("Stepwise with Interaction Terms")

ggplot(data=test.data) + 
  geom_point(mapping = aes(x=preds.petal.length.lm, y=Sepal.Length)) + 
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  ggtitle("Petal Length Only")

# do these look like good relationships?

# accuracy metrics for each model
library(forecast)
accuracy(test.data$preds.full.lm, test.data$Sepal.Length)
accuracy(test.data$preds.step.lm, test.data$Sepal.Length)
accuracy(test.data$preds.inter.lm, test.data$Sepal.Length)
accuracy(test.data$preds.step.inter.lm, test.data$Sepal.Length)
accuracy(test.data$preds.petal.length.lm, test.data$Sepal.Length)

# which model performs best against the test set?
# have any of the models overfit the training data? how can we tell?

# R-squared for the test results
cor(test.data$preds.full.lm, test.data$Sepal.Length)^2
cor(test.data$preds.step.lm, test.data$Sepal.Length)^2
cor(test.data$preds.inter.lm, test.data$Sepal.Length)^2
cor(test.data$preds.step.inter.lm, test.data$Sepal.Length)^2
cor(test.data$preds.petal.length.lm, test.data$Sepal.Length)^2

# is this a good measure of actual model performance?
# does it have any issues?
cor(test.data$preds.full.lm, test.data$Sepal.Length)^2
cor(test.data$preds.full.lm + 10, test.data$Sepal.Length)^2

# an unbiased metric that is comparable to R-squared
# but against out-of-sample data
rsq.test <- function(preds, actuals) {
  SSE <- sum((actuals - preds) ^ 2)
  SST <- sum((actuals - mean(actuals)) ^ 2)
  rsq.test.value <- (1 - SSE / SST)
  return(rsq.test.value)
}

# "Test R-squared" for each model
rsq.test(test.data$preds.full.lm, test.data$Sepal.Length)
rsq.test(test.data$preds.step.lm, test.data$Sepal.Length)
rsq.test(test.data$preds.inter.lm, test.data$Sepal.Length)
rsq.test(test.data$preds.step.inter.lm, test.data$Sepal.Length)
rsq.test(test.data$preds.petal.length.lm, test.data$Sepal.Length)

# for comparison:
cor(test.data$preds.full.lm, test.data$Sepal.Length)^2
rsq.test(test.data$preds.full.lm, test.data$Sepal.Length)

cor(test.data$preds.full.lm+1, test.data$Sepal.Length)^2
rsq.test(test.data$preds.full.lm+1, test.data$Sepal.Length)

