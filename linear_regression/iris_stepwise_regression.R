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

# note: there appear to be different relationships between these variables 
# for different species


### Train some models ###

# train a linear model for Petal Length against each predictor individually

# Petal Length as predictor
Petal.Length.lm <- lm(data = train.data, Sepal.Length ~ Petal.Length)
summary(Petal.Length.lm)

# Petal Width as predictor
petal.lm <- lm(data = train.data, Sepal.Length ~ Petal.Width)
summary(petal.lm)

# Sepal Width as predictor
sepal.width.lm <- lm(data = train.data, Sepal.Length ~ Sepal.Width)
summary(sepal.width.lm)
# what does the coefficient mean?

# Species 
species.lm <- lm(data = train.data, Sepal.Length ~ Species)
summary(species.lm)

  # aside: what does a linear model with only categorical predictors actually do?
  species.lm.preds <- predict(species.lm, newdata = train.data)
  table(species.lm.preds)
  
  library(tidyverse)
  train.data %>%
    group_by(Species) %>%
    summarise(avg_length=mean(Sepal.Length))

# Throw everything in together
full.lm <- lm(data=train.data, Sepal.Length ~ .)
summary(full.lm)

# 'stats' should be loaded by default, but if not, load it before running step()
# library(stats)

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

# any difference?


### Feature Engineering ###

# add an interaction term
inter.lm <- lm(data=train.data, Sepal.Length ~ . + Sepal.Width*Species)
summary(inter.lm)

# run stepwise regression again
step.lm.backward.inter <- step(inter.lm, direction="backward")
summary(step.lm.backward.inter)

# what looks like the best model?

# test each model
test.data$preds.full.lm <- predict(full.lm, newdata=test.data)
test.data$preds.step.lm <- predict(step.lm.backward, newdata=test.data)
test.data$preds.step.inter.lm <- predict(step.lm.backward.inter, newdata = test.data)
test.data$preds.Petal.Length.lm <- predict(Petal.Length.lm, newdata = test.data)

# R-squared for the test results
cor(test.data$preds.full.lm, test.data$Sepal.Length)^2
cor(test.data$preds.step.lm, test.data$Sepal.Length)^2
cor(test.data$preds.step.inter.lm, test.data$Sepal.Length)^2
cor(test.data$preds.Petal.Length.lm, test.data$Sepal.Length)^2

# plot actual values vs. predictions for each
ggplot(data=test.data) + 
  geom_point(mapping = aes(x=preds.full.lm, y=Sepal.Length)) + 
  geom_abline(intercept = 0, slope = 1, color = "red")

ggplot(data=test.data) + 
  geom_point(mapping = aes(x=preds.step.lm, y=Sepal.Length)) + 
  geom_abline(intercept = 0, slope = 1, color = "red")

ggplot(data=test.data) + 
  geom_point(mapping = aes(x=preds.step.inter.lm, y=Sepal.Length)) + 
  geom_abline(intercept = 0, slope = 1, color = "red")

ggplot(data=test.data) + 
  geom_point(mapping = aes(x=preds.Petal.Length.lm, y=Sepal.Length)) + 
  geom_abline(intercept = 0, slope = 1, color = "red")


