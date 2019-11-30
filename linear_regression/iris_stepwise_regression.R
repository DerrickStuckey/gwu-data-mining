
# 'iris' dataset included with base R
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

# data exploration
# install.packages("GGally")
library(GGally)
ggpairs(subset(train.data,select=c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)))

# boxplot of Petal Length vs Species
ggplot(data=iris) +
  geom_boxplot(mapping = aes(x=Species, y=Petal.Length))

# plot of Petal Length vs Sepal Length for different species
ggplot(data=train.data) + 
  geom_point(mapping = aes(x=Sepal.Length, y=Petal.Length, col=Species))

# note: there appear to be different relationships between these variables 
# for different species

# train a linear model for Petal Length against each predictor individually

# Sepal Width as predictor
sepal.width.lm <- lm(data = train.data, Petal.Length ~ Sepal.Width)
summary(sepal.width.lm)

# Sepal Length as predictor
sepal.length.lm <- lm(data = train.data, Petal.Length ~ Sepal.Length)
summary(sepal.length.lm)

# Petal Width as predictor
petal.lm <- lm(data = train.data, Petal.Length ~ Petal.Width)
summary(petal.lm)

# Species 
species.lm <- lm(data = train.data, Petal.Length ~ Species)
summary(species.lm)

  # aside: what does a linear model with only categorical predictors actually do?
  species.lm.preds <- predict(species.lm, newdata = train.data)
  table(species.lm.preds)
  
  library(tidyverse)
  train.data %>%
    group_by(Species) %>%
    summarise(avg_length=mean(Petal.Length))

# Throw everything in together
full.lm <- lm(data=train.data, Petal.Length ~ .)
summary(full.lm)

# 'stats' should be loaded by default, but if not, load it before running step()
# library(stats)

# stepwise regression
step.lm.backward <- step(full.lm, direction = "backward")
summary(step.lm.backward)

# keeps everything!


