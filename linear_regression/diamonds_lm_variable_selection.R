library(tidyverse)

# check out our data
diamonds

# TODO some visualizations / data exploration

# training/test/validation split
set.seed(12345)
train.proportion <- 0.6
test.proportion <- 0.2
validation.proportion <- 0.2

# pull out the training data
train.index <- sample(1:nrow(diamonds), nrow(diamonds)*train.proportion)
train.data <- diamonds[train.index,]

# select test and validation from what's left over
holdout.data <- diamonds[-train.index,]
test.index <- sample(1:nrow(holdout.data), nrow(diamonds)*test.proportion)
test.data <- holdout.data[test.index,]
validation.data <- holdout.data[-test.index,]


### Try a model using all available predictors ###

full.lm <- lm(data = train.data, price ~ .)
summary(full.lm)

nrow(train.data)


# sample just 1000 points from the validation data for easy plotting
train.data.sample.idx <- sample(1:nrow(train.data), 1000)

# make predictions for each point in the training data
train.data$full.lm.preds <- predict(full.lm, newdata=train.data)

# plot price vs. the model predictions
ggplot(data=train.data[train.data.sample.idx,]) + 
  geom_point(mapping = aes(x=full.lm.preds,y=price)) +
  coord_equal(ratio=1) + 
  geom_abline(intercept=0, slope=1, color="red")

# TODO move this to a later section
# look at residuals
# train.data$full.lm.residuals <- train.data$price - train.data$full.lm.preds
# ggplot(data=train.data[train.data.sample.idx,]) +
#   geom_histogram(mapping = aes(x=full.lm.residuals)) + 
#   geom_vline(xintercept = 0, color="red")

backward.step.lm <- step(full.lm, direction = "backward")
summary(backward.step.lm)
# keeps everything!

# *** NOT a great example of stepwise regression ***

# TODO throw in a bunch of higher-order and interaction terms, and try again
