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


### train a linear regression model using a single variable as predictor
carat.lm <- lm(data = train.data, price ~ carat)
summary(carat.lm)

train.data$price.pred.carat <- predict(carat.lm, newdata=train.data)

# sample just 1000 points from the validation data for easy plotting
train.data.sample.idx <- sample(1:nrow(train.data), 1000)

# plot price vs. the model predictions
ggplot(data=train.data[train.data.sample.idx,]) + 
  geom_point(mapping = aes(x=price.pred.carat,y=price))

# compare with the plot of price vs carat directly
ggplot(data=train.data[train.data.sample.idx,]) + 
  geom_point(mapping = aes(x=carat,y=price))

# check out the distribution of residuals for this model
train.data$carat.lm.residuals <- train.data$price - train.data$price.pred.carat
ggplot(data=train.data[train.data.sample.idx,]) +
  geom_histogram(mapping = aes(x=carat.lm.residuals))

# QQ plot for an assumed normal distribution
# TODO decide whether this should actually be included
ggplot(data=train.data[train.data.sample.idx,]) +
  stat_qq(mapping = aes(sample=carat.lm.residuals),
          distribution = stats::qnorm)

# look at the distributions of carat and price independently
ggplot(data=train.data) +
  geom_histogram(mapping = aes(x=carat))

ggplot(data=train.data) +
  geom_histogram(mapping = aes(x=price))

# TODO try a variable transformation on price and carat


### Polynomial models ###

# try adding a higher-order term
# aka fitting price to a 2nd-order polynomial of carat
carat.lm.poly.2 <- lm(data = train.data, price ~ carat + I(carat^2))
summary(carat.lm.poly.2)

# plot price vs predicted price for the polynomial model
train.data$price.pred.carat.poly.2 <- predict(carat.lm.poly.2, newdata=train.data)
ggplot(data=train.data[train.data.sample.idx,]) + 
  geom_point(mapping = aes(x=price.pred.carat.poly.2,y=price))

# look at the distribution of residuals
train.data$residuals.carat.poly.2 <- train.data$price - train.data$price.pred.carat.poly.2
ggplot(data=train.data[train.data.sample.idx,]) +
  geom_histogram(mapping = aes(x=residuals.carat.poly.2))

# QQ plot of poly 2 residuals vs a normal distribution
ggplot(data=train.data[train.data.sample.idx,]) +
  stat_qq(mapping = aes(sample=residuals.carat.poly.2),
          distribution = stats::qnorm)

# try a 3rd-order polynomial
carat.lm.poly.3 <- lm(data = train.data, price ~ carat + I(carat^2) + I(carat^3))
summary(carat.lm.poly.3)

# plot price vs predicted price for the 3rd-order polynomial model
train.data$price.pred.carat.poly.3 <- predict(carat.lm.poly.3, newdata=train.data)

ggplot(data=train.data[train.data.sample.idx,]) + 
  geom_point(mapping = aes(x=price.pred.carat.poly.3,y=price))

# look at the distribution of residuals
train.data$residuals.carat.poly.3 <- train.data$price - train.data$price.pred.carat.poly.3
ggplot(data=train.data[train.data.sample.idx,]) +
  geom_histogram(mapping = aes(x=residuals.carat.poly.3))

# QQ plot of poly 3 residuals vs a normal distribution
ggplot(data=train.data[train.data.sample.idx,]) +
  stat_qq(mapping = aes(sample=residuals.carat.poly.3),
          distribution = stats::qnorm)




# train a basic linear model to predict price using all other variables as predictors
dlm <- lm(data = train.data, price ~ .)
summary(dlm)




