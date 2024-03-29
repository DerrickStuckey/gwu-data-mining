library(tidyverse)

# check out our data (included with 'tidyverse' package)
diamonds

# training/test/validation split
set.seed(12345)
train.proportion <- 0.6
test.proportion <- 0.2
validation.proportion <- 0.2

nrow(diamonds)

# pull out the training data
train.index <- sample(1:nrow(diamonds), nrow(diamonds)*train.proportion)
train.data <- diamonds[train.index,]

# select test and validation from what's left over
holdout.data <- diamonds[-train.index,]
test.index <- sample(1:nrow(holdout.data), nrow(diamonds)*test.proportion)
test.data <- holdout.data[test.index,]
validation.data <- holdout.data[-test.index,]

dim(train.data)
dim(validation.data)
dim(test.data)

### a bit of data exploration ###

# 'price' data distribution
summary(train.data$price)
ggplot(data=train.data) +
  geom_histogram(mapping = aes(x=price))

# 'carat' data distribution
summary(train.data$carat)
ggplot(data=train.data) +
  geom_histogram(mapping = aes(x=carat))

# price vs carat
ggplot(data=train.data) + 
  geom_point(mapping = aes(x=carat,y=price))

# sample just 1000 points from the training data for easy plotting
train.plot.sample.idx <- sample(1:nrow(train.data), 1000)

# price vs carat (sample only)
ggplot(data=train.data[train.plot.sample.idx,]) + 
  geom_point(mapping = aes(x=carat,y=price))


### train a linear regression model using a single variable, carat, as predictor
carat.lm <- lm(data = train.data, price ~ carat)
summary(carat.lm)

train.data$price.pred.carat <- predict(carat.lm, newdata=train.data)


# plot price vs. the model predictions
ggplot(data=train.data[train.plot.sample.idx,]) + 
  geom_point(mapping = aes(x=price.pred.carat,y=price)) +
  geom_abline(intercept=0, slope=1, color="red")

# compare with the plot of price vs carat directly
ggplot(data=train.data[train.plot.sample.idx,]) + 
  geom_point(mapping = aes(x=carat,y=price))

# ...in other words
ggplot(data=train.data[train.plot.sample.idx,]) + 
  geom_point(mapping = aes(x=carat,y=price.pred.carat))


# plot price vs. the model predictions with forced equal x, y scales
ggplot(data=train.data[train.plot.sample.idx,]) + 
  geom_point(mapping = aes(x=price.pred.carat,y=price)) +
  coord_equal(ratio=1) +
  geom_abline(intercept=0, slope=1, color="red")

# note: some price predictions are less than 0 - why is this?


# check out the distribution of residuals for this model
train.data$carat.lm.residuals <- train.data$price - train.data$price.pred.carat
ggplot(data=train.data[train.plot.sample.idx,]) +
  geom_histogram(mapping = aes(x=carat.lm.residuals)) + 
  geom_vline(xintercept = 0, color="red")

# linear regression assumes error is normally distributed - are we close?


### Polynomial models ###

# try adding a higher-order term
# aka fitting price to a 2nd-order polynomial of carat
carat.lm.poly.2 <- lm(data = train.data, price ~ carat + I(carat^2))
summary(carat.lm) 0.8468 0.8468
summary(carat.lm.poly.2) 0.8479

# plot price vs predicted price for the polynomial model
train.data$price.pred.carat.poly.2 <- predict(carat.lm.poly.2, newdata=train.data)
ggplot(data=train.data[train.plot.sample.idx,]) + 
  geom_point(mapping = aes(x=price.pred.carat.poly.2,y=price)) +
  coord_equal(ratio=1) + 
  geom_abline(intercept=0, slope=1, color="red")

# look at the distribution of residuals
train.data$residuals.carat.poly.2 <- train.data$price - train.data$price.pred.carat.poly.2
ggplot(data=train.data[train.plot.sample.idx,]) +
  geom_histogram(mapping = aes(x=residuals.carat.poly.2)) + 
  geom_vline(xintercept = 0, color="red")

# try a 3rd-order polynomial
carat.lm.poly.3 <- lm(data = train.data, price ~ carat + I(carat^2) + I(carat^3))
summary(carat.lm.poly.3)

# plot price vs predicted price for the 3rd-order polynomial model
train.data$price.pred.carat.poly.3 <- predict(carat.lm.poly.3, newdata=train.data)

ggplot(data=train.data[train.plot.sample.idx,]) + 
  geom_point(mapping = aes(x=price.pred.carat.poly.3,y=price)) +
  coord_equal(ratio=1) + 
  geom_abline(intercept=0, slope=1, color="red")

# look at the distribution of residuals for the poly 3 model
train.data$residuals.carat.poly.3 <- train.data$price - train.data$price.pred.carat.poly.3
ggplot(data=train.data[train.plot.sample.idx,]) +
  geom_histogram(mapping = aes(x=residuals.carat.poly.3)) + 
  geom_vline(xintercept = 0, color="red")

# do these residuals appear normally distributed?
# are they highly skewed?

# compare performance of each model against the validation set
# which do you expect to perform best?

# compute predicted price by each model for the validation set
validation.data$price.pred.carat.lm <- predict(carat.lm, newdata=validation.data)
validation.data$price.pred.carat.poly.2 <- predict(carat.lm.poly.2, newdata=validation.data)
validation.data$price.pred.carat.poly.3 <- predict(carat.lm.poly.3, newdata=validation.data)

# look at some actual accuracy metrics
library(forecast)
accuracy(validation.data$price.pred.carat.lm, validation.data$price)
accuracy(validation.data$price.pred.carat.poly.2, validation.data$price)
accuracy(validation.data$price.pred.carat.poly.3, validation.data$price)

# ME: Mean Error
# RMSE: Root Mean Squared Error
# MAE: Mean Absolute Error
# MPE: Mean Percentage Error
# MAPE: Mean Absolute Percentage Error

# simple R-squared for validation data predictions vs validation data actual prices
cor(validation.data$price.pred.carat.lm, validation.data$price)^2
cor(validation.data$price.pred.carat.poly.2, validation.data$price)^2
cor(validation.data$price.pred.carat.poly.3, validation.data$price)^2

# is this a good measure of actual model performance?
# does it have any issues?
cor(validation.data$price.pred.carat.lm, validation.data$price)^2
cor(validation.data$price.pred.carat.lm + 100000, validation.data$price)^2

# an unbiased statistic that is comparable to R-squared
# but against out-of-sample data
rsq.test <- function(preds, actuals) {
  SSE <- sum((actuals - preds) ^ 2)
  SST <- sum((actuals - mean(actuals)) ^ 2)
  rsq.test.value <- (1 - SSE / SST)
  return(rsq.test.value)
}

# both versions for simple linear model
cor(validation.data$price.pred.carat.lm, validation.data$price)^2
rsq.test(validation.data$price.pred.carat.lm, validation.data$price)

# both versions for poly 2 model
cor(validation.data$price.pred.carat.poly.2, validation.data$price)^2
rsq.test(validation.data$price.pred.carat.poly.2, validation.data$price)

# both versions for poly 3 model
cor(validation.data$price.pred.carat.poly.3, validation.data$price)^2
rsq.test(validation.data$price.pred.carat.poly.3, validation.data$price)

# what about adjusted R-squared? Do we need to account for model complexity when 
# measuring against out-of-sample data?


# what is our best guess at real model performance?
# note: this is why we have validation and test data
# best.model <- carat.lm
# best.model <- carat.lm.poly.2
# best.model <- carat.lm.poly.3
test.data$predicted.price <- predict(best.model, newdata=test.data)
accuracy(test.data$predicted.price, test.data$price)
rsq.test(test.data$predicted.price, test.data$price)


### Why not use higher-order polynomials all the time? ###

# try with a tiny training dataset
set.seed(11235)
train.data.tiny.index <- sample(1:nrow(train.data), 10)
train.data.tiny <- train.data[train.data.tiny.index,]

ggplot(data=train.data.tiny) + 
  geom_point(mapping = aes(x = carat, y = price))

# train several polynomial models on the tiny dataset
tiny.poly.1 <- lm(data=train.data.tiny, price ~ carat)
summary(tiny.poly.1)
# Multiple R-squared:  0.9642,	Adjusted R-squared:  0.9597 

tiny.poly.2 <- lm(data=train.data.tiny, price ~ carat + I(carat^2))
summary(tiny.poly.2)
# Multiple R-squared:  0.9833,	Adjusted R-squared:  0.9785

tiny.poly.3 <- lm(data=train.data.tiny, price ~ carat + I(carat^2) + I(carat^3))
summary(tiny.poly.3)
# Multiple R-squared:  0.9834,	Adjusted R-squared:  0.975 

tiny.poly.5 <- lm(data=train.data.tiny, price ~ carat + I(carat^2) + I(carat^3)
                  + I(carat^4) + I(carat^5))
summary(tiny.poly.5)
# Multiple R-squared:  0.9996,	Adjusted R-squared:  0.9991

tiny.poly.8 <- lm(data=train.data.tiny, price ~ carat + I(carat^2) + I(carat^3)
                  + I(carat^4) + I(carat^5) + I(carat^6) + I(carat^7)+ I(carat^8))
summary(tiny.poly.8)

# how does R-squared change when we add terms?
# what about adjusted R-squared?

# look at the curves fit by each model

# simple linear model
tiny.poly.1$coefficients
ggplot(data=train.data.tiny) + 
  geom_point(mapping = aes(x = carat, y = price)) + 
  stat_function(geom = "line",
                fun = function(x) tiny.poly.1$coefficients[1] + 
                  tiny.poly.1$coefficients[2]*x
                ) + 
  ggtitle("Linear Model")


# 2nd-order polynomial
tiny.poly.2$coefficients
ggplot(data=train.data.tiny) + 
  geom_point(mapping = aes(x = carat, y = price)) + 
  stat_function(geom = "line",
                fun = function(x) tiny.poly.2$coefficients[1] + 
                  tiny.poly.2$coefficients[2]*x + 
                  tiny.poly.2$coefficients[3]*x^2
  ) + 
  ggtitle("Polynomial Order 2")

# 3rd-order polynomial
tiny.poly.3$coefficients
ggplot(data=train.data.tiny) + 
  geom_point(mapping = aes(x = carat, y = price)) + 
  stat_function(geom = "line",
                fun = function(x) tiny.poly.3$coefficients[1] + 
                  tiny.poly.3$coefficients[2]*x + 
                  tiny.poly.3$coefficients[3]*x^2 + 
                  tiny.poly.3$coefficients[4]*x^3
  ) + 
  ggtitle("Polynomial Order 3")


# 5th-order polynomial
tiny.poly.5$coefficients
ggplot(data=train.data.tiny) + 
  geom_point(mapping = aes(x = carat, y = price)) + 
  stat_function(geom = "line",
                fun = function(x) tiny.poly.5$coefficients[1] + 
                  tiny.poly.5$coefficients[2]*x + 
                  tiny.poly.5$coefficients[3]*x^2 + 
                  tiny.poly.5$coefficients[4]*x^3 + 
                  tiny.poly.5$coefficients[5]*x^4 + 
                  tiny.poly.5$coefficients[6]*x^5
  ) + 
  ggtitle("Polynomial Order 5")

# what happens when we extrapolate?
ggplot(data=train.data.tiny) + 
  geom_point(mapping = aes(x = carat, y = price)) + 
  stat_function(geom = "line",
                fun = function(x) tiny.poly.5$coefficients[1] + 
                  tiny.poly.5$coefficients[2]*x + 
                  tiny.poly.5$coefficients[3]*x^2 + 
                  tiny.poly.5$coefficients[4]*x^3 + 
                  tiny.poly.5$coefficients[5]*x^4 + 
                  tiny.poly.5$coefficients[6]*x^5
  ) + xlim(0,3) + 
  ggtitle("Polynomial Order 5")

# 8th-order polynomial
tiny.poly.8$coefficients
ggplot(data=train.data.tiny) + 
  geom_point(mapping = aes(x = carat, y = price)) + 
  stat_function(geom = "line",
                fun = function(x) tiny.poly.8$coefficients[1] + 
                  tiny.poly.8$coefficients[2]*x + 
                  tiny.poly.8$coefficients[3]*x^2 + 
                  tiny.poly.8$coefficients[4]*x^3 + 
                  tiny.poly.8$coefficients[5]*x^4 + 
                  tiny.poly.8$coefficients[6]*x^5 + 
                  tiny.poly.8$coefficients[7]*x^6 + 
                  tiny.poly.8$coefficients[8]*x^7 + 
                  tiny.poly.8$coefficients[9]*x^8
  ) + 
  xlim(0.2,1.6) + ylim(0,15000) + 
  ggtitle("Polynomial Order 8")

# check out the validation results
validation.data$val.preds.tiny.poly.1 <- predict(tiny.poly.1, newdata = validation.data)
ggplot(data=validation.data) +
  geom_point(mapping = aes(x=val.preds.tiny.poly.1, y=price)) +
  coord_equal(ratio=1) + 
  geom_abline(intercept=0, slope=1, color="red") + 
  ggtitle("Tiny Polynomial Model 1")

validation.data$val.preds.tiny.poly.2 <- predict(tiny.poly.2, newdata = validation.data)
ggplot(validation.data) +
  geom_point(mapping = aes(x=val.preds.tiny.poly.2, y=price)) +
  coord_equal(ratio=1) + 
  geom_abline(intercept=0, slope=1, color="red") + 
  ggtitle("Tiny Polynomial Model 2")

validation.data$val.preds.tiny.poly.3 <- predict(tiny.poly.3, newdata = validation.data)
ggplot(validation.data) +
  geom_point(mapping = aes(x=val.preds.tiny.poly.3, y=price)) +
  coord_equal(ratio=1) + 
  geom_abline(intercept=0, slope=1, color="red") + 
  ggtitle("Tiny Polynomial Model 3")

validation.data$val.preds.tiny.poly.5 <- predict(tiny.poly.5, newdata = validation.data)
ggplot(validation.data) +
  geom_point(mapping = aes(x=val.preds.tiny.poly.5, y=price)) +
  geom_abline(intercept=0, slope=1, color="red") + 
  ggtitle("Tiny Polynomial Model 5")

validation.data$val.preds.tiny.poly.8 <- predict(tiny.poly.8, newdata = validation.data)
ggplot(validation.data) +
  geom_point(mapping = aes(x=val.preds.tiny.poly.8, y=price)) +
  geom_abline(intercept=0, slope=1, color="red") + 
  ggtitle("Tiny Polynomial Model 8")

# some predictions WAY off for the higher-order models - why is this?

# compare accuracy measures across models
library(forecast)
accuracy(validation.data$val.preds.tiny.poly.1, validation.data$price)
accuracy(validation.data$val.preds.tiny.poly.2, validation.data$price)
accuracy(validation.data$val.preds.tiny.poly.3, validation.data$price)
accuracy(validation.data$val.preds.tiny.poly.5, validation.data$price)
accuracy(validation.data$val.preds.tiny.poly.8, validation.data$price)

# ME: Mean Error
# RMSE: Root Mean Squared Error
# MAE: Mean Absolute Error
# MPE: Mean Percentage Error
# MAPE: Mean Absolute Percentage Error
