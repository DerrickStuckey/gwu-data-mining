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
  geom_point(mapping = aes(x=price.pred.carat,y=price)) +
  coord_equal(ratio=1) + 
  geom_abline(intercept=0, slope=1, color="red")
# coord_equal ensures the x and y scales are the same

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
  geom_point(mapping = aes(x=price.pred.carat.poly.2,y=price)) +
  coord_equal(ratio=1) + 
  geom_abline(intercept=0, slope=1, color="red")

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
  geom_point(mapping = aes(x=price.pred.carat.poly.3,y=price)) +
  coord_equal(ratio=1) + 
  geom_abline(intercept=0, slope=1, color="red")

# look at the distribution of residuals for the poly 3 model
train.data$residuals.carat.poly.3 <- train.data$price - train.data$price.pred.carat.poly.3
ggplot(data=train.data[train.data.sample.idx,]) +
  geom_histogram(mapping = aes(x=residuals.carat.poly.3))

# QQ plot of poly 3 residuals vs a normal distribution
ggplot(data=train.data[train.data.sample.idx,]) +
  stat_qq(mapping = aes(sample=residuals.carat.poly.3),
          distribution = stats::qnorm)


### Why not use higher-order polynomials all the time? ###

# try with a tiny training dataset
set.seed(11235)
train.data.tiny.index <- sample(1:nrow(train.data), 10)
train.data.tiny <- train.data[train.data.tiny.index,]

ggplot(data=train.data.tiny) + 
  geom_point(mapping = aes(x = carat, y = price))

# train our 3 polynomial models on the tiny dataset
tiny.lm.1 <- lm(data=train.data.tiny, price ~ carat)
summary(tiny.lm.1)

tiny.lm.2 <- lm(data=train.data.tiny, price ~ carat + I(carat^2))
summary(tiny.lm.2)

tiny.lm.3 <- lm(data=train.data.tiny, price ~ carat + I(carat^2) + I(carat^3))
summary(tiny.lm.3)

# look at the curves fit by each model

# simple linear model
tiny.lm.1$coefficients
ggplot(data=train.data.tiny) + 
  geom_point(mapping = aes(x = carat, y = price)) + 
  stat_function(geom = "line",
                fun = function(x) tiny.lm.1$coefficients[1] + 
                  tiny.lm.1$coefficients[2]*x
                )


# 2nd-order polynomial
tiny.lm.2$coefficients
ggplot(data=train.data.tiny) + 
  geom_point(mapping = aes(x = carat, y = price)) + 
  stat_function(geom = "line",
                fun = function(x) tiny.lm.2$coefficients[1] + 
                  tiny.lm.2$coefficients[2]*x + 
                  tiny.lm.2$coefficients[3]*x^2
  )

# 3rd-order polynomial
tiny.lm.3$coefficients
ggplot(data=train.data.tiny) + 
  geom_point(mapping = aes(x = carat, y = price)) + 
  stat_function(geom = "line",
                fun = function(x) tiny.lm.3$coefficients[1] + 
                  tiny.lm.3$coefficients[2]*x + 
                  tiny.lm.3$coefficients[3]*x^2 + 
                  tiny.lm.3$coefficients[4]*x^3
  )


# 5th-order polynomial
tiny.lm.5 <- lm(data=train.data.tiny, price ~ carat + I(carat^2) + I(carat^3)
                + I(carat^4) + I(carat^5))
summary(tiny.lm.5)

tiny.lm.5$coefficients
ggplot(data=train.data.tiny) + 
  geom_point(mapping = aes(x = carat, y = price)) + 
  stat_function(geom = "line",
                fun = function(x) tiny.lm.5$coefficients[1] + 
                  tiny.lm.5$coefficients[2]*x + 
                  tiny.lm.5$coefficients[3]*x^2 + 
                  tiny.lm.5$coefficients[4]*x^3 + 
                  tiny.lm.5$coefficients[5]*x^4 + 
                  tiny.lm.5$coefficients[6]*x^5
  )


# 8th-order polynomial
tiny.lm.8 <- lm(data=train.data.tiny, price ~ carat + I(carat^2) + I(carat^3)
                + I(carat^4) + I(carat^5) + I(carat^6) + I(carat^7)+ I(carat^8))
summary(tiny.lm.8)

tiny.lm.8$coefficients
ggplot(data=train.data.tiny) + 
  geom_point(mapping = aes(x = carat, y = price)) + 
  stat_function(geom = "line",
                fun = function(x) tiny.lm.8$coefficients[1] + 
                  tiny.lm.8$coefficients[2]*x + 
                  tiny.lm.8$coefficients[3]*x^2 + 
                  tiny.lm.8$coefficients[4]*x^3 + 
                  tiny.lm.8$coefficients[5]*x^4 + 
                  tiny.lm.8$coefficients[6]*x^5 + 
                  tiny.lm.8$coefficients[7]*x^6 + 
                  tiny.lm.8$coefficients[8]*x^7 + 
                  tiny.lm.8$coefficients[9]*x^8
  )

# check out the validation results
val.preds.tiny.lm.1 <- predict(tiny.lm.1, newdata = validation.data)
ggplot() +
  geom_point(mapping = aes(x=val.preds.tiny.lm.1, y=validation.data$price)) +
  coord_equal(ratio=1)

val.preds.tiny.lm.2 <- predict(tiny.lm.2, newdata = validation.data)
plot(val.preds.tiny.lm.2,validation.data$price)

val.preds.tiny.lm.3 <- predict(tiny.lm.3, newdata = validation.data)
plot(val.preds.tiny.lm.3,validation.data$price)

val.preds.tiny.lm.5 <- predict(tiny.lm.5, newdata = validation.data)
plot(val.preds.tiny.lm.5,validation.data$price)

val.preds.tiny.lm.8 <- predict(tiny.lm.8, newdata = validation.data)
plot(val.preds.tiny.lm.8,validation.data$price)


