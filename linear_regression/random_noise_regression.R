library(ggplot2)

# for 'accuracy' function
library(forecast)

### generate some data

# generate a 20x10 matrix of completely random values between 0 and 10
set.seed(12345)
# x <- matrix(runif(200, min=0, max=10),nrow=20)
x <- matrix(rnorm(1000, mean=0, sd=1),nrow=100)
random.df <- data.frame(x)
head(random.df)
dim(random.df)
summary(random.df)

# name the last column 'Y'
names(random.df)
names(random.df)[10]
names(random.df)[10] <- "Y"
names(random.df)

# visualize the relationships
ggplot(random.df) + 
  geom_point(mapping = aes(x=X1, y=Y))

ggplot(random.df) + 
  geom_point(mapping = aes(x=X2, y=Y))
# and so on...

# pretend we have only seen 10 of the data points when we build our model
# aka train size = 10
train.size <- 10
train.idx <- sample(1:nrow(random.df), train.size)
train.idx

train.data <- random.df[train.idx,]
test.data <- random.df[-train.idx,]
dim(train.data)
dim(test.data)

# train a linear model with one predictor
lm.1 <- lm(Y ~ X1, data=train.data)
summary(lm.1)
# is this performance what you expect?

# train a linear model with 2 predictors
# do you expect R-squared to go up or down? Adjusted R-squared?
lm.2 <- lm(Y ~ X1 + X2, data=train.data)
summary(lm.2)

# train a linear model with 5 predictors
lm.5 <- lm(Y ~ X1 + X2 + X3 + X4 + X5, data=train.data)
summary(lm.5)

# train a linear model with 8 predictors
lm.8 <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8, data=train.data)
summary(lm.8)

# train a linear model with 9 predictors
# what R-squared do you expect? Adjusted R-squared?
lm.9 <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9, data=train.data)
summary(lm.9)

# plot predictions vs actuals for train, test

## LM 1
preds.lm.1.train <- predict(lm.1, newdata=train.data)
preds.lm.1.test <- predict(lm.1, newdata=test.data)

# LM 1 training predictions
p <- ggplot() +
  geom_point(mapping = aes(x=preds.lm.1.train, y=train.data$Y)) + 
  ggtitle("LM 1 training predictions")

p
p + xlim(-3,3) + ylim(-3,3) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")

# LM 1 test predictions
p <- ggplot() +
  geom_point(mapping = aes(x=preds.lm.1.test, y=test.data$Y)) + 
  ggtitle("LM 1 test predictions")

p
p + xlim(-3,3) + ylim(-3,3) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")


## LM 5
preds.lm.5.train <- predict(lm.5, newdata=train.data)
preds.lm.5.test <- predict(lm.5, newdata=test.data)

# LM 5 training predictions
p <- ggplot() +
  geom_point(mapping = aes(x=preds.lm.5.train, y=train.data$Y)) + 
  ggtitle("LM 5 training predictions")

p + xlim(-3,3) + ylim(-3,3) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")

# LM 5 test predictions
p <- ggplot() +
  geom_point(mapping = aes(x=preds.lm.5.test, y=test.data$Y)) + 
  ggtitle("LM 5 test predictions")

p + xlim(-3,3) + ylim(-3,3) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")

# some data points won't even fit
summary(preds.lm.5.test)
p + xlim(-10,10) + ylim(-10,10) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")


## LM 8
preds.lm.8.train <- predict(lm.8, newdata=train.data)
preds.lm.8.test <- predict(lm.8, newdata=test.data)

# LM 8 training predictions
p <- ggplot() +
  geom_point(mapping = aes(x=preds.lm.8.train, y=train.data$Y)) + 
  ggtitle("LM 8 training predictions")

p + xlim(-3,3) + ylim(-3,3) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")

# LM 8 test predictions
p <- ggplot() +
  geom_point(mapping = aes(x=preds.lm.8.test, y=test.data$Y)) + 
  ggtitle("LM 8 test predictions")

summary(preds.lm.8.test)
p + xlim(-25,25) + ylim(-25,25) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")


## LM 9
preds.lm.9.train <- predict(lm.9, newdata=train.data)
preds.lm.9.test <- predict(lm.9, newdata=test.data)

# LM 9 training predictions
p <- ggplot() +
  geom_point(mapping = aes(x=preds.lm.9.train, y=train.data$Y)) + 
  ggtitle("LM 9 training predictions")

p + xlim(-3,3) + ylim(-3,3) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")

# LM 9 test predictions
p <- ggplot() +
  geom_point(mapping = aes(x=preds.lm.9.test, y=test.data$Y)) + 
  ggtitle("LM 9 test predictions")

summary(preds.lm.9.test)
p + xlim(-8,8) + ylim(-8,8) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")

# compare test performance of the models
accuracy(preds.lm.1.test, test.data$Y)
preds.lm.2.test <- predict(lm.2, newdata=test.data)
accuracy(preds.lm.2.test, test.data$Y)
accuracy(preds.lm.5.test, test.data$Y)
accuracy(preds.lm.8.test, test.data$Y)
accuracy(preds.lm.9.test, test.data$Y)

# compare with the simplest possible linear model
# (intercept only, no predictors)
lm.0 <- lm(Y ~ 1, data=train.data)
summary(lm.0)

preds.lm.0.test <- predict(lm.0, newdata=test.data)
head(preds.lm.0.test)
mean(train.data$Y)

accuracy(preds.lm.0.test, test.data$Y)
sd(test.data$Y)
# why are these numbers slightly different?

mean(train.data$Y)
mean(test.data$Y)

# if we knew the "true" mean, RMSE = std dev of Y
y.mean <- mean(test.data$Y)
preds.y.mean <- rep(y.mean, nrow(test.data))

accuracy(preds.y.mean, test.data$Y)
sd(test.data$Y)


# plot LM 0 test predictions
p <- ggplot() +
  geom_point(mapping = aes(x=preds.lm.0.test, y=test.data$Y)) + 
  ggtitle("LM 0 test predictions")

p + xlim(-3,3) + ylim(-3,3) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="blue")

