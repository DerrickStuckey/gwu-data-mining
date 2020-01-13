
# generate a vector of random values between 0 and 1
set.seed(12345)
x <- runif(10, min=0, max=10)
x

# choose "real" b0 and b1
b0 <- 2.5
b1 <- 0.5

# add random error terms
errs <- rnorm(10, mean=0, sd=1)
errs

y <- b0 + b1 * x + errs
y

# plot y vs x
ggplot() + 
  geom_point(mapping = aes(x=x, y=y)) + 
  xlim(0,10) + ylim(0,10)

# make a guess at the parameters for b0 and b1 and plot with that line added
b0.guess <- 2
b1.guess <- 0.3
ggplot() + 
  geom_point(mapping = aes(x=x, y=y)) + 
  geom_abline(mapping = aes(intercept = b0.guess, slope = b1.guess))

# try another guess
b0.guess <- 3
b1.guess <- 0.4
ggplot() + 
  geom_point(mapping = aes(x=x, y=y)) + 
  geom_abline(mapping = aes(intercept = b0.guess, slope = b1.guess))

# actually use a linear model to estimate b0 and b1
linear.model <- lm(y ~ x)
summary(linear.model)

linear.model$coefficients
linear.model$coefficients[1]
linear.model$coefficients[2]

  # aside: calculate the coefficients manually
  xm <- mean(x)
  ym <- mean(y)
  b1.calc <- sum( (x-xm)*(y-ym) ) / sum( (x-xm)^2 )
  b0.calc <- ym - b1.calc*xm
  b0.calc
  b1.calc
  linear.model$coefficients

b0.lm <- linear.model$coefficients[1]
b1.lm <- linear.model$coefficients[2]
ggplot() + 
  geom_point(mapping = aes(x=x, y=y)) + 
  geom_abline(mapping = aes(intercept = b0.lm, 
                            slope = b1.lm))

# why did the linear model choose these parameters instead of 3 and 0.4 ?

# calculate SSE (sum of squared errors) for the "guess" model
preds.guess <- b0.guess + b1.guess * x
errors.guess <- y - preds.guess
errors.guess
sum.squared.errors.guess <- sum(errors.guess^2)
sum.squared.errors.guess

# calculate SSE for the linear model
preds.lm <- b0.lm + b1.lm * x
errors.lm <- y - preds.lm
sum.squared.errors.lm <- sum(errors.lm^2)
sum.squared.errors.lm

# which one is lower?


# calculate RMSE, ME, MPE, MAPE, MAE

# ME (mean error)
me.lm <- mean(errors.lm)
me.lm

# RMSE (root mean squared error)
mse.lm <- sum.squared.errors.lm / length(errors.lm)
rmse.lm <- sqrt(mse.lm)
rmse.lm

# MAE (mean absolute error)
abs.errors.lm <- abs(errors.lm)
abs.errors.lm
mae.lm <- mean(abs.errors.lm)

# MPE (mean percentage error)
mpe.lm <- mean(errors.lm / y) * 100
mpe.lm

# MAPE (mean absolute percentage error)
mape.lm <- mean(abs.errors.lm / y) * 100
mape.lm

# or we can get these values much more easily from the accuracy() function
# in the "forecast" package 
library(forecast)
accuracy(preds.lm, y)

# compare with the "guess" values
accuracy(preds.guess, y)
# are the model predictions best on every measure?

