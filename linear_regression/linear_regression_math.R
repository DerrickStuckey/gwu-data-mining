
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
b0.est <- 2
b1.est <- 0.3
ggplot() + 
  geom_point(mapping = aes(x=x, y=y)) + 
  geom_abline(mapping = aes(intercept = b0.est, slope = b1.est))

# try another guess
b0.est <- 3
b1.est <- 0.4
ggplot() + 
  geom_point(mapping = aes(x=x, y=y)) + 
  geom_abline(mapping = aes(intercept = b0.est, slope = b1.est))

# actually use a linear model
linear.model <- lm(y ~ x)
summary(linear.model)

linear.model$coefficients
linear.model$coefficients[1]
linear.model$coefficients[2]

b0.lm <- linear.model$coefficients[1]
b1.lm <- linear.model$coefficients[2]
ggplot() + 
  geom_point(mapping = aes(x=x, y=y)) + 
  geom_abline(mapping = aes(intercept = b0.lm, 
                            slope = b1.lm))

# why did the linear model choose these parameters instead of 3 and 0.4 ?

# TODO look at residuals


