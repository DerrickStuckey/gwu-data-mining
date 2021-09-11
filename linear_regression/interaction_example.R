library(ggplot2)

# for 'accuracy' function
library(forecast)

# generate some contrived random data
set.seed(12345)
x <- rnorm(100, mean=0, sd=10)
z <- rnorm(100, mean=0, sd=10)

errs <- rnorm(100, mean=0, sd=10)

y <- x*z + errs

# plot y vs x
ggplot() +
  geom_point(mapping = aes(x=x, y=y))

# plot y vs z
ggplot() +
  geom_point(mapping = aes(x=z, y=y))

# plot x, y and z together
ggplot() +
  geom_point(mapping = aes(x=x, y=z, col=y)) + 
  scale_color_gradient(low="blue", high="red")

# linear regression model for y with x as a predictor
lm(y ~ x) %>% 
  summary()
# equivalent to:
summary(lm(y ~ x))

# with z as a predictor
lm(y ~ z) %>% 
  summary()

# with x, z and interaction terms
lm(y ~ x*z) %>%
  summary()

# x*z interaction term only
lm(y ~ x*z - x - z) %>%
  summary()
