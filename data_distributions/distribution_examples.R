library(tidyverse)

set.seed(12345)
x <- runif(300, min=0, max=2)

# uniform distribution
ggplot() + 
  geom_histogram(mapping = aes(x=x), bins=30) + 
  ggtitle("Uniform Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_abline(mapping = aes(intercept = 10, slope=0), col="blue")


# normal distribution
set.seed(12345)
x.norm <- rnorm(200, mean=1, sd=0.3)

x.ticks <- seq(0,2,by=0.01)

ggplot() + 
  geom_histogram(mapping = aes(x=x.norm), bins=20) + 
  ggtitle("Normal Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # stat_function(geom = "line",
  #               fun = function(x) dnorm(x, mean=1, sd=1),
  #               col="blue"
  # )
  geom_line(aes(x=x.ticks, y=20*dnorm(x.ticks, mean=1, sd=0.3)), colour="blue")


# log-normal
x.lognorm <- rlnorm(200, meanlog=1, sdlog=0.3)
x.ticks <- seq(1,7,by=0.01)

p <- ggplot() + 
  geom_histogram(mapping = aes(x=x.lognorm), bins=20) + 
  ggtitle("Log-Normal Distribution") +
  theme(plot.title = element_text(hjust = 0.5))
p
p + geom_line(aes(x=x.ticks, y=200/20*dlnorm(x.ticks, mean=exp(1), sd=exp(0.3))), colour="blue")


# Bi-Modal
library(MASS)
data(geyser)


