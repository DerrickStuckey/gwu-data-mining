library(tidyverse)

set.seed(12345)
x <- runif(300, min=0, max=2)

# uniform distribution
ggplot() + 
  geom_histogram(mapping = aes(x=x, y=..density..), bins=20) + 
  ggtitle("Uniform Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("x") +
  geom_abline(mapping = aes(intercept = 0.5, slope=0), col="blue")


# normal distribution
set.seed(12345)
x.norm <- rnorm(300, mean=1, sd=0.3)

x.ticks <- seq(0,2,by=0.01)

ggplot() + 
  geom_histogram(mapping = aes(x=x.norm, y=..density..), bins=20) + 
  ggtitle("Normal Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("x") +
  geom_line(aes(x=x.ticks, y=dnorm(x.ticks, mean=1, sd=0.3)), colour="blue")


# log-normal
x.lognorm <- rlnorm(300, meanlog=1, sdlog=0.3)
x.ticks <- seq(1,8,by=0.01)

ggplot() + 
  geom_histogram(mapping = aes(x=x.lognorm, y=..density..), bins=20) + 
  ggtitle("Log-Normal Distribution") +
  xlab("x") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_line(aes(x=x.ticks, y=dlnorm(x.ticks, meanlog=1, sdlog=0.3)), colour="blue")

# exponential
x.exp <- rexp(300, rate=2)
x.ticks <- seq(0,3,by=0.01)

ggplot() + 
  geom_histogram(mapping = aes(x=x.exp, y=..density..), bins=20) + 
  xlim(0,3) + 
  ggtitle("Exponential Distribution") +
  xlab("x") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_line(aes(x=x.ticks, y=dexp(x.ticks, rate=2)), colour="blue")


# Bi-Modal
x.norm.1 <- rnorm(100, mean=1,sd=0.25)
x.norm.2 <- rnorm(200, mean=2,sd=0.25)
x.bimodal <- c(x.norm.1, x.norm.2)

x.ticks <- seq(0,3,by=0.01)

ggplot() + 
  geom_histogram(mapping = aes(x=x.bimodal, y=..density..), bins=20) + 
  ggtitle("Bi-Modal Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("x")
+
  geom_line(aes(x=x.ticks, y=dnorm(x.ticks, mean=1, sd=0.3)), colour="blue")



library(MASS)
data(geyser)


