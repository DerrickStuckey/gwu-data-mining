library(tidyverse)

# define the logistic response function
logistic.response <- function(x) { 
  return(
    1 / (1 + exp(-1*x) )
  )
}

# test a few key values
logistic.response(0)
logistic.response(Inf)
logistic.response(-Inf)

logistic.response(5)
logistic.response(-5)

logistic.response(1)
logistic.response(-1)

# create a set of inputs to plot
x <- seq(-7,7,0.1)
head(x)
tail(x)

# plot the logistic response function from -10 to 10
ggplot() + 
  geom_line(mapping = aes(x=x, y=logistic.response(x))) + 
  ylab("Logistic Response")

# plot for a greater range
x.2 <- seq(-100,100,0.1)

ggplot() + 
  geom_line(mapping = aes(x=x.2, y=logistic.response(x.2))) + 
  ylab("Logistic Response")


# probit function
y <- seq(0,1,by=0.01)

# probit is the quantile function for the normal distribution
# we use its inverse, so defined with "y" input instead of "x"
probit <- function(y) {
  return(
    qnorm(y, mean = 0, sd=1)
  )
}

# compare logistic function and probit
x <- seq(-6,6,by=0.1)
y <- seq(0,1,by=0.01)

# construct dataframe with x, y values using both functions
logistic.vals <- data.frame(x=x,
                            y=logistic.response(x),
                            Function="logistic")
probit.vals <- data.frame(x=probit(y),
                          y=y,
                          Function="probit")
combined.vals <- rbind(logistic.vals, probit.vals)

ggplot(data=combined.vals) + 
  geom_line(mapping = aes(x=x, y=y, col=Function)) +
  xlab("x") + ylab("y") + 
  xlim(-5,5)

