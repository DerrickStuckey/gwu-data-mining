library(tidyverse)

# define the logistic response function
logistic.response <- function(l) { 
  return(
    1 / (1 + exp(-1*l) )
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
l <- seq(-7,7,0.1)
head(l)
tail(l)

# plot the logistic response function from -7 to 7
ggplot() + 
  geom_line(mapping = aes(x=l, y=logistic.response(l))) + 
  xlab("\U2113") + ylab("p")

# plot for a greater range
l.2 <- seq(-100,100,0.1)

ggplot() + 
  geom_line(mapping = aes(x=l.2, y=logistic.response(l.2))) + 
  xlab("\U2113") + ylab("p")


# probit function
p <- seq(0,1,by=0.01)

# probit is the quantile function for the normal distribution
# we use its inverse, so defined with "p" input instead of "l"
probit <- function(p) {
  return(
    qnorm(p, mean = 0, sd=1)
  )
}

# compare logistic function and probit
g <- seq(-6,6,by=0.1)
p <- seq(0,1,by=0.01)

# construct dataframe with p,g values using both functions
logistic.vals <- data.frame(l=l,
                            p=logistic.response(l),
                            Function="logistic")
probit.vals <- data.frame(l=probit(p),
                          p=p,
                          Function="probit")
combined.vals <- rbind(logistic.vals, probit.vals)

ggplot(data=combined.vals) + 
  geom_line(mapping = aes(x=l, y=p, col=Function)) +
  xlab("\U2113") + ylab("p") + 
  xlim(-5,5)

# neural net logistic activation function example
ggplot() + 
  geom_line(mapping = aes(x=l, y=logistic.response(l))) + 
  xlab("weighted input") + ylab("output")
