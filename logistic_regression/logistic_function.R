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

logistic.response(10)
logistic.response(-10)

# create a set of inputs to plot
x <- seq(-10,10,0.1)
head(x)
tail(x)

# plot the logistic response function from -10 to 10
ggplot() + 
  geom_line(mapping = aes(x=x, y=logistic.response(x)))
