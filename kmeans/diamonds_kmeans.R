library(tidyverse)
library(stats)

diamonds

# set up dataframes to hold normalized values
diamonds.norm <- diamonds

# select all numeric variables
head(diamonds)
numeric.vars <- c("carat","depth","table","x","y","z")

# normalize the data
# this effectively counts each variable equally
normalizer <- preProcess(diamonds[,numeric.vars],
                         method = c("center", "scale"))
diamonds.norm[,numeric.vars] <- 
  predict(normalizer, diamonds[,numeric.vars])


