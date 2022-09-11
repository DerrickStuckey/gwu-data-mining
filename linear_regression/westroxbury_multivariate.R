# load libraries
library(tidyverse)

library(ggplot2)
# for 'accuracy' function
library(forecast)

# load dataset
westroxbury <- read.csv("~/Downloads/WestRoxbury.csv")
head(westroxbury)
dim(westroxbury)

# drop the "tax" variable as it's just a multiple of total value
westroxbury <- westroxbury %>% select(-TAX)
head(westroxbury)
dim(westroxbury)

# training/test split
set.seed(12345)
train.proportion <- 0.6
train.index <- sample(1:nrow(westroxbury),
                      train.proportion*nrow(westroxbury),
                      replace = FALSE)
train.data <- westroxbury[train.index,]
test.data <- westroxbury[-train.index,]

# disable scientific notation to make coefficients easier to read
options(scipen=999)

# build a linear regression model to predict Total Value
# using only "Gross Area" as a predictor
lm.gross.area <- lm(TOTAL.VALUE ~ GROSS.AREA,
                    data=train.data)
summary(lm.gross.area)
# coefficients:
# GROSS.AREA:         0.090973
# Multiple R-squared: 0.6488

# Instructions
# 1. Train another linear regression model using only the variable "LIVING.AREA" 
#    as a predictor. Note the coefficient for Living Area and the R-squared.
# 2. Train another linear regression model using both the variable "LIVING.AREA"
#    and "GROSS.AREA" as predictors. Compare the coefficients for each of these
#    variables to their coefficients in the one-variable models.
# 3. Now try a linear regression model using only "YR.BUILT" as a predictor. Note
#    the coefficient for that variable and the R-squared
# 4. Train a linear model using all 3 predictors, "LIVING.AREA", "GROSS.AREA", and
#    "YR.BUILT". How do the coefficients change? Is this what you expected?


