# load libraries
library(tidyverse)

library(ggplot2)
# for 'accuracy' function
library(forecast)

# load dataset
westroxbury <- read.csv("~/Downloads/WestRoxbury.csv")
head(westroxbury)
dim(westroxbury)

# disable scientific notation to make coefficients easier to read
options(scipen=999)

# build a linear regression model to predict Total Value
# using only "Gross Area" as a predictor
lm.gross.area <- lm(TOTAL.VALUE ~ GROSS.AREA,
                    data=westroxbury)
summary(lm.gross.area)
# coefficients:
# GROSS.AREA:         0.090973
# Multiple R-squared: 0.6488

# Instructions
# 1. Train another linear regression model using only the variable "LIVING.AREA" 
#    as a predictor. Note the coefficient for Living Area and the R-squared.

# 2. What is the correlation between LIVING.AREA and GROSS.AREA? If you train a
#    linear model using both of those variables as predictors, how do you expect
#    the coefficients to change? The R-squared to change?

# 3. Train another linear regression model using both the variable "LIVING.AREA"
#    and "GROSS.AREA" as predictors. Compare the coefficients for each of these
#    variables to their coefficients in the one-variable models.

# 4. Now try a linear regression model using only "YR.BUILT" as a predictor. Note
#    the coefficient for that variable and the R-squared

# 5. Train a linear model using all 3 predictors, "LIVING.AREA", "GROSS.AREA", and
#    "YR.BUILT". How do the coefficients change? Is this what you expected?


