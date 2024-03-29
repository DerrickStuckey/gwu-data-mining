library(ggplot2)
library(forecast) # for 'accuracy' function

# load 'mtcars' dataset
head(mtcars)
dim(mtcars)


## Relationship between MPG (miles per gallon) and Weight
ggplot(data=mtcars) + 
  geom_point(mapping = aes(x=wt, y=mpg))

# correlation of MPG and Weight
mpg.wt.correlation <- cor(mtcars$mpg, mtcars$wt)
mpg.wt.correlation

# what determines correlation?
cov(mtcars$wt, mtcars$mpg) / sd(mtcars$mpg) / sd(mtcars$wt)

# compare with linear model 
mpg.lm.wt <- lm(mpg ~ wt, data=mtcars)
summary(mpg.lm.wt)

# look familiar?
mpg.wt.correlation ^ 2

# look familiar
mpg.wt.correlation * sd(mtcars$mpg) / sd(mtcars$wt)
cov(mtcars$wt, mtcars$mpg) / var(mtcars$wt)



## Relationship between MPG and Horsepower
ggplot(data=mtcars) + 
  geom_point(mapping = aes(x=hp, y=mpg))

# correlation of MPG and Horsepower
mpg.hp.correlation <- cor(mtcars$mpg, mtcars$hp)
mpg.hp.correlation

# compare with linear model 
mpg.lm.hp <- lm(mpg ~ hp, data=mtcars)
summary(mpg.lm.hp)

# look familiar?
mpg.hp.correlation ^ 2
mpg.hp.correlation * sd(mtcars$mpg) / sd(mtcars$hp)


## Check out predictions generated by each model

# predictions based on Weight alone
mpg.lm.wt$coefficients
mpg.preds.wt <- mpg.lm.wt$coefficients[1] + 
  mpg.lm.wt$coefficients[2] * mtcars$wt

# how good are these predictions?
ggplot(data=mtcars) + 
  geom_point(mapping = aes(x=mpg.preds.wt, y=mtcars$mpg)) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="red") +
  coord_fixed(ratio=1)

accuracy(mpg.preds.wt, mtcars$mpg)

# predictions based on Horsepower alone
mpg.lm.hp$coefficients
mpg.preds.hp <- mpg.lm.hp$coefficients[1] + 
  mpg.lm.hp$coefficients[2] * mtcars$hp

# how good are these predictions?
ggplot(data=mtcars) + 
  geom_point(mapping = aes(x=mpg.preds.hp, y=mtcars$mpg)) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="red") + 
  coord_fixed(ratio=1)

accuracy(mpg.preds.hp, mtcars$mpg)


## Try to manually build a multivariate model
mpg.lm.wt$coefficients
mpg.lm.hp$coefficients

# try adding in the 'hp' coefficient to the 'wt' model
mpg.preds.combined <- mpg.lm.wt$coefficients[1] + 
  mpg.lm.wt$coefficients[2] * mtcars$wt + 
  mpg.lm.hp$coefficients[2] * mtcars$hp

# how good are the predictions from this model?
ggplot(data=mtcars) + 
  geom_point(mapping = aes(x=mpg.preds.combined, y=mtcars$mpg)) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="red") + 
  coord_fixed(ratio=1) + ggtitle("Weight and Horsepower Coefs")

accuracy(mpg.preds.combined, mtcars$mpg)
accuracy(mpg.preds.hp, mtcars$mpg)
accuracy(mpg.preds.wt, mtcars$mpg)

# what's the problem?

# try choosing a new intercept
mean(mpg.preds.combined)
mean(mtcars$mpg)
mpg.preds.combined.offset <- mpg.preds.combined + 
  (mean(mtcars$mpg) - mean(mpg.preds.combined))

# did this fix things?
ggplot(data=mtcars) + 
  geom_point(mapping = aes(x=mpg.preds.combined.offset, y=mtcars$mpg)) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="red") + 
  coord_fixed(ratio=1) + ggtitle("Weight and Horsepower Coefs")
  
accuracy(mpg.preds.combined.offset, mtcars$mpg)
accuracy(mpg.preds.hp, mtcars$mpg)
accuracy(mpg.preds.wt, mtcars$mpg)

# what's the relationship between the two predictors themselves?
ggplot(data=mtcars) + 
  geom_point(mapping = aes(x=wt, y=hp))


## Let R try to find the optimal coefficients
mpg.lm.combined <- lm(mpg ~ wt + hp, data=mtcars)
summary(mpg.lm.combined)

mpg.lm.wt$coefficients
mpg.lm.hp$coefficients
mpg.lm.combined$coefficients

mpg.preds.combined <- predict(mpg.lm.combined, newdata = mtcars)

# how do the combined model predictions look now?
ggplot(data=mtcars) + 
  geom_point(mapping = aes(x=mpg.preds.combined, y=mtcars$mpg)) + 
  geom_abline(mapping = aes(intercept = 0, slope = 1), col="red") + 
  coord_fixed(ratio=1) + ggtitle("Weight and Horsepower Coefs")

accuracy(mpg.preds.combined, mtcars$mpg)
accuracy(mpg.preds.hp, mtcars$mpg)
accuracy(mpg.preds.wt, mtcars$mpg)




