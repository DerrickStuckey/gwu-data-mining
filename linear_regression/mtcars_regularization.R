library(tidyverse)

# install.packages("glmnet")
library(glmnet)

# install.packages("glmnetUtils")
library(glmnetUtils)

# for normalizer
library(caret)

mtcars

head(mtcars)
dim(mtcars)

set.seed(12345)
train.proportion <- 0.75

train.idx <- sample(1:nrow(mtcars), nrow(mtcars)*train.proportion)
train.idx

# train/test split
train.data <- mtcars[train.idx,]
test.data <- mtcars[-train.idx,]
dim(train.data)
dim(test.data)

summary(mtcars$mpg)

# normalize predictors before running ridge regression
normalizer <- preProcess(train.data %>% select(-mpg),
                         method=c("center","scale"))
train.data.norm <- predict(normalizer, train.data)
test.data.norm <- predict(normalizer, test.data)

# what did this actually do?
summary(train.data)
summary(train.data.norm)
summary(test.data)
summary(test.data.norm)

lapply(train.data,sd)
lapply(train.data.norm,sd)

# did this change relationships between variables?
cor(train.data$mpg, train.data$carb)
cor(train.data.norm$mpg, train.data.norm$carb)
ggplot(data=train.data) + geom_point(mapping=aes(x=carb, y=mpg))
ggplot(data=train.data.norm) + geom_point(mapping=aes(x=carb, y=mpg))

# why do we use only the training data to construct the normalizer?

# why don't we normalize the target variable (mpg)?


# glmnet requires a predictor matrix and target variable vector
train.predictors <- train.data.norm[,-1]
head(train.predictors)
train.predictor.matrix <- as.matrix(train.predictors)

train.target <- train.data.norm$mpg

# train ridge regression with a lambda (penalty parameter) of 0.2 (arbitrarily chosen)
lambda <- 0.2
ridge.lm <- glmnet(x = train.predictor.matrix, 
                   y = train.target, 
                   alpha = 0, lambda = lambda)
ridge.lm$beta

# train a basic linear model on the same data
basic.lm <- lm(mpg ~ ., data=train.data.norm)
summary(basic.lm)
basic.lm$coefficients

# compare a few coefficients
basic.lm$coefficients["wt"]
ridge.lm$beta["wt",]

basic.lm$coefficients["cyl"]
ridge.lm$beta["cyl",]

# compare predictions on test data
basic.preds <- predict(basic.lm, newdata = test.data.norm)
test.predictor.matrix <- as.matrix(test.data.norm[,-1])
ridge.preds <- predict(ridge.lm, newx = test.predictor.matrix)

# which predictions are more "conservative"?
summary(basic.preds)
summary(ridge.preds)
sd(basic.preds)
sd(ridge.preds)

# which are actually better?
ggplot() + 
  geom_point(mapping = aes(x=basic.preds, y=test.data$mpg)) + 
  coord_fixed() +
  geom_abline(intercept=0, slope=1, col="red") +
  ggtitle("Basic Linear Regression") +
  xlab("Predicted MPG") + ylab("Actual MPG")
ggplot() + 
  geom_point(mapping = aes(x=ridge.preds, y=test.data$mpg)) + 
  coord_fixed() +
  geom_abline(intercept=0, slope=1, col="red") +
  ggtitle("Ridge Regression") +
  xlab("Predicted MPG") + ylab("Actual MPG")

library(forecast)
accuracy(basic.preds, test.data.norm$mpg)
accuracy(as.vector(ridge.preds), test.data.norm$mpg)

# Lasso regression too
lambda <- 0.05
lasso.lm <- glmnet(x = train.predictor.matrix, 
                   y = train.target, 
                   alpha = 1, lambda = lambda)
lasso.lm$beta
# compare with ridge
ridge.lm$beta
# compare with basic LM
basic.lm$coefficients


# plot all 3 together
coef.names <- names(basic.lm$coefficients[-1])
ridge.lm.beta.vec <- ridge.lm$beta[,1] %>% 
  cbind(rep("Ridge",10)) %>% cbind(coef.names)
lasso.lm.beta.vec <- lasso.lm$beta[,1] %>% 
  cbind(rep("Lasso",10)) %>% cbind(coef.names)
basic.lm.beta.vec <- basic.lm$coefficients[-1] %>% 
  cbind(rep("Basic",10)) %>% cbind(coef.names)

comp.df <- ridge.lm.beta.vec %>% 
  rbind(lasso.lm.beta.vec) %>%
  rbind(basic.lm.beta.vec) %>% data.frame()
comp.df
names(comp.df) <- c("Beta","Model","Index")
comp.df$Beta <- as.numeric(as.character(comp.df$Beta))

# plot the Beta coefficients for each model
ggplot(comp.df) +
  geom_line(mapping = aes(x=as.numeric(Index), y=Beta, col=Model)) +
  geom_point(mapping = aes(x=as.numeric(Index), y=Beta, col=Model, shape=Model), 
             size=5) +
  geom_abline(slope=0, intercept = 0)

# same data in a bar plot
ggplot(comp.df) +
  geom_col(position="dodge", mapping = aes(x=Index, y=Beta, fill=Model)) +
  geom_abline(slope=0, intercept = 0)

# What about other values of lambda? is 0.1 the best choice?

# ridge with multiple lambdas tested
ridge.lm.multilambda <- glmnet(x = train.predictor.matrix, 
                   y = train.target, 
                   alpha = 0)
plot(ridge.lm.multilambda, xvar = "lambda")

# lasso with multiple lambdas tested
lasso.lm.multilambda <- glmnet(x = train.predictor.matrix, 
                               y = train.target, 
                               alpha = 1)
plot(lasso.lm.multilambda, xvar = "lambda")


# plot test set performance vs lambda for each

# lasso
lambdas.lasso <- 10 ^ seq(-6,2,by=0.1)
rmse.vals.lasso <- c()
for (lambda in lambdas.lasso) {
  lasso.lm.trial <- glmnet(x = train.predictor.matrix, 
                                 y = train.target, 
                                 alpha = 1, lambda=lambda)
  test.preds.trial <- predict(lasso.lm.trial, newx=test.predictor.matrix)
  test.accuracy.trial <- accuracy(as.vector(test.preds.trial), test.data.norm$mpg)
  rmse.vals.lasso <- c(rmse.vals.lasso, test.accuracy.trial[2])
}
ggplot() +
  geom_line(mapping = aes(x=lambdas.lasso, y=rmse.vals.lasso)) + 
  scale_x_log10() + ggtitle("Lasso") + xlab("Lambda") +
  ylab("RMSE")

# ridge
lambdas.ridge <- 10 ^ seq(-6,4,by=0.1)
rmse.vals.ridge <- c()
for (lambda in lambdas.ridge) {
  ridge.lm.trial <- glmnet(x = train.predictor.matrix, 
                           y = train.target, 
                           alpha = 0, lambda=lambda)
  test.preds.trial <- predict(ridge.lm.trial, newx=test.predictor.matrix)
  test.accuracy.trial <- accuracy(as.vector(test.preds.trial), test.data.norm$mpg)
  rmse.vals.ridge <- c(rmse.vals.ridge, test.accuracy.trial[2])
}
ggplot() +
  geom_line(mapping = aes(x=lambdas.ridge, y=rmse.vals.ridge)) + 
  scale_x_log10() + ggtitle("Ridge") + xlab("Lambda") +
  ylab("RMSE")

# tune parameters using cross-validation
lambda.sequence <- 10 ^ seq(-5, 5, 1)
lambda.sequence
ridge.lm.crossval <- cv.glmnet(x = train.predictor.matrix, 
                               y = train.target, 
                               alpha = 0,
                               lambda=lambda.sequence)

# look at cross-validation avg error vs lambda
ridge.lm.crossval$cvm
ridge.lm.crossval$lambda

ggplot() + 
  geom_line(mapping = aes(x=ridge.lm.crossval$lambda, y=ridge.lm.crossval$cvm)) + 
  scale_x_log10() + 
  ggtitle("Ridge Cross-validation") + xlab("Lambda (log scale)") + ylab("CV Error")

# what is the best lambda?
ridge.lm.crossval$lambda.min

# try again without specifying range for lambda
ridge.lm.crossval <- cv.glmnet(x = train.predictor.matrix, 
                               y = train.target, 
                               alpha = 0)

# evaluate against test set
cv.ridge.preds <- predict(ridge.lm.crossval, newx = test.predictor.matrix)

# compare performance to earlier versions
accuracy(basic.preds, test.data.norm$mpg)
accuracy(as.vector(ridge.preds), test.data.norm$mpg)
accuracy(as.vector(cv.ridge.preds), test.data.norm$mpg)

# try elastic net
# cva.glmnet from library(glmnetUtils)
lambda.sequence <- 10 ^ seq(-6,2,by=0.5)
lambda.sequence
alpha.sequence <- seq(0,1,0.2)
alpha.sequence
elastic.lm.crossval <- cva.glmnet(x = train.predictor.matrix, 
                                  y = train.target, 
                                  alpha = alpha.sequence,
                                  lambda=lambda.sequence,
                                  nfolds=5)

elastic.lm.crossval$alpha

# a list of elastic net models, one for each value of alpha
elastic.lm.crossval$modlist
elastic.lm.crossval$modlist[[1]]$cvm

# set up CV error as matrix over alpha, lambda
elastic.mse.vals <- data.frame(elastic.lm.crossval$modlist[[1]]$cvm)
names(elastic.mse.vals)[1] <- alpha.sequence[1]
for (i in 2:length(alpha.sequence)) {
  elastic.mse.vals <- cbind(
    elastic.mse.vals,
    data.frame(elastic.lm.crossval$modlist[[i]]$cvm)
  )
  names(elastic.mse.vals)[i] <- alpha.sequence[i]
}

row.names(elastic.mse.vals) <- log(lambda.sequence,10)

# plot a heatmap of CV error vs Alpha and Lambda
heatmap(as.matrix(elastic.mse.vals),
        xlab="Alpha", ylab="Lambda (Log 10)",
        keep.dendro = FALSE,
        Colv=NA, Rowv=NA, scale='none')

