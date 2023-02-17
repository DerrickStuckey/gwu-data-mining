library(tidyverse)
library(stats)

# from "Data Mining for Business Analytics"
# https://www.dataminingbook.com/book/r-edition
riding.mowers <- read_csv("./data/RidingMowers.csv")
riding.mowers
dim(riding.mowers)

# target variable
table(riding.mowers$Ownership)

# create a binary version of the target variable
riding.mowers$Owner <- riding.mowers$Ownership=="Owner"
table(riding.mowers$Owner)

# predictor variables
summary(riding.mowers$Income)
summary(riding.mowers$Lot_Size)

# train/test split
dim(riding.mowers)
train.proportion <- 2/3
train.size <- nrow(riding.mowers) * train.proportion
train.size
set.seed(12345)
train.idx <- sample(1:nrow(riding.mowers),
                    train.size)
train.data <- riding.mowers[train.idx,]
test.data <- riding.mowers[-train.idx,]

# check the distribution of the target in our training and test sets
table(train.data$Ownership)
table(test.data$Ownership)

# is this OK?


# try a different split
# set.seed(12345)

# train a model on Income only
income.glm <- glm(Owner ~ Income,
                  data=train.data,
                  family=binomial)
# probit regression: family = binomial(link = "probit")
summary(income.glm)

# what would this model predict for different ranges of incomes?
summary(train.data$Income)
income.range <- 0:150
income.range
# income.range <- 50:80
income.range.df <- data.frame("Income"=income.range)
income.range.df$owner.prob.income.glm <- predict(income.glm, newdata = income.range.df,
                                                 type="response")
head(income.range.df)
tail(income.range.df)

ggplot(data=income.range.df) + 
  geom_line(mapping = aes(x=Income, y=owner.prob.income.glm))

# what does it predict for each actual data point?
train.data$owner.prob.income.glm <- predict(income.glm, newdata = train.data,
                                           type="response")

ggplot(data=train.data) + 
  geom_point(mapping = aes(x=Income, y=owner.prob.income.glm))

# color according to the actual value
ggplot(data=train.data) + 
  geom_point(mapping = aes(x=Income, y=owner.prob.income.glm, col=Owner))


# train a model on Lot_Size only
lotsize.glm <- glm(Owner ~ Lot_Size,
                  data=train.data,
                  family="binomial")
summary(income.glm)
summary(lotsize.glm)

# obtain and plot predictions for the training data points
train.data$owner.prob.lotsize.glm <- predict(lotsize.glm, newdata = train.data,
                                           type="response")

ggplot(data=train.data) + 
  geom_point(mapping = aes(x=Lot_Size, y=owner.prob.lotsize.glm, col=Owner))

# looks like some data points are hidden
ggplot(data=train.data) + 
  geom_jitter(mapping = aes(x=Lot_Size, y=owner.prob.lotsize.glm, col=Owner))


# train a model on both variables
full.glm <- glm(Owner ~ Income + Lot_Size,
                data=train.data,
                family="binomial")
summary(full.glm)

# obtain and plot predictions for the test data
train.data$owner.prob.full.glm <- predict(full.glm, newdata = train.data,
                                            type="response")

# try plotting again (with x=Income)
ggplot(data=train.data) + 
  geom_jitter(mapping = aes(x=Income, y=owner.prob.full.glm, col=Owner))

# with x=Lot_Size
ggplot(data=train.data) + 
  geom_jitter(mapping = aes(x=Lot_Size, y=owner.prob.full.glm, col=Owner))

# more complex relationship since we have 2 model inputs now
# try to capture all 4 variables in one plot
ggplot(data=train.data) + 
  geom_jitter(mapping = aes(x=Lot_Size, y=Income, col=owner.prob.full.glm, shape=Owner),
              size=4)



# evaluate the performance of the full model on the test set
test.data$owner.prob.full.glm <- predict(full.glm, newdata = test.data,
                                         type="response")

# same plot for the test data
ggplot(data=test.data) + 
  geom_jitter(mapping = aes(x=Lot_Size, y=Income, col=owner.prob.full.glm, shape=Owner),
              size=4)

# does it correctly pick out all the Owners?
test.data$owner.pred.full.glm <- test.data$owner.prob.full.glm > 0.5

# simple table
table(test.data$owner.pred.full.glm, test.data$Owner)

# confusion matrix
library(caret)
test.data$owner.pred.full.glm <- as.factor(test.data$owner.pred.full.glm)
test.data$Owner <- as.factor(test.data$Owner)
confusionMatrix(test.data$owner.pred.full.glm, test.data$Owner)

# ROC curve
table(test.data$owner.prob.full.glm) # how many cuts?
library(plotROC)
ggplot(data=test.data,
       mapping = aes(m = owner.prob.full.glm, d = Owner)) +
  geom_roc(n.cuts=9,labels=FALSE) + 
  style_roc(theme = theme_grey)

# AUC (area under the curve) for ROC curve
library(pROC)
roc.obj <- roc(response = test.data$Owner,
               predictor = test.data$owner.prob.full.glm)
auc(roc.obj)

View(test.data)
