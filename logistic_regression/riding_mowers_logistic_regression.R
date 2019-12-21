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


# train a model on Income only
income.glm <- glm(Owner ~ Income,
                  data=train.data,
                  family="binomial")
summary(income.glm)

# what would this model predict for different ranges of incomes?
income.range <- 0:150
# income.range <- 50:80
income.range.df <- data.frame("Income"=income.range)
income.range.df$owner.prob.income.glm <- predict(income.glm, newdata = income.range.df,
                                                 type="response")

ggplot(data=income.range.df) + 
  geom_line(mapping = aes(x=Income, y=owner.prob.income.glm))

# what does it predict for the actual test data?
test.data$owner.prob.income.glm <- predict(income.glm, newdata = test.data,
                                           type="response")

ggplot(data=test.data) + 
  geom_point(mapping = aes(x=Income, y=owner.prob.income.glm))

# color according to the actual value
ggplot(data=test.data) + 
  geom_point(mapping = aes(x=Income, y=owner.prob.income.glm, col=Owner))

# looks like one point is hidden
ggplot(data=test.data) + 
  geom_jitter(mapping = aes(x=Income, y=owner.prob.income.glm, col=Owner))


# train a model on Lot_Size only
lotsize.glm <- glm(Owner ~ Lot_Size,
                  data=train.data,
                  family="binomial")
summary(lotsize.glm)

# obtain and plot predictions for the test data
test.data$owner.prob.lotsize.glm <- predict(lotsize.glm, newdata = test.data,
                                           type="response")

ggplot(data=test.data) + 
  geom_jitter(mapping = aes(x=Lot_Size, y=owner.prob.lotsize.glm, col=Owner))


# train a model on both variables
full.glm <- glm(Owner ~ Income + Lot_Size,
                data=train.data,
                family="binomial")

# obtain and plot predictions for the test data
test.data$owner.prob.full.glm <- predict(full.glm, newdata = test.data,
                                            type="response")

# try plotting again (with x=Income)
ggplot(data=test.data) + 
  geom_jitter(mapping = aes(x=Income, y=owner.prob.full.glm, col=Owner))

# with x=Lot_Size
ggplot(data=test.data) + 
  geom_jitter(mapping = aes(x=Lot_Size, y=owner.prob.full.glm, col=Owner))
