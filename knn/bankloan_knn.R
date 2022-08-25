# install.packages("caret")
# install.packages("FNN")

library(caret)
library(tidyverse)
library(FNN) # for knn function

# from "Data Mining for Business Analytics"
# https://www.dataminingbook.com/book/r-edition
bankdata <- read_csv("./data/UniversalBank.csv")
bankdata
View(bankdata)

# clean up some column types
bankdata <- read_csv("./data/UniversalBank.csv",
                     col_types = cols(
                       ID = col_double(),
                       Age = col_double(),
                       Experience = col_double(),
                       Income = col_double(),
                       `ZIP Code` = col_factor(),
                       Family = col_double(),
                       CCAvg = col_double(),
                       Education = col_double(),
                       Mortgage = col_double(),
                       `Personal Loan` = col_logical(),
                       `Securities Account` = col_logical(),
                       `CD Account` = col_logical(),
                       Online = col_logical(),
                       CreditCard = col_logical()
                     ))
head(bankdata)

# construct a factor variable to use as output for our knn model
table(bankdata$`Personal Loan`)
bankdata$Loan.Status <- "Rejects"
bankdata$Loan.Status[bankdata$`Personal Loan`] <- "Accepts"
bankdata$Loan.Status <- factor(bankdata$Loan.Status,levels=c("Accepts","Rejects"))

# training/test/validation split
set.seed(12345)
train.proportion <- 0.6
test.proportion <- 0.2
validation.proportion <- 0.2

# pull out the training data
train.index <- sample(1:nrow(bankdata), nrow(bankdata)*train.proportion)
train.data <- bankdata[train.index,]

# select test and validation from what's left over
holdout.data <- bankdata[-train.index,]
test.index <- sample(1:nrow(holdout.data), nrow(bankdata)*test.proportion)
test.data <- holdout.data[test.index,]
validation.data <- holdout.data[-test.index,]

# check sizes
dim(train.data)
dim(validation.data)
dim(test.data)

# set up dataframes to hold normalized values
train.data.norm <- train.data
validation.data.norm <- validation.data
test.data.norm <- test.data
# why do we have to normalize?

# select a subset of numeric variables to use as KNN predictors
head(train.data)
names(train.data)
table(train.data$Education)
selected.vars <- c("Age","Experience","Income","Education")

# normalize numeric variables
# using preProcess() from caret package
normalizer <- preProcess(train.data[,selected.vars],
                         method = c("center", "scale"))
                         # method = "range")
train.data.norm[,selected.vars] <- 
  predict(normalizer, train.data[,selected.vars])
validation.data.norm[,selected.vars] <- 
  predict(normalizer, validation.data[,selected.vars])
test.data.norm[,selected.vars] <- 
  predict(normalizer, test.data[,selected.vars])
# why do we normalize the test and validation data using the training data normalizer?

# what did this normalization actually do?
mean(train.data$Age)
sd(train.data$Age)

mean(train.data.norm$Age)
sd(train.data.norm$Age)

mean(train.data.norm$Income)
sd(train.data.norm$Income)

mean(validation.data.norm$Income)
sd(validation.data.norm$Income)

# but we haven't lost any information
head(train.data$Age)
head(train.data.norm$Age)

cor(train.data$Age, train.data.norm$Age)
cor(train.data$Income, train.data.norm$Income)

cor(train.data$Age, train.data$Income)
cor(train.data.norm$Age, train.data.norm$Income)

# predict Loan Status with a k-nearest neighbors model with k=3
# note how this model setup is different - we jump straight to predictions
# there is no real "training" for a knn model, as there are no coefficients to be estimated
# we can just choose our predictors, and choose 'k'
loan.knn.3.preds <- FNN::knn(train = train.data.norm[,selected.vars],
                 test = validation.data.norm[,selected.vars],
                 cl = train.data.norm$Loan.Status,
                 k=3)

# look at our predictions
head(loan.knn.3.preds)
length(loan.knn.3.preds)
summary(loan.knn.3.preds)
summary(validation.data.norm$Loan.Status)

# other attributes available from our predictions
names(attributes(loan.knn.3.preds))
head( attr(loan.knn.3.preds, "nn.index") )

# find the neighbors for the first data point in the validation data
dim( attr(loan.knn.3.preds, "nn.index") )
first.point.neighbor.indexes <- attr(loan.knn.3.preds, "nn.index")[1,]
first.point.neighbor.indexes
first.point.neighbors <- train.data.norm[first.point.neighbor.indexes,selected.vars]

# compare the first point and its neighbors
validation.data.norm[1,selected.vars]
first.point.neighbors

# non-normalized versions:
validation.data[1,selected.vars]
train.data[first.point.neighbor.indexes,selected.vars]

# check the distance calculation
head( attr(loan.knn.3.preds, "nn.dist") )
first.point.first.neighbor.distance <- attr(loan.knn.3.preds, "nn.dist")[1,1]
first.point.first.neighbor.distance

validation.data.norm[1,selected.vars]
first.point.neighbors[1,selected.vars]

# compute the distance manually and compare
variable.distances <- validation.data.norm[1,selected.vars] - first.point.neighbors[1,selected.vars]
variable.distances
sqrt( sum(variable.distances^2) )
first.point.first.neighbor.distance

# check the actual prediction for the first validation data point
train.data.norm[first.point.neighbor.indexes,"Loan.Status"]
loan.knn.3.preds[1]


# confusion matrix for the whole set of predictions
confusionMatrix(loan.knn.3.preds, validation.data.norm$Loan.Status,
                positive="Accepts")
# note: 'positive' parameter defines which class we want to consider positive


# try a different value of 'k'

# predict Loan Status with a k-nearest neighbors model with k=5
loan.knn.5.preds <- FNN::knn(train = train.data.norm[,selected.vars],
                        test = validation.data.norm[,selected.vars],
                        cl = train.data.norm$Loan.Status,
                        k=5)

confusionMatrix(loan.knn.5.preds, validation.data.norm$Loan.Status,
                positive="Accepts")

# what if we want to tune our Sensitivity / Specificity trade-off?
# we need to add the 'prob' parameter to get probability values
loan.knn.5.preds <- FNN::knn(train = train.data.norm[,selected.vars],
                             test = validation.data.norm[,selected.vars],
                             cl = train.data.norm$Loan.Status,
                             k=5,
                             prob=TRUE)

# 'prob' = proportion of votes for the "winning" class
names( attributes(loan.knn.5.preds) )
loan.knn.5.probabilities <- attr(loan.knn.5.preds,'prob')
head(loan.knn.5.probabilities)
table(loan.knn.5.probabilities)

# convert these to probabilities of "Accepts"
loan.knn.5.prob.accepts <- ifelse(loan.knn.5.preds=="Accepts",
                                  loan.knn.5.probabilities,
                                  1-loan.knn.5.probabilities)
table(loan.knn.5.prob.accepts)

# now we can create an ROC curve
library(plotROC)
ggplot(mapping = aes(m = loan.knn.5.prob.accepts, 
                     d = validation.data$Loan.Status=="Accepts")) +
  geom_roc(n.cuts=100,labels=FALSE) + 
  style_roc(theme = theme_grey)


### try several different values of k to see how they perform ###

# this is how we would try to choose the "best" k

# arrays to hold our sensitivity and specificity for each model
sensitivity.vals <- c()
specificity.vals <- c()

# for each value of k, build a knn model, obtain validation predictions, 
# and measure the accuracy
k.vals <- c(1, 3, 5, 9, 15, 21, 25, 31, 51, 99)
for (k.val in k.vals) {
  loan.knn.preds <- FNN::knn(train = train.data.norm[,selected.vars],
                        test = validation.data.norm[,selected.vars],
                        cl = train.data.norm$Loan.Status,
                        k=k.val)
  # obtain accuracy metrics from a confusion matrix
  cf <- confusionMatrix(loan.knn.preds, validation.data.norm$Loan.Status, positive="Accepts")
  sensitivity <- cf$byClass['Sensitivity']
  specificity <- cf$byClass['Specificity']
  # save off our accuracy metrics for this model
  sensitivity.vals <- c(sensitivity.vals, sensitivity)
  specificity.vals <- c(specificity.vals, specificity)
}

# construct a dataframe to check out the results
sensitivity.df <- data.frame("k"=k.vals,
                             "metric"="Sensitivity (TPR)",
                             "value"=sensitivity.vals)
specificity.df <- data.frame("k"=k.vals,
                             "metric"="Specificity (TNR)",
                             "value"=specificity.vals)
results.df <- rbind(sensitivity.df, specificity.df)
head(results.df)
tail(results.df)

# plot sensitivity and specificity vs k
ggplot(results.df) + 
  geom_point(mapping = aes(x=k, y=value, col=metric)) + 
  ylim(c(0,1)) + 
  scale_x_log10()
# why does Sensitivity (aka True Positive Rat aka Recall) drop off 
# while Specificity (True Negative Rate) remains high?

balanced.accuracy <- (sensitivity.vals + specificity.vals) / 2

# plot balanced accuracy vs k
ggplot() + 
  geom_point(mapping = aes(x=k.vals, y=balanced.accuracy)) + 
  ylim(c(0.5,1)) + 
  scale_x_log10()

# which model performed best in terms of balanced accuracy?
best.index <- which.max(balanced.accuracy)
best.index
k.vals[best.index]

# test the best model against the actual test set
loan.knn.preds.test <- FNN::knn(train = train.data.norm[,selected.vars],
                      test = test.data.norm[,selected.vars],
                      cl = train.data.norm$Loan.Status,
                      k=k.vals[best.index])

# our unbiased estimate of actual model performance
confusionMatrix(loan.knn.preds.test, test.data.norm$Loan.Status,
                positive = "Accepts")

# would 'Zip Code' be a good predictor to add?

View(train.data)

