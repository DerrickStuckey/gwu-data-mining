# install.packages("caret")
# install.packages("gains")
# install.packages("e1071")
# install.packages("plotROC")

# for naiveBayes() function
library(e1071)

# for lift charts
library(gains)

# for confusionMatrix
library(caret)

# for geom_roc() option in ggplot
library(plotROC)

library(tidyverse)

# from "Data Mining for Business Analytics"
# https://www.dataminingbook.com/book/r-edition
bankdata <- read_csv("./data/UniversalBank.csv")
bankdata
# View(bankdata)

# check column types
table(bankdata$`Securities Account`)
is.logical(bankdata$`Securities Account`)

# re-import and specify type for each column
bankdata <- read_csv("./data/UniversalBank.csv",
                     col_types = cols(
                       ID = col_double(),
                       Age = col_double(),
                       Experience = col_double(),
                       Income = col_double(),
                       `ZIP Code` = col_factor(),
                       Family = col_factor(),
                       CCAvg = col_double(),
                       Education = col_factor(),
                       Mortgage = col_double(),
                       `Personal Loan` = col_logical(),
                       `Securities Account` = col_logical(),
                       `CD Account` = col_logical(),
                       Online = col_logical(),
                       CreditCard = col_logical()
                     ))
# View(bankdata)
table(bankdata$`Securities Account`)
is.logical(bankdata$`Securities Account`)

# construct a factor version of the target variable
# the e1071 version of Naive Bayes requires the target variable to be a factor
# https://stackoverflow.com/questions/10942003/predict-returns-nothing-for-type-class-works-fine-with-type-raw
# and results will be easier to read if we use clear labels e.g. "Accepts", "Rejects"
table(bankdata$`Personal Loan`)
bankdata$Loan.Status <- "Rejects"
bankdata$Loan.Status[bankdata$`Personal Loan`] <- "Accepts"
bankdata$Loan.Status <- factor(bankdata$Loan.Status)
table(bankdata$Loan.Status)

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

# chech the sizes of each split
dim(train.data)
dim(test.data)
dim(validation.data)

# explore some basic relationships
table(train.data$`CD Account`,train.data$Loan.Status)
table(train.data$`Securities Account`,train.data$Loan.Status)
table(train.data$CreditCard,train.data$Loan.Status)
cor(train.data$`CD Account`,train.data$Loan.Status=="Accepts")
cor(train.data$`Securities Account`,train.data$Loan.Status=="Accepts")
cor(train.data$CreditCard,train.data$Loan.Status=="Accepts")


## first model

# train a naive bayes model using just a single variable
bd.nb.1 <- naiveBayes(Loan.Status ~ `CD Account`,
                    data=train.data)
bd.nb.1

# prob(Accepts | CD Account ) = prob(Accepts) * prob(CD Account | Accepts) / prob(CD Account)
p.accepts <- mean(train.data$Loan.Status=="Accepts")
p.rejects <- 1 - p.accepts
p.cd.if.accepts <- train.data$`CD Account`[train.data$Loan.Status=="Accepts"] %>% mean()
p.cd.if.rejects <- train.data$`CD Account`[train.data$Loan.Status=="Rejects"] %>% mean()
p.accepts.if.cd <- p.accepts * p.cd.if.accepts / 
  (p.accepts * p.cd.if.accepts + p.rejects * p.cd.if.rejects)

# equivalently in the 1-predictor case:
  p.cd <- mean(train.data$`CD Account`)
  p.accepts.if.cd <- p.accepts * p.cd.if.accepts / p.cd

p.accepts.if.cd

# prob(Accepts | No CD Account ) = prob(Accepts) * prob(No CD Account | Accepts) / prob(No CD Account)
p.no.cd <- 1 - p.cd
p.no.cd.if.accepts <- 1 - p.cd.if.accepts
p.accepts.if.no.cd <- p.accepts * p.no.cd.if.accepts / p.no.cd
p.accepts.if.no.cd


# look at predictions this model makes for a few examples
example.data <- data.frame('CD Account'=c(TRUE,TRUE,FALSE,FALSE),
                           'Securities Account'=c(TRUE,FALSE,TRUE,FALSE))
names(example.data) <- c('CD Account','Securities Account') # fix name formatting
example.data$probs.bd.nb.1 <- predict(bd.nb.1, newdata = example.data, type = "raw")
head(example.data)

# compare with manually computed probabilities
p.accepts.if.cd
p.accepts.if.no.cd


### next model

# add another variable
bd.nb.2 <- naiveBayes(Loan.Status ~ `CD Account` + `Securities Account`,
                    data=train.data)
bd.nb.2

# manually verify the conditional probabilities
train.data %>%
  group_by(Loan.Status) %>%
  summarise(
    cd.account.prob = mean(`CD Account`),
    securities.prob = mean(`Securities Account`)
  )

# manually compute predictions some example cases
p.securities.if.accepts <- 
  train.data$`Securities Account`[train.data$Loan.Status=="Accepts"] %>% mean()
p.securities.if.rejects <- 
  train.data$`Securities Account`[train.data$Loan.Status=="Rejects"] %>% mean()
# p.securities <- p.accepts * p.securities.if.accepts + (1-p.accepts) * p.securities.if.rejects

# probability of "Accepts" if CD Account AND Securities Account
p.accepts.if.cd.and.securities <- (p.accepts * p.securities.if.accepts * p.cd.if.accepts) /
  (p.accepts * p.securities.if.accepts * p.cd.if.accepts + 
     p.rejects * p.securities.if.rejects * p.cd.if.rejects)
p.accepts.if.cd.and.securities

# probability of "Accepts" if No CD Account and Securities Account
p.no.cd.if.rejects <- 1 - p.cd.if.rejects
p.accepts.if.nocd.and.securities <- (p.accepts * p.securities.if.accepts * p.no.cd.if.accepts) /
  (p.accepts * p.securities.if.accepts * p.no.cd.if.accepts + 
     p.rejects * p.securities.if.rejects * p.no.cd.if.rejects)
p.accepts.if.nocd.and.securities

# look at predictions this model makes for a few examples
example.data <- data.frame('CD Account'=c(TRUE,TRUE,FALSE,FALSE),
                           'Securities Account'=c(TRUE,FALSE,TRUE,FALSE))
names(example.data) <- c('CD Account','Securities Account') # fix name formatting
example.data$probs.bd.nb.2 <- predict(bd.nb.2, newdata = example.data, type = "raw")
head(example.data)

# compare with manually computed probabilities
p.accepts.if.cd.and.securities
p.accepts.if.nocd.and.securities
# etc...


## validation predictions

# predicted probability of each class
val.probs.nb.2 <- predict(bd.nb.2, newdata = validation.data, type = "raw")
head(val.probs.nb.2)
summary(val.probs.nb.2)
val.probs.nb.2[,1] %>% round(3) %>% table()
# only 4 possible values since we have only 2 binary predictors

# force the model to actually choose a class
val.preds.nb.2 <- predict(bd.nb.2, newdata = validation.data, type = "class")
# note: this throws an error if validation.data has a column for probability predictions
# but can be avoided by either a) keeping the probability predictions as a separate vector
# or b) removing the probabilities column from validation.data before passing it as newdata, e.g.
# newdata = select(validation.data,-probs.nb.2)
head(val.preds.nb.2)
summary(val.preds.nb.2)

# look at a confusion matrix (confusionMatrix() function from the 'caret' library)
confusionMatrix(val.preds.nb.2, validation.data$Loan.Status)

#             Reference
# Prediction Accepts Rejects
# Accepts      11      25
# Rejects      80     884

# Recall aka "Sensitivity": accuracy when the true value is "Accepts"
# Recall = True Positives / (True Positives + False Negatives)
11 / (11 + 80)

# Specificity: accuracy when the true value is "Rejects"
# Specificity = True Negatives / (True Negatives + False Positives)
884 / (884 + 25)

# Precision aka "Positive Predictive Value": Accuracy when predicting is "Accepts"
# Precision = True Positives / (True Positives + False Positives)
11 / (11 + 25)

# Balanced Accuracy: average of Sensitivity (Recall) and Specificity
# Balanced Accuracy = accuracy if each case were equally common
(0.1208791 + 0.9724972) / 2


# use a different threshold for predicting likely loan 
# we have to format these as factors or confusionMatrix() will throw an error
val.preds.nb.2.b <- ifelse(val.probs.nb.2[,1] > 0.25,
                                    "Accepts","Rejects")
val.preds.nb.2.b <- factor(val.preds.nb.2.b)
summary(val.preds.nb.2.b)

# original
          Reference
Prediction Accepts Rejects
Accepts      11      25
Rejects      80     884

# new cutoff
            Reference
Prediction Accepts Rejects
Accepts      27      37
Rejects      64     872

confusionMatrix(val.preds.nb.2.b, validation.data$Loan.Status)
# Sensitivity : 0.1209          
# Specificity : 0.9725     
# Pos Pred Value : 0.3056 
confusionMatrix(val.preds.nb.2.b, validation.data$Loan.Status)
# Sensitivity : 0.2967         
# Specificity : 0.9593         
# Pos Pred Value : 0.4219      
# how do Sensitivity, Specificity change?


# plot an ROC chart with geom_roc() from 'plotRoc' library
# library(plotROC)
ggplot(mapping = aes(m = val.probs.nb.2[,1], 
                     d = validation.data$Loan.Status=="Accepts")) + 
  geom_roc(n.cuts=5,labels=FALSE) + 
  style_roc(theme = theme_grey) + 
  ggtitle("Model #2 Validation ROC") + 
  theme(plot.title = element_text(hjust = 0.5))

table(val.probs.nb.2[,1])
# n.cuts = 5 because we have 4 distinct values for probability, 
# giving us 5 places to choose a cutoff:
# less than prob #1, between prob #1 and #2, ..., greater than prob #4

## plot a lift curve using 'gains' library
gain <- gains(ifelse(validation.data$Loan.Status=="Accepts",1,0), 
              val.probs.nb.2[,1],
              groups=4)
# groups=4 because we only have 4 possible predicted values
# (4 total combinations of 2 binary variables)

# set up lift chart variables
total.accepted <- sum(validation.data$Loan.Status=="Accepts")
yvals.2 <- c(0,gain$cume.pct.of.total*total.accepted)
xvals.2 <- c(0,gain$cume.obs)

# plot the lift chart
ggplot() + 
  geom_line(mapping = aes(x=xvals.2, y=yvals.2)) +
  xlab("Offers Made") + ylab("Number Accepted") + 
  ggtitle("Model #2 Validation Lift") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_abline(intercept = c(0,0), 
              slope=total.accepted/nrow(validation.data),
              linetype="dashed")



## Model 3

# look at some numeric variables that could be added
boxplot(train.data$Age ~ train.data$Loan.Status)
boxplot(train.data$Experience ~ train.data$Loan.Status)
boxplot(train.data$Income ~ train.data$Loan.Status)

# convert 'Income' to a categorical variable
summary(train.data$Income)
hist(train.data$Income)
train.data$Income.Level <- cut(train.data$Income,breaks=4)
table(train.data$Income.Level)

val.income.cuts <- cut(validation.data$Income,breaks=4)
table(val.income.cuts)

# choose our breaks explicitly so they can be applied to training, validation, and test sets
income.breaks <- c(0,50,100,150,Inf)
# use cut() function to convert Income to a factor using the chosen breaks
train.data$Income.Level <- cut(train.data$Income, breaks=income.breaks)
validation.data$Income.Level <- cut(validation.data$Income,breaks=income.breaks)
test.data$Income.Level <- cut(test.data$Income,breaks=income.breaks)
table(train.data$Income.Level)
is.factor(train.data$Income.Level)
table(validation.data$Income.Level)

# add 'Income Level' as a predictor
bd.nb.3 <- naiveBayes(Loan.Status ~ `Securities Account` + `CD Account` + Income.Level,
                    data=train.data)
bd.nb.3

# predicted probability of each class using the updated model
val.probs.3 <- predict(bd.nb.3, newdata = validation.data, type = "raw")
head(val.probs.3)
summary(val.probs.3)
table(val.probs.3[,1])

# force the model to actually choose a class
val.preds.3 <- predict(bd.nb.3, newdata = validation.data, type = "class")
head(val.preds.3)
summary(val.preds.3)

# look at a confusion matrix for the updated model
confusionMatrix(val.preds.3, validation.data$Loan.Status)

# plot a gain chart for the updated model
gain <- gains(ifelse(validation.data$Loan.Status=="Accepts",1,0), 
              val.probs.3[,1],
              groups=16)

total.accepted <- sum(validation.data$Loan.Status=="Accepts")
yvals.3 <- c(0,gain$cume.pct.of.total*total.accepted)
xvals.3 <- c(0,gain$cume.obs)
ggplot() + 
  geom_line(mapping = aes(x=xvals.3, y=yvals.3)) +
  xlab("Offers Made") + ylab("Number Accepted") + 
  ggtitle("Model #3 Validation") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_abline(intercept = c(0,0), 
              slope=total.accepted/nrow(validation.data),
              linetype="dashed")

## Model 4

# try a model with all available categorical variables (and Income)
bd.nb.4 <- naiveBayes(Loan.Status ~ `Securities Account` + `CD Account` + Income.Level + Education + Family + 
                        CreditCard + Online,
                      data=train.data)
bd.nb.4

val.preds.4 <- predict(bd.nb.4, newdata = validation.data, type = "class")
head(val.preds.4)
summary(val.preds.4)

# look at a confusion matrix for the updated model
confusionMatrix(val.preds.4, validation.data$Loan.Status)

# plot a gain chart for the updated model
val.probs.4 <- predict(bd.nb.4, newdata = validation.data, type = "raw")
gain <- gains(ifelse(validation.data$Loan.Status=="Accepts",1,0), 
              val.probs.4[,1],
              groups=100)

total.accepted <- sum(validation.data$Loan.Status=="Accepts")
yvals.4 <- c(0,gain$cume.pct.of.total*total.accepted)
xvals.4 <- c(0,gain$cume.obs)
ggplot() + 
  geom_line(mapping = aes(x=xvals.4, y=yvals.4)) +
  xlab("Offers Made") + ylab("Number Accepted") + 
  ggtitle("Model #4 Validation") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_abline(intercept = c(0,0), 
              slope=total.accepted/nrow(validation.data),
              linetype="dashed")


## Model 5

# What about Zip Code?
summary(train.data$`ZIP Code`)
bd.nb.5 <- naiveBayes(Loan.Status ~ `Securities Account` + `CD Account` + Income.Level + Education + Family + 
                        CreditCard + Online + `ZIP Code`,
                      data=train.data)
bd.nb.5

val.preds.5 <- predict(bd.nb.5, newdata = validation.data, type = "class")
head(val.preds.5)
summary(val.preds.5)

# look at a confusion matrix for the updated model
confusionMatrix(val.preds.5, validation.data$Loan.Status)

# plot a gain chart for the updated model
validation.probabilities.5 <- predict(bd.nb.5, newdata = validation.data, type = "raw")
gain <- gains(ifelse(validation.data$Loan.Status=="Accepts",1,0), 
              validation.probabilities.5[,1],
              groups=100)

total.accepted <- sum(validation.data$Loan.Status=="Accepts")
yvals.5 <- c(0,gain$cume.pct.of.total*total.accepted)
xvals.5 <- c(0,gain$cume.obs)
ggplot() + 
  geom_line(mapping = aes(x=xvals.5, y=yvals.5)) +
  xlab("Offers Made") + ylab("Number Accepted") + 
  ggtitle("Model #5 Validation") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_abline(intercept = c(0,0), 
              slope=total.accepted/nrow(validation.data),
              linetype="dashed")
# how does model #5 look compared to Model #4?


### Evaluate Against the Final Test Set ###

# Which model do you think will perform best against the final test set?
test.preds.2 <- predict(bd.nb.2, newdata = test.data, type = "class")
test.preds.3 <- predict(bd.nb.3, newdata = test.data, type = "class")
test.preds.4 <- predict(bd.nb.4, newdata = test.data, type = "class")
test.preds.5 <- predict(bd.nb.5, newdata = test.data, type = "class")

# look at a confusion matrix of test results for each model
confusionMatrix(test.preds.2, test.data$Loan.Status)
confusionMatrix(test.preds.3, test.data$Loan.Status)
confusionMatrix(test.preds.4, test.data$Loan.Status)
confusionMatrix(test.preds.5, test.data$Loan.Status)

# plot lift for each model
total.accepted <- sum(test.data$Loan.Status=="Accepts")

test.probs.2 <- predict(bd.nb.2, newdata = test.data, type = "raw")
test.gain.2 <- gains(ifelse(test.data$Loan.Status=="Accepts",1,0), 
                     test.probs.2[,1],
                     groups=100)

test.xvals.2 <- c(0,test.gain.2$cume.obs)
test.yvals.2 <- c(0,test.gain.2$cume.pct.of.total*total.accepted)

test.probs.3 <- predict(bd.nb.3, newdata = test.data, type = "raw")
test.gain.3 <- gains(ifelse(test.data$Loan.Status=="Accepts",1,0), 
                     test.probs.3[,1],
                     groups=100)

test.xvals.3 <- c(0,test.gain.3$cume.obs)
test.yvals.3 <- c(0,test.gain.3$cume.pct.of.total*total.accepted)

test.probs.4 <- predict(bd.nb.4, newdata = test.data, type = "raw")
test.gain.4 <- gains(ifelse(test.data$Loan.Status=="Accepts",1,0), 
                     test.probs.4[,1],
                     groups=100)

test.xvals.4 <- c(0,test.gain.4$cume.obs)
test.yvals.4 <- c(0,test.gain.4$cume.pct.of.total*total.accepted)

test.probs.5 <- predict(bd.nb.5, newdata = test.data, type = "raw")
test.gain.5 <- gains(ifelse(test.data$Loan.Status=="Accepts",1,0), 
                     test.probs.5[,1],
                     groups=100)
test.xvals.5 <- c(0,test.gain.5$cume.obs)
test.yvals.5 <- c(0,test.gain.5$cume.pct.of.total*total.accepted)

# create a dataframe with all the gain values together
test.gain.2 <- data.frame("offers"=test.xvals.2,
                          "acceptances"=test.yvals.2,
                          "model"="Model 2")
test.gain.3 <- data.frame("offers"=test.xvals.3,
                          "acceptances"=test.yvals.3,
                          "model"="Model 3")
test.gain.4 <- data.frame("offers"=test.xvals.4,
                          "acceptances"=test.yvals.4,
                          "model"="Model 4")
test.gain.5 <- data.frame("offers"=test.xvals.5,
                          "acceptances"=test.yvals.5,
                          "model"="Model 5")
test.results.combined <- rbind(test.gain.2, test.gain.3, test.gain.4, test.gain.5)

ggplot(data=test.results.combined) + 
  geom_line(mapping = aes(x=offers, y=acceptances, color=model)) +
  xlab("Offers Made") + ylab("Number Accepted") + 
  ggtitle("Model Comparison") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_abline(intercept = c(0,0), 
              slope=total.accepted/nrow(validation.data),
              linetype="dashed")


