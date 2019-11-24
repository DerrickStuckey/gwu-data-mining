# install.packages("caret")
# install.packages("gains")
# install.packages("e1071")

library(caret)
library(gains)
library(e1071)

library(tidyverse)

# from "Data Mining for Business Analytics"
# https://www.dataminingbook.com/book/r-edition
bankdata <- read_csv("./data/UniversalBank.csv")
bankdata
View(bankdata)

# clean up some column types
# doesn't change any results, just easier to read
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
View(bankdata)
bankdata$Loan.Status <- "Rejects"
bankdata$Loan.Status[bankdata$`Personal Loan`] <- "Accepts"
# the e1071 version of Naive Bayes has problems if the target class is anything other than type 'factor'
# https://stackoverflow.com/questions/10942003/predict-returns-nothing-for-type-class-works-fine-with-type-raw
bankdata$Loan.Status <- factor(bankdata$Loan.Status)

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

# TODO add heatmap of correlations, other visualizations

# try with just a few variables
bd.nb <- naiveBayes(Loan.Status ~ `Securities Account` + `CD Account`,
                    data=train.data)

bd.nb

# manually verify the conditional probabilities
train.data %>%
  group_by(Loan.Status) %>%
  summarise(
    securities.pct = mean(`Securities Account`),
    cd.account.pct = mean(`CD Account`)
  )

# score the validation data
# predict() throws up some warnings if newdata contains extra columns, 
# so pull out just the ones that the model needs
validation.data.predictors <- validation.data %>% select(`Securities Account`, `CD Account`)

# predicted probability of each class
validation.probabilities.1 <- predict(bd.nb, newdata = validation.data.predictors, type = "raw")
head(validation.probabilities.1)
summary(validation.probabilities.1)
table(validation.probabilities.1[,1]) # only 4 possible values since we have only 2 binary predictors

# TODO manually calculate the probabilties for a few example data points

# force the model to actually choose a class
validation.predictions.1 <- predict(bd.nb, newdata = validation.data.predictors, type = "class")
head(validation.predictions.1)
summary(validation.predictions.1)

# look at a confusion matrix (from the 'caret' library)
confusionMatrix(validation.predictions.1, validation.data$Loan.Status)

#             Reference
# Prediction Accepts Rejects
# Accepts      11      25
# Rejects      80     884

# Sensitivity: accuracy when the true value is "Accepts"
11 / (11+80)

# Sensitivity: accuracy when the true value is "Rejects"
884 / (884 + 25)

# Balanced Accuracy: average of the two
# aka accuracy if each case was equally common
(0.1208791 + 0.9724972) / 2


# use a different threshold for predicting likely loan 
validation.predictions.1b <- validation.probabilities.1[,1] > 0.5
summary(validation.predictions.1b)

validation.predictions.1c <- ifelse(validation.probabilities.1[,1] > 0.25,
                                   "Accepts","Rejects")
validation.predictions.1c <- factor(validation.predictions.1c)
summary(validation.predictions.1c)

confusionMatrix(validation.predictions.1c, validation.data$Loan.Status)


# plot a lift curve (using 'gains' library)
gain <- gains(ifelse(validation.data$Loan.Status=="Accepts",1,0), 
              validation.probabilities.1[,1],
              groups=4)

# set up lift chart variables
total.accepted <- sum(validation.data$Loan.Status=="Accepts")
yvals <- c(0,gain$cume.pct.of.total*total.accepted)
xvals <- c(0,gain$cume.obs)
ggplot() + 
  geom_line(mapping = aes(x=xvals, y=yvals)) +
  xlab("Offers Made") + ylab("Number Accepted") + 
  ggtitle("Model #1 Validation") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_abline(intercept = c(0,0), 
              slope=total.accepted/nrow(validation.data),
              linetype="dashed")

# look at some numeric variables that could be added
boxplot(train.data$Age ~ train.data$Loan.Status)
boxplot(train.data$Experience ~ train.data$Loan.Status)
boxplot(train.data$Income ~ train.data$Loan.Status)

# convert 'Income' to a categorical variable
summary(train.data$Income)
hist(train.data$Income)
train.data$Income.Level <- cut(train.data$Income,breaks=4)
table(train.data$Income.Level)

# choose our breaks explicitly so they can be applied to training, validation, and test sets
income.breaks <- c(0,50,100,150,Inf)
train.data$Income.Level <- cut(train.data$Income,breaks=income.breaks)
validation.data$Income.Level <- cut(validation.data$Income,breaks=income.breaks)
test.data$Income.Level <- cut(test.data$Income,breaks=income.breaks)
table(train.data$Income.Level)
is.factor(train.data$Income.Level)

# add 'Income' and some other variables
bd.nb.2 <- naiveBayes(Loan.Status ~ `Securities Account` + `CD Account` + Income.Level,
                    data=train.data)

bd.nb.2

# predicted probability of each class using the updated model
validation.probabilities.2 <- predict(bd.nb.2, newdata = validation.data, type = "raw")
head(validation.probabilities.2)
summary(validation.probabilities.2)
table(validation.probabilities.2[,1])

# force the model to actually choose a class
validation.predictions.2 <- predict(bd.nb.2, newdata = validation.data, type = "class")
head(validation.predictions.2)
summary(validation.predictions.2)

# look at a confusion matrix for the updated model
confusionMatrix(validation.predictions.2, validation.data$Loan.Status)

# plot a gain chart for the updated model
gain <- gains(ifelse(validation.data$Loan.Status=="Accepts",1,0), 
              validation.probabilities.2[,1],
              groups=16)

total.accepted <- sum(validation.data$Loan.Status=="Accepts")
yvals <- c(0,gain$cume.pct.of.total*total.accepted)
xvals <- c(0,gain$cume.obs)
ggplot() + 
  geom_line(mapping = aes(x=xvals, y=yvals)) +
  xlab("Offers Made") + ylab("Number Accepted") + 
  ggtitle("Model #2 Validation") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_abline(intercept = c(0,0), 
              slope=total.accepted/nrow(validation.data),
              linetype="dashed")



# try a model with all available categorical variables
bd.nb.3 <- naiveBayes(Loan.Status ~ `Securities Account` + `CD Account` + Income.Level + Education + Family + 
                        CreditCard + Online,
                      data=train.data)

validation.predictions.3 <- predict(bd.nb.3, newdata = validation.data, type = "class")
head(validation.predictions.3)
summary(validation.predictions.3)

# look at a confusion matrix for the updated model
confusionMatrix(validation.predictions.3, validation.data$Loan.Status)

# plot a gain chart for the updated model
validation.probabilities.3 <- predict(bd.nb.3, newdata = validation.data, type = "raw")
gain <- gains(ifelse(validation.data$Loan.Status=="Accepts",1,0), 
              validation.probabilities.3[,1],
              groups=100)

total.accepted <- sum(validation.data$Loan.Status=="Accepts")
yvals <- c(0,gain$cume.pct.of.total*total.accepted)
xvals <- c(0,gain$cume.obs)
ggplot() + 
  geom_line(mapping = aes(x=xvals, y=yvals)) +
  xlab("Offers Made") + ylab("Number Accepted") + 
  ggtitle("Model #3 Validation") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_abline(intercept = c(0,0), 
              slope=total.accepted/nrow(validation.data),
              linetype="dashed")


# What about Zip Code?
summary(train.data$`ZIP Code`)
bd.nb.4 <- naiveBayes(Loan.Status ~ `Securities Account` + `CD Account` + Income.Level + Education + Family + 
                        CreditCard + Online + `ZIP Code`,
                      data=train.data)

bd.nb.4

validation.predictions.4 <- predict(bd.nb.4, newdata = validation.data, type = "class")
head(validation.predictions.4)
summary(validation.predictions.4)

# look at a confusion matrix for the updated model
confusionMatrix(validation.predictions.4, validation.data$Loan.Status)

# plot a gain chart for the updated model
validation.probabilities.4 <- predict(bd.nb.4, newdata = validation.data, type = "raw")
gain <- gains(ifelse(validation.data$Loan.Status=="Accepts",1,0), 
              validation.probabilities.4[,1],
              groups=100)

total.accepted <- sum(validation.data$Loan.Status=="Accepts")
yvals <- c(0,gain$cume.pct.of.total*total.accepted)
xvals <- c(0,gain$cume.obs)
ggplot() + 
  geom_line(mapping = aes(x=xvals, y=yvals)) +
  xlab("Offers Made") + ylab("Number Accepted") + 
  ggtitle("Model #4 Validation") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_abline(intercept = c(0,0), 
              slope=total.accepted/nrow(validation.data),
              linetype="dashed")

# Which model do you think will perform best against the final test set?
test.predictions.1 <- predict(bd.nb, newdata = test.data, type = "class")
test.predictions.2 <- predict(bd.nb.2, newdata = test.data, type = "class")
test.predictions.3 <- predict(bd.nb.3, newdata = test.data, type = "class")
test.predictions.4 <- predict(bd.nb.4, newdata = test.data, type = "class")

# look at a confusion matrix of test results for each model
confusionMatrix(test.predictions.1, test.data$Loan.Status)
confusionMatrix(test.predictions.2, test.data$Loan.Status)
confusionMatrix(test.predictions.3, test.data$Loan.Status)
confusionMatrix(test.predictions.4, test.data$Loan.Status)

# plot lift for each model


