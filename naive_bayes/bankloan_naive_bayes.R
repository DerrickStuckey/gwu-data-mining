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
                       `ZIP Code` = col_double(),
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

validation.predictions.2 <- ifelse(validation.probabilities.1[,1] > 0.25,
                                   "Accepts","Rejects")
validation.predictions.2 <- factor(validation.predictions.2)
summary(validation.predictions.2)

confusionMatrix(validation.predictions.2, validation.data$Loan.Status)


# TODO add more variables by converting some numeric variables to factors


# plot a lift curve (using 'gains' library)
gain <- gains(ifelse(validation.data$Loan.Status=="Accepts",1,0), 
              validation.probabilities.1[,1],
              groups=4)

# set up lift chart variables
total.accepted <- sum(validation.data$Loan.Status=="Accepts")
yvals <- c(0,gain$cume.pct.of.total*total.accepted)
xvals <- c(0,gain$cume.obs)
plot(xvals, yvals, type="l", xlab="Offers Made", ylab="Number Accepted",
     main="")
lines(x=c(0,nrow(validation.data)), y=c(0,total.accepted), lty=2)
# TODO convert to a ggplot

