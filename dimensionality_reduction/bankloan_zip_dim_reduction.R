# install.packages("caret")
# install.packages("gains")

library(caret)
library(tidyverse)
library(gains) # for lift chart

# from "Data Mining for Business Analytics"
# https://www.dataminingbook.com/book/r-edition
bankdata <- read_csv("./data/UniversalBank.csv")
bankdata
# View(bankdata)

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
table(bankdata$Loan.Status)

# training/test/validation split
set.seed(123456)
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

## base model (a few numerical predictors only)

# train logistic regression model
lm.base <- glm(`Personal Loan` ~ Age + Income + Experience + Education, 
               data=train.data,
               family=binomial)
summary(lm.base)

# evaluate against validation data
validation.probs.base <- predict(lm.base, newdata = validation.data,
                                 type = "response")
validation.preds.base <- validation.probs.base>0.5
confusionMatrix(
  factor(validation.preds.base), 
  factor(validation.data$`Personal Loan`)
)
# Balanced Accuracy : 0.8135

library(pROC)
roc.obj <- roc(validation.data$`Personal Loan`,
               predictor = validation.probs.base)
auc(roc.obj)
# Area under the curve: 0.9425

## include raw zip code (as a factor)

# train logistic regression model including zip code
lm.zip <- glm(`Personal Loan` ~ Age + Income + Experience + Education + `ZIP Code`, 
               data=train.data,
               family=binomial)
summary(lm.zip)

# evaluate against validation data
validation.probs.zip <- predict(lm.zip, newdata = validation.data,
                                 type = "response")

# impute new levels as mode from training data
c <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
validation.data.imputed <- validation.data
mode.zip <- getmode(train.data$`ZIP Code`)
mode.zip
# validation.data.imputed$`ZIP Code`[validation.data.imputed$`ZIP Code` %in% c(94509, 95678, 92116, 94087, 90639, 94302)] <- mode.zip
validation.data.imputed %>% filter(
  `ZIP Code` %in% c(94509, 95678, 92116, 94087, 90639, 94302)
) %>% mutate(
  `ZIP Code` = mode.zip
)

# try obtaining predictions against validation data again
validation.probs.zip <- predict(lm.zip, newdata = validation.data.imputed,
                                type = "response")
validation.preds.zip <- validation.probs.zip>0.5
confusionMatrix(
  factor(validation.preds.zip), 
  factor(validation.data.imputed$`Personal Loan`)
)
# Balanced Accuracy : 0.7155 

roc.obj <- roc(validation.data.imputed$`Personal Loan`,
               predictor = validation.probs.zip)
auc(roc.obj)
# Area under the curve: 0.7451


## transform zip code to zip 3
bankdata$zip3 <- bankdata$`ZIP Code` %>% as.character() %>% substr(1,3)
head(bankdata$zip3)
table(bankdata$zip3)
table(bankdata$`ZIP Code`)
bankdata$zip3 %>% unique() %>% length()
bankdata$`ZIP Code` %>% unique() %>% length()

# redo training/test/validation splits
# pull out the training data
train.data <- bankdata[train.index,]
holdout.data <- bankdata[-train.index,]
test.data <- holdout.data[test.index,]
validation.data <- holdout.data[-test.index,]

# train logistic regression model with zip3
lm.zip3 <- glm(`Personal Loan` ~ Age + Income + Experience + Education + zip3, 
              data=train.data,
              family=binomial)
summary(lm.zip3)

# evaluate against validation data
validation.probs.zip3 <- predict(lm.zip3, newdata = validation.data,
                                type = "response")
validation.preds.zip3 <- validation.probs.zip3>0.5
confusionMatrix(
  factor(validation.preds.zip3), 
  factor(validation.data.imputed$`Personal Loan`)
)
# Balanced Accuracy : 0.7926

roc.obj <- roc(validation.data.imputed$`Personal Loan`,
               predictor = validation.probs.zip3)
auc(roc.obj)
# Area under the curve: 0.8646


## try zip-2 instead

## try mapping zip code to state instead


