# install.packages("caret")

library(caret)
library(tidyverse)
library(plotROC)
library(pROC)

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

# plot ROC curve
ggplot(mapping = aes(m = validation.probs.base, 
                     d = validation.data$`Personal Loan`)) +
  geom_roc(n.cuts=0,labels=FALSE) + 
  style_roc(theme = theme_grey)

# Measure area under the ROC curve
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

# impute new levels as mode (most common zip) from training data
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
validation.data.imputed <- validation.data
mode.zip <- getmode(train.data$`ZIP Code`)
mode.zip
validation.data.imputed$`ZIP Code`[
  validation.data.imputed$`ZIP Code` %in% 
    c(94509, 95678, 92116, 94087, 90639, 94302)] <- mode.zip
# can you think of a better way to replace these values?

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
train.data$zip3 <- train.data$`ZIP Code` %>% as.character() %>% substr(1,3)
head(train.data$zip3)
table(train.data$`ZIP Code`)
table(train.data$zip3)
train.data$`ZIP Code` %>% unique() %>% length()
train.data$zip3 %>% unique() %>% length()

# repeat for validation and test data
validation.data$zip3 <- validation.data$`ZIP Code` %>% as.character() %>% substr(1,3)
test.data$zip3 <- test.data$`ZIP Code` %>% as.character() %>% substr(1,3)

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
## transform zip code to zip 2
train.data$zip2 <- train.data$`ZIP Code` %>% as.character() %>% substr(1,2)
validation.data$zip2 <- validation.data$`ZIP Code` %>% as.character() %>% substr(1,2)
test.data$zip2 <- test.data$`ZIP Code` %>% as.character() %>% substr(1,2)

# train logistic regression model with zip2
lm.zip2 <- glm(`Personal Loan` ~ Age + Income + Experience + Education + zip2, 
               data=train.data,
               family=binomial)
summary(lm.zip2)

# evaluate against validation data
validation.probs.zip2 <- predict(lm.zip2, newdata = validation.data,
                                 type = "response")
validation.preds.zip2 <- validation.probs.zip2>0.5
confusionMatrix(
  factor(validation.preds.zip2), 
  factor(validation.data$`Personal Loan`)
)
# Balanced Accuracy : 0.8199

roc.obj <- roc(validation.data$`Personal Loan`,
               predictor = validation.probs.zip2)
auc(roc.obj)
# Area under the curve: 0.942



## try mapping zip code to state instead







## Performance against Test

## Base model vs Test
# evaluate against test data
test.probs.base <- predict(lm.base, newdata = test.data,
                           type = "response")
test.preds.base <- test.probs.base>0.5
confusionMatrix(
  factor(test.preds.base), 
  factor(test.data$`Personal Loan`)
)
# Balanced Accuracy : 0.7695

roc.obj <- roc(test.data$`Personal Loan`,
               predictor = test.probs.base)
auc(roc.obj)
# Area under the curve: 0.9391

## Zip-2 model vs Test
test.probs.zip2 <- predict(lm.zip2, newdata = test.data,
                           type = "response")
test.preds.zip2 <- test.probs.zip2>0.5
confusionMatrix(
  factor(test.preds.zip2), 
  factor(test.data$`Personal Loan`)
)
# Balanced Accuracy : 0.7640

roc.obj <- roc(test.data$`Personal Loan`,
               predictor = test.probs.zip2)
auc(roc.obj)
# Area under the curve: 0.9378

## Zip-3 model vs Test
test.probs.zip3 <- predict(lm.zip3, newdata = test.data,
                           type = "response")
test.preds.zip3 <- test.probs.zip3>0.5
confusionMatrix(
  factor(test.preds.zip3), 
  factor(test.data$`Personal Loan`)
)
# Balanced Accuracy : 0.7575

roc.obj <- roc(test.data$`Personal Loan`,
               predictor = test.probs.zip3)
auc(roc.obj)
# Area under the curve: 0.9186

## Full zip code model vs test
# impute new levels as mode (most common zip) from training data
test.data.imputed <- test.data
mode.zip <- getmode(train.data$`ZIP Code`)
mode.zip
test.data.imputed$`ZIP Code`[
  test.data.imputed$`ZIP Code` %in% 
    c(90745, 91941, 94509, 95678, 92116, 90016, 90639, 94302, 90280, 96145, 94598)] <- mode.zip

# try obtaining predictions against test data again
test.probs.zip <- predict(lm.zip, newdata = test.data.imputed,
                          type = "response")
test.preds.zip <- test.probs.zip>0.5
confusionMatrix(
  factor(test.preds.zip), 
  factor(test.data.imputed$`Personal Loan`)
)
# Balanced Accuracy : 0.6998 

roc.obj <- roc(test.data.imputed$`Personal Loan`,
               predictor = test.probs.zip)
auc(roc.obj)
# Area under the curve: 0.737

