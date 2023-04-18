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


# training/validation split
set.seed(12345)
train.proportion <- 0.7
validation.proportion <- 0.3

# pull out the training data
train.index <- sample(1:nrow(bankdata), nrow(bankdata)*train.proportion)
train.data <- bankdata[train.index,]

validation.data <- bankdata[-train.index,]

# check sizes
dim(train.data)
dim(validation.data)

## base model (a few numerical predictors only)

# train logistic regression model
lm.base <- glm(`Personal Loan` ~ Age + Income + Experience + Education, 
               data=train.data,
               family=binomial)
summary(lm.base)

# evaluate against train data
train.probs.base <- predict(lm.base, newdata = train.data,
                            type = "response")
train.preds.base <- train.probs.base>0.5
confusionMatrix(
  factor(train.preds.base), 
  factor(train.data$`Personal Loan`)
)
# Balanced Accuracy : 0.7671

roc.obj <- roc(train.data$`Personal Loan`,
               predictor = train.probs.base)
base.auc.train <- auc(roc.obj)
base.auc.train
# Area under the curve: 0.9301

# evaluate against validation data
validation.probs.base <- predict(lm.base, newdata = validation.data,
                                 type = "response")
validation.preds.base <- validation.probs.base>0.5
confusionMatrix(
  factor(validation.preds.base), 
  factor(validation.data$`Personal Loan`)
)
# Balanced Accuracy : 0.7456

# plot ROC curve
ggplot(mapping = aes(m = validation.probs.base, 
                     d = validation.data$`Personal Loan`)) +
  geom_roc(n.cuts=0,labels=FALSE) + 
  style_roc(theme = theme_grey) + ggtitle("Base Model")

# Measure area under the ROC curve
roc.obj <- roc(validation.data$`Personal Loan`,
               predictor = validation.probs.base)
auc(roc.obj)
base.auc.validation <- auc(roc.obj)
base.auc.validation
# Area under the curve: 0.9425

## include raw zip code (as a factor)

# train logistic regression model including zip code
lm.zip <- glm(`Personal Loan` ~ Age + Income + Experience + Education + `ZIP Code`, 
               data=train.data,
               family=binomial)
summary(lm.zip)

# evaluate full zip model against train data
train.probs.zip <- predict(lm.zip, newdata = train.data,
                           type = "response")
train.preds.zip <- train.probs.zip>0.5
confusionMatrix(
  factor(train.preds.zip), 
  factor(train.data$`Personal Loan`)
)
# Balanced Accuracy: 0.8381

roc.obj <- roc(train.data$`Personal Loan`,
               predictor = train.probs.zip)
full.zip.auc.train <- auc(roc.obj)
full.zip.auc.train
# Area under the curve: 0.972

# evaluate full zip model against validation data
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
    c(90505, 90011, 90044, 94019, 95037, 94526, 94965, 90280, 94970, 
      90813, 94404, 94087)] <- mode.zip
# can you think of a better way to replace these values?

# try obtaining predictions against validation data again
validation.probs.zip <- predict(lm.zip, newdata = validation.data.imputed,
                                type = "response")
validation.preds.zip <- validation.probs.zip>0.5
confusionMatrix(
  factor(validation.preds.zip), 
  factor(validation.data.imputed$`Personal Loan`)
)
# Balanced Accuracy : 0.6541 

# plot ROC curve
ggplot(mapping = aes(m = validation.probs.zip, 
                     d = validation.data$`Personal Loan`)) +
  geom_roc(n.cuts=0,labels=FALSE) + 
  style_roc(theme = theme_grey) + ggtitle("With Zip Code")

# area under the curve
roc.obj <- roc(validation.data.imputed$`Personal Loan`,
               predictor = validation.probs.zip)
full.zip.auc.validation <- auc(roc.obj)
full.zip.auc.validation
# Area under the curve: 0.7128


## transform zip code to zip 3
train.data$zip3 <- train.data$`ZIP Code` %>% as.character() %>% substr(1,3)
head(train.data$`ZIP Code`)
head(train.data$zip3)
table(train.data$`ZIP Code`)
table(train.data$zip3)
train.data$`ZIP Code` %>% unique() %>% length()
train.data$zip3 %>% unique() %>% length()

# repeat for validation data
validation.data$zip3 <- validation.data$`ZIP Code` %>% as.character() %>% substr(1,3)

# train logistic regression model with zip3
lm.zip3 <- glm(`Personal Loan` ~ Age + Income + Experience + Education + zip3, 
              data=train.data,
              family=binomial)
summary(lm.zip3)

## Zip-3 model vs Train
train.probs.zip3 <- predict(lm.zip3, newdata = train.data,
                            type = "response")
train.preds.zip3 <- train.probs.zip3>0.5
confusionMatrix(
  factor(train.preds.zip3), 
  factor(train.data$`Personal Loan`)
)
# Balanced Accuracy: 0.7790

roc.obj <- roc(train.data$`Personal Loan`,
               predictor = train.probs.zip3)
zip3.auc.train <- auc(roc.obj)
zip3.auc.train
# Area under the curve: 0.9391

# evaluate against validation data
validation.probs.zip3 <- predict(lm.zip3, newdata = validation.data,
                                type = "response")
validation.preds.zip3 <- validation.probs.zip3>0.5
confusionMatrix(
  factor(validation.preds.zip3), 
  factor(validation.data.imputed$`Personal Loan`)
)
# Balanced Accuracy : 0.7390

roc.obj <- roc(validation.data.imputed$`Personal Loan`,
               predictor = validation.probs.zip3)
zip3.auc.validation <- auc(roc.obj)
zip3.auc.validation
# Area under the curve: 0.9053

# plot ROC curve
ggplot(mapping = aes(m = validation.probs.zip3, 
                     d = validation.data$`Personal Loan`)) +
  geom_roc(n.cuts=0,labels=FALSE) + 
  style_roc(theme = theme_grey) + ggtitle("Zip-3 Model")

## try zip-2 instead
## transform zip code to zip 2
train.data$zip2 <- train.data$`ZIP Code` %>% as.character() %>% substr(1,2)
validation.data$zip2 <- validation.data$`ZIP Code` %>% as.character() %>% substr(1,2)

train.data$`ZIP Code` %>% unique() %>% length()
train.data$zip2 %>% unique() %>% length()

# train logistic regression model with zip2
lm.zip2 <- glm(`Personal Loan` ~ Age + Income + Experience + Education + zip2, 
               data=train.data,
               family=binomial)
summary(lm.zip2)

# evaluate zip-2 model against train data
train.probs.zip2 <- predict(lm.zip2, newdata = train.data,
                            type = "response")
train.preds.zip2 <- train.probs.zip2>0.5
confusionMatrix(
  factor(train.preds.zip2), 
  factor(train.data$`Personal Loan`)
)
# Balanced Accuracy: 0.7669

roc.obj <- roc(train.data$`Personal Loan`,
               predictor = train.probs.zip2)
zip2.auc.train <- auc(roc.obj)
zip2.auc.train
# Area under the curve: 0.9307

# evaluate zip-2 model against validation data
validation.probs.zip2 <- predict(lm.zip2, newdata = validation.data,
                                 type = "response")
validation.preds.zip2 <- validation.probs.zip2>0.5
confusionMatrix(
  factor(validation.preds.zip2), 
  factor(validation.data$`Personal Loan`)
)
# Balanced Accuracy : 0.7477

roc.obj <- roc(validation.data$`Personal Loan`,
               predictor = validation.probs.zip2)

zip2.auc.validation <- auc(roc.obj)
zip2.auc.validation
# Area under the curve: 0.9398

# plot ROC curve
ggplot(mapping = aes(m = validation.probs.zip2, 
                     d = validation.data$`Personal Loan`)) +
  geom_roc(n.cuts=0,labels=FALSE) + 
  style_roc(theme = theme_grey) + ggtitle("Zip-2 Model")


## try mapping zip code to state instead


# write out full results to a .csv
auc.df <- data.frame("Model"=c("Base Model","Zip-2","Zip-3","Full Zip"),
                     "Train AUC"=c(base.auc.train, zip2.auc.train,
                                   zip3.auc.train, full.zip.auc.train),
                     "Validation AUC"=c(base.auc.validation, zip2.auc.validation,
                                        zip3.auc.validation, full.zip.auc.validation))

write.csv(auc.df,"./dimensionality_reduction/bankloan_results.csv",
          row.names=FALSE)






