# install.packages("randomForest")
library(randomForest)
library(caret)
library(gains)
library(tidyverse)

# from https://www.kaggle.com/c/titanic/data
# see link for data dictionary
trainval.data <- read.csv("./data/titanic/train.csv")
test.data <- read.csv("./data/titanic/test.csv")
dim(trainval.data)
dim(test.data)
lapply(trainval.data, class)

# make 'Survived' a factor so randomForest() will treat it as a classification problem
trainval.data$Survived <- as.factor(trainval.data$Survived)

# need to make character type predictors into
# factors for randomForest() to accept them
trainval.data$Sex <- as.factor(trainval.data$Sex)
test.data$Sex <- as.factor(test.data$Sex)

# Annoyingly, the implementation of randomForest() does not automatically handle missing
# values, as it is based on a slightly different tree implementation than rpart()

# Go ahead and impute missing values for 'Embarked' as we're just going to always use 'S'
# as it's the most common class by a large margin
table(trainval.data$Embarked)
trainval.data$Embarked.Imputed <- trainval.data$Embarked
trainval.data$Embarked.Imputed[is.na(trainval.data$Embarked.Imputed)] <- 'S'
trainval.data$Embarked.Imputed[trainval.data$Embarked.Imputed==""] <- 'S'
trainval.data$Embarked.Imputed <- factor(trainval.data$Embarked.Imputed)
table(trainval.data$Embarked.Imputed)

test.data$Embarked.Imputed <- test.data$Embarked
test.data$Embarked.Imputed[is.na(test.data$Embarked.Imputed)] <- 'S'
test.data$Embarked.Imputed[test.data$Embarked.Imputed==""] <- 'S'
test.data$Embarked.Imputed <- factor(test.data$Embarked.Imputed)
table(test.data$Embarked.Imputed)


# we will impute Age separately for each split
# this is probably overkill, but just to be safe

# partition into k 'folds' for cross-validation
set.seed(12345)
num.folds <- 5

nrow(trainval.data)

# create k fold labels
fold.labels.base <- 1:nrow(trainval.data) %% num.folds
head(fold.labels.base,10)

# randomize the fold labels
random.index <- sample(1:nrow(trainval.data), nrow(trainval.data), replace=FALSE)
head(random.index,10)
fold.labels <- fold.labels.base[random.index]
head(fold.labels,10)

table(fold.labels.base)
table(fold.labels)

# apply the fold labels to the data
trainval.data$fold <- fold.labels
table(trainval.data$fold)
head(trainval.data$fold,10)

## try different values of mtry between 1 and 7 (the total number of variables)
# mtry.vals <- c(1:7)
mtry.vals <- rep(c(1:7),5)
folds <- unique(trainval.data$fold)
folds
attempt.mtry.used <- c()
attempt.results <- c()
attempt.fold.values <- c()

# select training and validation data for each fold
for (current.fold in folds) {
  # validation data is the selected fold
  validation.data <- trainval.data %>% filter(fold == current.fold)
  # training data is all other folds
  train.data <- trainval.data %>% filter(fold != current.fold)
  
  # impute missing values for 'Age'
  train.data$Age.Imputed <- ifelse(is.na(train.data$Age),
                                   median(train.data$Age, na.rm=TRUE),
                                   train.data$Age)
  validation.data$Age.Imputed <- ifelse(is.na(validation.data$Age),
                                        median(train.data$Age, na.rm=TRUE),
                                        validation.data$Age)
  
  # now try each parameter value for each fold
  for (mtry.val in mtry.vals) {
    print(paste("fold:",current.fold))
    print(paste("mtry value:", mtry.val))
    # train a random forest with mtry = mtry.val
    rf.current <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
                                 Age.Imputed + Embarked.Imputed,
                               data=train.data,
                               mtry = mtry.val,
                               ntree=200)
    
    # obtain predictions for the current model 
    # and "Balanced Accuracy" for those predictions
    rf.preds <- predict(rf.current, newdata=validation.data)
    cm <- confusionMatrix(rf.preds, validation.data$Survived)
    balanced.accuracy <- cm$byClass['Balanced Accuracy']
    
    # save off the results and settings for this attempt
    attempt.results <- c(attempt.results, balanced.accuracy)
    attempt.mtry.used <- c(attempt.mtry.used, mtry.val)
    attempt.fold.values <- c(attempt.fold.values, current.fold)
  }
}

# create a dataframe to hold all the results for these attempts
mtry.results.df <- data.frame("fold"=factor(attempt.fold.values),
                              "mtry"=attempt.mtry.used,
                              "balanced.accuracy"=attempt.results)
head(mtry.results.df)

# plot the results
ggplot(data=mtry.results.df) + 
  geom_jitter(mapping = aes(x=mtry, y=balanced.accuracy, col=fold),
              width = 0.1) + 
  xlab("mtry") + ylab("Balanced Accuracy")
# notice some folds are 'easier' than others

# boxplot version
ggplot(data=mtry.results.df) + 
  geom_boxplot(mapping = aes(x=factor(mtry), y=balanced.accuracy)) + 
  xlab("mtry") + ylab("Balanced Accuracy")


## 'nodesize' parameter
nodesize.vals <- c(1,2,3,4,5,7,10,15,20,30,50,100,200,300)
folds <- unique(trainval.data$fold)
attempt.nodesize.used <- c()
attempt.results <- c()
attempt.fold.values <- c()

# select training and validation data for each fold
for (current.fold in folds) {
  # validation data is the selected fold
  validation.data <- trainval.data %>% filter(fold == current.fold)
  # training data is all other folds
  train.data <- trainval.data %>% filter(fold != current.fold)
  
  # impute missing values for 'Age' and 'Embarked'
  train.data$Age.Imputed <- ifelse(is.na(train.data$Age),
                                   median(train.data$Age, na.rm=TRUE),
                                   train.data$Age)
  validation.data$Age.Imputed <- ifelse(is.na(validation.data$Age),
                                        median(train.data$Age, na.rm=TRUE),
                                        validation.data$Age)
  
  # now try each parameter value for each fold
  for (nodesize.val in nodesize.vals) {
    print(paste("fold:",current.fold))
    print(paste("nodesize value:", nodesize.val))
    # train a random forest with nodesize = nodesize.val
    rf.current <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
                                 Age.Imputed + Embarked.Imputed,
                               data=train.data,
                               nodesize = nodesize.val,
                               ntree=200)
    
    # obtain predictions for the current model 
    # and "Balanced Accuracy" for those predictions
    rf.preds <- predict(rf.current, newdata=validation.data)
    cm <- confusionMatrix(rf.preds, validation.data$Survived)
    balanced.accuracy <- cm$byClass['Balanced Accuracy']
    
    # save off the results and settings for this attempt
    attempt.results <- c(attempt.results, balanced.accuracy)
    attempt.nodesize.used <- c(attempt.nodesize.used, nodesize.val)
    attempt.fold.values <- c(attempt.fold.values, current.fold)
  }
}

# create a dataframe to hold all the results for these attempts
nodesize.results.df <- data.frame("fold"=factor(attempt.fold.values),
                                  "nodesize"=attempt.nodesize.used,
                                  "balanced.accuracy"=attempt.results)
head(nodesize.results.df)

# plot the results
ggplot(data=nodesize.results.df) + 
  geom_jitter(mapping = aes(x=nodesize, y=balanced.accuracy, col=fold),
              width = 0.02) + 
  scale_x_log10() + 
  xlab("nodesize") + ylab("Balanced Accuracy")

# this is getting a bit too noisy, try a boxplot
ggplot(data=nodesize.results.df) + 
  geom_boxplot(mapping = aes(x=factor(nodesize), y=balanced.accuracy)) + 
  xlab("nodesize") + ylab("Balanced Accuracy")
# have to make nodesize a factor for boxplot to use it for grouping

# how is nodesize = 1 performing well? Shouldn't this be highly overfitting?

nodesize.results.df %>% 
  group_by(nodesize) %>%
  summarise(
    avg.balanced.accuracy = mean(balanced.accuracy)
  )


## 'sampsize' param
# 570 is max since each training set will only have 713 * 4/5 data points
sampsize.vals <- c(10,20,50,100,200,300,400,500,570)
folds <- unique(trainval.data$fold)
attempt.sampsize.used <- c()
attempt.results <- c()
attempt.fold.values <- c()

# select training and validation data for each fold
for (current.fold in folds) {
  # validation data is the selected fold
  validation.data <- trainval.data %>% filter(fold == current.fold)
  # training data is all other folds
  train.data <- trainval.data %>% filter(fold != current.fold)
  
  # impute missing values for 'Age' and 'Embarked'
  train.data$Age.Imputed <- ifelse(is.na(train.data$Age),
                                   median(train.data$Age, na.rm=TRUE),
                                   train.data$Age)
  validation.data$Age.Imputed <- ifelse(is.na(validation.data$Age),
                                        median(train.data$Age, na.rm=TRUE),
                                        validation.data$Age)
  
  # now try each parameter value for each fold
  for (sampsize.val in sampsize.vals) {
    print(paste("fold:",current.fold))
    print(paste("sampsize value:", sampsize.val))
    # train a random forest with sampsize = sampsize.val
    rf.current <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
                                 Age.Imputed + Embarked.Imputed,
                               data=train.data,
                               sampsize = sampsize.val,
                               ntree=200)
    
    # obtain predictions for the current model 
    # and "Balanced Accuracy" for those predictions
    rf.preds <- predict(rf.current, newdata=validation.data)
    cm <- confusionMatrix(rf.preds, validation.data$Survived)
    balanced.accuracy <- cm$byClass['Balanced Accuracy']
    
    # save off the results and settings for this attempt
    attempt.results <- c(attempt.results, balanced.accuracy)
    attempt.sampsize.used <- c(attempt.sampsize.used, sampsize.val)
    attempt.fold.values <- c(attempt.fold.values, current.fold)
  }
}

# create a dataframe to hold all the results for these attempts
sampsize.results.df <- data.frame("fold"=factor(attempt.fold.values),
                                  "sampsize"=attempt.sampsize.used,
                                  "balanced.accuracy"=attempt.results)
head(sampsize.results.df)

# plot the results
ggplot(data=sampsize.results.df) + 
  geom_boxplot(mapping = aes(x=factor(sampsize), y=balanced.accuracy)) + 
  xlab("sampsize") + ylab("Balanced Accuracy")

sampsize.results.df %>% 
  group_by(sampsize) %>%
  summarise(
    avg.balanced.accuracy = mean(balanced.accuracy)
  )


### Test against Kaggle-provided test set ###

# missing value imputation once more
trainval.data$Age.Imputed <- ifelse(is.na(trainval.data$Age),
                                    median(trainval.data$Age, na.rm=TRUE),
                                    trainval.data$Age)
test.data$Age.Imputed <- ifelse(is.na(test.data$Age),
                                median(trainval.data$Age, na.rm=TRUE),
                                test.data$Age)

summary(test.data$Fare)
# a single test datapoint has 'Fare' missing, so impute that with the median as well
test.data$Fare <- ifelse(is.na(test.data$Fare),
                         median(trainval.data$Fare, na.rm=TRUE),
                         test.data$Fare)

# train our 'optimal' model to predict the test set
# use the whole trainval dataset as that's 'everything but the test data'
# increase the number of trees here because we are less concerned with processing time
rf.optimized <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
                               Age.Imputed + Embarked.Imputed,
                             data=trainval.data,
                             nodesize = 1,
                             mtry = 3,
                             sampsize = 500,
                             ntree=500)

# compare with a 'vanilla' random forest using default parameters
rf.default <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
                             Age.Imputed + Embarked.Imputed,
                           data=trainval.data,
                           ntree=500)

# also compare with a single rpart tree
library(rpart)
surv.tree.xval <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Age,
                        data=trainval.data,
                        method="class",
                        xval=10,
                        cp=0.0001)

# prune the tree using the best complexity parameter
best.cp.index <- which.min(surv.tree.xval$cptable[,"xerror"])
best.cp <- surv.tree.xval$cptable[best.cp.index,"CP"]
best.cp
pruned.tree <- prune(surv.tree.xval, cp=best.cp)

# write out test predictions for submission
test.submission.rf.optimized <- test.data %>% select(PassengerId)
test.submission.rf.optimized$Survived <- predict(rf.optimized, newdata=test.data)
head(test.submission.rf.optimized)
table(test.submission.rf.optimized$Survived)
write.csv(test.submission.rf.optimized, "./data/titanic/rf_optimized_submission.csv",
          row.names = FALSE)

test.submission.rf.default <- test.data %>% select(PassengerId)
test.submission.rf.default$Survived <- predict(rf.default, newdata=test.data)
head(test.submission.rf.default)
table(test.submission.rf.default$Survived)
write.csv(test.submission.rf.default, "./data/titanic/rf_default_submission.csv",
          row.names = FALSE)

test.submission.rpart.tree <- test.data %>% select(PassengerId)
test.submission.rpart.tree$Survived <- predict(pruned.tree, newdata=test.data, type="class")
head(test.submission.rpart.tree)
table(test.submission.rpart.tree$Survived)
write.csv(test.submission.rpart.tree, "./data/titanic/rpart_tree_submission.csv",
          row.names = FALSE)


# ...actual kaggle submission results
# Rpart Tree:
# 0.76555
# Default RF:
# 0.77990
# Optimized RF: 
# 0.77990



# cross-validation using caret::trainControl method

# impute age for trainval data
median.age <- median(trainval.data$Age, na.rm = TRUE)
median.age
trainval.data$Age.Imputed <- trainval.data$Age
trainval.data$Age.Imputed[is.na(trainval.data$Age.Imputed)] <- median.age
summary(trainval.data$Age)
summary(trainval.data$Age.Imputed)

# 5-fold cross-validation, tuning 'mtry' parameter
tunegrid <- expand.grid(mtry=1:15)
train.control <- trainControl(method = "cv", number = 5)

# randomForest model (only tunes 'mtry' parameter)
tuned.rf <- caret::train(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
                    Age.Imputed + Embarked.Imputed,
                  data=trainval.data, method = "rf",
                  trControl = train.control,
                  tuneGrid = tunegrid)
tuned.rf

tuned.rf.preds <- predict(tuned.rf, newdata=test.data)
head(tuned.rf.preds)

# 'caret' boosted trees 
tuned.adaboost <- train(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
                          Age.Imputed + Embarked.Imputed,
                        data=trainval.data, method = "adaboost",
                        trControl = train.control)

tuned.adaboost

# many other validation and tuning options:
?trainControl
?train
?e1071::tune

