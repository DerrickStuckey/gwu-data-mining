# install.packages("randomForest")
library(randomForest)
library(caret)
library(gains)
library(tidyverse)

# from https://www.kaggle.com/c/titanic/data
# see link for data dictionary
titanic.data <- read.csv("./data/titanic/train.csv")
dim(titanic.data)
lapply(titanic.data, class)

# make 'Survived' a factor so randomForest() will treat it as a classification problem
table(titanic.data$Survived)
titanic.data$Survived <- ifelse(titanic.data$Survived,"Y","N")
titanic.data$Survived <- as.factor(titanic.data$Survived)
table(titanic.data$Survived)

# need to make character type predictors into
# factors for randomForest() to accept them
titanic.data$Sex <- as.factor(titanic.data$Sex)
titanic.data$Embarked <- as.factor(titanic.data$Embarked)

# go ahead and impute missing values for 'Embarked' as we're just going to always use 'S'
titanic.data$Embarked.Imputed <- titanic.data$Embarked
titanic.data$Embarked.Imputed[is.na(titanic.data$Embarked.Imputed)] <- 'S'
titanic.data$Embarked.Imputed <- factor(titanic.data$Embarked.Imputed)

# partition into training and test sets
set.seed(12345)
num.folds <- 5

nrow(titanic.data)

# create k fold labels
fold.labels.base <- 1:nrow(titanic.data) %% num.folds
head(fold.labels.base,10)

# randomize the fold labels
random.index <- sample(1:nrow(titanic.data), nrow(titanic.data), replace=FALSE)
head(random.index,10)
fold.labels <- fold.labels.base[random.index]
head(fold.labels,10)

# apply the fold labels to the data
titanic.data$fold <- fold.labels
table(titanic.data$fold)
head(titanic.data$fold,10)

# try different values of mtry between 1 and 7 (the total number of variables)
mtry.vals <- c(1:7)
folds <- unique(titanic.data$fold)
attempt.mtry.used <- c()
attempt.results <- c()
attempt.fold.values <- c()

# select training and validation data for each fold
for (current.fold in folds) {
  # validation data is the selected fold
  validation.data <- titanic.data %>% filter(fold == current.fold)
  # training data is all other folds
  train.data <- titanic.data %>% filter(fold != current.fold)
  
  # impute missing values for 'Age' and 'Embarked'
  train.data$Age.Imputed <- ifelse(is.na(train.data$Age),
                                   median(train.data$Age, na.rm=TRUE),
                                   train.data$Age)
  validation.data$Age.Imputed <- ifelse(is.na(validation.data$Age),
                                        median(train.data$Age, na.rm=TRUE),
                                        validation.data$Age)
  
  # now try each parameter value for each fold
  for (mtry.val in mtry.vals) {
    print(paste("fold:",fold))
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
View(mtry.results.df)

# plot the results
ggplot(data=mtry.results.df) + 
  geom_jitter(mapping = aes(x=mtry, y=balanced.accuracy, col=fold),
              width = 0.1) + 
  xlab("mtry") + ylab("Balanced Accuracy")


# repeat for nodesize
nodesize.vals <- c(1,2,3,4,5,7,10,15,20,30,50,100,200,300)
folds <- unique(titanic.data$fold)
attempt.nodesize.used <- c()
attempt.results <- c()
attempt.fold.values <- c()

# select training and validation data for each fold
for (current.fold in folds) {
  # validation data is the selected fold
  validation.data <- titanic.data %>% filter(fold == current.fold)
  # training data is all other folds
  train.data <- titanic.data %>% filter(fold != current.fold)
  
  # impute missing values for 'Age' and 'Embarked'
  train.data$Age.Imputed <- ifelse(is.na(train.data$Age),
                                   median(train.data$Age, na.rm=TRUE),
                                   train.data$Age)
  validation.data$Age.Imputed <- ifelse(is.na(validation.data$Age),
                                        median(train.data$Age, na.rm=TRUE),
                                        validation.data$Age)
  
  # now try each parameter value for each fold
  for (nodesize.val in nodesize.vals) {
    print(paste("fold:",fold))
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
# View(nodesize.results.df)

# plot the results
ggplot(data=nodesize.results.df) + 
  geom_jitter(mapping = aes(x=nodesize, y=balanced.accuracy, col=fold),
              width = 0.02) + 
  scale_x_log10() + 
  xlab("nodesize") + ylab("Balanced Accuracy")


# we can also still use multiple attempts per fold
# since there are other random factors at play besides the train/validation split
nodesize.vals <- rep(c(1,2,3,4,5,7,10,15,20,30,50,100,200,300),5)
folds <- unique(titanic.data$fold)
attempt.nodesize.used <- c()
attempt.results <- c()
attempt.fold.values <- c()

# repeat the for-loops

# now, just take the average for each parameter value
nodesize.results.df <- data.frame("fold"=factor(attempt.fold.values),
                                  "nodesize"=attempt.nodesize.used,
                                  "balanced.accuracy"=attempt.results)
nodesize.results.agg <- 
  nodesize.results.df %>%
  group_by(nodesize) %>%
  summarise(
    avg.balanced.accuracy = mean(balanced.accuracy)
  )
nodesize.results.agg

# plot the results, useing geom_line now as results should be stable
ggplot(data=nodesize.results.agg) + 
  geom_line(mapping = aes(x=nodesize, y=avg.balanced.accuracy, col=fold)) + 
  scale_x_log10() + 
  xlab("nodesize") + ylab("Balanced Accuracy")

# or to see the variation for each nodesize value:
ggplot(data=nodesize.results.df) + 
  geom_boxplot(mapping = aes(x=factor(nodesize), y=balanced.accuracy)) + 
  xlab("nodesize") + ylab("Balanced Accuracy")
# have to make nodesize a factor for boxplot to use it for grouping

