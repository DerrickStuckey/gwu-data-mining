# install.packages("randomForest")
library(randomForest)
library(caret)
library(gains)

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

# try each value of mtry for each fold
for (fold in folds) {
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
    attempt.fold.values <- c(attempt.fold.values, fold)
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


