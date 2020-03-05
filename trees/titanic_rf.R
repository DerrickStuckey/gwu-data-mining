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
train.proportion <- 0.75
val.proportion <- 0.25

train.index <- sample(1:nrow(titanic.data), nrow(titanic.data)*train.proportion)
train.data <- titanic.data[train.index,]
validation.data <- titanic.data[-train.index,]

# check out the data
View(train.data)

# try a model with all variables which are either numeric or have a small number of classes
surv.rf.1 <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Age + Embarked,
                     data=train.data)
summary(surv.rf.1)

# annoyingly, randomForest() does not automatically deal with missing values as rpart() does
# which variables have missing values?
summary(is.na(train.data))
summary(is.na(validation.data))
table(train.data$Embarked)

# impute 'Age' using median age
median.age <- median(train.data$Age, na.rm = TRUE)
median.age
train.data$Age.Imputed <- train.data$Age
train.data$Age.Imputed[is.na(train.data$Age.Imputed)] <- median.age
summary(train.data$Age)
summary(train.data$Age.Imputed)

# impute 'Embarked' using the mode (most common value)
summary(train.data$Embarked)
mode.embarked <- "S"
train.data$Embarked.Imputed <- train.data$Embarked
train.data$Embarked.Imputed[is.na(train.data$Embarked.Imputed)] <- mode.embarked
summary(train.data$Embarked.Imputed)

# train a new random forest using the same variables as before, plus the new ones
surv.rf.1 <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
                            Age.Imputed + Embarked.Imputed,
                          data=train.data)
summary(surv.rf.1)

# measure performance against a validation set
validation.data$preds.rf.1 <- predict(surv.rf.1,
                                      newdata=validation.data,
                                      type="class")

# we have to also fill in the missing values for the validation set
validation.data$Age.Imputed <- validation.data$Age
validation.data$Age.Imputed[is.na(validation.data$Age.Imputed)] <- median.age
validation.data$Embarked.Imputed <- validation.data$Embarked
validation.data$Embarked.Imputed[is.na(validation.data$Embarked.Imputed)] <- mode.embarked

# why don't we use median(validation.data$Age) 
# or the mode of validation.data$Embarked ?

# try again with validation data missing values filled in 
validation.data$preds.rf.1 <- predict(surv.rf.1,
                                      newdata=validation.data,
                                      type="class")
summary(validation.data$preds.rf.1)

# check actual accuracy of the new model
confusionMatrix(validation.data$preds.rf.1,
                validation.data$Survived)

# get probabilities instead, to plot another lift chart
probs.rf.1 <- predict(surv.rf.1,
                      newdata=validation.data,
                      type="prob")
head(probs.rf.1)
validation.data$survival.probs.1 <- probs.rf.1[,2]

# plot a lift chart with the probabilities
gain.1 <- gains(as.numeric(validation.data$Survived), 
              validation.data$survival.probs.1,
              groups=50)
gain.1

# combine with values from the first lift chart, to compare the models in one plot
total.survived <- sum(as.numeric(validation.data$Survived))
yvals <- c(0,gain.1$cume.pct.of.total*total.survived)
xvals <- c(0,gain.1$cume.obs)

lift.inputs <- data.frame("xvals"=xvals,
                            "yvals"=yvals,
                            "model"="Model 1")

# plot the actual lift chart
ggplot(data=lift.inputs) + 
  geom_line(mapping = aes(x=xvals, y=yvals, col=model)) +
  xlab("Predicted Survivors") + ylab("Actual Survivors") + 
  ggtitle("Model Comparison") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_abline(intercept = c(0,0), 
              slope=total.survived/nrow(validation.data),
              linetype="dashed")


# How important is each variable?
surv.rf.1$importance


### Model parameter tuning ###

# from ?randomForest

# ntree	
# Number of trees to grow. This should not be set to too small a number, 
# to ensure that every input row gets predicted at least a few times.

# mtry	
# Number of variables randomly sampled as candidates at each split. 
# Note that the default values are different for 
# classification (sqrt(p) where p is number of variables in x) and regression (p/3)

# try mtry between 1 and 7 (the total number of variables)
# default is sqrt(# of variables) = sqrt(7) ~ 3
mtry.vals <- c(1:7)
mtry.results <- c()

for (mtry.val in mtry.vals) {
  print(mtry.val)
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
  mtry.results <- c(mtry.results, balanced.accuracy)
}

# plot the results
ggplot() + 
  geom_point(mapping = aes(x=mtry.vals, y=mtry.results)) + 
  xlab("mtry") + ylab("Balanced Accuracy")

# how can we be more confident that these results are not just due to random luck?
mtry.vals <- rep(c(1:7),5)
mtry.results <- c()

# re-run the for loop

# plot the results with geom_jitter
ggplot() + 
  geom_jitter(mapping = aes(x=mtry.vals, y=mtry.results), width=0.1) + 
  xlab("mtry") + ylab("Balanced Accuracy")

# this is an improvement, but can we do even better?


# try several different values for nodesize
set.seed(12345)
nodesize.vals <- rep(c(1,2,3,4,5,7,10,15,20,30,50,100,200,300),10)
nodesize.results <- c()

for (nodesize.val in nodesize.vals) {
  print(nodesize.val)
  # train a random forest with nodesize = nodesize.val
  rf.current <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
                               Age.Imputed + Embarked.Imputed,
                             data=train.data,
                             ntree = 200,
                             mtry=4,
                             nodesize=nodesize.val)
  
  # obtain predictions for the current model 
  # and "Balanced Accuracy" for those predictions
  rf.preds <- predict(rf.current, newdata=validation.data)
  cm <- confusionMatrix(rf.preds, validation.data$Survived)
  balanced.accuracy <- cm$byClass['Balanced Accuracy']
  nodesize.results <- c(nodesize.results, balanced.accuracy)
}

# plot the results
ggplot() + 
  geom_jitter(mapping = aes(x=nodesize.vals, y=nodesize.results), width=0.01) + 
  xlab("nodesize") + ylab("Balanced Accuracy") + 
  scale_x_log10()

# boxplot alternative
ggplot() + 
  geom_boxplot(mapping = aes(x=factor(nodesize.vals), y=nodesize.results)) + 
  xlab("nodesize") + ylab("Balanced Accuracy")


# try several different values for sampsize
set.seed(12345)
sampsize.vals <- rep(c(10,20,50,100,200,300,400,500,668),10)
sampsize.results <- c()

for (sampsize.val in sampsize.vals) {
  print(sampsize.val)
  # train a random forest with sampsize = sampsize.val
  rf.current <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
                               Age.Imputed + Embarked.Imputed,
                             data=train.data,
                             ntree = 200,
                             mtry=4,
                             sampsize=sampsize.val)
  
  # obtain predictions for the current model 
  # and "Balanced Accuracy" for those predictions
  rf.preds <- predict(rf.current, newdata=validation.data)
  cm <- confusionMatrix(rf.preds, validation.data$Survived)
  balanced.accuracy <- cm$byClass['Balanced Accuracy']
  sampsize.results <- c(sampsize.results, balanced.accuracy)
}

# plot the results
ggplot() + 
  geom_jitter(mapping = aes(x=sampsize.vals, y=sampsize.results), width=0.01) + 
  xlab("sampsize") + ylab("Balanced Accuracy") + 
  scale_x_log10()



# try several different values for ntree
ntree.vals <- rep(c(1,2,5,10,20,50,100,200,500,1000,2000),5)
ntree.results <- c()

for (ntree.val in ntree.vals) {
  print(ntree.val)
  # train a random forest with ntree = ntree.val
  rf.current <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
                               Age.Imputed + Embarked.Imputed,
                             data=train.data,
                             ntree = ntree.val,
                             mtry=4,
                             sampsize=300)
  
  # obtain predictions for the current model 
  # and "Balanced Accuracy" for those predictions
  rf.preds <- predict(rf.current, newdata=validation.data)
  cm <- confusionMatrix(rf.preds, validation.data$Survived)
  balanced.accuracy <- cm$byClass['Balanced Accuracy']
  ntree.results <- c(ntree.results, balanced.accuracy)
}

# plot the results
ggplot() + 
  geom_jitter(mapping = aes(x=ntree.vals, y=ntree.results), width=0.1, height=0) + 
  xlab("ntree") + ylab("Balanced Accuracy") + 
  scale_x_log10()

# why are we getting different results for a single value of ntree?






