# install.packages("randomForest")
library(randomForest)
library(caret)
library(gains)

# from https://www.kaggle.com/c/titanic/data
# see link for data dictionary
titanic.data <- read_csv("./data/titanic/train.csv")
titanic.data
dim(titanic.data)

# make 'Survived' a factor so randomForest() will treat it as a classification problem
titanic.data$Survived <- as.factor(titanic.data$Survived)

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

# try a model with all "tidy" variables
# aka those which are either numeric or have a small number of classes
surv.rf.1 <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Age + Embarked,
                     data=train.data)
summary(surv.rf.1)

# which variables have missing values?
summary(train.data)

# for now, just drop the variables with missing values
surv.rf.1 <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare,
                          data=train.data)
summary(surv.rf.1)

# measure performance against a validation set
validation.data$preds.rf.1 <- predict(surv.rf.1,
                                        newdata=validation.data,
                                        type="class")
summary(validation.data$preds.rf.1)

confusionMatrix(validation.data$preds.rf.1,
                validation.data$Survived)

# get probabilities instead
probs.rf.1 <- predict(surv.rf.1,
                        newdata=validation.data,
                        type="prob")
head(probs.rf.1)
validation.data$survival.probs.1 <- probs.rf.1[,2]

# plot a lift chart with the probabilities
gain <- gains(as.numeric(validation.data$Survived), 
              validation.data$survival.probs.1,
              groups=50)
gain

# set up lift chart variables
total.survived <- sum(as.numeric(validation.data$Survived))
yvals <- c(0,gain$cume.pct.of.total*total.survived)
xvals <- c(0,gain$cume.obs)

# plot the actual lift chart
ggplot() + 
  geom_line(mapping = aes(x=xvals, y=yvals)) +
  xlab("Predicted Survivors") + ylab("Actual Survivors") + 
  ggtitle("RF #1 Validation") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_abline(intercept = c(0,0), 
              slope=total.survived/nrow(validation.data),
              linetype="dashed")



# what about the variables we left out?
# what values should we use in place of missing values?

# let's create a new variable using 'Age', but with no missing values
median.age <- median(train.data$Age, na.rm = TRUE)
median.age
train.data$Age.Full <- train.data$Age
train.data$Age.Full[is.na(train.data$Age.Full)] <- median.age
summary(train.data$Age)
summary(train.data$Age.Full)

# same for 'Embarked'
summary(train.data$Embarked)
mode.embarked <- "S"
train.data$Embarked.Full <- train.data$Embarked
train.data$Embarked.Full[is.na(train.data$Embarked.Full)] <- mode.embarked
summary(train.data$Embarked.Full)

# train a new random forest using the same variables as before, plus the new ones
surv.rf.2 <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
                            Age.Full + Embarked.Full,
                          data=train.data)
summary(surv.rf.2)

# measure performance against a validation set
validation.data$preds.rf.2 <- predict(surv.rf.2,
                                      newdata=validation.data,
                                      type="class")

# we have to also fill in the missing values for the validation set
validation.data$Age.Full <- validation.data$Age
validation.data$Age.Full[is.na(validation.data$Age.Full)] <- median.age
validation.data$Embarked.Full <- validation.data$Embarked
validation.data$Embarked.Full[is.na(validation.data$Embarked.Full)] <- mode.embarked

# why don't we use median(validation.data$Age) 
# or the mode of validation.data$Embarked ?

# try again with validation data missing values filled in 
validation.data$preds.rf.2 <- predict(surv.rf.2,
                                      newdata=validation.data,
                                      type="class")
summary(validation.data$preds.rf.2)

# check actual accuracy of the new model
confusionMatrix(validation.data$preds.rf.2,
                validation.data$Survived)

# get probabilities instead, to plot another lift chart
probs.rf.2 <- predict(surv.rf.2,
                      newdata=validation.data,
                      type="prob")
head(probs.rf.2)
validation.data$survival.probs.2 <- probs.rf.2[,2]

# plot a lift chart with the probabilities
gain.2 <- gains(as.numeric(validation.data$Survived), 
              validation.data$survival.probs.2,
              groups=50)
gain.2

# combine with values from the first lift chart, to compare the models in one plot
total.survived <- sum(as.numeric(validation.data$Survived))
yvals.2 <- c(0,gain.2$cume.pct.of.total*total.survived)
xvals.2 <- c(0,gain.2$cume.obs)

lift.inputs.1 <- data.frame("xvals"=xvals,
                            "yvals"=yvals,
                            "model"="Model 1")
lift.inputs.2 <- data.frame("xvals"=xvals.2,
                            "yvals"=yvals.2,
                            "model"="Model 2")
lift.inputs <- rbind(lift.inputs.1, lift.inputs.2)

# plot the actual lift chart
ggplot(data=lift.inputs) + 
  geom_line(mapping = aes(x=xvals, y=yvals, col=model)) +
  xlab("Predicted Survivors") + ylab("Actual Survivors") + 
  ggtitle("Model Comparison") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_abline(intercept = c(0,0), 
              slope=total.survived/nrow(validation.data),
              linetype="dashed")


# How important is each variable in each model?
surv.rf.1$importance
surv.rf.2$importance


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
# default is sqrt(7)
mtry.vals <- c(1:7)
mtry.results <- c()

for (mtry.val in mtry.vals) {
  print(mtry.val)
  # train a random forest with mtry = mtry.val
  rf.current <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
                               Age.Full + Embarked.Full,
                             data=train.data,
                             mtry = mtry.val)
  
  # obtain predictions for the current model 
  # and "Balanced Accuracy" for those predictions
  rf.preds <- predict(rf.current, newdata=validation.data)
  cm <- confusionMatrix(rf.preds, validation.data$Survived)
  balanced.accuracy <- cm$byClass['Balanced Accuracy']
  mtry.results <- c(mtry.results, balanced.accuracy)
}

# plot the results
ggplot() + 
  geom_line(mapping = aes(x=mtry.vals, y=mtry.results)) + 
  xlab("mtry") + ylab("Balanced Accuracy")

# how can we be more confident that these results are not just due to random luck?


# try several different values for ntree
# ntree.vals <- c(1,2,5,10,20,50,100,200,500,1000,2000)
ntree.vals <- rep(c(1,2,5,10,20,50,100,200,500,1000,2000),5)
ntree.results <- c()

for (ntree.val in ntree.vals) {
  print(ntree.val)
  # train a random forest with ntree = ntree.val
  rf.current <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
                               Age.Full + Embarked.Full,
                             data=train.data,
                             ntree = ntree.val)
  
  # obtain predictions for the current model 
  # and "Balanced Accuracy" for those predictions
  rf.preds <- predict(rf.current, newdata=validation.data)
  cm <- confusionMatrix(rf.preds, validation.data$Survived)
  balanced.accuracy <- cm$byClass['Balanced Accuracy']
  ntree.results <- c(ntree.results, balanced.accuracy)
}

# plot the results
ggplot() + 
  geom_point(mapping = aes(x=ntree.vals, y=ntree.results)) + 
  xlab("ntree") + ylab("Balanced Accuracy") + 
  scale_x_log10()
# if too many points are overlapping, try geom_jitter instead


