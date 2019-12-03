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

# TODO clean up data with missing values e.g. Age, Embarked

# partition into training and test sets
set.seed(12345)
train.proportion <- 0.75
val.proportion <- 0.25

train.index <- sample(1:nrow(titanic.data), nrow(titanic.data)*train.proportion)
train.data <- titanic.data[train.index,]
validation.data <- titanic.data[-train.index,]

# check out the data
View(train.data)

surv.rf.1 <- randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare,
                     data=train.data)
summary(surv.rf.1)

# TODO investigate properties of the RF


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
              groups=10)
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



