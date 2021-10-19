library(rpart) # for rpart function
library(rpart.plot) # for prp function to plot rpart trees
library(gains)
library(tidyverse)
library(caret) # for confusionMatrix

# from https://www.kaggle.com/c/titanic/data
# see link for data dictionary
titanic.data <- read_csv("./data/titanic/train.csv")
titanic.data
dim(titanic.data)

# confusionMatrix() works best if the target is a factor
# rather than a numeric or logical variable
table(titanic.data$Survived)
titanic.data$Survived <- ifelse(titanic.data$Survived,"Y","N")
titanic.data$Survived <- as.factor(titanic.data$Survived)
table(titanic.data$Survived)

# partition into training and test sets
set.seed(12345)
train.proportion <- 0.75
val.proportion <- 0.25

train.index <- sample(1:nrow(titanic.data), nrow(titanic.data)*train.proportion)
train.data <- titanic.data[train.index,]
validation.data <- titanic.data[-train.index,]

# data exploration
View(train.data)
train.data %>% summary()

# 'Age' is probably useful but has many missing values
# come back to this later

# train a classification tree to predict passenger survival
# use only "tidy" variables
# aka those which are either numeric or have a small number of classes
surv.tree.1 <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                     data=train.data,
                     method="class")

# visualize the tree
prp(surv.tree.1, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
    main="Default")

# try some different parameters for minsplit, minbucket, maxdepth, 
# cp (complexity parameter; default = 0.01)
# ?rpart.control explains meaning of these parameters

# Maxdepth
surv.tree.2 <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                     data=train.data,
                     method="class"
                     ,maxdepth=3
                     )
prp(surv.tree.2, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
    main="Maxdepth 3")
# compare with original
prp(surv.tree.1, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
    main="Default")


# Minsplit
surv.tree.3 <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                     data=train.data,
                     method="class"
                     ,minsplit=40
                     )
prp(surv.tree.3, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
    main="Minsplit 40")
# compare with original
prp(surv.tree.1, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
    main="Default")


# Complexity Parameter
surv.tree.4 <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                     data=train.data,
                     method="class",
                     cp=0.1
)
prp(surv.tree.4, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
    main="CP 0.1")
# compare with original
prp(surv.tree.1, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
    main="Default (CP = 0.01)")

# Complexity Parameter
surv.tree.5 <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                     data=train.data,
                     method="class",
                     cp=0.002
)
prp(surv.tree.5, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
    main="CP 0.002")
# compare with original
prp(surv.tree.1, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
    main="Default (CP = 0.01)")
# what does "complexity parameter" appear to be doing?

# Split type
surv.tree.6 <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                     data=train.data,
                     method="class",
                     parms = list(split = "information")
                     ,cp=0.005
)
prp(surv.tree.6, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
    main="Information (Entropy) Splitting")
# compare with original
prp(surv.tree.1, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
    main="Default (Gini Splitting)")

# Add 'Age', a variable with missing values
surv.tree.7 <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Age,
                     data=train.data,
                     method="class"
)
prp(surv.tree.7, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
    main="With 'Age'")
# compare with original
prp(surv.tree.1, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
    main="Original")
# note: this is why we prune rather than limiting growth up front


# variable importance
surv.tree.1$variable.importance

## measure performance against a validation set
validation.data$preds.tree.1 <- predict(surv.tree.1,
                                        newdata=validation.data,
                                        type="class")
summary(validation.data$preds.tree.1)

confusionMatrix(validation.data$preds.tree.1,
                validation.data$Survived,
                positive="Y")

# get probabilities instead
val.probs.1 <- predict(surv.tree.1,
                        newdata=validation.data,
                        type="prob")
head(val.probs.1)
validation.data$survival.probs.1 <- val.probs.1[,2]

# aside: where do these probabilities actually come from?
# check out their distribution:
val.probs.1[,2] %>% round(3) %>% table()
81 / (81 + 353)
119 / (6 + 119)

# plot an ROC curve of validation performance
library(plotROC)
ggplot(mapping = aes(m = validation.data$survival.probs.1, 
                     d = validation.data$Survived=="Y")) +
  geom_roc(n.cuts=100,labels=FALSE) + 
  style_roc(theme = theme_grey) + 
  ggtitle("Tree 1 Validation")


## Train using unique factors e.g. Cabin, Name
surv.tree.name <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Age + Name,
                     data=train.data,
                     method="class"
)
validation.data$preds.tree.name <- predict(surv.tree.name,
                                        newdata=validation.data,
                                        type="class")
# this is why we don't use variables which are "too unique"


### Parameter Tuning ###

# plot accuracy vs training, accuracy vs holdout against CP
# cps <- c(0.1, 0.05, 0.02, 0.01, 0.005, 0.002, 0.001, 0.0005, 0.0002, 0.0001)
cps <- c(0.1, 0.07, 0.05, 0.03, 0.02, 0.015,
         0.01, 0.007, 0.005, 0.003, 0.002, 0.0015,
         0.001, 0.0007, 0.0005, 0.0003, 0.0002, 0.00015, 0.0001)
balanced.accuracy.train <- c()
balanced.accuracy.val <- c()

# set minsplit, minbucket, maxdepth to be extremely loose so that only 
# cp (complexity parameter) determines the tree shape
for (cp in cps) {
  print(cp)
  surv.tree.cp <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Age,
                       data=train.data,
                       method="class",
                       cp=cp,
                       minsplit=2,
                       minbucket=1,
                       maxdepth=30,
                       xval=0
  )
  
  # get predictions for each value in the training and validation sets
  train.preds <- predict(surv.tree.cp, newdata=train.data, type="class")
  val.preds <- predict(surv.tree.cp, newdata=validation.data, type="class")
  
  # confusion matrix for train and test sets
  cm.train <- confusionMatrix(train.preds, train.data$Survived)
  cm.val <- confusionMatrix(val.preds, validation.data$Survived)
  
  # pull out balanced accuracy from each confusion matrix and add it to the array
  balanced.accuracy.train <- c(balanced.accuracy.train,
                               cm.train$byClass['Balanced Accuracy'])
  balanced.accuracy.val <- c(balanced.accuracy.val,
                             cm.val$byClass['Balanced Accuracy'])
}

# set up a dataframe to plot both series with a legend
train.accuracy <- data.frame("cp"=cps,
                             "dataset"="Training",
                             "accuracy"=balanced.accuracy.train)
val.accuracy <- data.frame("cp"=cps,
                             "dataset"="Validation",
                             "accuracy"=balanced.accuracy.val)
full.accuracy <- rbind(train.accuracy, val.accuracy)
head(full.accuracy)

# take the inverse of Complexity Parameter 
# so that x axis shows increasing complexity
full.accuracy$cp.inverse <- 1 / full.accuracy$cp

# plot the performance vs. complexity parameter inverse
ggplot(data=full.accuracy) + 
  geom_line(mapping = aes(x=cp.inverse, y=accuracy, col=dataset)) + 
  ylab("Balanced Accuracy") + 
  # xlab("Complexity Parameter (Inverse)") +
  xlab("Model Complexity ->") +
  scale_x_log10() + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# actual complexity parameter (non-inverse)
ggplot(data=full.accuracy) + 
  geom_line(mapping = aes(x=cp, y=accuracy, col=dataset)) + 
  ylab("Balanced Accuracy") + 
  xlab("Complexity Parameter") +
  scale_x_log10()

# which complexity parameter actually performs best?
best.cp.index <- which.max(val.accuracy$accuracy)
best.cp.index
best.cp.manual <- cps[best.cp.index]
best.cp.manual



### Actually, rpart does this for us automatically ###
set.seed(12345)
surv.tree.xval <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Age,
                      data=train.data,
                      method="class",
                      minsplit=2,
                      minbucket=1,
                      maxdepth=30,
                      xval=10,
                      cp=0.001)

# view the complexity parameter table
surv.tree.xval$cptable

# make it a dataframe
cp.df <- data.frame(surv.tree.xval$cptable)

# drop the first row as it's a big outlier
cp.df <- cp.df[-1,]
head(cp.df)

# View validation performance vs CP for the rpart cptable
ggplot(data=cp.df) + 
  geom_line(mapping = aes(x=CP, y=xerror)) + 
  scale_x_reverse() + 
  scale_y_reverse() + 
  xlab("Complexity Parameter (reverse)") + 
  ylab("Cross-Validation Performance (1 - Error)")


# prune the tree using the best complexity parameter
best.cp.index <- which.min(surv.tree.xval$cptable[,"xerror"])
best.cp <- surv.tree.xval$cptable[best.cp.index,"CP"]
best.cp
# compare with our manual findings
best.cp.manual

# prune the tree
pruned <- prune(surv.tree.xval, cp=best.cp)

# what has changed?
nrow(surv.tree.xval$splits)
nrow(pruned$splits)
surv.tree.xval$variable.importance
pruned$variable.importance

# Visualize the pruned tree
# prp(pruned, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
#     main="Pruned")
prp(pruned, main="Pruned", fallen.leaves = FALSE, tweak=1.1)

# compare with original tree
prp(surv.tree.xval, type=1, extra=1, under=TRUE, split.font=2, varlen=-10,
    main="Un-Pruned")


## Constrain tree size further to make plot reasonable
set.seed(12345)
surv.tree.xval.2 <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Age,
                        data=train.data,
                        method="class",
                        minsplit=10,
                        minbucket=3,
                        maxdepth=30,
                        xval=10,
                        cp=0.001)

# find best cp and prune with it
best.cp.index.2 <- which.min(surv.tree.xval.2$cptable[,"xerror"])
best.cp.2 <- surv.tree.xval.2$cptable[best.cp.index.2,"CP"]
pruned.2 <- prune(surv.tree.xval.2, cp=best.cp.2)


prp(surv.tree.xval.2, main="Un-pruned\n(minsplit=10, minbucket=3)", tweak=0.9)

prp(pruned.2, main="Pruned\n(minsplit=10, minbucket=3)", tweak=0.9)
