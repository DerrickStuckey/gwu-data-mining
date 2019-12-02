library(rpart)
library(rpart.plot)

# from https://www.kaggle.com/c/titanic/data
# see link for data dictionary
titanic.data <- read_csv("./data/titanic/train.csv")
titanic.data
dim(titanic.data)

# 'Survived' should be a logical variable
titanic.data$Survived <- as.logical(titanic.data$Survived)

# partition into training and test sets
set.seed(12345)
train.proportion <- 0.75
val.proportion <- 0.25

train.index <- sample(1:nrow(titanic.data), nrow(titanic.data)*train.proportion)
train.data <- titanic.data[train.index,]
validation.data <- titanic.data[-train.index,]

# data exploration
View(train.data)

# verify which variables are ready to use directly
table(train.data$Survived)
table(train.data$Pclass)
length(unique(train.data$Name))
table(train.data$Sex)
summary(train.data$Age)
table(train.data$SibSp)
table(train.data$Parch)
length(unique(train.data$Ticket))
summary(train.data$Fare)
length(unique(train.data$Cabin))
length(unique(train.data$Embarked))

# 'Age' is probably useful but has many missing values
# come back to this later

# train a classification tree to predict passenger survival
# use only "tidy" variables
# aka those which are either numeric or have a small number of classes
surv.tree.1 <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                     data=train.data,
                     method="class")

prp(surv.tree.1, type=1, extra=1, under=TRUE, split.font=2, varlen=-10)

# try some different parameters for minsplit, minbucket, maxdepth, 
# cp (complexity parameter; default = 0.01)
# ?rpart.control explains meaning of these parameters
surv.tree.2 <- rpart(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                     data=train.data,
                     method="class"
                     # ,minsplit=10
                     # ,minbucket=5
                     ,maxdepth=3
                     # ,cp=0.02
                     )

prp(surv.tree.2, type=1, extra=1, under=TRUE, split.font=2, varlen=-10)

