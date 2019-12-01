
# from https://www.kaggle.com/c/titanic/data
titanic.train <- read_csv("./data/titanic/train.csv")
titanic.train

# View(titanic.train)

# 'Survived' should be a logical variable
titanic.train$Survived <- as.logical(titanic.train$Survived)



