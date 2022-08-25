library(tidyverse)


# http://jse.amstat.org/v19n3/decock/AmesHousing.txt
ames <- read.table("~/Downloads/AmesHousing.txt", header = TRUE,
                   sep="\t")

head(ames)
dim(ames)

View(ames)

# target variable
hist(ames$SalePrice)

# training/test split
set.seed(12345)
train.proportion <- 0.7
test.proportion <- 0.3

train.index <- sample(1:nrow(ames), nrow(ames)*train.proportion)
train.data <- ames[train.index,]
tet.data <- ames[!train.index,]


