# install.packages("neuralnet")
library(neuralnet)
library(tidyverse)
library(forecast) # for accuracy() function
library(caret) # for preProcess() function

# from https://s3-ap-south-1.amazonaws.com/av-blog-media/wp-content/uploads/2017/09/07122416/cereals.csv
# via https://www.analyticsvidhya.com/blog/2017/09/creating-visualizing-neural-network-in-r/
cereals <- read_csv("./data/cereals.csv")
cereals

# training/validation split
set.seed(12345)
train.proportion <- 0.7
validation.proportion <- 0.3

# pull out the training data
train.index <- sample(1:nrow(cereals), nrow(cereals)*train.proportion)
train.data <- cereals[train.index,]
validation.data <- cereals[-train.index,]

head(train.data)

# look at relationships between rating and the predictor variables
ggplot(data=train.data) +
  geom_point(mapping = aes(x=calories, y=rating))

ggplot(data=train.data) +
  geom_point(mapping = aes(x=protein, y=rating))

ggplot(data=train.data) +
  geom_point(mapping = aes(x=fat, y=rating))

ggplot(data=train.data) +
  geom_point(mapping = aes(x=sodium, y=rating))

ggplot(data=train.data) +
  geom_point(mapping = aes(x=fiber, y=rating))


### data preparation ###

# normalize all variables to a range [0,1]
# note: including target variable 'rating'
normalizer <- preProcess(train.data, method="range")
train.norm <- predict(normalizer, train.data)
validation.norm <- predict(normalizer, validation.data)

### build and validate a basic model ###

# train a neural net with a single layer of 3 hidden nodes
nn.1 <- neuralnet(rating ~ calories + protein + fat + sodium + fiber, train.norm, 
                  hidden = 3, linear.output = TRUE)

# visualize the neural net
plot(nn.1)

# obtain predictions for the validation set
validation.preds.norm.nn.1 <- predict(nn.1, newdata=validation.norm)

# check the accuracy
# note: these results are in the transformed scale
accuracy(validation.norm$rating, validation.preds.norm.nn.1)

# plot predictions vs actuals (in the transformed scale)
ggplot() +
  geom_point(mapping = aes(x=validation.preds.norm.nn.1, y=validation.norm$rating))

# plot predictions vs actuals (in original scale)
rating.scale <- normalizer$ranges[,"rating"][2] - normalizer$ranges[,"rating"][1]
rating.offset <- normalizer$ranges[,"rating"][1]
validation.preds.raw.nn.1 <- rating.offset + 
  validation.preds.norm.nn.1 * rating.scale

ggplot() +
  geom_point(mapping = aes(x=validation.preds.raw.nn.1, y=validation.data$rating))

# train a neural net with two layers of 2 hidden nodes each
nn.2 <- neuralnet(rating ~ calories + protein + fat + sodium + fiber, train.norm, 
                  hidden = c(2,2), linear.output = TRUE)

# visualize the neural net
plot(nn.2)



### model tuning ###

# try a range of values for the number of hidden nodes
hidden.vals <- rep(c(0,1,2,3,5,10,20,50,100),5)
val.rmse.results <- c()
train.rmse.results <- c()

set.seed(12345)
for (hidden.val in hidden.vals) {
  
  # train the model
  # limit steps with stepmax, to reduce runtime
  nn.current <- neuralnet(rating ~ calories + protein + fat + sodium + fiber, train.norm, 
                    hidden = hidden.val, linear.output = TRUE, stepmax = 10^4)
  
  # obtain predictions on the validation set
  validation.preds.norm.nn.current <- predict(nn.current, newdata=validation.norm)
  train.preds.norm.nn.current <- predict(nn.current, newdata=train.norm)
  
  # measure the accuracy in terms of RMSE and save it
  val.rmse.current <- RMSE(validation.norm$rating, validation.preds.norm.nn.current)
  val.rmse.results <- c(val.rmse.results, val.rmse.current)
  
  train.rmse.current <- RMSE(train.norm$rating, train.preds.norm.nn.current)
  train.rmse.results <- c(train.rmse.results, train.rmse.current)
}

# plot accuracy vs # of hidden nodes
# for training data
ggplot() +
  geom_point(mapping = aes(x=hidden.vals, y=train.rmse.results)) +
  xlab("# of Hidden Nodes") + ylab("Training RMSE") +
  scale_x_log10()

# for validation data
ggplot() +
  geom_point(mapping = aes(x=hidden.vals, y=val.rmse.results)) +
  xlab("# of Hidden Nodes") + ylab("Validation RMSE") +
  scale_x_log10()

ggplot() +
  geom_boxplot(mapping = aes(x=as.factor(hidden.vals), y=val.rmse.results)) +
  xlab("# of Hidden Nodes") + ylab("Validation RMSE")


# note: other parameters can be tuned too: stepmax, learning rate, momentum


# spreadsheet example
set.seed(12345)

nn.3 <- neuralnet(rating ~ protein + fat, train.norm, 
                  hidden = 2, linear.output = TRUE)

plot(nn.3)

nn.3.val.preds <- predict(nn.3, newdata=validation.norm)

validation.norm[11,]
nn.3.val.preds[11]
