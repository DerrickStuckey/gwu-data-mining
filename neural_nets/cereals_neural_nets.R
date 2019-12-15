# install.packages("neuralnet")
library(neuralnet)
library(tidyverse)
library(forecast) # for accuracy function

# from https://s3-ap-south-1.amazonaws.com/av-blog-media/wp-content/uploads/2017/09/07122416/cereals.csv
# via https://www.analyticsvidhya.com/blog/2017/09/creating-visualizing-neural-network-in-r/
cereals <- read_csv("./data/cereals.csv")
nrow(cereals)

# training/test split
set.seed(12345)
train.proportion <- 0.7
test.proportion <- 0.3

# pull out the training data
train.index <- sample(1:nrow(cereals), nrow(cereals)*train.proportion)
train.data <- cereals[train.index,]
test.data <- cereals[-train.index,]

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

# get the min and max for each column
cols.max = apply(train.data, 2, max) # applies max() function to each column
cols.max
cols.min = apply(train.data, 2, min) # applies min() function to each column
cols.min

# rescale the training and test data using these values
train.scale <- scale(train.data, center = cols.min, scale = cols.max - cols.min)
train.normalized = as.data.frame(train.scale)
summary(train.normalized)
test.scale <- scale(test.data, center = cols.min, scale = cols.max - cols.min)
test.normalized = as.data.frame(test.scale)
# note: test data normalization is still done using cols.min, cols.max from training data


### build and test a basic model ###

# train the neural net
nn.1 <- neuralnet(rating ~ calories + protein + fat + sodium + fiber, train.normalized, 
                  hidden = 3, linear.output = TRUE)

# check it out
plot(nn.1)

# obtain predictions for the test set
test.preds.raw.nn.1 <- predict(nn.1, newdata=test.normalized)

# check the accuracy in the transformed scale
cor(test.normalized$rating, test.preds.raw.nn.1) ^ 2
accuracy(test.normalized$rating, test.preds.raw.nn.1)

# plot predictions vs actuals in the transformed scale
ggplot() +
  geom_point(mapping = aes(x=test.preds.raw.nn.1, y=test.normalized$rating))

# re-scale the raw test predictions back to the original scale
rating.center <- attr(train.scale, 'scaled:center')['rating']
rating.scale <- attr(train.scale, 'scaled:scale')['rating']
test.preds.unscaled.nn.1 <- test.preds.raw.nn.1 * rating.scale + rating.center

# plot predictions vs actuals in the original scale
ggplot() + 
  geom_point(mapping = aes(x=test.preds.unscaled.nn.1, y=test.data$rating))

# check accuracy metrics in the original scale
cor(test.data$rating, test.preds.unscaled.nn.1) ^ 2
accuracy(test.data$rating, test.preds.unscaled.nn.1)

# are these different than before?


### model tuning ###

# try a range of values for the number of hidden nodes
hidden.vals <- c(1,2,3,5,10,20,30,50,100,200,500,1000)
rsq.results <- c()

set.seed(12345)
for (hidden.val in hidden.vals) {
  
  # train the model
  nn.current <- neuralnet(rating ~ calories + protein + fat + sodium + fiber, train.normalized, 
                    hidden = hidden.val, linear.output = TRUE, stepmax = 10^5)
  
  # obtain predictions on the validation set
  test.preds.raw.nn.current <- predict(nn.current, newdata=test.normalized)
  
  # measure the accuracy in terms of r-squared and save it
  rsq.current <- cor(test.normalized$rating, test.preds.raw.nn.current) ^ 2
  rsq.results <- c(rsq.results, rsq.current)
}

# plot accuracy vs # of hidden layers
ggplot() +
  geom_line(mapping = aes(x=hidden.vals, y=rsq.results)) +
  xlab("# of Hidden Layers") + ylab("R-Squared") +
  scale_x_log10()


# note: other parameters can be tuned too: stepmax, learning rate, momentum


