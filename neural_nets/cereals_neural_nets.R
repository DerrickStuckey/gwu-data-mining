# install.packages("neuralnet")
library(neuralnet)

# from https://s3-ap-south-1.amazonaws.com/av-blog-media/wp-content/uploads/2017/09/07122416/cereals.csv
# via https://www.analyticsvidhya.com/blog/2017/09/creating-visualizing-neural-network-in-r/
cereals <- read_csv("./data/cereals.csv")
nrow(cereals)

# training/test/validation split
set.seed(12345)
train.proportion <- 0.7
test.proportion <- 0.3

# pull out the training data
train.index <- sample(1:nrow(cereals), nrow(cereals)*train.proportion)
train.data <- cereals[train.index,]
test.data <- cereals[-train.index,]

# normalize all variables to a range [0,1]
# TODO may want to redo this in a less fancy way
cols.max = apply(train.data, 2, max)
cols.min = apply(train.data, 2, min)
train.scale <- scale(train.data, center = cols.min, scale = cols.max - cols.min)
train.normalized = as.data.frame(train.scale)
summary(train.normalized)
test.scale <- scale(test.data, center = cols.min, scale = cols.max - cols.min)
test.normalized = as.data.frame(test.scale)

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

