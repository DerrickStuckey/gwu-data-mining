# install.packages("BiocManager")
# BiocManager::install("rhdf5")

library(rhdf5)
library(neuralnet)

# data from https://www.kaggle.com/bistaumanga/usps-dataset#usps.h5
# help from https://stackoverflow.com/questions/15974643/how-to-deal-with-hdf5-files-in-r

# list the contents of our h5 file
h5ls("./data/usps.h5")

# pull out training data into a matrix
train.data <- h5read(file="./data/usps.h5", "/train/data")
dim(train.data)
# transpose the matrix
train.data.t <- t(train.data)
dim(train.data.t)

# same for test
test.data <- h5read(file="./data/usps.h5", "/test/data")
dim(test.data)
test.data.t <- t(test.data)
dim(test.data.t)

# pull out training data labels
train.label <- h5read(file="./data/usps.h5", "/train/target")
dim(train.label)

# construct a dataframe to train on
train.data.df <- as.data.frame(train.data.t)
# add labels to the training data
# make label a factor as we are trying to recognize individual digits
train.data.df$label <- as.factor(train.label)


# pull out testing data labels
test.label <- h5read(file="./data/usps.h5", "/test/target")
dim(test.label)

# construct a dataframe to test on
test.data.df <- as.data.frame(test.data.t)
test.data.df$label <- as.factor(test.label)


# train a neural net
nn.1 <- neuralnet(label ~ ., 
                  data=train.data.df,
                  hidden = 20, linear.output = FALSE,
                  stepmax = 10^3)
# limit steps just to limit runtime

# obtain test probabilities
test.probs <- predict(nn.1,
                      newdata=test.data.df,
                      type="raw")
head(test.probs,2)
dim(test.probs)
levels(test.data.df$label)

# confusionMatrix(test.preds, test.data.df$label)
# table(test.preds, test.data.df$label)

# plot ROC for each digit
library(plotROC)
dir.create("./neural_nets/digits")
for (i in 1:length(levels(test.data.df$label))) {
  digit <- levels(test.data.df$label)[i]
  print(digit)
  p <- ggplot(mapping = aes(m = test.probs[,i], d = ifelse(test.data.df$label==digit,1,0))) + 
    geom_roc(n.cuts=20,labels=FALSE) + 
    style_roc(theme = theme_grey) + 
    ggtitle(paste("Digit",digit))
  p
  plot.filename <- paste("./neural_nets/digits/",digit,".png",sep="")
  ggsave(filename=plot.filename, plot=p, device=png())
}

# plot an actual image from the dataset
dim(test.data.t)
test.data.first.row <- test.data.t[3,]
length(test.data.first.row)

filled.pixels.x <- c()
filled.pixels.y <- c()
filled.pixels.shade <- c()
idx <- 1

for (x in 1:16) {
  for (y in 1:16) {
    print(paste("x:",x))
    print(paste("y:",y))
    print(paste("idx:",idx))
    pixel.val <- test.data.first.row[idx]
    print(paste("pixel.val:",pixel.val))
    if(pixel.val>0) {
      filled.pixels.x <- c(filled.pixels.x, x)
      filled.pixels.y <- c(filled.pixels.y, y)
      filled.pixels.shade <- c(filled.pixels.shade, pixel.val)
    }
    idx <- idx + 1
  }
}

p <- ggplot() + 
  geom_point(mapping = aes(x=filled.pixels.x, y=filled.pixels.y, alpha=filled.pixels.shade),
             shape=15, size=10)
ggsave(filename = "./neural_nets/digits/digit0.png", plot=p)

test.label[1]

