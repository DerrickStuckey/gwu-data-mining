# install.packages("BiocManager")
# BiocManager::install("rhdf5")

library(rhdf5)
library(neuralnet)
library(ggplot2)
library(caret)

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

# look at the first row
train.data.t[1,]

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


# before training any models, let's look at this data visually

# create a directory to hold charts
chart.dir <- "./neural_nets/digits"
if(!dir.exists(chart.dir)) {
  dir.create(chart.dir)
}

# plot the first 20 actual images from the dataset
for (img.index in 1:20) {
  
  train.data.first.row <- train.data.t[img.index,]
  length(train.data.first.row)
  
  filled.pixels.x <- c()
  filled.pixels.y <- c()
  filled.pixels.shade <- c()
  idx <- 1
  
  # start from top left, filling out rows, then columns
  for (y in 16:1) {
    for (x in 1:16) {
      print(paste("x:",x))
      print(paste("y:",y))
      print(paste("idx:",idx))
      pixel.val <- train.data.first.row[idx]
      print(paste("pixel.val:",pixel.val))
      # capture the x coordinate, y coordinate, and alpha for the pixel
      if(pixel.val>0) {
        filled.pixels.x <- c(filled.pixels.x, x)
        filled.pixels.y <- c(filled.pixels.y, y)
        filled.pixels.shade <- c(filled.pixels.shade, pixel.val)
      }
      idx <- idx + 1
    }
  }
  
  # plot the bitmap with no title or legend
  p <- ggplot() + 
    geom_point(mapping = aes(x=filled.pixels.x, y=filled.pixels.y, alpha=filled.pixels.shade),
               shape=15, size=10) + 
    theme_void() + 
    theme(legend.position = "none") + coord_fixed()
  p
  
  ggsave(filename = paste(chart.dir,"/train_image_",img.index,".png",sep=""), plot=p, width=5, height=5)
  
}

# check against the labels
train.label[1:20]

# digital representation of the first image
train.data.t[1,]
# first row of pixels for the first image
train.data.t[1,1:16]


# before training, check the range of our inputs
apply(train.data.df, 2, min)
apply(train.data.df, 2, max)

# do we need to normalize?
# normalizer <- preProcess(select(train.data.df, -label), method="range")
# train.data.norm <- predict(normalizer, train.data.df)

# train a neural net
nn.1 <- neuralnet(label ~ .,
                  data=train.data.df,
                  hidden = 20, linear.output = FALSE,
                  stepmax = 10^3)
# limit steps (stepmax) just to limit the runtime

# save the model
saveRDS(nn.1, "./neural_nets/digits_nn1.rds")

# or just load a model trained earlier
nn.1 <- readRDS("./neural_nets/digits_nn1.rds")

# obtain test probabilities
test.probs <- predict(nn.1,
                      newdata=test.data.df,
                      type="raw")
head(test.probs,2)
dim(test.probs)
levels(test.data.df$label)

# pick out the predicted digit for each test data point
test.preds.factor.index <- apply(test.probs, 1, which.max)
test.preds <- levels(test.data.df$label)[test.preds.factor.index]
head(test.preds)

  # aside: which.max and apply()
  which.max(c(1,2,3))
  which.max(c(1,3,2))
  m <- data.frame("a"=c(1,2),"b"=c(3,4),"c"=c(5,6))
  m
  apply(m, 1, sum)
  apply(m, 2, sum)
  apply(m, 1, max)
  apply(m, 1, which.max)

# now look at our actual accuracy
test.preds <- as.factor(test.preds)
confusionMatrix(test.preds, test.data.df$label)

# look at the first 10 predictions and actual values
head(test.preds,n=10)
head(test.data.df$label,n=10)

# plot ROC for each digit
# library(plotROC)
# 
# for (i in 1:length(levels(test.data.df$label))) {
#   digit <- levels(test.data.df$label)[i]
#   print(digit)
#   p <- ggplot(mapping = aes(m = test.probs[,i], d = ifelse(test.data.df$label==digit,1,0))) + 
#     geom_roc(n.cuts=20,labels=FALSE) + 
#     style_roc(theme = theme_grey) + 
#     ggtitle(paste("Digit",digit))
#   p
#   plot.filename <- paste(chart.dir,digit,".png",sep="")
#   ggsave(filename=plot.filename, plot=p, device=png())
# }






