library(tidyverse)
library(stats) # for kmeans
library(caret) # for normalizer

# load the data
# from https://docs.google.com/spreadsheets/d/1rhUJJ40zsQuGFk5_vxbLmN-cJCbaSkWb4YTyHtBoXZY/edit?usp=sharing
example.data <- read.csv("./kmeans/K-means clustering example - data.csv")

# don't normalize in this case

# visualize the raw data prior to clustering
ggplot(data=example.data) +
  geom_point(mapping = aes(x=x, y=y))
# what do you think the 3 clusters will turn out to be?

seed.val <- 12345

set.seed(seed.val)
km.3a <- kmeans(example.data, 
               centers=3)

# add the cluster info to our dataframe
# (as a factor so ggplot will know what to do with it)
example.data$cluster.a <- as.factor(km.3a$cluster)

# visualize the clusters against x and y
ggplot(data=example.data) +
  geom_point(mapping = aes(x=x, y=y, col=cluster.a,
                           shape=cluster.a)) +
  ggtitle(paste("Seed:",seed.val))

km.3a$tot.withinss

# try again
seed.val <- 67890

set.seed(seed.val)
km.3b <- kmeans(example.data, 
                centers=3)

# add the cluster info to our dataframe
# (as a factor so ggplot will know what to do with it)
example.data$cluster.b <- as.factor(km.3b$cluster)

# visualize the clusters against x and y
ggplot(data=example.data) +
  geom_point(mapping = aes(x=x, y=y, col=cluster.b,
                           shape=cluster.b)) +
  ggtitle(paste("Seed:",seed.val))

km.3b$tot.withinss

# which solution is better?

