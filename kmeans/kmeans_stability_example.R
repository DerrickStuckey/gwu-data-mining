library(tidyverse)
library(stats) # for kmeans
library(caret) # for normalizer

# load the data
# from https://docs.google.com/spreadsheets/d/1zVbakwp2gQ8b0MpYcHBt8ykMA3x938z-RfwMHX5_LpI/edit?usp=sharing
example.data <- read.csv("./kmeans/K-means clustering example - data.csv")

# don't normalize in this case

# seed.val <- 12345
seed.val <- 67890

set.seed(seed.val)
km.3a <- kmeans(example.data, 
               centers=3)

# add the cluster info to our dataframe
# (as a factor so ggplot will know what to do with it)
example.data$cluster <- as.factor(km.3a$cluster)

# visualize the clusters against x and y
ggplot(data=example.data) +
  geom_point(mapping = aes(x=x, y=y, col=cluster,
                           shape=cluster)) +
  ggtitle(paste("Seed:",seed.val))


