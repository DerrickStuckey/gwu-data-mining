library(tidyverse)
library(stats) # for kmeans

# load the data
iris

# convert to a tibble
iris.tibble <- as_tibble(iris)
iris.tibble

# what do our data distributions look like for the measurement variables?
summary(iris.tibble$Sepal.Length)
summary(iris.tibble$Sepal.Width)
summary(iris.tibble$Petal.Length)
summary(iris.tibble$Petal.Width)

# pull out just the numeric variables to run our k-means algo against
iris.numeric <- iris.tibble %>%
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)

# try running k-means with no normalization, and 2 clusters
# (should be OK as our numeric variables are all of similar scale)
set.seed(12345)
km.1 <- kmeans(iris.numeric, 2)

# look at a summary of the results
km.1

# what are the centers of each cluster that our k-means algo has found?
km.1$centers

# what cluster does it put the first few data points in?
head(km.1$cluster)

# add the cluster info to our dataframe
# (as a factor so ggplot will know what to do with it)
iris.numeric$cluster <- as.factor(km.1$cluster)

# visualize the clusters against Petal Size
ggplot(data=iris.numeric) +
  geom_point(mapping = aes(x=Petal.Width, y=Petal.Length, col=cluster))

# visualize the clusters against Sepal Size
ggplot(data=iris.numeric) +
  geom_point(mapping = aes(x=Sepal.Width, y=Sepal.Length, col=cluster))



# try again, with 3 clusters
set.seed(12345)
km.1 <- kmeans(iris.numeric, 3)

# look at a summary of the results
km.1

# what are the centers of each cluster that our k-means algo has found?
km.1$centers

# what cluster does it put the first few data points in?
head(km.1$cluster)

# add the cluster info to our dataframe
# (as a factor so ggplot will know what to do with it)
iris.numeric$cluster <- as.factor(km.1$cluster)

# visualize the clusters against Petal Size
ggplot(data=iris.numeric) +
  geom_point(mapping = aes(x=Petal.Width, y=Petal.Length, col=cluster))

# visualize the clusters against Sepal Size
ggplot(data=iris.numeric) +
  geom_point(mapping = aes(x=Sepal.Width, y=Sepal.Length, col=cluster))


# how well do the clusters it has found line up with the actual Species labels?
table(km.1$cluster, iris.tibble$Species)


# TODO try k-means with different numbers of iterations, visualizing the intermediate results after each iteration
