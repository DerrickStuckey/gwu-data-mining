library(tidyverse)
library(stats) # for kmeans
library(caret) # for normalizer

# load the data
iris

# what is a sepal anyway?
# https://www.pngkey.com/png/full/82-826789_iris-iris-sepal-and-petal.png

# convert to a tibble
iris.tibble <- as_tibble(iris)
iris.tibble

# what do our data distributions look like for the measurement variables?
summary(iris.tibble)
apply(iris.tibble %>% select(-Species), 2, mean)
apply(iris.tibble %>% select(-Species), 2, sd)

# pull out just the numeric variables to run our k-means algo against
iris.numeric <- iris.tibble %>%
  select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)

# normalize the numeric data
# (we could probably skip this as the variables are of similar scale)
normalizer <- preProcess(iris.numeric,
                         method = c("center", "scale"))

iris.normalized <- predict(normalizer, iris.numeric)

# what did this normalization accomplish?
apply(iris.normalized, 2, mean)
apply(iris.normalized, 2, sd)

# try running k-means on the normalized numeric data
# (should be OK as our numeric variables are all of similar scale)
# with k = 2
set.seed(12345)
km.2 <- kmeans(iris.normalized, 
               centers=2)

# look at a summary of the results
km.2

# what are the centers of each cluster that our k-means algo has found?
km.2$centers

# what cluster does it put the first few data points in?
head(km.2$cluster)

# add the cluster info to our dataframe
# (as a factor so ggplot will know what to do with it)
iris.numeric$cluster <- as.factor(km.2$cluster)

# visualize the clusters against Petal Size (Length vs Width)

# first, without cluster labels
ggplot(data=iris.numeric) +
  geom_point(mapping = aes(x=Petal.Width, y=Petal.Length))
# what do you think the clustering will do?

# with cluster labels:
ggplot(data=iris.numeric) +
  geom_point(mapping = aes(x=Petal.Width, y=Petal.Length, col=cluster,
                           shape=cluster))

# visualize the clusters against Sepal Size (Length vs Width)
ggplot(data=iris.numeric) +
  geom_point(mapping = aes(x=Sepal.Width, y=Sepal.Length, col=cluster,
                           shape=cluster))

# Sepal Length vs Petal Length visualization
ggplot(data=iris.numeric) +
  geom_point(mapping = aes(x=Petal.Length, y=Sepal.Length, col=cluster,
                           shape=cluster))

# try again, with 3 clusters
set.seed(12345)
km.3 <- kmeans(iris.normalized, centers = 3)

# look at a summary of the results
km.3

# what are the centers of each cluster that our k-means algo has found?
km.3$centers

# what cluster does it put the first few data points in?
head(km.3$cluster)

# add the cluster info to our dataframe
# (as a factor so ggplot will know what to do with it)
iris.numeric$cluster <- as.factor(km.3$cluster)

# visualize the clusters against Petal Size

# first, without cluster labels
ggplot(data=iris.numeric) +
  geom_point(mapping = aes(x=Petal.Width, y=Petal.Length))
# what do you think the clustering will do?

# with cluster labels:
ggplot(data=iris.numeric) +
  geom_point(mapping = aes(x=Petal.Width, y=Petal.Length, col=cluster,
                           shape=cluster))
# why is there some "mixing" of the clusters, rather than clean separation?

# visualize the clusters against Sepal Size
ggplot(data=iris.numeric) +
  geom_point(mapping = aes(x=Sepal.Width, y=Sepal.Length, col=cluster,
                           shape=cluster))

# Sepal Length vs Petal Length visualization
ggplot(data=iris.numeric) +
  geom_point(mapping = aes(x=Petal.Length, y=Sepal.Length, col=cluster,
                           shape=cluster))

# how well do the clusters it has found line up with the actual Species labels?
# (this wasn't explicitly our goal though, as we didn't use the Species labels for clustering)
table(km.3$cluster, iris.tibble$Species)


# compare Sum of Squares for k=2, k=3
km.2$betweenss
km.2$tot.withinss

km.3$betweenss
km.3$tot.withinss

km.2$betweenss + km.2$tot.withinss
km.3$betweenss + km.3$tot.withinss


# try a few different values of k
# measure the 'goodness of fit' for each
k.vals <- c(1,2,3,4,5)
k.tot.withinss <- c()
for (k.val in k.vals) {
  km.k <- kmeans(iris.normalized, k.val)
  current.tot.withinss <- km.k$tot.withinss
  k.tot.withinss <- c(k.tot.withinss, current.tot.withinss)
}

# plot SS within clusters vs k
ggplot() +
  geom_line(mapping = aes(x=k.vals, y=k.tot.withinss)) + 
  xlab("k") + ylab("Total Sum of Squares within Clusters")


# how stable are these clusters?

# try different random seeds for initialization
seed.vals <- c(12345, 42, 8675309, 202)
for (seed.val in seed.vals) {
  set.seed(seed.val)
  km.current <- kmeans(iris.normalized, centers=3)
  iris.numeric$cluster <- as.factor(km.current$cluster)
  p <- ggplot(data=iris.numeric) +
    geom_point(mapping = aes(x=Petal.Length, y=Sepal.Length, col=cluster,
                             shape=cluster)) +
    ggtitle(paste("Seed:",seed.val))
  ggsave(filename = paste("./kmeans/iris_plots/",seed.val,".png",sep=""))
}



