# cities hierarchical clustering

# data from https://public.opendatasoft.com/explore/dataset/1000-largest-us-cities-by-population-with-geographic-coordinates/table/?sort=-rank
us.cities <- read_delim("./data/1000-largest-us-cities-by-population-with-geographic-coordinates.csv",
                        delim=';')
us.cities

# split the Coordinates into latitude and longitude
us.cities <-
  us.cities %>%
  separate(Coordinates, into=c("latitude","longitude"), sep=",", convert = TRUE)
us.cities

# top 50 largest US cities only
top.50 <- us.cities %>%
  filter(Rank <= 50)

# look at the top few
top.50 %>%
  arrange(Rank)

# plot the locations on a grid
ggplot(data=top.50) + 
  geom_point(mapping = aes(x=longitude, y=latitude))
# TODO plot these points on an actual map

# keep only the coordinates and city name
top.50.coords <- 
  top.50 %>%
  select(City, latitude, longitude) %>%
  remove_rownames %>%
  column_to_rownames(var = "City")

head(top.50.coords)

# compute Euclidean distance
dist.euclid <- dist(top.50.coords, method="euclidean")


# create a directory to save charts
chart.dir <- "./hierarchical_clustering/us_cities_charts"
if(!dir.exists(chart.dir)) {
  dir.create(chart.dir)
}


# perform clustering using average cluster distance
hc.avg <- hclust(dist.euclid, method="average")

plot(hc.avg)

# split into 5 clusters
hc.avg.clusters.5 <- cutree(hc.avg, k=5)

# create a new dataframe to plot this particular clustering
top.50.avg.clusters <- top.50.coords
top.50.avg.clusters$cluster <- as.factor(hc.avg.clusters.5)

ggplot(data=top.50.avg.clusters) + 
  geom_point(mapping = aes(x=longitude, y=latitude, col=cluster, shape=cluster)) + 
  ggtitle("Method: Average")
ggsave(paste(chart.dir,"/average_clustering.png",sep=""))


# perform clustering using the 'Single' distance method
hc.single <- hclust(dist.euclid, method="single")

# split into 5 clusters
hc.single.clusters.5 <- cutree(hc.single, k=5)

# create a new dataframe to plot this particular clustering
top.50.single.clusters <- top.50.coords
top.50.single.clusters$cluster <- as.factor(hc.single.clusters.5)

# plot the 5 clusters using the 'Single' distance method
ggplot(data=top.50.single.clusters) + 
  geom_point(mapping = aes(x=longitude, y=latitude, col=cluster, shape=cluster)) + 
  ggtitle("Method: Single")
ggsave(paste(chart.dir,"/single_clustering.png",sep=""))

# how is this different from the "Average" method?


# perform clustering using complete cluster distance
hc.complete <- hclust(dist.euclid, method="complete")

# split into 5 clusters
hc.complete.clusters.5 <- cutree(hc.complete, k=5)

# create a new dataframe to plot this particular clustering
top.50.complete.clusters <- top.50.coords
top.50.complete.clusters$cluster <- as.factor(hc.complete.clusters.5)

ggplot(data=top.50.complete.clusters) + 
  geom_point(mapping = aes(x=longitude, y=latitude, col=cluster, shape=cluster)) + 
  ggtitle("Method: Complete")
ggsave(paste(chart.dir,"/complete_clustering.png",sep=""))


# try 'Centroid' distance
hc.centroid <- hclust(dist.euclid, method="centroid")

# split into 5 clusters
hc.centroid.clusters.5 <- cutree(hc.centroid, k=5)

# create a new dataframe to plot this particular clustering
top.50.centroid.clusters <- top.50.coords
top.50.centroid.clusters$cluster <- as.factor(hc.centroid.clusters.5)

ggplot(data=top.50.centroid.clusters) + 
  geom_point(mapping = aes(x=longitude, y=latitude, col=cluster, shape=cluster)) + 
  ggtitle("Method: Centroid")
ggsave(paste(chart.dir,"/centroid_clustering.png",sep=""))


# try 'Ward' distance
hc.ward <- hclust(dist.euclid, method="ward.D")

# split into 5 clusters
hc.ward.clusters.5 <- cutree(hc.ward, k=5)

# create a new dataframe to plot this particular clustering
top.50.ward.clusters <- top.50.coords
top.50.ward.clusters$cluster <- as.factor(hc.ward.clusters.5)

ggplot(data=top.50.ward.clusters) + 
  geom_point(mapping = aes(x=longitude, y=latitude, col=cluster, shape=cluster)) + 
  ggtitle("Method: Ward")
ggsave(paste(chart.dir,"/ward_clustering.png",sep=""))

