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

top.50 <- us.cities %>%
  filter(Rank <= 50)

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



# perform clustering using average cluster distance
hc.avg <- hclust(dist.euclid, method="average")

plot(hc.avg)

# split into 5 clusters
hc.avg.clusters.5 <- cutree(hc.avg, k=5)

# create a new dataframe to plot this particular clustering
top.50.avg.clusters <- top.50.coords
top.50.avg.clusters$cluster <- as.factor(hc.avg.clusters.5)

ggplot(data=top.50.avg.clusters) + 
  geom_point(mapping = aes(x=longitude, y=latitude, col=cluster)) + 
  ggtitle("Method: Average")



# perform clustering using single cluster distance
hc.single <- hclust(dist.euclid, method="single")

# plot(hc.single)

# split into 5 clusters
hc.single.clusters.5 <- cutree(hc.single, k=5)

# create a new dataframe to plot this particular clustering
top.50.single.clusters <- top.50.coords
top.50.single.clusters$cluster <- as.factor(hc.single.clusters.5)

ggplot(data=top.50.single.clusters) + 
  geom_point(mapping = aes(x=longitude, y=latitude, col=cluster)) + 
  ggtitle("Method: Single")



# perform clustering using complete cluster distance
hc.complete <- hclust(dist.euclid, method="complete")

# plot(hc.complete)

# split into 5 clusters
hc.complete.clusters.5 <- cutree(hc.complete, k=5)

# create a new dataframe to plot this particular clustering
top.50.complete.clusters <- top.50.coords
top.50.complete.clusters$cluster <- as.factor(hc.complete.clusters.5)

ggplot(data=top.50.complete.clusters) + 
  geom_point(mapping = aes(x=longitude, y=latitude, col=cluster)) + 
  ggtitle("Method: Complete")

