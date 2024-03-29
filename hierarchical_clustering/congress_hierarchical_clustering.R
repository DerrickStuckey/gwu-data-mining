library(tidyverse)
library(stats) # for hclust


# data from https://archive.ics.uci.edu/ml/datasets/Congressional+Voting+Records
# (header added manually based on data description)
housevotes <- read_csv("./data/house-votes-84.csv")
housevotes

# drop the 'Class Name' variable before clustering
votes.only <- 
  housevotes %>%
  select(-party)
votes.only

# set 'y' votes to 1, 'n' votes to 0, '?' votes to 0.5
vote.to.number <- function(x) {
  if(x=="y") {
    return(1)
  }
  if(x=="n") {
    return(0)
  }
  return(0.5)
}
votes.only.numeric <- apply(votes.only,c(1,2),vote.to.number)
head(votes.only.numeric)
View(votes.only.numeric)

# add back the class name as row names
# this lets us keep the name as a label but not as a variable
row.names(votes.only.numeric) <- housevotes$party
head(votes.only.numeric)

# compute Euclidean distance on the numeric votes data
# dist.votes <- dist(votes.only.numeric, method="euclidean")
dist.votes <- dist(votes.only.numeric, method="manhattan")

# perform clustering using average cluster distance
hc1 <- hclust(dist.votes, method="average")

# perform clustering using ___ cluster distance
# hc1 <- hclust(dist.votes, method="centroid")
# hc1 <- hclust(dist.votes, method="ward.D")
# hc1 <- hclust(dist.votes, method="single")
# hc1 <- hclust(dist.votes, method="complete")

# view the cluster (without labels as they are not useful)
plot(hc1, labels=FALSE)

# cut the tree after the first split
single.split <- cutree(hc1, k=2)
head(single.split,n=20)

# how well do the two groups line up with our class labels?
table(single.split, names(single.split))

# what do the specific votes look like for each of these 2 clusters?
single.split.avgs <- aggregate(votes.only.numeric,
                               by=list("cluster"=single.split),
                               FUN=mean)
View(single.split.avgs)

# slightly simplified version
View(round(single.split.avgs,1))

# what do things look like for the first 4 clusters?
four.clusters <- cutree(hc1, k=4)
head(four.clusters,n=10)

# how well do the four groups line up with our class labels?
table(four.clusters, names(four.clusters))

four.clusters.avgs <- aggregate(votes.only.numeric,
                               by=list("cluster"=four.clusters),
                               FUN=mean)
View(four.clusters.avgs)

# compare the two "mostly democrat" clusters
View(four.clusters.avgs[four.clusters.avgs$cluster %in% c(2,3),])

# compare the two "mostly republican" clusters
View(four.clusters.avgs[four.clusters.avgs$cluster %in% c(1,4),])

# rounded version of all 4 clusters
View(round(four.clusters.avgs,1))

