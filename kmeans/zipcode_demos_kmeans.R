library(tidyverse)
library(stats) # for kmeans

# from https://numeracy.co/public/4WLS9jaVTof#query
zipcode.demographics <- read_csv("data/zipcode/Demographics by ZIP.csv")
zipcode.demographics


# select only the demographics we want to use
names(zipcode.demographics)
zipcode.demos.sel <- 
  zipcode.demographics %>%
  select(zip, population,pct_high_school:pct_graduate,
         `_0_to_17`:`_65_plus`, `_0_to_50k`:`_200_plus`)

# make the zip code a row name rather than an actual variable
# zipcode.demos.sel <-
#   zipcode.demos.sel %>%
#   remove_rownames %>%
#   column_to_rownames(var = "zip")

# head(zipcode.demos.sel)
# dim(zipcode.demos.sel)

# take a sample for plotting
set.seed(12345)
zipcode.demos.sel.sample <- zipcode.demos.sel[sample(row.names(zipcode.demos.sel),1000),]
# TODO the zip code row name is being dropped; fix this
dim(zipcode.demos.sel.sample)
head(zipcode.demos.sel.sample)

# drop entries with population <= 500
zipcode.demos.sel.sample <- 
  zipcode.demos.sel.sample %>%
  filter(population > 500)
dim(zipcode.demos.sel.sample)

# look at some data distributions and relationships
ggplot(data = zipcode.demos.sel.sample) + 
  geom_histogram(mapping = aes(x=population))

ggplot(data = zipcode.demos.sel.sample) + 
  geom_histogram(mapping = aes(x=pct_high_school))

ggplot(data = zipcode.demos.sel.sample) + 
  geom_histogram(mapping = aes(x=pct_bachelors))

ggplot(data = zipcode.demos.sel.sample) + 
  geom_histogram(mapping = aes(x=pct_graduate))

# education vs age
ggplot(data = zipcode.demos.sel.sample) + 
  geom_point(mapping = aes(x=`_65_plus`, y=pct_bachelors))

ggplot(data = zipcode.demos.sel.sample) + 
  geom_point(mapping = aes(x=`_0_to_17`, y=pct_bachelors))

# income vs education
ggplot(data = zipcode.demos.sel.sample) + 
  geom_point(mapping = aes(x=pct_bachelors, `_100k_to_200k`))

ggplot(data = zipcode.demos.sel.sample) + 
  geom_point(mapping = aes(x=pct_graduate, `_200_plus`))



# all but population are already normalized to [0,1]

# normalize population variable
# TODO do this for the whole dataset

# first, transform to log scale
zipcode.demos.sel.sample$population.log <-
  log(zipcode.demos.sel.sample$population)
summary(zipcode.demos.sel.sample$population.log)

# now normalize to the range [0,1]
min.val <- min(zipcode.demos.sel.sample$population.log)
max.val <- max(zipcode.demos.sel.sample$population.log)
zipcode.demos.sel.sample$population.norm <- 
  (zipcode.demos.sel.sample$population.log - min.val) / (max.val - min.val)
summary(zipcode.demos.sel.sample$population.norm)


zipcode.demos.train <- 
  zipcode.demos.sel.sample %>%
  select(population.norm, pct_high_school:pct_graduate,
         `_0_to_17`:`_65_plus`, `_0_to_50k`:`_200_plus`)
head(zipcode.demos.train)

# try k-means on the variables with range [0,1]
km.1 <- kmeans(zipcode.demos.train, 5)

km.1

# since the rows are in the same order, 
# we can apply the cluster results straight back to the original dataframe
zipcode.demos.sel.sample$cluster <- as.factor(km.1$cluster)

# plot with color
ggplot(data = zipcode.demos.sel.sample) + 
  geom_point(mapping = aes(x=pct_graduate, `_200_plus`, col=cluster)) + 
  scale_x_log10() + scale_y_log10()

ggplot(data = zipcode.demos.sel.sample) + 
  geom_point(mapping = aes(x=`_0_to_17`, `_65_plus`, col=cluster))

ggplot(data = zipcode.demos.sel.sample) + 
  geom_point(mapping = aes(x=`pct_bachelors`, `pct_graduate`, col=cluster))

ggplot(data = zipcode.demos.sel.sample) + 
  geom_point(mapping = aes(x=`pct_bachelors`, `_100k_to_200k`, col=cluster))

ggplot(data = zipcode.demos.sel.sample) + 
  geom_boxplot(mapping = aes(x=cluster, y=population.norm))

ggplot(data = zipcode.demos.sel.sample) + 
  geom_point(mapping = aes(x=population.norm, y=pct_bachelors, col=cluster))

ggplot(data = zipcode.demos.sel.sample) + 
  geom_point(mapping = aes(x=`pct_high_school`, `_18_to_24`, col=cluster))

# what are these zip codes in cluster 4?
zipcode.demos.sel.sample$zip[zipcode.demos.sel.sample$cluster==4]


