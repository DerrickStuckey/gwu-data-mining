library(tidyverse)

# "diamonds" dataset included in the tidyverse package
diamonds

# what does the data look like?
head(diamonds)

dim(diamonds)

summary(diamonds)

# randomly sample 200 data points to look at
set.seed(12345)
sample.idx <- sample(1:nrow(diamonds), 200)
sample.data <- diamonds[sample.idx,]

## Example charts

# distribution of price for the sample data
ggplot(sample.data) +
  geom_histogram(mapping=aes(x=price))

# plot price vs carat for the sample data
ggplot(sample.data) +
  geom_point(mapping=aes(x=carat, y=price))

# try log-scaling both axes
ggplot(sample.data) +
  geom_point(mapping=aes(x=carat, y=price)) +
  scale_x_log10() + scale_y_log10()


# relationship of price and color
ggplot(sample.data) + 
  geom_boxplot(mapping=aes(x=color, y=price))


## Exercise problems
## use the sample.data data frame for all of the below

# what is the relationship of cut and price? (in a chart)


# show the relationship of price, cut and carat all in one chart


# can you also add color into the mix?


# how about price, carat, and depth in one chart?


# what is the correlation of price and carat? (the numeric value)


# how many diamonds of each color are in the sample data?


# can you show the relationship of cut and color in a chart?

