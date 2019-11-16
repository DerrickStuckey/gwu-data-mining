library(ggplot2)

# from "Data Mining for Business Analytics"
# https://www.dataminingbook.com/book/r-edition
westroxbury <- read.csv("./WestRoxbury.csv")

head(westroxbury)

hist(westroxbury$TOTAL.VALUE)

# simple scatter plot of total value vs square footage
ggplot(data=westroxbury) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE))

# add a dimension
ggplot(data=westroxbury) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE, color=REMODEL))

# too crowded in some areas; take a random sample
nrow(westroxbury)
set.seed(12345)
sample.index <- sample(row.names(westroxbury), 500)
roxbury.sample <- westroxbury[sample.index,]

# plot the sample only
ggplot(data=roxbury.sample) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE, color=REMODEL))

# still crowded; try a log scale instead
ggplot(data=roxbury.sample) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE, color=REMODEL)) + 
  scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2')

# use shape instead of color
ggplot(data=roxbury.sample) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE, shape=REMODEL))

# plot each of the "remodel" types in separate charts
ggplot(data=roxbury.sample) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE)) + 
  facet_wrap(~ REMODEL, nrow=2)

ggplot(data=roxbury.sample) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE))


