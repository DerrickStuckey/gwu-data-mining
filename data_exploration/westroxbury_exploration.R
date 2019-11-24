library(ggplot2)

# from "Data Mining for Business Analytics"
# https://www.dataminingbook.com/book/r-edition
westroxbury <- read.csv("./data/WestRoxbury.csv")

head(westroxbury)

# ggplot histogram of total value
ggplot(data=westroxbury) + 
  geom_histogram(mapping=aes(x=TOTAL.VALUE))

# simple scatter plot of total value vs square footage
ggplot(data=westroxbury) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE))

# add a dimension
ggplot(data=westroxbury) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE, color=REMODEL))

# look at distribution of values for "REMODEL" alone
ggplot(data=westroxbury) + 
  geom_bar(mapping=aes(x=REMODEL))

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

# try geom_smooth to show a fitted curve
ggplot(data=roxbury.sample) + 
  geom_smooth(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE))

# show the fitted curve AND the actual points
ggplot(data=roxbury.sample) + 
  geom_smooth(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE)) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE))







# add another variable, BEDROOMS, into the mix

# distribution of values for REMODEL variable
# with number of bedrooms shaded
ggplot(data=westroxbury) + 
  geom_bar(mapping=aes(x=REMODEL, fill=BEDROOMS))

# why doesn't this work?
summary(westroxbury$BEDROOMS)
is.factor(westroxbury$BEDROOMS)

# make BEDROOMS variable a factor, and try again
westroxbury$BEDROOMS <- as.factor(westroxbury$BEDROOMS)
summary(westroxbury$BEDROOMS)
ggplot(data=westroxbury) + 
  geom_bar(mapping=aes(x=REMODEL, fill=BEDROOMS))

# one more way to view the same data
ggplot(data=westroxbury) + 
  geom_bar(mapping=aes(x=REMODEL, fill=BEDROOMS),
           position="dodge")

# simple heatmap of correlations between numeric variables
heatmap(cor(subset(westroxbury,select=-c(REMODEL))))

# plot combinations of variables
library(GGally)
ggpairs(subset(roxbury.sample,select=c(TOTAL.VALUE, BEDROOMS, LOT.SQFT)))

# jitter example
ggplot(data=roxbury.sample) + 
  geom_point(mapping=aes(x=BEDROOMS, y=TOTAL.VALUE))

ggplot(data=roxbury.sample) + 
  geom_jitter(mapping=aes(x=BEDROOMS, y=TOTAL.VALUE))

