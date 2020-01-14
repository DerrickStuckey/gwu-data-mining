# from "Data Mining for Business Analytics"
# https://www.dataminingbook.com/book/r-edition
westroxbury <- read.csv("./data/WestRoxbury.csv")


### basic R data functions ###

# view the whole dataset
View(westroxbury)

# some other useful 'quick look' functions
head(westroxbury)
dim(westroxbury)
names(westroxbury)

# select individual rows
westroxbury[1,]
westroxbury[2,]
westroxbury[1:5,]
westroxbury[c(1,3,5),]

# hold on - what is c(1,3,5)?
avector <- c(1,3,5)
avector
avector + 1
anothervector <- c(avector,99)
anothervector
westroxbury[anothervector,]
onethroughten <- 1:10
onethroughten

# select individual columns (and rows 1-10)
westroxbury[1:10,1]
westroxbury[1:10,2]
westroxbury$TOTAL.VALUE[1:10]
westroxbury$TAX[1:10]

# other row/column selection tricks
head(westroxbury)
head(westroxbury[-1,])
head(westroxbury[-c(1,3,5),])
head(westroxbury[,1:3])
head(westroxbury[,-(1:3)])
head(westroxbury[,-c(1,2,3)])
head(westroxbury[c(1,2,3),-c(1,2,3)])

# view the whole table
View(westroxbury)

# some summary stats
mean(westroxbury$TOTAL.VALUE)
median(westroxbury$TOTAL.VALUE)
summary(westroxbury$TOTAL.VALUE)

summary(westroxbury$FLOORS)
table(westroxbury$FLOORS)
table(westroxbury$REMODEL)

# correlation between two variables
cor(westroxbury$TOTAL.VALUE, westroxbury$LOT.SQFT)


### data visualization ###

# ggplot library
# install.packages("ggplot2")
library(ggplot2)

# ggplot histogram of total value
ggplot(data=westroxbury) + 
  geom_histogram(mapping=aes(x=TOTAL.VALUE))

# density plot of total value (like histogram but with proportion)
ggplot(data=westroxbury) + 
  geom_density(mapping=aes(x=TOTAL.VALUE))

# simple scatter plot of total value vs square footage
ggplot(data=westroxbury) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE))

# add a dimension
ggplot(data=westroxbury) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE, color=REMODEL))

# too crowded in some areas; take a random sample
nrow(westroxbury)
set.seed(12345) # ensures we will get the same sample each time
sample.index <- sample(1:nrow(westroxbury), 500)
head(sample.index)
length(sample.index)
roxbury.sample <- westroxbury[sample.index,]
dim(westroxbury)
dim(roxbury.sample)

# plot the sample only
ggplot(data=roxbury.sample) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE, color=REMODEL))

# still crowded; try a log scale instead
ggplot(data=roxbury.sample) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE, color=REMODEL)) + 
  scale_x_log10() + 
  scale_y_log10()

# use shape instead of color
ggplot(data=roxbury.sample) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE, shape=REMODEL))

# make the points a bit larger to see the shapes clearly
ggplot(data=roxbury.sample) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE, shape=REMODEL), size=2)

# plot each of the "remodel" types in separate charts
ggplot(data=roxbury.sample) + 
  geom_point(mapping = aes(x=LOT.SQFT, y=TOTAL.VALUE)) + 
  facet_wrap(~ REMODEL, nrow=2)


# make a box plot of Lot Sq. Ft. vs Remodel type
ggplot(data=westroxbury) + 
  geom_boxplot(mapping=aes(x=REMODEL, y=LOT.SQFT))

# remove outliers
ggplot(data=westroxbury) + 
  geom_boxplot(mapping=aes(x=REMODEL, y=LOT.SQFT),
               outlier.size = -1)

# remove outliers and manually set the y limits
ggplot(data=westroxbury) + 
  geom_boxplot(mapping=aes(x=REMODEL, y=LOT.SQFT),
               outlier.size = -1) +
  ylim(0,15000)

# or just use a base R boxplot
boxplot(westroxbury$LOT.SQFT ~ westroxbury$REMODEL,
        outline = FALSE)


# add another variable, BEDROOMS, into the mix

# distribution of values for REMODEL variable
ggplot(data=westroxbury) + 
  geom_bar(mapping=aes(x=REMODEL))

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
# (all the variables except REMODEL and BEDROOMS are numeric)
heatmap(cor(subset(westroxbury,select=-c(REMODEL,BEDROOMS))))

# plot combinations of variables
library(GGally)
ggpairs(roxbury.sample[,c("TOTAL.VALUE", "BEDROOMS", "LOT.SQFT")])

# jitter example
ggplot(data=roxbury.sample) + 
  geom_point(mapping=aes(x=FLOORS, y=BEDROOMS))
# what's wrong here?

# geom_jitter shows the points in a way that is less accurate
# but far more useful
ggplot(data=roxbury.sample) + 
  geom_jitter(mapping=aes(x=FLOORS, y=BEDROOMS))

# tune the spacing
ggplot(data=roxbury.sample) + 
  geom_jitter(mapping=aes(x=FLOORS, y=BEDROOMS),
              width=0.1, height=0.2)

### Aesthetic options ###

# basic plot
ggplot(data=roxbury.sample) + 
  geom_point(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE))

# specify axis labels
ggplot(data=roxbury.sample) + 
  geom_point(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE)) + 
  xlab("Living Area") + ylab("Home Value")

# add a title
ggplot(data=roxbury.sample) + 
  geom_point(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE)) + 
  xlab("Living Area") + ylab("Home Value") + 
  ggtitle("Home Value vs Bedrooms")

# center the title
ggplot(data=roxbury.sample) + 
  geom_point(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE)) + 
  xlab("Living Area") + ylab("Home Value") + 
  ggtitle("Home Value vs Bedrooms") + 
  theme(plot.title = element_text(hjust = 0.5))

# change the colour
ggplot(data=roxbury.sample) + 
  geom_point(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE), color="darkblue") + 
  xlab("Living Area") + ylab("Home Value") + 
  ggtitle("Home Value vs Bedrooms") + 
  theme(plot.title = element_text(hjust = 0.5))

# note how this is different from
ggplot(data=roxbury.sample) + 
  geom_point(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE, col=REMODEL)) + 
  xlab("Living Area") + ylab("Home Value") + 
  ggtitle("Home Value vs Bedrooms") + 
  theme(plot.title = element_text(hjust = 0.5))

# or this:
ggplot(data=roxbury.sample) + 
  geom_point(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE, col=BEDROOMS)) + 
  xlab("Living Area") + ylab("Home Value") + 
  ggtitle("Home Value vs Bedrooms") + 
  theme(plot.title = element_text(hjust = 0.5))
is.factor(roxbury.sample$BEDROOMS)
is.factor(roxbury.sample$REMODEL)

# and what is going on here?
ggplot(data=roxbury.sample) + 
  # geom_point(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE), color="darkblue") + 
  geom_point(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE, color="darkblue")) + 
  xlab("Living Area") + ylab("Home Value") + 
  ggtitle("Home Value vs Bedrooms") + 
  theme(plot.title = element_text(hjust = 0.5))

# customize the colors used
p <- ggplot(data=roxbury.sample) + 
  geom_point(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE, col=BEDROOMS)) + 
  xlab("Living Area") + ylab("Home Value") + 
  ggtitle("Home Value vs Bedrooms") + 
  theme(plot.title = element_text(hjust = 0.5))

p + scale_color_gradient(low="blue", high="red")
p + scale_color_gradient(low = "#d69ce1", high = "#bd5915")
# from https://www.color-hex.com/

# change the theme
ggplot(data=roxbury.sample) + 
  geom_point(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE)) + 
  xlab("Living Area") + ylab("Home Value") + 
  ggtitle("Home Value vs Bedrooms") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))


# even more themes
# install.packages("ggthemes")
library(ggthemes)

ggplot(data=roxbury.sample) + 
  geom_point(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE)) + 
  xlab("Living Area") + ylab("Home Value") + 
  ggtitle("Home Value vs Bedrooms") + 
  theme_economist() + 
  theme(plot.title = element_text(hjust = 0.5))

# also, we can define the data within aes() instead of up front:
ggplot() + 
  geom_point(mapping=aes(x=roxbury.sample$LIVING.AREA, 
                         y=roxbury.sample$TOTAL.VALUE))

# and the data does not need to be in a data frame
# we can just use vectors directly if we want
ggplot() + 
  geom_point(mapping=aes(x=c(0,1,2,3,4,5,6), y=c(5,0,3,2,4,1,6)))


