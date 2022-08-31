# install.packages("ggplot2")
library(ggplot2)
# install.packages("GGally")
library(GGally)
# install.packages("ggthemes")
library(ggthemes)


# from "Data Mining for Business Analytics"
# https://www.dataminingbook.com/book/r-edition
westroxbury <- read.csv("./data/WestRoxbury.csv")
# how did I know where to find this file?


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
avector
westroxbury[avector,]
westroxbury[anothervector,]

# create a vector of a specified range of integers
one.through.ten <- 1:10
one.through.ten

# select individual columns (and rows 1-10)
westroxbury[1:10,1]
westroxbury[1:10,2]
westroxbury$TOTAL.VALUE[1:10]
westroxbury$TAX[1:10]

# other row/column selection tricks
head(westroxbury)
head(westroxbury[-1,]) # leave out first row
head(westroxbury[-c(1,3,5),]) # leave out rows 1,3,5
head(westroxbury[,1:3]) # only columns 1-3
head(westroxbury[,-(1:3)]) # all but columns 1-3
head(westroxbury[,-c(1,2,3)]) # same thing
head(westroxbury[c(1,2,3),c(1,2,3)])
head(westroxbury[c(1,2,3),-c(1,2,3)])

# some summary stats
mean(westroxbury$TOTAL.VALUE)
median(westroxbury$TOTAL.VALUE)
summary(westroxbury$TOTAL.VALUE)

summary(westroxbury$FLOORS)
table(westroxbury$FLOORS)
summary(westroxbury$REMODEL)
table(westroxbury$REMODEL)

# correlation between two variables
cor(westroxbury$TOTAL.VALUE, westroxbury$LOT.SQFT)


### data visualization ###

# using ggplot library

# ggplot histogram of total value
ggplot(data=westroxbury) + 
  geom_histogram(mapping=aes(x=TOTAL.VALUE))

# histogram with specified number of bins
ggplot(data=westroxbury) + 
  geom_histogram(mapping=aes(x=TOTAL.VALUE), bins=50)

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
sample.index <- sample(1:nrow(westroxbury), 200)
head(sample.index)
length(sample.index)
roxbury.sample <- westroxbury[sample.index,]
  # just like westroxbury[c(1,3,5),]
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


# make a box plot of Total Value vs Remodel type
ggplot(data=westroxbury) + 
  geom_boxplot(mapping=aes(x=REMODEL, y=TOTAL.VALUE))

# compute average value for each Remodel type
aggregate(westroxbury$TOTAL.VALUE, 
          by=list(westroxbury$REMODEL), 
          FUN=mean)

# violin plot of value vs number of floors
# (need to make FLOORS a factor for geom_violin to recognize it as categorical)
ggplot(data=westroxbury) + 
  geom_violin(mapping=aes(x=as.factor(FLOORS), y=TOTAL.VALUE))

# (version without factorizing FLOORS )
# ggplot(data=westroxbury) + 
#   geom_violin(mapping=aes(x=FLOORS, y=TOTAL.VALUE))

# compare data types
summary(westroxbury$FLOORS)
summary(as.factor(westroxbury$FLOORS))
is.numeric(westroxbury$FLOORS)
is.numeric(as.factor(westroxbury$FLOORS))
typeof(as.factor(westroxbury$FLOORS))

# try with "REMODEL" variable instead
ggplot(data=westroxbury) + 
  geom_violin(mapping=aes(x=REMODEL, y=TOTAL.VALUE))

# why is this different?
is.factor(westroxbury$REMODEL)
typeof(westroxbury$REMODEL)
is.numeric(westroxbury$REMODEL)
is.numeric(westroxbury$FLOORS)

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
# (as opposed to ggplot2)
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
is.numeric(westroxbury$BEDROOMS)

# make BEDROOMS variable a factor, and try again
westroxbury$BEDROOMS <- as.factor(westroxbury$BEDROOMS)
is.factor(westroxbury$BEDROOMS)
is.numeric(westroxbury$BEDROOMS)
summary(westroxbury$BEDROOMS)

ggplot(data=westroxbury) + 
  geom_bar(mapping=aes(x=REMODEL, fill=BEDROOMS))

# one more way to view the same data
ggplot(data=westroxbury) + 
  geom_bar(mapping=aes(x=REMODEL, fill=BEDROOMS),
           position="dodge")

# simple heatmap of correlations between numeric variables
# (all the variables except REMODEL and BEDROOMS are numeric)
heatmap(
  cor(
    subset(westroxbury,select=-c(REMODEL,BEDROOMS))
    )
  )

# plot combinations of variables (choose just 3 variables to keep it reasonable)
# ggpairs is from 'GGally' library
ggpairs(roxbury.sample[,c("TOTAL.VALUE", "LIVING.AREA", "LOT.SQFT")])

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
  ggtitle("Home Value vs Living Area")

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
p

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
# (from library 'ggthemes')

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
x.vector <- c(0,1,2,3,4,5,6)
y.vector <- c(5,0,3,2,4,1,6)
ggplot() + 
  geom_point(mapping=aes(x=x.vector, y=y.vector))

# same data in a line graph
ggplot() + 
  geom_line(mapping=aes(x=x.vector, y=y.vector))


# curve fit example using geom_smooth
ggplot(data=roxbury.sample) + 
  geom_point(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE)) +
  geom_smooth(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE),
              method=lm,
              formula=y ~ x)

# polynomial curve fit
ggplot(data=roxbury.sample) + 
  geom_point(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE)) +
  geom_smooth(mapping=aes(x=LIVING.AREA, y=TOTAL.VALUE),
              method=lm,
              formula=y ~ poly(x,2))

