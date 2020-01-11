# Data transformation examples using dplyr and tidyverse

# load "tidyverse" library
# aka all the various libraries used in "R for Data Science"
# e.g. "readr", "dplyr", "magrittr"
library(tidyverse)

# from "Data Mining for Business Analytics"
# https://www.dataminingbook.com/book/r-edition
# use read_csv() function from "readr" library
westroxbury.tibble <- read_csv("./data/WestRoxbury.csv")

westroxbury.tibble
# what is a tibble?

# compare with a regular dataframe:
westroxbury.dataframe <- read.csv("./data/WestRoxbury.csv")
westroxbury.dataframe

westroxbury.dataframe
westroxbury.tibble

names(westroxbury.dataframe)
names(westroxbury.tibble)

summary(westroxbury.dataframe$TOTAL.VALUE)
summary(westroxbury.tibble$`TOTAL VALUE`)

# let's just use the tibble, and shorten the name
westroxbury <- westroxbury.tibble

westroxbury


### data transformations using the "dplyr" package

## filtering

# filter by number of floors
single.floor.homes <- filter(westroxbury, FLOORS == 1)
single.floor.homes
table(westroxbury$FLOORS)
table(single.floor.homes$FLOORS)

# this is totally equivalent to:
single.floor.homes <- westroxbury[westroxbury$FLOORS==1,]
table(westroxbury$FLOORS)
table(single.floor.homes$FLOORS)

# multi-floor homes
multi.floor.homes <- filter(westroxbury, FLOORS > 1)
table(westroxbury$FLOORS)
table(multi.floor.homes$FLOORS)


## sorting

# sort the tibble using arrange() function
arrange(westroxbury, `TOTAL VALUE`)

# did we change the original tibble or make a new one?
westroxbury
westroxbury.sorted <- arrange(westroxbury, `TOTAL VALUE`)
westroxbury.sorted

# sort by value descending
arrange(westroxbury, desc(`TOTAL VALUE`))

# non-dplyr version
westroxbury.sorted <- westroxbury[order(westroxbury$`TOTAL VALUE`, decreasing = TRUE),]
westroxbury.sorted

## select columns

# select only kitchen and fireplace columns
kitchen.fireplace <- select(westroxbury, KITCHEN, FIREPLACE)
kitchen.fireplace
names(westroxbury)
names(kitchen.fireplace)

# select all but one column
all.but.one <- select(westroxbury, -`TOTAL VALUE`)
all.but.one
names(westroxbury)
names(all.but.one)

# select all but two columns
all.but.two <- select(westroxbury, -`TOTAL VALUE`, -TAX)
all.but.two
names(westroxbury)
names(all.but.two)

# equivalently:
all.but.two <- select(westroxbury, -c(`TOTAL VALUE`, TAX))
all.but.two
names(all.but.two)

## add new variables

# value per square foot
mutate(westroxbury, value.per.sqft = `TOTAL VALUE` / `LOT SQFT`)
names(westroxbury)

westroxbury.2 <- mutate(westroxbury, value.per.sqft = `TOTAL VALUE` / `LOT SQFT`)
names(westroxbury.2)
summary(westroxbury.2$value.per.sqft)

## data aggregation with "summarize"

# average square footage
summarize(westroxbury, avg.sq.ft = mean(`LOT SQFT`))
mean(westroxbury$`LOT SQFT`)

# note: the result of summarize is a tibble
# tibbles don't display all digits by default
# this is annoying, but we can deal with this a few ways:
x <- summarize(westroxbury, avg.sq.ft = mean(`LOT SQFT`))
x
as.data.frame(x)
as.numeric(x)
View(x)

# or by just not using tibbles in the first place
summarize(westroxbury.dataframe, avg.sq.ft = mean(LOT.SQFT))

# average square footage and living area
summarize(westroxbury, 
          avg.sq.ft = mean(`LOT SQFT`), 
          avg.living.area = mean(`LIVING AREA`))

# group by remodel
remodel.groups <- group_by(westroxbury, REMODEL)
summarise(remodel.groups,
          avg.sq.ft = mean(`LOT SQFT`), 
          avg.living.area = mean(`LIVING AREA`))

## pipe operator: %>%

# equivalent statements:
mean(westroxbury.2$`TOTAL VALUE`)
westroxbury.2$`TOTAL VALUE` %>% mean()

# the pipe just inserts the first argument
# you can still use additional arguments
mean(westroxbury.2$`TOTAL VALUE`, na.rm = TRUE)
westroxbury.2$`TOTAL VALUE` %>% mean(na.rm=TRUE)

# why use %>% ?
# it allows us to string statements together

# average value of houses with 1 floor
westroxbury %>%
  filter(FLOORS == 1) %>%
  summarize(avg.value = mean(`TOTAL VALUE`))

# equivalent without pipes:
summarize(
  filter(westroxbury, FLOORS == 1),
  avg.value = mean(`TOTAL VALUE`)
  )

# average value of houses recent remodeled houses, 
# grouped by number of baths
# ranked descending
westroxbury %>%
  filter(REMODEL == "Recent") %>%
  mutate(BATHS = `FULL BATH` + 0.5 * `HALF BATH`) %>%
  group_by(BATHS) %>%
  summarise(
    AVG.VALUE = mean(`TOTAL VALUE`)
  ) %>% 
  arrange(
    desc(AVG.VALUE)
  )
