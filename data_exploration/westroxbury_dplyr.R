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

# tax rate



## pipe operator: %>%

