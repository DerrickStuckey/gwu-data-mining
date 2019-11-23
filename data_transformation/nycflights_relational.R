# install.packages("tidyverse")
# install.packages("nycflights13")

# modified version of example code in "R for Data Science"
# https://r4ds.had.co.nz/relational-data.html

library(tidyverse)
library(nycflights13)

# check out the available data tables individually
flights
airlines
airports
planes
weather

# pull out a few specific columns from "flights"
flights2 <- flights %>% 
  select(year:day, hour, origin, dest, tailnum, arr_delay, carrier)

flights2

# join with the "airlines" table
flights3 <- flights2 %>%
  left_join(airlines, by = "carrier")
flights3

# make the variable name more obvious
airlines2 <- rename(airlines, airline_name = name)

# who flies to Dulles the most?
flights %>%
  filter(dest == "IAD") %>%
  left_join(airlines2, by = "carrier") %>%
  group_by(airline_name) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

# which airport is toughest to get out of on time?
flights %>%
  left_join(airports, by=c("origin"="faa")) %>%
  group_by(name) %>%
  mutate(delayed = dep_delay>0) %>%
  summarise(
    avg_delay = mean(dep_delay, na.rm=TRUE),
    pct_delayed = mean(delayed, na.rm=TRUE)
  )

