# install.packages("tidyverse")
# install.packages("nycflights13")

# modified version of example code in "R for Data Science"
# https://r4ds.had.co.nz

library(tidyverse)
library(nycflights13)

# what is this weird %>% thing?
flights
filter(flights, month == 1, day == 2)
flights %>% filter(month == 1, day == 2)

# what is going on here?
sum(select(filter(flights, month == 1, day ==2), arr_delay), na.rm = TRUE)

# ...better, but still pretty rough
sum(
  select(
    filter(flights, month == 1, day ==2),
    arr_delay
    ),
  na.rm = TRUE
  )

# that's more like it!
flights %>% 
  filter(month == 1, day == 2) %>% 
  select(arr_delay) %>%
  sum(na.rm = TRUE)


