# install.packages("tidyverse")
# install.packages("nycflights13")

# modified version of example code in "R for Data Science"
# https://r4ds.had.co.nz

library(tidyverse)
library(nycflights13)

flights
filter(flights, month == 1, day == 1)
flights %>% filter(month == 1, day == 1)
