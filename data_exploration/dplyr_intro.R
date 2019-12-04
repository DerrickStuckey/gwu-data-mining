# install.packages("nycflights13")
# install.packages("tidyverse")

library(nycflights13)
library(tidyverse)

?flights

# select only flights on January 1 
jan1 <- filter(flights, month == 1, day == 1)
jan1

# sort by year, month, and then day
arrange(flights, year, month, day)

# sort by arrival delay, descending
arrange(flights, desc(arr_delay))

# select a few columns by name
select(flights, year, month, day)

# select a range of columns
select(flights, year:day)

# select all but a few columns
select(flights, -(year:day))

# rename a column
rename(flights, arrival_time = arr_time)

# add a variable with 'mutate'
mutate(flights, gain = arr_delay - dep_delay)

# perform several transformations at once
delays <- flights %>% group_by(dest) %>% 
  summarize(count=n(),
  dist=mean(distance, na.rm=TRUE),
  delay=mean(arr_delay, na.rm=TRUE)) %>%
  filter(count > 20, dest != "HNL")
delays

# plot the distribution of delays
ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_histogram()


