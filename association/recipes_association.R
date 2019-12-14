library(tidyverse)

# all data from https://www.kaggle.com/shuyangli94/food-com-recipes-and-user-interactions

recipes <- read_csv("./data/food-com-recipes-and-user-interactions/RAW_recipes.csv")
recipes

interactions.train <- read_csv("./data/food-com-recipes-and-user-interactions/interactions_train.csv")
interactions.train

# TODO do we really need a train/test split?

# how many reviews per user do we have
# repeat reviewers only
reviews.per.user <- interactions.train %>% 
  group_by(user_id) %>%
  summarise(count=n()) %>%
  filter(count > 1)

summary(reviews.per.user$count)

ggplot() + geom_histogram(mapping = aes(x=reviews.per.user$count))
ggplot() + geom_histogram(mapping = aes(x=reviews.per.user$count)) + xlim(0,1000)


# merge transactions data with recipes and select key columns
interactions.train.2 <-
  interactions.train %>%
  select(user_id, recipe_id, rating) %>%
  left_join(
    select(recipes,name,id),
    by=c("recipe_id"="id"))

interactions.train.2


