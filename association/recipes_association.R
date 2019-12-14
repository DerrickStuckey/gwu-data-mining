library(tidyverse)
library(recommenderlab)

# all data from https://www.kaggle.com/shuyangli94/food-com-recipes-and-user-interactions

recipes <- read_csv("./data/food-com-recipes-and-user-interactions/RAW_recipes.csv")
recipes

interactions.train <- read_csv("./data/food-com-recipes-and-user-interactions/interactions_train.csv")
interactions.train

# TODO do we really need a train/test split?

# how many reviews per user do we have
reviews.per.user <- interactions.train %>% 
  group_by(user_id) %>%
  summarise(count=n())

summary(reviews.per.user$count)
nrow(reviews.per.user)

ggplot() + geom_histogram(mapping = aes(x = reviews.per.user$count))
ggplot() + geom_histogram(mapping = aes(x = reviews.per.user$count)) + xlim(0,1000)

# reviews per recipe
reviews.per.recipe <- interactions.train %>% 
  group_by(recipe_id) %>%
  summarise(review.count=n())

reviews.per.recipe
summary(reviews.per.recipe$review.count)
nrow(reviews.per.recipe)

ggplot() + geom_histogram(mapping = aes(x=reviews.per.recipe$review.count))
ggplot() + geom_histogram(mapping = aes(x=reviews.per.recipe$review.count)) + xlim(0,400)

# get top N recipes by review count
top.recipes <- 
  reviews.per.recipe %>% 
  arrange(desc(review.count)) %>%
  top_n(10)

top.recipes

# merge with reviews per recipe, select only the top 100 recipes
interactions.train.2 <- 
  interactions.train %>%
  inner_join(top.recipes, by=c("recipe_id"="recipe_id"))

interactions.train.2

interactions.train.2 %>% 
  summarise(
    unique_users = n_distinct(user_id),
    unique_recipes = n_distinct(recipe_id)
  )

# select only N users to reduce computation
N <- 1000
all.users <- unique(interactions.train.2$user_id)
selected.users <- all.users[sample(length(all.users),N)]
length(selected.users)

interactions.train.small <-
  interactions.train.2[interactions.train.2$user_id %in% selected.users,]

# verify the numbe of unique users and unique recipes in the selected subset
interactions.train.small %>% 
  summarise(
    unique_users = n_distinct(user_id),
    unique_recipes = n_distinct(recipe_id)
  )

# make user_id and recipe_id factors
# TODO is this necessary?
# interactions.train.small$user_id <- as.factor(interactions.train.tmp$user_id)
# interactions.train.small$recipe_id <- as.factor(interactions.train.tmp$recipe_id)

# convert to a matrix
interactions.train.df <- 
  interactions.train.small %>%
  select(user_id, recipe_id, rating) %>%
  spread(recipe_id, rating)

dim(interactions.train.df)
interactions.train.df

# make 'user_id' the row names rather than an actual column
# row.names(interactions.train.df) <- interactions.train.df$user_id
interactions.train.df <- 
  interactions.train.df %>% 
  remove_rownames %>%
  column_to_rownames(var = "user_id")

dim(interactions.train.df)
head(interactions.train.df)

interactions.train.matrix <-
  interactions.train.df %>%
  data.matrix()

# convert to a realRatingMatrix
ratingmatrix.train <- as(interactions.train.matrix, "realRatingMatrix")
ratingmatrix.train

# item-based collaborative filtering recommendations
recipes.rec <- Recommender(ratingmatrix.train, "IBCF")
pred <- predict(recipes.rec, ratingmatrix.train, type="ratings")
View(as(pred,"matrix"))

# merge transactions data with recipes and select key columns
# interactions.train.2 <-
#   interactions.train %>%
#   select(user_id, recipe_id, rating) %>%
#   left_join(
#     select(recipes,name,id),
#     by=c("recipe_id"="id"))

# look at the details for the historical ratings for an individual user
user.26719.ratings <-
  interactions.train.2 %>% 
  filter(user_id == 26719) %>%
  left_join(recipes, by=c("recipe_id"="id"))

View(user.26719.ratings)

# obtain top 3 recommended recipes for user_id '26719'
recommended.items.26719.raw <- predict(recipes.rec, ratingmatrix.train["26719",], n=3)
recommended.items.26719.raw
recommended.items.26719 <- 
  as(recommended.items.26719.raw, "list")
recommended.items.26719.vector <- recommended.items.26719[[1]]

# look at the details for their recommendations
user.26719.recommendations <- 
  recipes %>%
  filter(id %in% recommended.items.26719.vector)
user.26719.recommendations

# compare the user's ratings and their generated recommendations
user.26719.ratings %>%
  select(rating,name,minutes,contributor_id)
user.26719.recommendations %>%
  select(name,minutes,contributor_id)

# out of this possible set of recipes
top.recipes %>%
  left_join(recipes, by=c("recipe_id" = "id")) %>%
  select(recipe_id, name)

# item-based collaborative filtering recommendations
recipes.rec <- Recommender(ratingmatrix.train, "IBCF")
pred <- predict(recipes.rec, ratingmatrix.train, type="ratings")
View(as(pred,"matrix"))

