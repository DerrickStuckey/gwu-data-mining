library(tidyverse)
library(recommenderlab)

# all data from https://www.kaggle.com/shuyangli94/food-com-recipes-and-user-interactions

recipes <- read_csv("./data/food-com-recipes-and-user-interactions/RAW_recipes.csv")
recipes

interactions.train <- read_csv("./data/food-com-recipes-and-user-interactions/interactions_train.csv")
interactions.train

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
num.recipes <- 100
top.recipes <- 
  reviews.per.recipe %>% 
  arrange(desc(review.count)) %>%
  top_n(num.recipes)

top.recipes

# merge with reviews per recipe, select only the top 100 recipes
interactions.train.top <- 
  interactions.train %>%
  inner_join(top.recipes, by=c("recipe_id"="recipe_id"))

interactions.train.top

interactions.train.top %>% 
  summarise(
    unique_users = n_distinct(user_id),
    unique_recipes = n_distinct(recipe_id)
  )

# select only N users to reduce computation
N <- 1000
set.seed(12345)
all.users <- unique(interactions.train.top$user_id)
selected.users <- all.users[sample(length(all.users),N)]
length(selected.users)

interactions.train.small <-
  interactions.train.top[interactions.train.top$user_id %in% selected.users,]

# verify the numbe of unique users and unique recipes in the selected subset
interactions.train.small %>% 
  summarise(
    unique_users = n_distinct(user_id),
    unique_recipes = n_distinct(recipe_id)
  )

# convert to a "wide-format" dataframe
# with user_id for rows, recipe_id for columns, and the rating value in each cell
interactions.train.wide <- 
  interactions.train.small %>%
  select(user_id, recipe_id, rating) %>%
  spread(recipe_id, rating)

# what did we just do?
dim(interactions.train.top)
dim(interactions.train.wide)
interactions.train.small
interactions.train.wide

# make 'user_id' the row names rather than an actual column
# row.names(interactions.train.wide) <- interactions.train.wide$user_id
interactions.train.wide <- 
  interactions.train.wide %>% 
  remove_rownames %>%
  column_to_rownames(var = "user_id")

dim(interactions.train.wide)
head(interactions.train.wide)

# convert our wide training dataframe to a matrix
interactions.train.matrix <-
  interactions.train.wide %>%
  data.matrix()

# convert to a realRatingMatrix
ratingmatrix.train <- as(interactions.train.matrix, "realRatingMatrix")
ratingmatrix.train

# item-based collaborative filtering recommendations
recipes.rec <- Recommender(ratingmatrix.train, "IBCF")
pred <- predict(recipes.rec, ratingmatrix.train, type="ratings")
View(as(pred,"matrix"))

# look at the details for the historical ratings for an individual user
user.26075.ratings <-
  interactions.train.top %>% 
  filter(user_id == 26075) %>%
  left_join(recipes, by=c("recipe_id"="id"))

user.26075.ratings
user.26075.ratings %>%
  select(rating, name)

# obtain top 5 recommended recipes for user_id '26075'
recommended.items.26075.raw <- predict(recipes.rec, ratingmatrix.train["26075",], n=5)
recommended.items.26075.raw
recommended.items.26075 <- 
  as(recommended.items.26075.raw, "list")
recommended.items.26075
recommended.items.26075.vector <- recommended.items.26075[[1]]
recommended.items.26075.vector

# look at the recipe details for their recommended recipes
user.26075.recommendations <- 
  recipes %>%
  filter(id %in% recommended.items.26075.vector)

user.26075.recommendations
user.26075.recommendations %>% 
  select(name)

# compare the user's ratings and their generated recommendations
user.26075.ratings %>%
  select(rating,name,minutes,contributor_id)
user.26075.recommendations %>%
  select(name,minutes,contributor_id)

# out of this possible set of recipes
# top.recipes %>%
#   left_join(recipes, by=c("recipe_id" = "id")) %>%
#   select(recipe_id, name)


# do the same for a different user: '46759'
user.46759.ratings <-
  interactions.train.top %>% 
  filter(user_id == 46759) %>%
  left_join(recipes, by=c("recipe_id"="id"))

user.46759.ratings
user.46759.ratings %>%
  select(rating,name)

# obtain top 5 recommended recipes for user_id '46759'
recommended.items.46759.raw <- predict(recipes.rec, ratingmatrix.train["46759",], n=5)
recommended.items.46759.raw
recommended.items.46759 <- 
  as(recommended.items.46759.raw, "list")
recommended.items.46759
recommended.items.46759.vector <- recommended.items.46759[[1]]
recommended.items.46759.vector

# look at the recipe details for their recommended recipes
user.46759.recommendations <- 
  recipes %>%
  filter(id %in% recommended.items.46759.vector)

user.46759.recommendations %>% 
  select(name)

# compare the user's ratings and their generated recommendations
user.46759.ratings %>%
  select(rating,name,minutes,contributor_id)
user.46759.recommendations %>%
  select(name,minutes,contributor_id)




# user-based collaborative filtering recommendations
recipes.rec <- Recommender(ratingmatrix.train, "UBCF")
pred <- predict(recipes.rec, ratingmatrix.train, type="ratings")
dim(as(pred,"matrix"))
View(as(pred,"matrix"))
# TODO seems like nonsense, some users have the same rating across the board
