library(tidyverse)
library(recommenderlab)

# data from https://grouplens.org/datasets/movielens/latest/

# ratings <- read_csv("data/ml-latest-small/ratings.csv")
ratings <- read_csv("data/ml-latest/ratings.csv")
ratings

# movies <- read_csv("data/ml-latest-small/movies.csv")
movies <- read_csv("data/ml-latest/movies.csv")
movies


# reviews per user
reviews.per.user <- ratings %>% 
  group_by(userId) %>%
  summarise(user.review.count=n())

summary(reviews.per.user)

# look at the distribution
ggplot() + geom_histogram(mapping = aes(x = reviews.per.user$user.review.count))


# reviews per movie
reviews.per.movie <- ratings %>% 
  group_by(movieId) %>%
  summarise(review.count=n())

# look at the distribution
ggplot() + geom_histogram(mapping = aes(x = reviews.per.movie$review.count))

# get top N movies by review count
num.movies <- 500
top.movies <- 
  reviews.per.movie %>% 
  arrange(desc(review.count)) %>%
  top_n(num.movies)

top.movies
ggplot() + geom_histogram(mapping = aes(x = top.movies$review.count))

# merge with reviews per movie, select only the top N movies
ratings.top <- 
  ratings %>%
  inner_join(top.movies, by=c("movieId"))

ratings.top

# take a sample of users to reduce the volume of data
num.users <- 100000
set.seed(12345)
all.users <- unique(ratings.top$userId)
length(all.users)
selected.users <- all.users[sample(length(all.users),num.users)]
length(selected.users)

# alternatively, select users with the most reviews
# selected.users <- (reviews.per.user %>% arrange(desc(user.review.count)) %>% top_n(num.users) %>% select(user_id))$user_id

# select only the ratings from the top users
ratings.train <-
  ratings.top[ratings.top$userId %in% selected.users,]


# verify the number of unique users and unique movies in the selected subset
ratings.train %>% 
  summarise(
    unique_users = n_distinct(userId),
    unique_movies = n_distinct(movieId)
  )

# check distribution of ratings within our selected set
ggplot() + 
  geom_histogram(mapping = aes(x=ratings.train$rating))

# convert to a "wide-format" dataframe
# with userId for rows, movieId for columns, and the rating value in each cell
ratings.wide <- 
  ratings.train %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating)

# what did this do?
dim(ratings)
dim(ratings.wide)
ratings
ratings.wide[1:5,1:5]


# make 'userId' the row names rather than an actual column
ratings.wide <- 
  ratings.wide %>% 
  remove_rownames %>%
  column_to_rownames(var = "userId")

dim(ratings.wide)
ratings.wide[1:5,1:5]

# convert our wide training dataframe to a matrix
ratings.matrix <-
  ratings.wide %>%
  data.matrix()

# convert to a realRatingMatrix
ratingmatrix.train <- as(ratings.matrix, "realRatingMatrix")
ratingmatrix.train

# item-based collaborative filtering recommendations

# movies.rec <- Recommender(ratingmatrix.train, "IBCF")
# save the recommender
# saveRDS(movies.rec, "./association/movielens_recommender_large.rds")

# load the already-saved recommender
movies.rec <- readRDS("./association/movielens_recommender_large.rds")


pred <- predict(movies.rec, ratingmatrix.train, type="ratings")
View(as(pred,"matrix"))




# look at the details for the historical ratings for an individual user
# good: 226840, 134002, 106219
selected.user.id <- selected.users[9]
selected.user.ratings <-
  ratings.top %>% 
  filter(userId == selected.user.id) %>%
  left_join(movies, by=c("movieId"))

selected.user.ratings
selected.user.ratings %>%
  select(rating, title)

# obtain top 5 recommended movies for the selected user
# recommended.items.sel.user.raw <- predict(movies.rec, ratingmatrix.train["26075",], n=5)
recommended.items.sel.user.raw <- predict(movies.rec, ratingmatrix.train[as.character(selected.user.id),], n=5)
recommended.items.sel.user.raw
recommended.items.sel.user <- 
  as(recommended.items.sel.user.raw, "list")
recommended.items.sel.user
recommended.items.sel.user.vector <- recommended.items.sel.user[[1]]
recommended.items.sel.user.vector

# look at the movie details for their recommended movies
selected.user.recommendations <- 
  movies %>%
  filter(movieId %in% recommended.items.sel.user.vector)

selected.user.recommendations

# compare the user's ratings and their generated recommendations
selected.user.ratings %>%
  select(rating,title,genres)
selected.user.recommendations %>%
  select(title,genres)

