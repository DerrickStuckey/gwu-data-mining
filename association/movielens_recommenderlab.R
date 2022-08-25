library(tidyverse)
library(recommenderlab)

# data from https://grouplens.org/datasets/movielens/latest/

# user/movie ratings
ratings <- read_csv("data/ml-latest-small/ratings.csv")
ratings

# movie metadata
movies <- read_csv("data/ml-latest-small/movies.csv")
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
num.movies <- 200
top.movies <- 
  reviews.per.movie %>% 
  arrange(desc(review.count)) %>%
  top_n(num.movies)

top.movies
ggplot() + geom_histogram(mapping = aes(x = top.movies$review.count))

# ratings for only the top N most-reviewed movies
# "inner join" - only include entries if both tables match on the specified key
head(ratings)
head(top.movies)
ratings.top <- 
  ratings %>%
  inner_join(top.movies, by=c("movieId"))

ratings.top

# verify the number of unique users and unique movies in the selected subset
ratings.top %>% 
  summarise(
    unique_users = n_distinct(userId),
    unique_movies = n_distinct(movieId)
  )

# check distribution of ratings within our selected set
ggplot() + 
  geom_histogram(mapping = aes(x=ratings.top$rating))

# convert to a "wide-format" dataframe
# with userId for rows, movieId for columns, and the rating value in each cell
ratings.wide <- 
  ratings.top %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating)

# what did this do?
dim(ratings)
dim(ratings.wide)
ratings
ratings.wide


# make 'userId' the row names rather than an actual column
# row.names(ratings.wide) <- ratings.wide$userId
ratings.wide <- 
  ratings.wide %>% 
  remove_rownames %>%
  column_to_rownames(var = "userId")

dim(ratings.wide)
ratings.wide[1:5,1:5]
# now every row represents a user, every column represents a movie,
# every cell represents that user's rating for that movie

# convert our wide training dataframe to a matrix
ratings.matrix <-
  ratings.wide %>%
  data.matrix()

# convert to a realRatingMatrix
ratingmatrix.train <- as(ratings.matrix, "realRatingMatrix")
ratingmatrix.train

# item-based collaborative filtering recommendations
# from 'recommenderlab' library
movies.rec <- Recommender(ratingmatrix.train, "UBCF") # user-based recommendations
# movies.rec <- Recommender(ratingmatrix.train, "IBCF") # item-based recommendations

# method 'Correlation' rather than the default 'Cosine':
# movies.rec <- Recommender(ratingmatrix.train, "UBCF",
#                           parameter=list("method"="Correlation"))

# show the model settings:
getModel(movies.rec)

# obtain actual predictions
pred <- predict(movies.rec, ratingmatrix.train, type="ratings")
View(as(pred,"matrix"))




# look at the details for the historical ratings for an individual user
# (for top N movies)
# good user id examples: 4, 13, 14
selected.user.id <- 4
selected.user.ratings <-
  ratings.top %>% 
  filter(userId == selected.user.id) %>%
  left_join(movies, by=c("movieId"))

selected.user.ratings
selected.user.ratings %>%
  select(rating, title)

# obtain top 5 recommended movies for the selected user
recommended.items.sel.user.raw <- predict(movies.rec, ratingmatrix.train[selected.user.id,], n=5)
recommended.items.sel.user.raw

# cast to a list and then to a vector
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


