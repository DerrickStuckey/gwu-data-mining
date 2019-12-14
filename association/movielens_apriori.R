library(tidyverse)

# data from https://grouplens.org/datasets/movielens/latest/

ratings <- read_csv("data/ml-latest-small/ratings.csv")
ratings

movies <- read_csv("data/ml-latest-small/movies.csv")
movies

# don't worry about filtering out infrequently viewed movies
# the apriori algorithm takes care of these naturally

# convert the movie ratings data to binary
ratings$viewed <- ifelse(!is.na(ratings$rating),1,0)

# drop movies with duplicate titles
movies.deduped <- movies[!duplicated(movies$title),]
# TODO make the duplicate titles unique and add them back in

# use movie titles rather than movie ids
ratings.titles <- 
  ratings %>% 
  left_join(movies.deduped,by="movieId")

# verify no duplicate entries
ratings.titles %>%
  summarise(
    unique_titles = n_distinct(title),
    unique_movieIds = n_distinct(title)
  )

# convert to a "wide-format" dataframe
# with userId for rows, title for columns, and the rating value in each cell
ratings.wide <- 
  ratings.titles %>%
  select(userId, title, viewed) %>%
  spread(title, viewed, fill=0)
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

# convert the binary matrix to a transactions database
ratings.transactions <- as(ratings.matrix, "transactions")
ratings.transactions

itemFrequency(ratings.transactions)

# run apriori algorithm to find frequent itemsets
rules <- apriori(ratings.transactions,
                 parameter = list(supp=0.1, conf = 0.75, maxlen = 3, target = "rules"))
rules

# see what rules were generated
inspect(
  sort(rules, by = "lift")
  )

