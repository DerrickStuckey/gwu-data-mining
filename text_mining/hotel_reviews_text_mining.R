library(tidyverse)
library(tm)

# from https://www.kaggle.com/datafiniti/hotel-reviews
hotel.reviews <- read_csv("./data/hotel-reviews/Datafiniti_Hotel_Reviews.csv")

dim(hotel.reviews)

# right now we only care about the review text
head(hotel.reviews$reviews.text)

# construct a corpus
corp <- Corpus(VectorSource(hotel.reviews$reviews.text))

# construct a Term-Document Matrix
tdm <- TermDocumentMatrix(corp)
inspect(tdm)

# terms that occur at least 20% of the time
findFreqTerms(tdm, nrow(hotel.reviews) * 0.20)

# terms that occur at least 10% of the time
findFreqTerms(tdm, nrow(hotel.reviews) * 0.10)

# highly associated terms
findAssocs(tdm, "breakfast", 0.1)
findAssocs(tdm, "location", 0.1)


### data cleaning techniques ###

# tokenize the corpus
corp.tok <- tm_map(corp, stripWhitespace)
corp.tok <- tm_map(corp.tok, removePunctuation)

# compare the two corpuses
corp
corp.tok

# create a Term-Document Matrix from the tokenized corpus
tdm.tok <- TermDocumentMatrix(corp.tok)

# compare the new TDM with the previous one
tdm
tdm.tok
findAssocs(tdm, "breakfast", 0.1)
findAssocs(tdm.tok, "breakfast", 0.1)


# try removing 'stopwords' aka very common words
stopwords("english")
corp.nostopwords <- tm_map(corp.tok, removeWords, stopwords("english"))
tdm.nostopwords <- TermDocumentMatrix(corp.nostopwords)

findFreqTerms(tdm.tok, nrow(hotel.reviews) * 0.20)
findFreqTerms(tdm.nostopwords, nrow(hotel.reviews) * 0.20)


# TF-IDF weighting


