# initialize data
docs <- c("this pizza sucks","this pizza is awesome","the pizza is great, the breadsticks are really great","great service!")

# construct a corpus
corp <- Corpus(VectorSource(docs))
corp

# construct a Term-Document Matrix
tdm <- TermDocumentMatrix(corp)
inspect(tdm)

# tokenization: remove punctuation, whitespace, 
# generally attempt to split into discrete 'tokens' aka words
corp.tok <- tm_map(corp, stripWhitespace)
corp.tok <- tm_map(corp.tok, removePunctuation)

# create a Term-Document Matrix from the tokenized corpus
tdm.tok <- TermDocumentMatrix(corp.tok)
inspect(tdm.tok)

corp.nostopwords <- tm_map(corp.tok, removeWords, stopwords("english"))
tdm.nostopwords <- TermDocumentMatrix(corp.nostopwords)

inspect(tdm.nostopwords)

tfidf <- weightTfIdf(tdm.nostopwords, normalize = TRUE)
inspect(tfidf)

