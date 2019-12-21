# install.packages("tm")
# install.packages("lsa")

library(tidyverse)
library(tm)
library(lsa)

# from https://www.kaggle.com/zynicide/wine-reviews
wine.reviews <- read_csv("./data/wine-reviews/winemag-data_first150k.csv")

dim(wine.reviews)
wine.reviews

# right now we only care about the review text
head(wine.reviews$description)

# work with just a sample of data, as some steps can take a while on large data
sample.size <- 1000
set.seed(12345)
sample.idx <- sample(1:nrow(wine.reviews),sample.size)
reviews.sample <- wine.reviews[sample.idx,]


### Basic text mining data transformations ###

# construct a corpus
corp <- Corpus(VectorSource(reviews.sample$description))

# construct a Term-Document Matrix
tdm <- TermDocumentMatrix(corp)
inspect(tdm)

# terms that occur at least 20% of the time
findFreqTerms(tdm, nrow(reviews.sample) * 0.20)

# terms that occur at least 10% of the time
findFreqTerms(tdm, nrow(reviews.sample) * 0.10)

# highly associated terms
findAssocs(tdm, "spice", 0.2)
findAssocs(tdm, "oak", 0.2)


### more data cleaning/preparation ###

# tokenization: remove punctuation, whitespace, 
# generally attempt to split into discrete 'tokens' aka words
corp.tok <- tm_map(corp, stripWhitespace)
corp.tok <- tm_map(corp.tok, removePunctuation)

# create a Term-Document Matrix from the tokenized corpus
tdm.tok <- TermDocumentMatrix(corp.tok)

# compare the new TDM with the previous one
tdm
tdm.tok
# what is different?

# compare highly associated terms for the original and tokenized versions
findAssocs(tdm, "spice", 0.1)$spice[1:10]
findAssocs(tdm.tok, "spice", 0.1)$spice[1:10]


# try removing 'stopwords' aka very common words
stopwords("english")
corp.nostopwords <- tm_map(corp.tok, removeWords, stopwords("english"))
tdm.nostopwords <- TermDocumentMatrix(corp.nostopwords)
inspect(tdm.nostopwords)

# compare terms that appear at least 20% of the time
# before and after removing stopwords
findFreqTerms(tdm.tok, nrow(reviews.sample) * 0.20)
findFreqTerms(tdm.nostopwords, nrow(reviews.sample) * 0.20)
# TODO why is 'the' still in here?

# stemming (e.g. "running" -> "run")
corp.stemmed <- tm_map(corp.nostopwords, stemDocument)
tdm.stemmed <- TermDocumentMatrix(corp.stemmed)

# compare terms that appear at least 10% of the time
# before and after stemming
findFreqTerms(tdm.nostopwords, nrow(reviews.sample) * 0.20)
findFreqTerms(tdm.stemmed, nrow(reviews.sample) * 0.20)


# TODO try also dropping infrequent terms
# tdm.unsparse <- removeSparseTerms(tdm.stemmed,0.999)
# tdm.stemmed
# tdm.unsparse


# TF-IDF weighting
tfidf <- weightTfIdf(tdm.nostopwords)
# tfidf <- weightTfIdf(tdm.unsparse)
tfidf
inspect(tfidf)
# how is this different from a basic TDM?

findAssocs(tdm.nostopwords, "spice", 0.1)$spice[1:10]
findAssocs(tfidf, "spice", 0.1)$spice[1:10]



### Concept extraction ###

# extract 10 "concepts" (lsa library)
lsa.tfidf <- lsa(tfidf, dim=10)

# look at the words associated with each concept
View(lsa.tfidf$tk)

# top 10 terms for each concept
# TODO is there a more elegant way to do this?
concepts.top.terms <- data.frame("rank"=1:10)
for (j in 1:ncol(lsa.tfidf$tk)) {
  top.terms <- row.names(lsa.tfidf$tk)[
    order(lsa.tfidf$tk[,j], decreasing = TRUE)
    ][1:10]
  concepts.top.terms <- cbind(concepts.top.terms, top.terms)
  names(concepts.top.terms)[length(names(concepts.top.terms))] <- paste("Concept",j)
}
View(concepts.top.terms)

# there is also a value for each concept for each document
document.concepts <- as.data.frame(as.matrix(lsa.tfidf$dk))
dim(document.concepts)
head(document.concepts)


### Build a predictive Model ###


# set up the data to train and test on
# attempt to predict the rating
document.concepts$points <- reviews.sample$points
head(document.concepts)

# train/test split
train.prop <- 0.7
set.seed(12345)
train.idx <- sample(1:nrow(document.concepts),nrow(document.concepts)*train.prop)
train.data <- document.concepts[train.idx,]
test.data <- document.concepts[-train.idx,]

# wait a minute
# is it OK that we did all our text feature extraction on the training and test data combined?
# depends on the use case

# try a linear regression model
points.lm <- lm(points ~ .,data=train.data)
summary(points.lm)

# how does the model perform on the holdout set?
points.preds <- predict(points.lm, newdata=test.data)
cor(points.preds, test.data$points)^2

ggplot() +
  geom_point(mapping = aes(x=points.preds, y=test.data$points))

ggplot() +
  geom_jitter(mapping = aes(x=points.preds, y=test.data$points))


# predict whether the wine is Italian
document.concepts <-
  document.concepts %>%
  select(-points)
document.concepts$Italian <- reviews.sample$country=="Italy"

# re-create the training and test data (same indexes)
train.data <- document.concepts[train.idx,]
test.data <- document.concepts[-train.idx,]

# Logistic Regression to predict whether the wine is Italian
italy.logistic <- glm(Italian ~ ., data=train.data, family='binomial')
summary(italy.logistic)

# obtain test predictions
test.probs <- predict(italy.logistic, newdata=test.data)
summary(test.probs)

# ROC curve
library(plotROC)
ggplot(mapping = aes(m = test.probs, d = test.data$Italian)) + 
  geom_roc(n.cuts=0,labels=FALSE) + 
  style_roc(theme = theme_grey)

# what did the model actually find?
summary(italy.logistic)
View(concepts.top.terms[,c('Concept 3','Concept 6','Concept 8')])


