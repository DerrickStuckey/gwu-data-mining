# install.packages("tm")
# install.packages("lsa")

library(tidyverse)
library(forecast) # for accuracy() function
library(tm) # general text mining tools
library(lsa) # concept extraction
library(caret) # for confusionMatrix()

# from https://www.kaggle.com/zynicide/wine-reviews
wine.reviews <- read_csv("./data/wine-reviews/winemag-data_first150k.csv")

dim(wine.reviews)
wine.reviews

# right now we only care about the review text: the 'description' column
head(wine.reviews$description)

# work with just a sample of data, as some steps can take a while on large data
sample.size <- 1000
# sample.size <- 10
set.seed(12345)
# sample.idx <- seq(1,1000,1)
sample.idx <- sample(1:nrow(wine.reviews),sample.size)
reviews.sample <- wine.reviews[sample.idx,]
# reviews.sample <- wine.reviews[1:1000,]

head(reviews.sample$description)

### Basic text mining data transformations ###

# construct a corpus
corp <- VCorpus(VectorSource(reviews.sample$description))
corp

# construct a Term-Document Matrix
tdm <- TermDocumentMatrix(corp)
inspect(tdm)

# terms that occur at least 20% of the time
nrow(reviews.sample) * 0.20
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
corp.tok <- tm_map(corp.tok, content_transformer(tolower))

# create a Term-Document Matrix from the tokenized corpus
tdm.tok <- TermDocumentMatrix(corp.tok)

# compare the new TDM with the previous one
inspect(tdm)
inspect(tdm.tok)
# what is different?

# compare highly associated terms for the original and tokenized versions
findAssocs(tdm, "spice", 0.1)$spice[1:10]
findAssocs(tdm.tok, "spice", 0.1)$spice[1:10]


# try removing 'stopwords' aka very common words
# using stopwords() function from 'tm' package
stopwords(kind="en") %>% head()
corp.nostopwords <- tm_map(corp.tok, removeWords, stopwords(kind="en"))
tdm.nostopwords <- TermDocumentMatrix(corp.nostopwords)

inspect(tdm.tok)
inspect(tdm.nostopwords)

# compare terms that appear at least 20% of the time
# before and after removing stopwords
findFreqTerms(tdm.tok, nrow(reviews.sample) * 0.20)
findFreqTerms(tdm.nostopwords, nrow(reviews.sample) * 0.20)

# stemming (e.g. "running" -> "run")
corp.stemmed <- tm_map(corp.nostopwords, stemDocument)
tdm.stemmed <- TermDocumentMatrix(corp.stemmed)
inspect(tdm.stemmed)

# compare terms that appear at least 10% of the time
# before and after stemming
findFreqTerms(tdm.nostopwords, nrow(reviews.sample) * 0.20)
findFreqTerms(tdm.stemmed, nrow(reviews.sample) * 0.20)

# we can also drop infrequent terms if we want to
# (in this case, let's keep them)
tdm.unsparse <- removeSparseTerms(tdm.stemmed,0.999)
tdm.stemmed
tdm.unsparse

# TF-IDF weighting
# tfidf <- weightTfIdf(tdm.nostopwords)
tfidf <- weightTfIdf(tdm.stemmed)
tfidf
inspect(tfidf)
# how is this different from a basic TDM?
inspect(tdm.stemmed)

findAssocs(tdm.nostopwords, "spice", 0.1)$spice[1:10]
findAssocs(tfidf, "spice", 0.1)$spice[1:10]
dim(tfidf)


### Concept extraction ###

# extract 10 "concepts" (lsa library)
lsa.tfidf <- lsa(tfidf, dim=10)

# look at the words associated with each concept
View(lsa.tfidf$tk)

# look at the top 10 terms for each concept
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
# attempt to predict the rating, using our 10 concepts as predictors
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

# which concepts are most associated with a high rating?
View(concepts.top.terms[,c('Concept 1','Concept 6','Concept 9')])
# with a low rating?
View(concepts.top.terms[,c('Concept 1','Concept 9','Concept 10')])

# how does the model perform on the holdout set?
points.preds <- predict(points.lm, newdata=test.data)
accuracy(points.preds, test.data$points)

# plot actual vs. predicted points
ggplot() +
  geom_point(mapping = aes(x=points.preds, y=test.data$points))

ggplot() +
  geom_jitter(mapping = aes(x=points.preds, y=test.data$points))


# predict whether the wine is Italian
document.concepts <-
  document.concepts %>%
  select(-points)
head(document.concepts)
document.concepts$Italian <- reviews.sample$country=="Italy"
table(document.concepts$Italian)

# re-create the training and test data (same indexes)
train.data <- document.concepts[train.idx,]
test.data <- document.concepts[-train.idx,]

# Logistic Regression to predict whether the wine is Italian
italy.logistic <- glm(Italian ~ ., data=train.data, family='binomial')
summary(italy.logistic)

# obtain test predictions
test.probs <- predict(italy.logistic, newdata=test.data, type="response")
summary(test.probs)

# confusion matrix
test.preds <- test.probs > 0.5
confusionMatrix(as.factor(test.preds), as.factor(test.data$Italian))

# ROC curve
library(plotROC)
ggplot(mapping = aes(m = test.probs, d = test.data$Italian)) + 
  geom_roc(n.cuts=0,labels=FALSE) + 
  style_roc(theme = theme_grey)

# what did the model actually find?
summary(italy.logistic)
View(concepts.top.terms[,c('Concept 2','Concept 4')])



