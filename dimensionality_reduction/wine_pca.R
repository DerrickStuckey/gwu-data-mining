# PCA on wine chemical properties dataset

# data from https://archive.ics.uci.edu/ml/datasets/wine
wine <- read.csv("./data/wine.csv")

# for PCA plot
# install.packages("devtools")
library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)

library(tidyverse)

## PCA on 2 variables

# scatter plot original variables
ggplot(data=wine) + 
  geom_point(mapping = aes(x=Flavanoids, y=Total.phenols))

# with Cultivar
ggplot(data=wine) + 
  geom_point(mapping = aes(x=Flavanoids, y=Total.phenols,
                           color=Cultivar))

# covariance matrix for Flavanoids vs Total Phenols
cov(wine %>% select(Flavanoids, Total.phenols))

# covariance example with one more var
cov(wine %>% select(Flavanoids, Total.phenols, Hue))
# note: the Flavanoids vs Total Phenols components do not change

# PCA on just Flavanoids, Total Phenols
wine.pca.simple <- prcomp(wine %>% select(Flavanoids,Total.phenols), center = TRUE,scale. = TRUE)

# properties of the PCA solution
summary(wine.pca.simple)
wine.pca.simple$rotation
wine.pca.simple$center
wine.pca.simple$scale

# PC1 and PC2 values for first two data points
head(wine.pca.simple$x, n=2)
dim(wine.pca.simple$x)

# raw variable values for first data point
wine %>% select(Flavanoids,Total.phenols) %>% head(n=1)

# calculate PC1, PC2 for first data point manually
raw.values <- wine %>% select(Flavanoids,Total.phenols) %>% head(n=1)
raw.values
normalized.values <- (raw.values - wine.pca.simple$center) / wine.pca.simple$scale
normalized.values

as.matrix(normalized.values)
wine.pca.simple$rotation
as.matrix(normalized.values) %*% wine.pca.simple$rotation

# long version
pc.1 <- normalized.values$Flavanoids * wine.pca.simple$rotation[1,1] + 
  normalized.values$Total.phenols * wine.pca.simple$rotation[2,1]
pc.1

pc.2 <- normalized.values$Flavanoids * wine.pca.simple$rotation[1,2] + 
  normalized.values$Total.phenols * wine.pca.simple$rotation[2,2]
pc.2

head(wine.pca.simple$x, n=1)

# or google sheet version:
# https://docs.google.com/spreadsheets/d/1XSNlfNe5bNQnUoAw0-offQ5nf2g1jtP61BWYxa89ICQ/edit?usp=sharing


head(wine.pca.simple$x)
pc1 <- wine.pca.simple$x[,1]
pc2 <- wine.pca.simple$x[,2]

# project all points onto PC1
pc1.Flavanoids.component <- pc1 * wine.pca.simple$rotation[1,1] * wine.pca.simple$scale[1] + 
  wine.pca.simple$center[1]
pc1.Phenols.component <- pc1 * wine.pca.simple$rotation[2,1] * wine.pca.simple$scale[2] + 
  wine.pca.simple$center[2]


## Plots of PCA Projections

# plot original values only
ggplot(data=wine) + 
  geom_point(mapping = aes(x=Flavanoids, y=Total.phenols), 
             col="black") + 
  ggtitle("Original Values") + 
  xlab("Flavanoids") + ylab("Total Phenols")

# plot PC1 projection only
ggplot() + 
  geom_point(mapping = aes(x=pc1.Flavanoids.component, y=pc1.Phenols.component)) + 
  ggtitle("Principal Component 1 Projection") + 
  xlab("Flavanoids") + ylab("Total Phenols")

# plot PC1 projection along with original values
ggplot(data=wine) + 
  geom_point(mapping = aes(x=Flavanoids, y=Total.phenols), 
             col="black") +
  geom_point(mapping = aes(x=pc1.Flavanoids.component, y=pc1.Phenols.component),
             col="blue") + 
  ggtitle("Original Values + PC 1 Projection") + 
  xlab("Flavanoids") + ylab("Total Phenols")

# add PC2 projection to the mix
pc2.Flavanoids.component <- pc2 * wine.pca.simple$rotation[1,2] * wine.pca.simple$scale[1] + 
  wine.pca.simple$center[1]
pc2.Phenols.component <- pc2 * wine.pca.simple$rotation[2,2] * wine.pca.simple$scale[2] + 
  wine.pca.simple$center[2]

# plot original points with PC1 and PC2 projections
ggplot() + 
  geom_point(mapping = aes(x=wine$Flavanoids, y=wine$Total.phenols), 
             col="black") +
  geom_point(mapping = aes(x=pc1.Flavanoids.component, y=pc1.Phenols.component),
             col="blue") +
  geom_point(mapping = aes(x=pc2.Flavanoids.component, y=pc2.Phenols.component),
             col="red") + 
  theme(aspect.ratio=1) + 
  ggtitle("PC 1 and PC 2 Projections") + 
  xlab("Flavanoids") + ylab("Total Phenols")

# with legend
ggplot() + 
  geom_point(mapping = aes(x=wine$Flavanoids, y=wine$Total.phenols,
                           color="Original")) +
  geom_point(mapping = aes(x=pc1.Flavanoids.component, y=pc1.Phenols.component,
                           color="PC 1")) +
  geom_point(mapping = aes(x=pc2.Flavanoids.component, y=pc2.Phenols.component,
                           color="PC 2")) + 
  theme(aspect.ratio=1) + 
  ggtitle("PC 1 and PC 2 Projections") + 
  xlab("Flavanoids") + ylab("Total Phenols") + 
  labs(color="Projection")

# plot all points in PC1, PC2 basis, with PC1 and PC2 alone
ggplot() + 
  geom_point(mapping = aes(x=pc1, y=pc2), col="black") + 
  geom_point(mapping = aes(x=pc1, y=0), col="blue") + 
  geom_point(mapping = aes(x=0, y=pc2), col="red") + 
  theme(aspect.ratio=1) + 
  xlab("PC 1") + ylab("PC 2") + 
  ggtitle("PC 1 and PC 2 Projections in PC Basis")




## % of variance by PC
importance.variance <- summary(wine.pca.simple)$importance[2,]
importance.variance

# plot % of variance by PC
ggplot() +
  geom_bar(mapping = aes(x=reorder(names(importance.variance),
                                   -importance.variance),
                         y=importance.variance),
           stat="identity") + 
  xlab("component") + ylab("% of variance explained") + 
  ggtitle("Principal Component Importance (2 Vars)") + 
  ylim(0,1)

# plot original variables on PC1, PC2 axes
ggbiplot(wine.pca.simple) + 
  theme(aspect.ratio=1) + 
  ggtitle("Flavanoids, Phenols Projected on PC1, PC2")



## compare predictions for Cultivar using 2 vars vs 1 PC

# predict if Cultivar="A" using Flavanoids and Total Phenols
wine$Cultivar.A <- wine$Cultivar=="A"

set.seed(12358)
train.idx <- sample(1:nrow(wine),
                    0.7 * nrow(wine))
wine.train <- wine[train.idx,]
wine.test <- wine[-train.idx,]
cultivar.a.logistic <- glm(Cultivar.A ~ Flavanoids + Total.phenols, data = wine.train)
summary(cultivar.a.logistic)

test.pred.probabilities <- predict(cultivar.a.logistic, newdata = wine.test,
                                   type="response")
test.preds <- ifelse(test.pred.probabilities>0.5,"Pred True","Pred False")

confusion.matrix <- table(test.preds, wine.test$Cultivar.A)
confusion.matrix

# accuracy
(confusion.matrix[1,1] + confusion.matrix[2,2]) / sum(confusion.matrix)
# 0.8518519

# predict Cultivar.A using only PC1
wine.train$pc1 <- pc1[train.idx]
wine.test$pc1 <- pc1[-train.idx]

cultivar.a.logistic.pc1 <- glm(Cultivar.A ~ pc1, data=wine.train)
summary(cultivar.a.logistic.pc1)

test.pred.probabilities.pc1 <- predict(cultivar.a.logistic.pc1, 
                                       newdata = wine.test,
                                       type="response")
test.preds.pc1 <- ifelse(test.pred.probabilities.pc1>0.5,"Pred True","Pred False")

confusion.matrix <- table(test.preds.pc1, wine.test$Cultivar.A)
confusion.matrix

# accuracy
(confusion.matrix[1,1] + confusion.matrix[2,2]) / sum(confusion.matrix)
# 0.8518519



## PCA on all numeric variables

# based on example from https://www.datacamp.com/tutorial/pca-analysis-r
wine.pca <- prcomp(wine[,2:14], center = TRUE,scale. = TRUE)
dim(wine[,2:14])

cov(wine %>% select(-Cultivar)) %>% View()

# shows % of variance at each PC
summary(wine.pca)

# % of variance by PC
importance.variance <- summary(wine.pca)$importance[2,]
importance.variance

# plot % of variance by PC
ggplot() +
  geom_bar(mapping = aes(x=reorder(names(importance.variance),
                                   -importance.variance),
                         y=importance.variance),
           stat="identity") + 
  xlab("component") + ylab("% of variance explained") + 
  ggtitle("Principal Component Importance (All Vars)")

# cumulative importance for first N components
importance.variance[1:2] %>% sum()
importance.variance[1:5] %>% sum()
importance.variance[1:8] %>% sum()
importance.variance[1:12] %>% sum()
importance.variance[1:13] %>% sum()

# matrix of variable loadings
wine.pca$rotation

# plot the variables as vectors on PC1, PC2 basis
ggbiplot(wine.pca) + 
  theme(aspect.ratio=1) + 
  ggtitle("All Variables Projected on PC1, PC2")
# Note: PC1 and PC2 are different vectors than those we
# obtained running PCA on Flavanoids + Total Phenols only

# try plotting on PC3, PC4 basis
ggbiplot(wine.pca, choices=c(3,4)) + 
  theme(aspect.ratio=1) + 
  ggtitle("All Variables Projected on PC3, PC4")

# plot with ellipses for Cultivar
wine.cultivar <- wine$Cultivar
ggbiplot(wine.pca, ellipse=TRUE, groups=wine.cultivar) + 
  theme(aspect.ratio=1) + 
  ggtitle("All Variables Projected on PC1, PC2")



## explore some other var relationships

ggplot(data=wine) + 
  geom_point(mapping = aes(x=Flavanoids, y=Color.intensity))

# how well would PCA work on these 2 vars?





##plot just a few points with shape in both bases to show translation
# # sample.indices <- c(147, 53, 122, 135, 124)
# sample.indices <- c(147, 53, 122, 135)
# wine.sample <- wine[sample.indices,]
# 
# # plot sample of points in original basis
# pointsize <- 5
# ggplot() + 
#   geom_point(mapping = aes(x=wine.sample$Flavanoids, y=wine.sample$Total.phenols,
#                            shape=as.factor(sample.indices)), 
#              col="black", size=pointsize) +
#   geom_point(mapping = aes(x=pc1.Flavanoids.component[sample.indices],
#                            y=pc1.Phenols.component[sample.indices],
#                            shape=as.factor(sample.indices)),
#              col="blue", size=pointsize) +
#   geom_point(mapping = aes(x=pc2.Flavanoids.component[sample.indices],
#                            y=pc2.Phenols.component[sample.indices],
#                            shape=as.factor(sample.indices)),
#              col="red", size=pointsize) + 
#   theme(aspect.ratio=1, legend.position = "none") + 
#   ggtitle("Original Basis") + 
#   xlab("Flavanoids") + ylab("Total Phenols")
# 
# # plot sample of points in PC1, PC2 basis
# ggplot() + 
#   geom_point(mapping = aes(x=pc1[sample.indices], y=pc2[sample.indices],
#                            shape=as.factor(sample.indices)),
#              col="black", size=pointsize) + 
#   geom_point(mapping = aes(x=pc1[sample.indices], y=0,
#                            shape=as.factor(sample.indices)),
#              col="blue", size=pointsize) + 
#   geom_point(mapping = aes(x=0, y=pc2[sample.indices],
#                            shape=as.factor(sample.indices)),
#              col="red", size=pointsize) + 
#   theme(aspect.ratio=1, legend.position = "none") + 
#   xlab("PC 1") + ylab("PC 2") + 
#   ggtitle("PC 1, PC 2 Basis")