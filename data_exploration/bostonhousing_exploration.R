library(ggplot2)

bostonhousing <- read.csv("./BostonHousing.csv")

head(bostonhousing)

hist(bostonhousing$CRIM, breaks=30)

ggplot(data=bostonhousing) + 
  geom_point(mapping = aes(x=))