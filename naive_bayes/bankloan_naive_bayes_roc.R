# install.packages("plotROC")
# https://cran.r-project.org/web/packages/plotROC/plotROC.pdf

# modified example from https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/

# source("naive_bayes/bankloan_naive_bayes.R")

library(plotROC)
ggplot(mapping = aes(m = test.probabilities.4[,1], d = ifelse(test.data$Loan.Status=="Accepts",1,0))) + 
  geom_roc(n.cuts=0,labels=FALSE) + 
  style_roc(theme = theme_grey)
# + geom_rocci(fill="pink") 

