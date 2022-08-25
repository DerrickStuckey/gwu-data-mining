# install.packages("plotROC")
# https://cran.r-project.org/web/packages/plotROC/plotROC.pdf

# modified example from https://rviews.rstudio.com/2019/03/01/some-r-packages-for-roc-curves/

# source("naive_bayes/bankloan_naive_bayes.R")

library(plotROC)
ggplot(mapping = aes(m = test.probabilities.4[,1], d = ifelse(test.data$Loan.Status=="Accepts",1,0))) + 
  geom_roc(n.cuts=0,labels=FALSE) + 
  style_roc(theme = theme_grey)
# + geom_rocci(fill="pink") 

# force the model to actually choose a class
val.preds.3 <- predict(bd.nb.3, newdata = validation.data, type = "class")
head(val.preds.3)
summary(val.preds.3)

# look at a confusion matrix for the updated model
confusionMatrix(val.preds.3, validation.data$Loan.Status)

# plot a gain chart for the updated model
gain <- gains(ifelse(validation.data$Loan.Status=="Accepts",1,0), 
              val.probs.3[,1],
              groups=16)

total.accepted <- sum(validation.data$Loan.Status=="Accepts")
yvals <- c(0,gain$cume.pct.of.total*total.accepted)
xvals <- c(0,gain$cume.obs)
ggplot() + 
  geom_line(mapping = aes(x=xvals, y=yvals)) +
  xlab("Offers Made (TP + FP)") + ylab("Number Accepted (TP)") + 
  ggtitle("Lift Chart Example") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_abline(intercept = c(0,0), 
              slope=total.accepted/nrow(validation.data),
              linetype="dashed")


# val.probs.3 <- predict(bd.nb.3, newdata = validation.data, type = "raw")
ggplot(mapping = aes(m = val.probs.3[,1], 
                     d = validation.data$Loan.Status=="Accepts")) + 
  geom_roc(n.cuts=0,labels=FALSE) + 
  style_roc(theme = theme_grey) + 
  ggtitle("ROC Curve Example") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_abline(intercept = c(0,0), 
              slope=1,
              linetype="dashed")
table(val.probs.3[,1])
