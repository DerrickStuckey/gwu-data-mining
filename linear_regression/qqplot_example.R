
# QQ plot for an assumed normal distribution
# TODO decide whether this should actually be included
ggplot(data=train.data[train.data.sample.idx,]) +
  stat_qq(mapping = aes(sample=carat.lm.residuals),
          distribution = stats::qnorm)
