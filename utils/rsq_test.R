# an unbiased metric that is comparable to R-squared
# but against out-of-sample data
rsq.test <- function(preds, actuals) {
  SSE <- sum((actuals - preds) ^ 2)
  SST <- sum((actuals - mean(actuals)) ^ 2)
  rsq.test.value <- (1 - SSE / SST)
  return(rsq.test.value)
}
