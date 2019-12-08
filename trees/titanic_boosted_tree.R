
# try a boosted tree
# library(adabag)
# library(rpart)
# 
# surv.boost <- boosting(Survived ~ Pclass + Sex + SibSp + Parch + Fare + 
#                          Age.Full + Embarked.Full,
#                        data=train.data)
# 
# 
# # measure performance of the boosted tree
# validation.data$preds.boost <- predict(surv.boost,
#                                       newdata=validation.data,
#                                       type="class")
# 
# confusionMatrix(validation.data$preds.boost, validation.data$Survived)

