# First, logistic regression.

rip.lr <- glm(Potability~.,data=trData2,family="binomial")   

# compute the test set predictions, on the "response" scale
rip.lr.pred <- predict(rip.lr,tsData2,type="response")

# training set predictions
rip.lr.train.pred <- predict(rip.lr,type="response")

# error rates
rip.lr.err.test <- sum(tsData2$Potability != (rip.lr.pred > 0.5))/nrow(tsData2)
rip.lr.err.train <- sum(trData2$Potability != (rip.lr.train.pred > 0.5))/nrow(trData2)
rip.lr.err.test
rip.lr.err.train

table(tsData2$Potability,rip.lr.pred > 0.5)
table(trData2$Potability,rip.lr.train.pred>0.5)


# compute LDA
rip.lda <- lda(Potability~.,data=trData2)

# careful, different classifiers provide output in different forms. Here, getting the posterior probabilities
rip.lda.pred <- predict(rip.lda,tsData2)$posterior[,2]

#confusion matrix, test set
table(tsData2$Potability,rip.lda.pred > 0.5)
rip.lda.test.err <- sum(tsData2$Potability != (rip.lda.pred > 0.5))/nrow(tsData2)
rip.lda.test.err


#compute QDA
rip.qda <- qda(Potability~.,data=trData2)
rip.qda.pred <- predict(rip.qda,tsData2)$posterior[,2]
#confusion matrix,test set
table(tsData2$Potability,rip.qda.pred > 0.5)
rip.qda.test.err <- sum(tsData2$Potability != (rip.qda.pred > 0.5))/nrow(tsData2)
rip.qda.test.err

# recall the comments about in-sample assessment

rip.qda.train.pred <- predict(rip.qda,trData2)$posterior[,2]
rip.lda.train.pred <- predict(rip.lda,trData2)$posterior[,2]

#confusion matrix, training set
table(trData2$Potability,rip.lda.train.pred > 0.5)
rip.lda.train.err <- sum(trData2$Potability!=(rip.lda.train.pred > 0.5))/nrow(trData2)
rip.lda.train.err

table(trData2$Potability,rip.qda.train.pred > 0.5)
rip.qda.train.err <- sum(trData2$Potability!=(rip.qda.train.pred > 0.5))/nrow(trData2)
rip.qda.train.err
