library(caret)
library(randomForest)


set.seed(1234)
my.rf <- randomForest(as.factor(Potability)~.,data=trData2)
my.rf
my.rf.pred <- predict(my.rf,tsData2,type="prob")[,2]
prediction.rf <- (my.rf.pred>0.5)
my.rf.err <- sum(tsData2$Potability != (my.rf.pred > 0.5))/nrow(tsData2)
my.rf.err
table(tsData2$Potability,my.rf.pred > 0.5)
table(tsData2$Potability, prediction.rf)

# Converting ‘Potability’ to a factor
trData2$Potability=as.factor(trData2$Potability)
# Set a random seed
set.seed(123)
# Training using ‘random forest’ algorithm
model <- train(Potability ~., data = trData2, method = 'rf',trControl = trainControl(method = 'cv', number = 5))
model             


set.seed(123)
train <- sample(nrow(trData2), 0.8*nrow(trData2), replace = FALSE)
TrainSet <- trData2[train,]
ValidSet <- trData2[-train,]

TrainSet$Potability=as.factor(TrainSet$Potability)
model1 <- randomForest(Potability ~ ., data = TrainSet, importance = TRUE)
model1

set.seed(123)
model2 <- randomForest(Potability ~ ., data = TrainSet, ntree = 1000, mtry = 7, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2,TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Potability)  

# Predicting on Validation set
predValid <- predict(model2, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ValidSet$Potability)                    
table(predValid,ValidSet$Potability)

importance(model2)        
varImpPlot(model2)

set.seed(12345)
a=c()
i=5
for (i in 3:9) {
  model3 <- randomForest(Potability ~ ., data = TrainSet, ntree = 1000, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$Potability)
}
a
plot(3:9,a,type="o",bty='l',xlab="mtry",ylab="Accuracy",main="Choosing best mtry value",pch=16)

set.seed(123)
# Predicting on Test set
predTest <- predict(model2, tsData2, type = "class")
# Checking classification accuracy
mean(predTest == tsData2$Potability)                    
table(predTest,tsData2$Potability)


# Define the control
trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")
set.seed(1234)
# Run the model
rf_default <- train(Potability~.,
                    data = trData2,
                    method = "rf",
                    metric = "Accuracy",
                    trControl = trControl)
# Print the results
print(rf_default)

set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 10))
rf_mtry <- train(Potability~.,
                 data = trData2,
                 method = "rf",
                 metric = "Accuracy",
                 tuneGrid = tuneGrid,
                 trControl = trControl,
                 importance = TRUE,
                 nodesize = 14,
                 ntree = 300)
print(rf_mtry)
rf_mtry$bestTune$mtry
max(rf_mtry$results$Accuracy)
best_mtry <- rf_mtry$bestTune$mtry 

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(5: 15)) {
  set.seed(1234)
  rf_maxnode <- train(Potability~.,
                      data = trData2,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  current_iteration <- toString(maxnodes)
  store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
summary(results_mtry)

store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = best_mtry)
for (maxnodes in c(30: 40)) {
  set.seed(1234)
  rf_maxnode <- train(Potability~.,
                      data = trData2,
                      method = "rf",
                      metric = "Accuracy",
                      tuneGrid = tuneGrid,
                      trControl = trControl,
                      importance = TRUE,
                      nodesize = 14,
                      maxnodes = maxnodes,
                      ntree = 300)
  key <- toString(maxnodes)
  store_maxnode[[key]] <- rf_maxnode
}
results_node <- resamples(store_maxnode)
summary(results_node)

store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
  set.seed(5678)
  rf_maxtrees <- train(Potability~.,
                       data = trData2,
                       method = "rf",
                       metric = "Accuracy",
                       tuneGrid = tuneGrid,
                       trControl = trControl,
                       importance = TRUE,
                       nodesize = 14,
                       maxnodes = 31,
                       ntree = ntree)
  key <- toString(ntree)
  store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)

fit_rf <- train(Potability~.,
                trData2,
                method = "rf",
                metric = "Accuracy",
                tuneGrid = tuneGrid,
                trControl = trControl,
                importance = TRUE,
                nodesize = 14,
                ntree = 450,
                maxnodes = 31)

prediction <-predict(fit_rf, tsData2)
confusionMatrix(prediction, tsData2$Potability)

prediction2<-predict(fit_rf, trData2)
confusionMatrix(prediction2, trData2$Potability)

