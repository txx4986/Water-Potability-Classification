library(rpart)
library(caret)
library(e1071)
trData2$Potability=as.factor(trData2$Potability)
tsData2$Potability=as.factor(tsData2$Potability)

set.seed(123)
# We will compare model 1 of Random Forest with Decision Tree model
model_dt = train(Potability ~ ., data = trData2, method = "rpart")
model_dt_1 = predict(model_dt, data = trData2)
table(model_dt_1, trData2$Potability)
mean(model_dt_1 == trData2$Potability)

# Running on Validation Set
model_dt_vs = predict(model_dt, newdata = tsData2)
table(model_dt_vs, tsData2$Potability)
mean(model_dt_vs == tsData2$Potability)
