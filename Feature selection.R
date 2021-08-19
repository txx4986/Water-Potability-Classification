water_potability_copy$Potability=as.factor(water_potability_copy$Potability)
df_table1_drop$Potability=as.factor(df_table1_drop$Potability)
df_table1_replacement$Potability=as.factor(df_table1_replacement$Potability)

library(randomForest)
fit_rf1 = randomForest(Potability~.,data=water_potability_copy,mtry=2, importance=TRUE)
# Create an importance based on mean decreasing gini
importance(fit_rf1)
varImpPlot(fit_rf1)

library(caret)
varImp(fit_rf1)

# Fit a logistic regression model
fit_glm1 = glm(Potability~.,data=water_potability_copy,family = "binomial")
# generate summary
summary(fit_glm1)

# Using varImp() function
library(caret)
varImp(fit_glm1)

df_table1 <- read.csv("/Users/tanxiaoxuan/Desktop/water_potability.csv", sep = ",")
list_na <- colnames(df_table1)[ apply(df_table1, 2, anyNA) ]
list_na
# Create mean
average_missing <- apply(df_table1[,colnames(df_table1) %in% list_na],2,mean,na.rm =TRUE)
average_missing
# Create a new variable with the mean and median
df_table1_replace <- df_table1 %>%
  mutate(replace_mean_ph  = ifelse(is.na(ph), average_missing[1], ph),
         replace_mean_Sulfate = ifelse(is.na(Sulfate), average_missing[2], Sulfate),
         replace_mean_Trihalomethanes  = ifelse(is.na(Trihalomethanes), average_missing[3], Trihalomethanes))
sum(is.na(df_table1_replace$ph))
sum(is.na(df_table1_replace$replace_mean_ph))
sum(is.na(df_table1_replace$Sulfate))
sum(is.na(df_table1_replace$replace_mean_Sulfate))
sum(is.na(df_table1_replace$Trihalomethanes))
sum(is.na(df_table1_replace$replace_mean_Trihalomethanes))

df_table1_replacement <- subset(df_table1_replace, select = -c(ph,Sulfate,Trihalomethanes))
df_table1_replacement2<-df_table1_replacement[,c(1,2,3,4,5,6,8,9,10,7)]


library(dplyr)
# Exclude the missing observations
df_table1_drop <-df_table1 %>% 
na.omit()		
dim(df_table1_drop)

library(randomForest)
fit_rf2 = randomForest(Potability~.,data=df_table1_drop,ntree=100, mtry=2, importance=TRUE)
# Create an importance based on mean decreasing gini
importance(fit_rf2)
varImpPlot(fit_rf2)

library(caret)
varImp(fit_rf2)

# Fit a logistic regression model
fit_glm2 = glm(Potability~.,data=df_table1_drop,family = "binomial")
# generate summary
summary(fit_glm2)

# Using varImp() function
library(caret)
varImp(fit_glm2)

library(randomForest)
fit_rf3 = randomForest(Potability~.,data=df_table1_replacement,ntree=100,mtry=2, importance=TRUE)
# Create an importance based on mean decreasing gini
importance(fit_rf3)
varImpPlot(fit_rf3)

library(caret)
varImp(fit_rf3)

# Fit a logistic regression model
fit_glm3 = glm(Potability~.,data=df_table1_replacement,family = "binomial")
# generate summary
summary(fit_glm3)

# Using varImp() function
library(caret)
varImp(fit_glm3)

plot(water_potability_copy[,1:6])
points(water_potability_copy[water_potability_copy$Potability==1,1:6],col="red")

cor(water_potability_copy,water_potability_copy$Potability)                                         
cor(df_table1_drop,df_table1_drop$Potability)
cor(df_table1_replacement,df_table1_replacement$Potability)

install.packages('FSelector')
library(FSelector)
information.gain(Potability~., water_potability_copy)
information.gain(Potability~.,df_table1_drop)
information.gain(Potability~.,df_table1_replacement)

library(mlbench)
library(caret)
# calculate correlation matrix
correlationMatrix1 <- cor(water_potability_copy[,1:6])
# summarize the correlation matrix
print(correlationMatrix1)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated1 <- findCorrelation(correlationMatrix1, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated1)

# calculate correlation matrix
correlationMatrix2 <- cor(df_table1_drop[,1:9])
# summarize the correlation matrix
print(correlationMatrix2)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated2 <- findCorrelation(correlationMatrix2, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated2)

# calculate correlation matrix
correlationMatrix3 <- cor(df_table1_replacement2[,1:9])
# summarize the correlation matrix
print(correlationMatrix3)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated3 <- findCorrelation(correlationMatrix3, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated3)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model1 <- train(Potability~., data=water_potability_copy,method="lvq",preProcess="scale", trControl=control)
# estimate variable importance
importance1 <- varImp(model1, scale=FALSE)
# summarize importance
print(importance1)
# plot importance
plot(importance1)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model2 <- train(Potability~., data=df_table1_drop,method="lvq",preProcess="scale", trControl=control)
# estimate variable importance
importance2 <- varImp(model2, scale=FALSE)
# summarize importance
print(importance2)
# plot importance
plot(importance2)

# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model3 <- train(Potability~., data=df_table1_replacement,method="lvq",preProcess="scale", trControl=control)
# estimate variable importance
importance3 <- varImp(model3, scale=FALSE)
# summarize importance
print(importance3)
# plot importance
plot(importance3)

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(water_potability_copy[,1:6], water_potability_copy[,7], sizes=c(1:6), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(df_table1_drop[,1:9], df_table1_drop[,10], sizes=c(1:9), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(df_table1_replacement2[,1:9], df_table1_replacement2[,10], sizes=c(1:9), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

library(correlation)
correlation::correlation(df_table1_replacement2,include_factors = TRUE, method = "auto")

corrplot2 <- function(data,
                      method = "pearson",
                      sig.level = 0.05,
                      order = "original",
                      diag = FALSE,
                      type = "upper",
                      tl.srt = 90,
                      number.font = 1,
                      number.cex = 1,
                      mar = c(0, 0, 0, 0)) {
  library(corrplot)
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(mat,
           method = "color", col = col(200), number.font = number.font,
           mar = mar, number.cex = number.cex,
           type = type, order = order,
           addCoef.col = "black", # add correlation coefficient
           tl.col = "black", tl.srt = tl.srt, # rotation of text labels
           # combine with significance level
           p.mat = p.mat, sig.level = sig.level, insig = "blank",
           # hide correlation coefficients on the diagonal
           diag = diag
  )
}

corrplot2(
  data = df_table1_replacement2[,1:9],
  method = "pearson",
  sig.level = 0.05,
  order = "original",
  diag = FALSE,
  type = "upper",
  tl.srt = 75
)

# improved correlation matrix
library(corrplot)

corrplot(cor(df_table1_replacement2[,1:9]),method="number",type = "upper") 

library(randomForest)
set.seed(71)
rf <-randomForest(Potability~.,data=df_table1_replacement2, ntree=500) 
print(rf)

floor(sqrt(ncol(df_table1_replacement2) - 1))

mtry <- tuneRF(df_table1_replacement2[-1],df_table1_replacement2$Potability, ntreeTry=500,
               stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)

set.seed(71)
rf <-randomForest(Potability~.,data=df_table1_replacement2, mtry=best.m, importance=TRUE,ntree=500)
print(rf)
#Evaluate variable importance
importance(rf)
varImpPlot(rf)

write.csv(df_table1_replacement2,"/Users/tanxiaoxuan/Desktop/df_table1_replacement2.csv", row.names = FALSE)

library(naniar)
vis_miss(df_table1)

library(GGally)
ggcorr(df_table1_replacement2, method = c("everything", "pearson"),label=TRUE) 
ggpairs(df_table1_replacement2)

df_table1_replacement2$Potability=as.factor(df_table1_replacement2$Potability)
library(ggplot2)
ggpairs(df_table1_replacement2, title = "Water Potability Analysis", upper = list(continuous = wrap("cor",
        size = 3)),lower = list(continuous = wrap("smooth",alpha = 0.3,size = 0.1)),mapping = aes(color = Potability))

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor(df_table1_replacement2[,1:9]), method="color", col=col(200),  type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         diag=FALSE)

library(Amelia)
missmap(df_table1,main="")

summary(df_table1_replacement2)
