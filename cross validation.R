trData2$Potability=as.numeric(trData2$Potability)-1
tsData2$Potability=as.numeric(tsData2$Potability)-1

set.seed(42)
rows <- sample(nrow(trData2))
trData3 <- trData2[rows, ]
# LR, train only, 10 fold cross validation. 
V <- 10
V.frac <- nrow(trData3)/V    # happily, sample size divides!
v.fold.lr.res <- numeric(V)

for (i in 1:V){
  v.test.idx <- ((i-1)*V.frac+1):(V.frac*i)
  v.train.idx <- setdiff(1:nrow(trData3),v.test.idx)
  rip.lr <-  glm(Potability~.,data=trData3,subset=v.train.idx,family="binomial")
  rip.lr.pred <- predict(rip.lr,trData3[v.test.idx,],type="response")
  err.rate <- sum(trData3[v.test.idx,"Potability"] != (rip.lr.pred > 0.5))/length(v.test.idx)
  v.fold.lr.res[i] <- err.rate
}
mean(v.fold.lr.res)
sqrt(var(v.fold.lr.res))/sqrt(V)

# QDA, train only, 10 fold cross validation. 

v.fold.qda.res <- numeric(V)

for (i in 1:V){
  v.test.idx <- ((i-1)*V.frac+1):(V.frac*i)
  v.train.idx <- setdiff(1:nrow(trData3),v.test.idx)
  rip.qda <-  qda(Potability~.,data=trData3,subset=v.train.idx)
  rip.qda.pred <- predict(rip.qda,trData3[v.test.idx,])$posterior[,2]
  err.rate <- sum(trData3[v.test.idx,"Potability"] != (rip.qda.pred > 0.5))/length(v.test.idx)
  v.fold.qda.res[i] <- err.rate
}
mean(v.fold.qda.res)
sqrt(var(v.fold.qda.res))/sqrt(V)

# V fold cross validation for k-nn. 
# Note, the basic structure is the same, it is the details of the classifier that have changed

krange <- 2:75
v.fold.knn.mat <- matrix(0,nrow=length(krange),ncol=V)
for (i in 1:V){
  v.test.idx <- ((i-1)*V.frac+1):(V.frac*i)
  v.train.idx <- setdiff(1:nrow(trData3),v.test.idx)
  k.test.res <- numeric(length(krange))
  # loop over k
  for (j in krange){
    my.knn <-  knn(trData3[v.train.idx,1:9],trData3[v.test.idx,1:9],trData3[v.train.idx,10],k=j)
    knn.cl <- as.numeric(my.knn)-1
    knn.prob <- (attributes(knn(trData3[v.train.idx,1:9],trData3[v.test.idx,1:9],trData3[v.train.idx,10],k=k,prob=T)))$prob
    my.test.pred <- knn.cl * knn.prob + (1-knn.cl)*(1-knn.prob)
    err.rate <- sum((my.test.pred >= 0.5) != trData3$Potability[v.test.idx])/length(v.test.idx) 
    k.test.res[j-1] <- err.rate
  }
  v.fold.knn.mat[,i] <- k.test.res 
}

boxplot(t(v.fold.knn.mat),xlab="k",ylab="Error rate",main="Boxplots of error rate with different k values by cross validation")


k.medians <- apply(v.fold.knn.mat,1,median)
min(k.medians)
which.min(k.medians)


k.means <- apply(v.fold.knn.mat,1,median)
ks.std.errs <- apply(v.fold.knn.mat,1,function(x) sqrt(var(x)))/sqrt(V)

plot(krange,k.means)


# compare the minimum mean choices

min.ks <- which(k.means == min(k.means))
cbind(krange[min.ks],k.means[min.ks],ks.std.errs[min.ks])
ks.std.errs[37]

# randomForest, train only, 10 fold cross validation. 

v.fold.rf.res <- numeric(V)

for (i in 1:V){
  v.test.idx <- ((i-1)*V.frac+1):(V.frac*i)
  v.train.idx <- setdiff(1:nrow(trData3),v.test.idx)
  
  my.rf <- randomForest(as.factor(Potability)~.,data=trData3,subset=v.train.idx)
  my.rf.pred <- predict(my.rf,trData3[v.test.idx,],type="prob")[,2]
  my.rf.err <- sum(trData3[v.test.idx,"Potability"] != (my.rf.pred > 0.5))/length(v.test.idx)
  v.fold.rf.res[i] <- my.rf.err
}
mean(v.fold.rf.res)
sqrt(var(v.fold.rf.res))/sqrt(V)






