set.seed(12345)
a <- splitFile(df_table1_replacement2,.8,'Potability')
trData <- a[[1]]
tsData <- a[[2]]
trL <- a[[3]]
tsL <- a[[4]]
trData<-as.data.frame(lapply(trData,normalize))
tsData<-as.data.frame(lapply(tsData,normalize))
table(trL)
table(tsL)

k <- 37

#compute k-nn on the test set

my.knn <-  knn(trData,tsData,trL,k=k)
knn.cl <- as.numeric(my.knn)-1

# extract the posterior probabilities, and compute the test set error rate
knn.prob <- (attributes(knn(trData,tsData,trL,k=k,prob=T)))$prob
my.test.pred <- knn.cl * knn.prob + (1-knn.cl)*(1-knn.prob)
err.rate <- sum((my.test.pred >= 0.5) != tsL)/nrow(tsData) 
table(knn.cl,tsL)
err.rate

#training error
my.knn2<-  knn(trData,trData,trL,k=k)
knn.tr<- as.numeric(my.knn2)-1
knn.prob2 <- (attributes(knn(trData,trData,trL,k=k,prob=T)))$prob
my.test.pred2 <- knn.tr * knn.prob2 + (1-knn.tr)*(1-knn.prob2)
err.rate2 <- sum((my.test.pred2 >= 0.5) != trL)/nrow(trData) 

table(knn.tr,trL)

err.rate2


# cheat knn on test set

krange <- 2:75
k.test.res <- numeric(length(krange))
for (k in krange){
  my.knn <-  knn(trData,tsData,trL,k=k)
  knn.cl <- as.numeric(my.knn)-1
  knn.prob <- (attributes(knn(trData,tsData,trL,k=k,prob=T)))$prob
  my.test.pred <- knn.cl * knn.prob + (1-knn.cl)*(1-knn.prob)
  err.rate <- sum((my.test.pred >= 0.5) != tsL)/nrow(tsData) 
  k.test.res[k-1] <- err.rate
}

which.min(k.test.res)


plot(krange,k.test.res,xlab="k",ylab="error rate",type="b")


# knn split training set to choose k
trData2<-trData
trData2$Potability<-trL
tsData2<-tsData
tsData2$Potability<-tsL

set.seed(12345)
tr.size <- 1310
tr.idx <- sample(1:nrow(trData2),tr.size,replace=F)
valid.idx <- setdiff(1:nrow(trData2),tr.idx)

rip.tr <- trData2[tr.idx,]
rip.valid <- trData2[valid.idx,]

# modification of the code above

krange <- 2:75
k.test.res <- numeric(length(krange))
for (k in krange){
  my.knn <-  knn(rip.tr[,1:9],rip.valid[,1:9],rip.tr[,10],k=k)
  knn.cl <- as.numeric(my.knn)-1
  knn.prob <- (attributes(knn(rip.tr[,1:9],rip.valid[,1:9],rip.tr[,10],k=k,prob=T)))$prob
  my.test.pred <- knn.cl * knn.prob + (1-knn.cl)*(1-knn.prob)
  err.rate <- sum((my.test.pred >= 0.5) != rip.valid$Potability)/nrow(rip.valid) 
  k.test.res[k-1] <- err.rate
}

which.min(k.test.res)
plot(krange,k.test.res,xlab="k",ylab="error rate",type="b")


# knn split training set to choose k
# and repeating multiple times
set.seed(12345)
tr.size <- 1310
krange <- 2:75
reps <- 30
rip.err.mat <- matrix(0,nrow=length(krange),ncol=reps)

for (j in 1:reps){
  tr.idx <- sample(1:nrow(trData2),tr.size,replace=F)
  valid.idx <- setdiff(1:nrow(trData2),tr.idx)
  rip.tr <- trData2[tr.idx,]
  rip.valid <- trData2[valid.idx,]
  k.test.res <- numeric(length(krange))
  for (k in krange){
    my.knn <-  knn(rip.tr[,1:9],rip.valid[,1:9],rip.tr[,10],k=k)
    knn.cl <- as.numeric(my.knn)-1
    knn.prob <- (attributes(knn(rip.tr[,1:9],rip.valid[,1:9],rip.tr[,10],k=k,prob=T)))$prob
    my.test.pred <- knn.cl * knn.prob + (1-knn.cl)*(1-knn.prob)
    err.rate <- sum((my.test.pred >= 0.5) != rip.valid$Potability)/nrow(rip.valid) 
    k.test.res[k-1] <- err.rate
  }
  rip.err.mat[,j] <- k.test.res
}

boxplot(t(rip.err.mat),xlab="k",ylab="Error rate",main="Boxplots for random partitions of training and validation data")


k.medians <- apply(rip.err.mat,1,median)
min(k.medians)
which.min(k.medians)

#plotting train and test error
bestK <- function(trData, trLabels, tsData, tsLabels) {
  ctr <- c(); cts <- c()
  for (k in 2:75) {
    knnTr <- knn(trData, trData, trLabels, k)
    knnTs <- knn(trData, tsData, trLabels, k)
    trTable <- prop.table(table(knnTr, trLabels))
    tsTable <- prop.table(table(knnTs, tsLabels))
    erTr <- trTable[1,2] + trTable[2,1]
    erTs <- tsTable[1,2] + tsTable[2,1]
    ctr <- c(ctr,erTr)
    cts <- c(cts,erTs)
  }
  #acc <- data.frame(k=1/c(1:100), trER=ctr, tsER=cts)
  err <- data.frame(k=2:75, trER=ctr, tsER=cts)
  return(err)
}

library(ggplot2)
err <- bestK(trData, trL, tsData, tsL)
plot(err$k,err$trER,type="o",bty='l',ylim=c(0,.5),xlab="k",ylab="Error rate",main="Comparing train and test error rate with k values",col=rgb(0.2,0.4,0.1,0.7) ,pch=17 )
lines(err$k,err$tsER,col=rgb(0.8,0.4,0.1,0.7),pch=19 , type="o")

legend("bottomright", 
       legend = c("Train error", "Test error"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))
