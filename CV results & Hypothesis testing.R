df <- data.frame(method=c("37-NN","RF","LR"),min=c(0.3444,0.3253,0.3791), max=c(0.3618,0.3473,0.4011),error_rate=c(0.3531,0.3363,0.3901))
library(ggplot2)
ggplot(df, aes(y=method))+
  geom_linerange(aes(xmin=min,xmax=max),linetype=1,color="black")+
  geom_point(aes(x=error_rate),size=4,color="red")+
  theme_bw()+
  labs(x="Error rate",y="Method")+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16))


knn.cl
prediction.rf

k_NN_correct<-ifelse(knn.cl==tsL,"Yes","No")
RF_correct<-ifelse(prediction.rf==tsL,"Yes","No")
df2<-data.frame(k_NN_correct,RF_correct)
df3<-cbind(count=1,df2)

library(dplyr)
df3 %>%
  group_by(k_NN_correct, RF_correct) %>%
  summarise_all(sum)

p<-377
q<-44
r<-62
s<-173
m=matrix(c(p,q,r,s),byrow=T,nrow=2)
mcnemar.test(m)




