library(ggplot2)
#k-NN confusion matrix
knn.cl=as.factor(knn.cl)
tsL=as.factor(tsL)

cm <- confusionMatrix(knn.cl, tsL, dnn = c("Prediction", "Reference"))
cm

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Reference,Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq),size=7) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "TRUE",y = "PREDICTED") +
  scale_x_discrete(labels=c("Not Potable","Potable"),position="top") +
  scale_y_discrete(labels=c("Potable","Not Potable"))+
  theme(text = element_text(size = 15))


prediction.rf=as.numeric(prediction.rf)+1
prediction.rf=as.factor(prediction.rf)
prediction.rf
cm2 <- confusionMatrix(prediction.rf, tsL, dnn = c("Prediction", "Reference"))
cm2

plt <- as.data.frame(cm2$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Reference,Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq),size=7) +
  scale_fill_gradient(low="white", high="#fe7541") +
  labs(x = "TRUE",y = "PREDICTED") +
  scale_x_discrete(labels=c("Not Potable","Potable"),position="top") +
  scale_y_discrete(labels=c("Potable","Not Potable"))+
  theme(text = element_text(size = 15))

prediction.lr=(rip.lr.pred>0.5)
prediction.lr=as.numeric(prediction.lr)
prediction.lr=as.factor(prediction.lr)
cm3 <- confusionMatrix(prediction.lr, tsL, dnn = c("Prediction", "Reference"))
cm3

plt <- as.data.frame(cm3$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Reference,Prediction, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq),size=7) +
  scale_fill_gradient(low="white", high="#a74ac7") +
  labs(x = "TRUE",y = "PREDICTED") +
  scale_x_discrete(labels=c("Not Potable","Potable"),position="top") +
  scale_y_discrete(labels=c("Potable","Not Potable"))+
  theme(text = element_text(size = 15))
