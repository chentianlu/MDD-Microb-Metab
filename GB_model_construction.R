ps <- c('openxlsx', 'pROC', 'ggplot2','gbm')
for(i in ps){library(i, character.only = T)};rm(i);rm(ps)
intsec<-read.xlsx("intsec.xlsx",rowNames = T,colNames = T)
intsec <- na.omit(intsec)
#自测
gb<- gbm(Y ~ .,data=intsec,n.trees = 50,distribution = "bernoulli",
         interaction.depth = 1,shrinkage = 0.2, n.minobsinnode = 10)
pred.gb<-predict(gb,data=intsec,n.trees=50,type = "response")
roc.gb<-roc(intsec$Y,pred.gb)
auc.z<-auc(roc.gb)
best_cut <- as.matrix(coords(roc.gb, 'best', transpose = FALSE))[1,1]
pred.gb[which(pred.gb<best_cut)]<-0;pred.gb[which(pred.gb>=best_cut)]<-1
table(intsec$Y,pred.gb,dnn=c("真实值","预测值"))
plot(roc.gb,col='blue')
#留一法
pred<-c()
for(i in 1:200){
  train<-intsec[-i,];test<-intsec[i,]
  gb<- gbm(Y ~ .,data=train,n.trees = 50,distribution = "bernoulli",
           interaction.depth = 1,shrinkage = 0.2, n.minobsinnode = 10)
  pred[i]<-predict(gb,newdata=test,n.trees=50,type = "response")
}
roc.m<-roc(intsec$Y,pred)
best_cut <- as.matrix(coords(roc.m, 'best', transpose = FALSE))[1,1]
pred.gb[which(pred.gb<best_cut)]<-0;pred.gb[which(pred.gb>=best_cut)]<-1
conf<-table(intsec$Y,pred,dnn=c("真实值","预测值"))
a<-conf[1,1];b<-conf[1,2];c<-conf[2,1];d<-conf[2,2]
auc.l<-auc(roc.m)
sen<-(a/(a+c));spe<-(d/(b+d))
F1<-2*(((a^2)/((a+c)*(a+b)))/((a/(a+c))+(a/(a+b))))
GB<-cbind(auc.l,sen,spe,F1)
plot(roc.m,add=T,col='red')
#3-7分
thrsev<-c()
for(i in 1:100){
  sam<-sample(1:200,140)
  train<-intsec[sam,];test<-intsec[-sam,]
  gb<- gbm(Y ~ .,data=train,n.trees = 50,distribution = "bernoulli",
           interaction.depth = 1,shrinkage = 0.2, n.minobsinnode = 10)
  pred.gb<-predict(gb,newdata=test,n.trees=50,type = "response")
  roc.gb<-roc(test$Y,pred.gb)
  thrsev[i]<-auc(roc.gb)
}
mean(thrsev);sd(thrsev)
#ROC
auc.sigmet<-cbind(colnames(intsec)[-31],1:30)
for (i in 1:30) {
  auc.sigmet[i,2]<-auc(roc(intsec$Y,intsec[,i]))
}
write.csv(auc.sigmet,file = "单个代谢物AUC.csv",row.names = F)
#建模并用验证集测试
gb<- gbm(Y ~ .,data=intsec,n.trees = 50,distribution = "bernoulli",
         interaction.depth = 1,shrinkage = 0.2, n.minobsinnode = 10)
test<-read.xlsx("test.xlsx",rowNames = T,colNames = T)
pred.gb<-predict(gb,newdata=test,n.trees=50,type = "response")
roc.gb<-roc(test$Y,pred.gb)
auc(roc.gb)
plot(roc.gb,col="purple")
#建模循环


#苏州测试
load("last_gb.rda")
testsu<-read.xlsx("testsu.xlsx",colNames = T,rowNames = T)
pred.gb<-predict(last_gb,newdata=testsu,n.trees=50,type = "response")
roc.gb<-roc(testsu$Y,pred.gb)
auc(roc.gb)
#重新选样本
summary(gb)

gb<- gbm(Y ~ .,data=test,n.trees = 50,distribution = "bernoulli",
         interaction.depth = 1,shrinkage = 0.2, n.minobsinnode = 10)
summary(gb)
delsam<-cbind(pred.gb,test$Y)#删样本
best_cut <- as.matrix(coords(roc.gb, 'best', transpose = FALSE))[1,1]
best_cut#cut值