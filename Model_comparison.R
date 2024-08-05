ps <- c('openxlsx', 'pROC', 'ggplot2','randomForest','gbm','e1071','rpart')
for(i in ps){library(i, character.only = T)};rm(i);rm(ps)
intsec<-read.xlsx("intsec.xlsx",rowNames = T,colNames = T)
intsec <- na.omit(intsec)
intsec.f<-intsec
intsec.f$Y<-as.factor(intsec.f$Y)
sam<-sample(1:230,161)
train<-intsec[sam,];test<-intsec[-sam,]
train.f<-intsec.f[sam,];test.f<-intsec.f[-sam,]

#随机森林
rf<-randomForest(Y~.,data=train.f,importance=TRUE,proximity=TRUE)
a<-rf$confusion[1,1];b<-rf$confusion[1,2];c<-rf$confusion[2,1];d<-rf$confusion[2,2]
pred.rf<-predict(rf,newdata = test.f,type = "prob")[,2]
roc.rf<-roc(test$Y,pred.rf)
auc.m<-auc(roc.rf)#输出AUC
sen<-(a/(a+c));spe<-(d/(b+d))
F1<-2*(((a^2)/((a+c)*(a+b)))/((a/(a+c))+(a/(a+b))))
RF<-cbind(auc.m,sen,spe,F1)
rf$confusion
plot(roc.rf,col='green')

#逻辑回归
lgr<-glm(Y~.,data=train.f,family = binomial)
pred.lgr<-predict(lgr,newdata = test.f,type = "response")
roc.lgr<-roc(test.f$Y,pred.lgr)
auc.m<-auc(roc.lgr)#输出AUC
pred.lgr[which(pred.lgr<0.5)]<-0;pred.lgr[which(pred.lgr>=0.5)]<-1
pred.lgr<-as.factor(pred.lgr)
conf<-table(test.f$Y,pred.lgr,dnn=c("真实值","预测值"))#混淆矩阵
a<-conf[1,1];b<-conf[1,2];c<-conf[2,1];d<-conf[2,2]
sen<-(a/(a+c));spe<-(d/(b+d))
F1<-2*(((a^2)/((a+c)*(a+b)))/((a/(a+c))+(a/(a+b))))
Logistic<-cbind(auc.m,sen,spe,F1)
conf
plot(roc.lgr,add=T,col='red')

#梯度提升
gb<- gbm(Y ~ .,data=train,n.trees = 400,distribution = "bernoulli",
       interaction.depth = 1,shrinkage = 0.03, n.minobsinnode = 4)
pred.gb<-predict(gb,newdata=test,n.trees=400,type = "response")
roc.gb<-roc(test$Y,pred.gb)
auc.m<-auc(roc.gb)
pred.gb[which(pred.gb<0.5)]<-0;pred.gb[which(pred.gb>=0.5)]<-1
conf<-table(test$Y,pred.gb,dnn=c("真实值","预测值"))
a<-conf[1,1];b<-conf[1,2];c<-conf[2,1];d<-conf[2,2]
sen<-(a/(a+c));spe<-(d/(b+d))
F1<-2*(((a^2)/((a+c)*(a+b)))/((a/(a+c))+(a/(a+b))))
GB<-cbind(auc.m,sen,spe,F1)
conf
plot(roc.gb,add=T,col='blue')

#支持向量机
svm.i<-svm(Y~.,data = train)
pred.svm<-predict(svm.i,newdata=test,type="response")
roc.svm<-roc(test$Y,pred.svm)
auc.m<-auc(roc.svm)
pred.svm[which(pred.svm<0.5)]<-0;pred.svm[which(pred.svm>=0.5)]<-1
conf<-table(test$Y,pred.svm,dnn=c("真实值","预测值"))
a<-conf[1,1];b<-conf[1,2];c<-conf[2,1];d<-conf[2,2]
sen<-(a/(a+c));spe<-(d/(b+d))
F1<-2*(((a^2)/((a+c)*(a+b)))/((a/(a+c))+(a/(a+b))))
SVM<-cbind(auc.m,sen,spe,F1)
conf
plot(roc.svm,add=T,col='purple')

#决策树
dt<-rpart(Y~.,data = train.f)
pred.dt<-predict(dt,newdata=test.f,type="prob")[,2]
roc.dt<-roc(test$Y,pred.dt)
auc.m<-auc(roc.dt)
pred.dt[which(pred.dt<0.5)]<-0;pred.dt[which(pred.dt>=0.5)]<-1
conf<-table(test.f$Y,pred.dt,dnn=c("真实值","预测值"))
a<-conf[1,1];b<-conf[1,2];c<-conf[2,1];d<-conf[2,2]
sen<-(a/(a+c));spe<-(d/(b+d))
F1<-2*(((a^2)/((a+c)*(a+b)))/((a/(a+c))+(a/(a+b))))
DT<-cbind(auc.m,sen,spe,F1)
conf
plot(roc.dt,add=T,col='orange')

rn<-c('RF','Logistic','GB','SVM','DT')
result<-rbind(RF,Logistic,GB,SVM,DT)
resout<-cbind(rn,result)
write.csv(resout,file = "模型比较37.csv",row.names = F)
