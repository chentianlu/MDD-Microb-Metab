library(openxlsx);library(psych);library(ppcor)

#代谢物-疾病逻辑回归
metlog<-read.xlsx("D:/Rdata/metlog.xlsx",sheet = 1,rowNames = T)
VarsC<-c("Y","sex")#把分类变量因子化
for(i in VarsC){metlog[,i]<-as.factor(metlog[,i])};rm(i)
summary(metlog)
logresout<-read.xlsx("D:/Rdata/logresout.xlsx",colNames = T,rowNames = T)
for(i in 1:10){
  logistic<-glm(Y~sex+age+metlog[,i],data=metlog,family = binomial,na.action = na.omit);
  logresout[i,1]<-summary(logistic)$coefficients[2,4];
  logresout[i,2]<-summary(logistic)$coefficients[3,4];
  logresout[i,3]<-summary(logistic)$coefficients[4,4];
  logresout[i,4]<-summary(logistic)$coefficients[4,1]
}
write.csv(logresout,file = "D:/Rdata/代谢物_抑郁_逻辑回归.csv")


