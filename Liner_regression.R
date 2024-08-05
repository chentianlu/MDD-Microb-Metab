library(openxlsx);library(psych);library(ppcor)

#代谢物-菌线性回归
biomet<-read.xlsx("D:/Rdata/biometlog.xlsx",colNames = T,rowNames = T)
lrp<-read.xlsx("D:/Rdata/lrp.xlsx",colNames = T,rowNames = T)
for(n in 1:11){
  for(i in 12:44){
    lr<-lm(biomet[,n]~biomet[,i]+biomet[,45]+biomet[,46],data=biomet);
    m<-(i-11);try(lrp[n,m]<-summary(lr)$coefficients[2,4],silent = F)
  }
}
for(i in 1:11){lrp[i,which(is.na(lrp[i,]))]<-1}#去除矩阵中NA值
write.csv(lrp,"D:/Rdata/代谢物-菌线性回归P值.csv")
lm<-cbind(rownames(lrp),c(1:11),c(1:11))#设置列名矩阵
for (i in 1:11) {
  lm[i,2]<-length((lrp[i,])[(lrp[i,]<0.05)&(lrp[i,]>=0.01)]);
  lm[i,3]<-length((lrp[i,])[(lrp[i,]<0.01)&(lrp[i,]>0)])
}
write.csv(lm,"D:/Rdata/代谢物-菌线性回归显著p数量.csv")
