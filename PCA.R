library(openxlsx);library(psych);library(ppcor)


#PCA-scale(正常使用)
pca<-read.xlsx("D:/Rdata/pca.xlsx",rowNames = T,colNames = T)
Neurotransmitters<-prcomp(pca[,1:12],scale. = T)$x[,1]
Aminoacids<-prcomp(pca[,13:49],scale. = T)$x[,1]
Benzenoids<-prcomp(pca[,50:57],scale. = T)$x[,1]
Bileacids<-prcomp(pca[,58:72],scale. = T)$x[,1]
Carbohydrates<-prcomp(pca[,73:87],scale. = T)$x[,1]
Carnitines<-prcomp(pca[,88:106],scale. = T)$x[,1]
Fattyacids<-prcomp(pca[,107:156],scale. = T)$x[,1]
Indoles<-prcomp(pca[,157:162],scale. = T)$x[,1]
Organicacids<-prcomp(pca[,163:199],scale. = T)$x[,1]
Phenylpropanoicacids<-prcomp(pca[,200:203],scale. = T)$x[,1]
pc1<-cbind(Neurotransmitters,Aminoacids,Benzenoids,Bileacids,Carbohydrates,
           Carnitines,Fattyacids,Indoles,Organicacids,Phenylpropanoicacids)
write.csv(pc1,file = "D:/Rdata/pc1.csv")

#加和
pca<-read.xlsx("D:/Rdata/pca.xlsx",rowNames = T,colNames = T)
relsum<-read.xlsx("D:/Rdata/pc1resout.xlsx",rowNames = T,colNames = T)
for (i in 1:469) {
  for (j in 1:203) {
    pca[i,j]<-(pca[i,j]/sum(pca[i,-c(204:206)]))
  }
  relsum[i,1]<-sum(pca[i,1:13])
  relsum[i,2]<-sum(pca[i,14:50])
  relsum[i,3]<-sum(pca[i,51:58])
  relsum[i,4]<-sum(pca[i,59:73])
  relsum[i,5]<-sum(pca[i,74:88])
  relsum[i,6]<-sum(pca[i,89:107])
  relsum[i,7]<-sum(pca[i,108:157])
  relsum[i,8]<-sum(pca[i,158:163])
  relsum[i,9]<-sum(pca[i,164:199])
  relsum[i,10]<-sum(pca[i,200:203])
}
write.csv(relsum,file = "D:/Rdata/相对浓度加和PC1.csv")

