library(openxlsx);library(psych);library(ppcor)

#缺失值填补
pcapre<-read.xlsx("D:/Rdata/pcapre1.xlsx",rowNames = T,colNames = T)
for (i in 1:203) {
  m<-mean(pcapre[-which(is.na(pcapre[,i])),i])
  pcapre[which(is.na(pcapre[,i])),i]<-m
}
write.csv(pcapre,file = "D:/Rdata/缺失值填补1.csv")

