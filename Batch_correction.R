library(openxlsx);library(psych);library(ppcor)

#消除批次差异
pici<-read.xlsx("D:/Rdata/pici.xlsx",rowNames = T,colNames = T)
pici[,4]<-as.character(pici[,4])#batch字符化
edata<-t(pici[,-dim(pici)[2]])#行列转置
picires<-ComBat(dat = edata, batch = pici$batch)
write.csv(picires
          ,file = "D:/Rdata/矫正批次差异.csv")

