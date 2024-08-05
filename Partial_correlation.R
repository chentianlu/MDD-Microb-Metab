#install.packages("openxlsx")
#install.packages("psych")
parcorr<-function(file,xcol,ycol,confoundercol){
  library(openxlsx);library(psych)
  parcor<-read.xlsx(file,colNames = T)
  parcor.ppcor<-partial.r(parcor,c(xcol,ycol),c(confoundercol),method = "spearman")
  r<-lowerUpper(parcor.ppcor)$lower
  p<-lowerUpper(corr.p(parcor.ppcor,n=nrow(parcor))$p)$lower
  wb<-createWorkbook()
  addWorksheet(wb, "Rvalue");addWorksheet(wb, "Pvalue")
  writeData(wb,"Rvalue",r,rowNames = T);writeData(wb,"Pvalue",p,rowNames = T)
  saveWorkbook(wb, paste0(dirname(file),"/parcor_result.xlsx"), overwrite = TRUE)
  print("Done!")
}
#file:文件地址,xcol:自变量所在的列,ycol:因变量所在的列,confoundercol:混杂变量所在的列