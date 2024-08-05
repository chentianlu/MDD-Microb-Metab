#install.packages("openxlsx")
#install.packages("psych")
parcorr<-function(file,xcol,ycol,confoundercol=NULL,method="spearman"){
  library(openxlsx);library(psych)
  parcor<-read.xlsx(file,colNames = T)
  if (is.null(confoundercol)) {
    r <- as.data.frame(matrix(NA, nrow = length(ycol), ncol = length(xcol)))
    rownames(r)<-colnames(parcor[,ycol]);colnames(r)<-colnames(parcor[,xcol])
    p<-r
    for (i in seq_along(xcol)) {
      for (j in seq_along(ycol)) {
        cor_test <- cor.test(parcor[, xcol[i]], parcor[, ycol[j]], method = method)
        r[j, i] <- cor_test$estimate
        p[j, i] <- cor_test$p.value
      }
    }
    wb<-createWorkbook()
    addWorksheet(wb, "Rvalue");addWorksheet(wb, "Pvalue")
    writeData(wb,"Rvalue",r,rowNames = T);writeData(wb,"Pvalue",p,rowNames = T)
    saveWorkbook(wb, paste0(dirname(file),"/cor_result.xlsx"), overwrite = TRUE)
    print("Done!")
  }else{
    parcor.ppcor<-partial.r(parcor,c(xcol,ycol),c(confoundercol),method = method)
    r<-lowerUpper(parcor.ppcor)$lower
    p<-lowerUpper(corr.p(parcor.ppcor,n=nrow(parcor))$p)$lower
    wb<-createWorkbook()
    addWorksheet(wb, "Rvalue");addWorksheet(wb, "Pvalue")
    writeData(wb,"Rvalue",r,rowNames = T);writeData(wb,"Pvalue",p,rowNames = T)
    saveWorkbook(wb, paste0(dirname(file),"/parcor_result.xlsx"), overwrite = TRUE)
    print("Done!")
  }
}
