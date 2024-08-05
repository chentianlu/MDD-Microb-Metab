library(openxlsx);library(mediation)

file<-"C:/Users/RyanG/Desktop/gutmet.xlsx"#输入文件地址

x<-read.xlsx(file,rowNames = T,colNames = T)
result<-data.frame(Column1 = numeric(0),Column2 = numeric(0),stringsAsFactors = FALSE)

for (i in 1:86) {#x列号
  
  for (j in 87:117) {#中介列号
    
    treat<-colnames(x)[i]
    mediator<-colnames(x)[j]
    
    y<-colnames(x)[118]#y结局列号
    
    mom<-lm(paste(mediator, "~", treat), data = x)
    moy<-lm(paste(y, "~", treat, "+", mediator),data = x)
    medres<-mediate(mom,moy,boot = T,treat = treat,mediator = mediator)
    res<-cbind(paste0(treat,"~",mediator,"~",y),capture.output(summary(medres))[7:10])
    result[(nrow(result)+1):(nrow(result)+4),]<-res
    
  }
}

write.csv(result,paste0(dirname(file),"/mediator_effect.csv"))
