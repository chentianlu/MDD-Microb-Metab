library(openxlsx);library(car)
#-------------------------------------------------------------------------------
fil<-"C:/Users/赵明亮/Desktop/CUMS_差异_万能分析.xlsx"#读取数据地址
metlog<-read.xlsx(fil)
metlog$pfgroup<-metlog[,37]#指定分组信息在第几列，分组必须用0，1表示
strtc<-2;endc<-36#代谢物从第几列到第几列
#-------------------------------------------------------------------------------
metlog$pfgroup<-as.factor(metlog$pfgroup)
nomt<-as.data.frame(matrix(nrow = 4,ncol = ncol(metlog)))
colnames(nomt)<-colnames(metlog)
rownames(nomt)<-c("normality","homosced","differP","method")

for (i in strtc:endc) {
  nomt[1,i]<-shapiro.test(metlog[,i])$p.value
  nomt[2,i]<-leveneTest(metlog[,i] ~ pfgroup, metlog)$`Pr(>F)`[1]
  x<-metlog[which(metlog$pfgroup==1),i]
  y<-metlog[which(metlog$pfgroup==0),i]
  if (nomt[1,i]>=0.05) {
    nomt[3,i]<-wilcox.test(x,y)$p.value
    nomt[4,i]<-"Utest"
  } else {
    if (nomt[2,i]<0.05) {
      nomt[3,i]<-t.test(x, y, var.equal = F)$p.value
      nomt[4,i]<-"WTtest"
    } else {
      nomt[3,i]<-t.test(x, y, var.equal = T)$p.value
      nomt[4,i]<-"Ttest"
    }
  }
}
write.csv(t(nomt[,strtc:endc]),paste0(dirname(fil),"/difference_analysis.csv"))
          