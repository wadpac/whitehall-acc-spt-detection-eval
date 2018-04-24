rm(list=ls())
graphics.off()
set.seed(300)
A = matrix(0,31,4)
A[1,] = c(0.10,15,30,60)
for (i in 1:30) {
  
  A[i+1,1] = round(runif(n = 1,min = 0.05,max= 0.15),digits=2)
  A[i+1,2] = round(runif(n = 1,min = 10,max= 20),digits=0)
  A[i+1,3] = round(runif(n = 1,min = 15,max= 45),digits=0)
  A[i+1,4] = round(runif(n = 1,min = 30,max= 90),digits=0)
}
plotname = c("whitehall","psgncl_left","psgncl_right","penn","combined")
for (ploti in 1:5) {
  if (ploti == 1) {
    senan_out = read.csv("/media/vincent/Exeter/sensitivity_output.csv")
    senan_out[,2] = senan_out[,2] * 60
    
  } else if (ploti == 2 | ploti == 3) { 
    senan_out = read.csv("/media/vincent/Exeter/psg_newcastle_sensitivity_analysis.csv")
    
    senan_out[,2] = senan_out[,ploti]
    senan_out =  senan_out[,-3]
  } else if (ploti == 4) { 
    senan_out = read.csv("/media/vincent/Exeter/psg_penn_sensitivity_analysis.csv")
  } else if (ploti == 5) { 
    whiteh = read.csv("/media/vincent/Exeter/sensitivity_output.csv")
    whiteh[,2] = whiteh[,2] * 60
    nclpsg = read.csv("/media/vincent/Exeter/psg_newcastle_sensitivity_analysis.csv")
    nclpsg[,2] = (nclpsg[,2] + nclpsg[,3]) / 2
    nclpsg = nclpsg[,-3]
    pennpsg = read.csv("/media/vincent/Exeter/psg_penn_sensitivity_analysis.csv")
    senan_out = pennpsg
    senan_out[,2] = (whiteh[,2] + nclpsg[,2] + pennpsg[,2]) / 3
  }
  
  senan_out = cbind(senan_out,A)
  colnames(senan_out) = c("id","MAE","percentage","threshold","shortW","longW")
  
  NAi = which(is.na(senan_out$MAE) == TRUE)
  if (length(NAi) > 0) senan_out$MAE[NAi] = 0
  
  
  
  senan_out$percentage= senan_out$percentage*100
  senan_out = senan_out[order(senan_out$MAE),]
  default = which(senan_out$id == 0)
  CXD = 2
  CXA = 1.8
  
  png(filename = paste0("/media/vincent/Exeter/whitehall_sensitivity/images/sensitivityanalysis_hdcza_parameters_",plotname[ploti],".jpg"),width = 5,height=5,units = "in",res = 600)
  par(mfrow=c(5,1),mar=c(1.5,5,1,2.5), oma=c(0,0,1,0),font.lab = 2,cex.axis=0.8,las=2)
  miny = floor(min(senan_out$MAE)/10)*10
  maxy = ceiling(max(senan_out$MAE)/10)*10
  # Ngroups = floor(abs(diff(range(senan_out$MAE))) / 10)
  YY = seq(miny,maxy,by=round((maxy-miny)/5))
  plot(0:30,senan_out$MAE,type="p",pch=20,axes=FALSE,xlab="",ylab="MAE (min)",bty="l",ylim=range(YY)*c(0.95,1.05),cex=CXA)
  axis(side = 2,at = YY,labels = YY,tick = TRUE)
  axis(side = 1,at = 0:30,labels = senan_out$id,tick = TRUE,las=2)
  lines(default-1,senan_out$MAE[default],type="p",pch=20,col="red",cex=CXD)
  grid()
  
  YY = seq(0.05,0.15,by=0.05) * 100
  plot(0:30,senan_out$percentage,type="p",pch=20,axes=FALSE,xlab="",ylab="percentile",bty="l",ylim=range(YY)*c(0.95,1.05),cex=CXA)
  axis(side = 2,at = YY,labels = YY,tick = TRUE)
  lines(default-1,senan_out$percentage[default],type="p",pch=20,col="red",cex=CXD)
  grid()
  
  YY = seq(10,20,by=5)
  plot(0:30,senan_out$threshold,type="p",pch=20,axes=FALSE,xlab="",ylab="threshold",bty="l",ylim=range(YY)*c(0.95,1.05),cex=CXA)
  axis(side = 2,at = YY,labels = YY,tick = TRUE)
  lines(default-1,senan_out$threshold[default],type="p",pch=20,col="red",cex=CXD)
  grid()
  
  
  YY = seq(15,45,by=15)
  plot(0:30,senan_out$shortW,type="p",pch=20,axes=FALSE,xlab="",ylab="short block (min)",bty="l",ylim=range(YY)*c(0.95,1.05),cex=CXA)
  axis(side = 2,at = YY,labels = YY,tick = TRUE)
  lines(default-1,senan_out$shortW[default],type="p",pch=20,col="red",cex=CXD)
  grid()
  
  YY = seq(30,90,by=15)
  plot(0:30,senan_out$longW,type="p",pch=20,axes=FALSE,xlab="",ylab="long block (min)",bty="l",ylim=range(YY)*c(0.95,1.05),cex=CXA)
  axis(side = 2,at = YY,labels = YY,tick = TRUE)
  lines(default-1,senan_out$longW[default],type="p",pch=20,col="red",cex=CXD)
  grid()
  
  dev.off()
}