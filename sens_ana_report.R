# Sensitivity analysis STEP 3
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

overview = 1:31

path_sens = "/media/vincent/Exeter/whitehall_sensitivity"

plotname = c("Sleep diary (Whitehall II Study)","Sleep clinic PSG left wrist (Newcastle)",
             "Sleep clinic PSG right wrist (Newcastle)","Healthy good sleepers PSG non-dominant wrist (Pennsylvania)","combined")
for (ploti in 1:5) {
  if (ploti == 1) {
    senan_out = read.csv(paste0(path_sens,"/sensitivity_output.csv"))
    senan_out[,2] = senan_out[,2] * 60
  } else if (ploti == 2 | ploti == 3) { 
    senan_out = read.csv(paste0(path_sens,"/psg_newcastle_sensitivity_analysis.csv"))
    
    senan_out[,2] = senan_out[,ploti]
    senan_out =  senan_out[,-3]
  } else if (ploti == 4) { 
    senan_out = read.csv(paste0(path_sens,"/psg_penn_sensitivity_analysis.csv"))
  } else if (ploti == 5) { 
    whiteh = read.csv(paste0(path_sens,"/sensitivity_output.csv"))
    whiteh[,2] = whiteh[,2] * 60
    nclpsg = read.csv(paste0(path_sens,"/psg_newcastle_sensitivity_analysis.csv"))
    nclpsg[,2] = (nclpsg[,2] + nclpsg[,3]) / 2
    nclpsg = nclpsg[,-3]
    pennpsg = read.csv(paste0(path_sens,"/psg_penn_sensitivity_analysis.csv"))
    senan_out = pennpsg
    senan_out[,2] = (whiteh[,2] + nclpsg[,2] + pennpsg[,2]) / 3
  }
  
  senan_out = cbind(senan_out,A)
  colnames(senan_out) = c("id","MAE","percentage","threshold","shortW","longW")
  
  # NAi = which(is.na(senan_out$MAE) == TRUE)
  # if (length(NAi) > 0) senan_out$MAE[NAi] = 0
  
  senan_out$percentage= senan_out$percentage*100
  senan_out = senan_out[order(senan_out$MAE),]
  default = which(senan_out$id == 0)

  
  senan_out$ranking = 1:31
  
  CXD = 3
  CXA = 3
  print(plotname[ploti])
  ZZ = c(0.95,1.05)
  png(filename = paste0(path_sens,"/images/sensitivityanalysis_hdcza_parameters_",plotname[ploti],".jpg"),
      width = 7,height=7,units = "in",res = 600)
  par(mfrow=c(5,1),mar=c(1.5,5,1,2.5), oma=c(0,0,1,0),font.lab = 2,cex.axis=1.2,las=2,cex.main=1.5,cex.lab=1.5)
  miny = floor(min(senan_out$MAE)/10)*10
  maxy = ceiling(max(senan_out$MAE)/10)*10
  
  # Ngroups = floor(abs(diff(range(senan_out$MAE))) / 10)
  YY = seq(miny,maxy,by=round((maxy-miny)/5))
  # if ((maxy - miny) > sd(senan_out$MAE) * 2) {
  #   YY = c(34.5, 35, 35.5)
  #   ZZ = c(0.99,1.01)
  # }
  plot(0:30,senan_out$MAE,type="p",pch=20,axes=FALSE,xlab="",ylab="MAE (min)",bty="l",ylim=range(YY)*ZZ,cex=CXA,
       main=plotname[ploti])
  axis(side = 2,at = YY,labels = YY,tick = TRUE)
  axis(side = 1,at = 0:30,labels = senan_out$id,tick = TRUE,las=2)
  lines(default-1,senan_out$MAE[default],type="p",pch=1,col="red",cex=CXD*1.5,lwd=2)
  print(range(senan_out$MAE[1:default]))
  grid()
  
  YY = seq(0.05,0.15,by=0.05) * 100
  plot(0:30,senan_out$percentage,type="p",pch=20,axes=FALSE,xlab="",ylab="percentile",bty="l",ylim=range(YY)*ZZ,cex=CXA)
  axis(side = 2,at = YY,labels = YY,tick = TRUE)
  lines(default-1,senan_out$percentage[default],type="p",pch=1,col="red",cex=CXD*1.5,lwd=2)
  grid()
  
  YY = seq(10,20,by=5)
  plot(0:30,senan_out$threshold,type="p",pch=20,axes=FALSE,xlab="",ylab="threshold",bty="l",ylim=range(YY)*c(0.95,1.05),cex=CXA)
  axis(side = 2,at = YY,labels = YY,tick = TRUE)
  lines(default-1,senan_out$threshold[default],type="p",pch=1,col="red",cex=CXD*1.5,lwd=2)
  grid()
  
  
  YY = seq(15,45,by=15)
  plot(0:30,senan_out$shortW,type="p",pch=20,axes=FALSE,xlab="",ylab="short block (min)",bty="l",ylim=range(YY)*c(0.95,1.05),cex=CXA)
  axis(side = 2,at = YY,labels = YY,tick = TRUE)
  # lines(default-1,senan_out$shortW[default],type="p",pch=20,col="red",cex=CXD)
  lines(default-1,senan_out$shortW[default],type="p",pch=1,col="red",cex=CXD*1.5,lwd=2)
  grid()
  
  YY = seq(30,90,by=15)
  plot(0:30,senan_out$longW,type="p",pch=20,axes=FALSE,xlab="",ylab="long block (min)",bty="l",ylim=range(YY)*c(0.95,1.05),cex=CXA)
  axis(side = 2,at = YY,labels = YY,tick = TRUE)
  lines(default-1,senan_out$longW[default],type="p",pch=1,col="red",cex=CXD*1.5,lwd=2)
  grid()
  
  dev.off()
  
  if (ploti == 1) {
    SWH = senan_out[order(senan_out$id),]
  } else if (ploti == 2) {
    SNL = senan_out[order(senan_out$id),]
  } else if (ploti == 3) {
    SNR = senan_out[order(senan_out$id),]
  } else if (ploti == 4) {
    SPE = senan_out[order(senan_out$id),]
  }
}

RM = rowMeans(cbind(SWH$MAE,SNL$MAE,SNR$MAE,SPE$MAE),dims=1)
RSD = rowMeans(abs(cbind(SWH$MAE,SNL$MAE,SNR$MAE,SPE$MAE)-RM),dims=1)

png(filename = paste0(path_sens,"/images/sensitivityanalysis_hdcza_parameters_aggregated.jpg"),
    width = 7,height=7,units = "in",res = 600)
par(mar=c(5,5,3,2),font.lab=2)
plot(RSD[1:31],RM[1:31],pch=20,type="p",xlab="std. dev. in MAE across studies (minutes)",
     ylab="mean MAE across studies (minutes)",cex=2,bty="n",cex.lab=1.4,cex.axis=1.2)
lines(RSD[1],RM[1],pch=1,type="p",col="red",cex=3,lwd=2.5)
txtx = RSD
txty = RM + 1
# txtx[6] = txtx[6] + 0.1
# txty[6] = txty[6] - 0.2
txtx[19] = txtx[19] + 0.1

txtx[25] = txtx[25] - 0.2
txty[25] = txty[25] - 0.2
txtx[31] = txtx[31] + 0.2
txty[31] = txty[31] - 0.3
text(txtx,txty, SNR$id,cex = 0.8)
dev.off()



