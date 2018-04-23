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
  # cat(paste0("perc = ",A[1],"; inbedthreshold=",A[2],
  #            "; bedblocksize =",A[3],"; outofbedsize=",A[4],"  #id=",i))
  # cat("\n")
}


senan_out = read.csv("/media/vincent/Exeter/sensitivity_output.csv")
senan_out[,2] = senan_out[,2] * 60
senan_out = cbind(senan_out,A)

colnames(senan_out) = c("id","MAE","percentage","threshold","shortW","longW")
senan_out$percentage= senan_out$percentage*100
senan_out = senan_out[order(senan_out$MAE),]
default = which(senan_out$id == 0)
CXD = 2

png(filename = "/media/vincent/Exeter/sensitivityanalysis_hdcza_parameters_whitehall.jpg",width = 7,height=5,units = "in",res = 600)
par(mfrow=c(5,1),mar=c(2,5,1,2.5), font.lab = 2,cex.axis=0.8,las=2)
YY = seq(34,48,by=4)
plot(0:30,senan_out$MAE,type="p",pch=20,axes=FALSE,xlab="",ylab="MAE",bty="l",ylim=range(YY))
axis(side = 2,at = YY,labels = YY,tick = TRUE)
lines(default-1,senan_out$MAE[default],type="p",pch=20,col="red",cex=CXD)
grid()

YY = seq(0.05,0.15,by=0.05) * 100
plot(0:30,senan_out$percentage,type="p",pch=20,axes=FALSE,xlab="",ylab="percentile",bty="l",ylim=range(YY))
axis(side = 2,at = YY,labels = YY,tick = TRUE)
lines(default-1,senan_out$percentage[default],type="p",pch=20,col="red",cex=CXD)
grid()

YY = seq(10,20,by=5)
plot(0:30,senan_out$threshold,type="p",pch=20,axes=FALSE,xlab="",ylab="threshold",bty="l",ylim=range(YY))
axis(side = 2,at = YY,labels = YY,tick = TRUE)
lines(default-1,senan_out$threshold[default],type="p",pch=20,col="red",cex=CXD)
grid()


YY = seq(15,45,by=15)
plot(0:30,senan_out$shortW,type="p",pch=20,axes=FALSE,xlab="",ylab="short block (min)",bty="l",ylim=range(YY))
axis(side = 2,at = YY,labels = YY,tick = TRUE)
lines(default-1,senan_out$shortW[default],type="p",pch=20,col="red",cex=CXD)
grid()

YY = seq(30,90,by=15)
plot(0:30,senan_out$longW,type="p",pch=20,axes=FALSE,xlab="",ylab="long block (min)",bty="l",ylim=range(YY))
axis(side = 2,at = YY,labels = YY,tick = TRUE)
lines(default-1,senan_out$longW[default],type="p",pch=20,col="red",cex=CXD)
grid()

dev.off()
