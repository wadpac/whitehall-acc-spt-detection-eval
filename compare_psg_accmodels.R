rm(list=ls()) # remove this line if you don't want your memory to be cleaned.
graphics.off()

#=============================================================================
# The analyses are done based on the csv output from R package GGIR named part4_....csv.
# which I created three times, with three configurations, the results are in the following folders
# as defined below (update with your own directories):
#=============================================================
# USER INPUT NEEDED:

# the new algorithm (hdcza)
path_hdcza = "/media/vincent/Exeter/exeter_2aug/analysis_logFALSE_defnoc1"
# SPT-window detection as described in the 2015 PLoSONE paper, van Hees, Sabia, et al.
path_logaided = "/media/vincent/Exeter/exeter_2aug/analysis_logTRUE_defnocempty" #<= not used in paper
# the L5+6 approach
path_l5hr6 = "/media/vincent/Exeter/exeter_2aug/analysis_logFALSE_defnocempty"

# path to demographic data file
path_demogra = "/media/vincent/Exeter/whitehall_pp_characteristics.csv"
# whether or not to do the auc calculation (can take 5 mintues)
include_auc_calculation = FALSE

# Disclaimer: The term timeined bed is used in this script, which actually refers to the SPT-window
#=================================
# load the data
Dhdcza = read.csv(paste0(path_hdcza,"/part4_nightsummary_sleep_cleaned.csv"))
Dlogaided = read.csv(paste0(path_logaided,"/part4_nightsummary_sleep_cleaned.csv"))
Dl5hr6 = read.csv(paste0(path_l5hr6,"/part4_nightsummary_sleep_cleaned.csv"))
# load particpant demographical information
demogra = read.csv(path_demogra)
# compare identifiers in psg and accelerometer files names and ignore non-matching files
matchdata = function(x,y) {
  fun.12 <- function(x.1,x.2,...){
    x.1p <- do.call("paste", x.1)
    x.2p <- do.call("paste", x.2)
    x.1[! x.1p %in% x.2p, ]
  }
  xt = x[,c(1:2,6)]
  yt = y[,c(1:2,6)]
  H = fun.12(xt,yt)
  G = fun.12(yt,xt)
  x.1p <- do.call("paste", xt)
  x.2p <- do.call("paste", H)
  x = x[! x.1p %in% x.2p, ]
  x.1p <- do.call("paste", yt)
  x.2p <- do.call("paste", G)
  y= y[! x.1p %in% x.2p, ]
  invisible(list(x=x,y=y))
  
}
Dhdcza$id = as.integer(as.character(Dhdcza$id))
Dl5hr6$id = as.integer(as.character(Dl5hr6$id))
md = matchdata(Dhdcza,Dlogaided)
Dhdcza = md$x
Dlogaided = md$y
md = matchdata(Dhdcza,Dl5hr6)
Dhdcza = md$x
Dl5hr6 = md$y
md = matchdata(Dlogaided,Dl5hr6)
Dlogaided = md$x
Dl5hr6 = md$y
# check that waking up times are not wrong by 24 hours and fix if so
fixwakingtime = function(x) {
  x$acc_wake[which(x$acc_wake < x$acc_onset)] = x$acc_wake[which(x$acc_wake < x$acc_onset)] + 24
  x$log_wake[which(x$log_wake < x$log_onset)] = x$log_wake[which(x$log_wake < x$log_onset)] + 24
  return(x)
}
Dhdcza = fixwakingtime(Dhdcza)
Dl5hr6 = fixwakingtime(Dl5hr6)
Dlogaided = fixwakingtime(Dlogaided)
# only keep nights with less than 33% indivalid data
removeinvalid =  function(x) x[which(x$fraction_night_invalid < 0.33),]
Dhdcza = removeinvalid(Dhdcza)
Dlogaided = removeinvalid(Dlogaided)
Dl5hr6 = removeinvalid(Dl5hr6)
# add a column to specify whether it was a weekend or a weekday
addweekhalf = function(x) {
  x$weekhalf = "weekenddays"
  x$weekhalf[which(x$weekday %in% c("Sunday","Monday","Tuesday","Wednesday","Thursday") == TRUE)] = "weekdays"
  return(x)
}
Dhdcza = addweekhalf(Dhdcza)
Dl5hr6 = addweekhalf(Dl5hr6)
Dlogaided = addweekhalf(Dlogaided)
# add a column to specify whether it was l5hr6ter or summer
addseason = function(x) {
  x$month = sapply(x$calendardate,FUN = function(y) {unlist(strsplit(as.character(y),"/"))[2]})
  x$season = "summer"
  x$season[which(x$month %in% c(11:12,1:4) == TRUE)] = "winter"
  return(x)
}
Dhdcza = addseason(Dhdcza)
Dl5hr6 = addseason(Dl5hr6)
Dlogaided = addseason(Dlogaided)
# merge the three data frames
Dlogaidedl5hr6 = merge(Dlogaided,Dl5hr6,by=c("id","night"),suffixes = c(".logaided",".l5hr6"))
D = merge(Dlogaidedl5hr6,Dhdcza,by=c("id","night"))
D = D[-which(D$acc_onset == 0 & D$acc_wake ==0),]
# remove individuals with more nocturnal sleep than the lengths of the SPT window (should not happen)
removeimplausable = which(D$acc_dur_noc > D$acc_timeinbed | D$acc_dur_noc.logaided > D$acc_timeinbed.logaided | D$acc_dur_noc.l5hr6 > D$acc_timeinbed.l5hr6)
if (length(removeimplausable) > 0) {
  D = D[-removeimplausable,]
}
# correct 24 hour shifts in sleeplog data
logincorrect2 = which(D$sleeplog_wake < D$sleeplog_onset)
logincorrect2.logaided = which(D$sleeplog_wake.logaided< D$sleeplog_onset.logaided)
if (length(logincorrect2) > 0) D$sleeplog_wake[logincorrect2] = D$sleeplog_wake[logincorrect2] + 24
if (length(logincorrect2.logaided) > 0) D$sleeplog_wake.logaided[logincorrect2.logaided] = D$sleeplog_wake.logaided[logincorrect2.logaided] + 24
# put relevant variables in a new dataframe
data = data.frame(id = D$id, night=D$night,
                  #  Note that the extension .logaided in the next 3 lines is just to indicate that it came from
                  # the logaided output but of course sleeplog itself is not aided by sleeplog.
                  durerror_l5hr6 = D$acc_timeinbed.l5hr6 - D$sleeplog_timeinbed.logaided, 
                  wakeerror_l5hr6 = D$acc_wake.l5hr6 - D$sleeplog_wake.logaided,
                  onseterror_l5hr6 = D$acc_onset.l5hr6 -  D$sleeplog_onset.logaided,
                  durerror_hdcza = D$acc_timeinbed - D$sleeplog_timeinbed.logaided,
                  wakeerror_hdcza = D$acc_wake - D$sleeplog_wake.logaided,
                  onseterror_hdcza = D$acc_onset -  D$sleeplog_onset.logaided,
                  weekhalf = D$weekhalf,season = D$season,

                  dur_l5hr6 = D$acc_timeinbed.l5hr6, dur_hdcza = D$acc_timeinbed, dur_logaided = D$sleeplog_timeinbed.logaided, 
                  onset_l5hr6 = D$acc_onset.l5hr6, onset_hdcza = D$acc_onset, onset_logaided = D$sleeplog_onset.logaided, 
                  wake_l5hr6 = D$acc_wake.l5hr6, wake_hdcza = D$acc_wake, wake_logaided = D$sleeplog_wake.logaided, 
                  sleepeff_l5hr6 = D$acc_dur_noc.l5hr6 / D$acc_timeinbed.l5hr6,
                  sleepeff_hdcza = D$acc_dur_noc / D$acc_timeinbed,
                  
                  # needed for investigating role wakefullness on error:
                  dur_logaided = D$sleeplog_timeinbed.logaided, 
                  sleepeff_logaided = D$acc_dur_noc.logaided / D$acc_timeinbed.logaided)

# remove duplicated rows to speed up analysis (not really needed, but good to check anyway)
data = data[!duplicated(data[,1:2]),]
# merge data with demographic data to get dataframe for regression model
modeldata = merge(x=data,y=demogra,by.x="id",by.y="STNO")
modeldata$SEX = modeldata$SEX - 1 # 0 is men (N = 3552), 1 is women (N=1328)
modeldata$BMI_uncorrected = modeldata$BMI
modeldata$age_uncorrected = modeldata$age
modeldata$age = modeldata$age - mean(modeldata$age,na.rm=TRUE)
modeldata$BMI = modeldata$BMI - mean(modeldata$BMI,na.rm=TRUE)


correct_morethan_24 = function(x) {
  lessthan24 = which(x < -24)
  morethan24 = which(x > 24)
  if (length(lessthan24) > 0) {
    x[lessthan24] = x[lessthan24] + 24
  }
  if (length(morethan24) > 0) {
    x[morethan24] = x[morethan24] - 24
  }
  return(x)
}
modeldata$onseterror_hdcza = correct_morethan_24(modeldata$onseterror_hdcza)
modeldata$onseterror_l5hr6 = correct_morethan_24(modeldata$onseterror_l5hr6)
modeldata$wakeerror_hdcza = correct_morethan_24(modeldata$wakeerror_hdcza)
modeldata$wakeerror_l5hr6 = correct_morethan_24(modeldata$wakeerror_l5hr6)
modeldata$durerror_hdcza = correct_morethan_24(modeldata$durerror_hdcza)
modeldata$durerror_l5hr6 = correct_morethan_24(modeldata$durerror_l5hr6)

print("AUC")
if (include_auc_calculation == TRUE) {
  # calculate auc
  library(pROC)
  calc_rocauc = function(x) {
    Nepochs_hour = 60 # auc calculated with 1 minute resolution data to speed up calculations
    i0 = min(x[1], x[3]) - 0.25 #start index
    i1_est = round((x[2]-i0) * Nepochs_hour) #end index for est sleep relative to start index
    i1_dia = round((x[4]-i0) * Nepochs_hour) #end index for diary sleep relative to start index
    i1 = max(c(i1_est,i1_dia, 24*60))
    rocdata = data.frame(est=rep(0,i1),dia=rep(0,i1))
    rocdata$est[round((x[1]-i0)*Nepochs_hour):i1_est] = 1 # start index for est sleep relative to start : i1_est
    rocdata$dia[round((x[3]-i0)*Nepochs_hour):i1_dia] = 1 # start index for diary sleep relative to start : i1_est
    roccurve = roc(rocdata$dia ~ rocdata$est)
    return(auc(roccurve))
  }
  rocdata_hdcza = data.frame(v=modeldata$onset_hdcza, y=modeldata$wake_hdcza, z=modeldata$onset_log, w=modeldata$wake_log)
  rocdata_l5hr6 = data.frame(v=modeldata$onset_l5hr6, y=modeldata$wake_l5hr6, z=modeldata$onset_log, w=modeldata$wake_log)
  t0 = Sys.time()
  modeldata$auc_hdcza = apply(rocdata_hdcza,1,FUN=calc_rocauc)
  t1 = Sys.time()
  print(t1-t0)
  modeldata$auc_l5hr6 = apply(rocdata_l5hr6,1,FUN=calc_rocauc)
  t2 = Sys.time()
  print(t2-t1)
}
require(nlme)
ctrl = lmeControl(opt="optim")
addLoAdots = function(xy1,xy2,d1,d2) {
  LoA = quantile(xy1,probs=c(0.05,0.25,0.75,0.95))
  for (i in 1:length(LoA)) {
    lines(x=LoA[i], y=d1$y[which.min(abs(d1$x - LoA[i]))],col="blue",type="p",pch=20,cex=0.9)
  }
  LoA = quantile(xy2,probs=c(0.05,0.25,0.75,0.95))
  for (i in 1:length(LoA)) {
    lines(x=LoA[i], y=d2$y[which.min(abs(d2$x - LoA[i]))],col="red",type="p",pch=20,cex=0.9)
  }
}
print("Figure 2")
outputmatrix = matrix("",9,9)
jpeg("/media/vincent/Exeter/Figure2.jpeg",unit="in",res=600,width = 7,height=3)
par(mfrow=c(1,3),mar=c(5,4,2,1))
d1 = density(modeldata$durerror_hdcza)
d2 = density(modeldata$durerror_l5hr6)
plot(d1,col="blue",ylim=range(d1$y)*c(1,1.25),xlim=c(-5,5),bty="l",main="Sleep duration",xlab="Difference with diary (hours)",lend=2,
     lab.cex=0.5,main.cex=0.6,axis.cex=0.5,cex=0.6)
lines(d2,col="red",lend=2)
addLoAdots(xy1=modeldata$durerror_hdcza,xy2=modeldata$durerror_l5hr6,d1,d2)
legend("topright",legend = c("HDCZA","L5+/-6"),col=c("blue","red"),lty=c(1,1),cex=0.8)
#-----------------------------
d1 = density(modeldata$onseterror_hdcza)
d2 = density(modeldata$onseterror_l5hr6)
plot(d1,col="blue",ylim=range(d1$y)*c(1,1.25),xlim=c(-5,5),bty="l",main="Sleep onset",xlab="Difference with diary (hours)",lend=2,
     lab.cex=0.5,main.cex=0.6,axis.cex=0.5,cex=0.5)
lines(d2,col="red",lend=2)
addLoAdots(xy1=modeldata$onseterror_hdcza,xy2=modeldata$onseterror_l5hr6,d1,d2)
legend("topright",legend = c("HDCZA","L5+/-6"),col=c("blue","red"),lty=c(1,1),cex=0.8)
#-----------------------------
d1 = density(modeldata$wakeerror_hdcza)
d2 = density(modeldata$wakeerror_l5hr6)
plot(d1,col="blue",ylim=range(d1$y)*c(1,1.25),xlim=c(-5,5),bty="l",main="Waking up",xlab="Difference with diary (hours)",lend=2,
     lab.cex=0.5,main.cex=0.6,axis.cex=0.5,cex=0.6)
lines(d2,col="red",lend=2)
addLoAdots(xy1=modeldata$wakeerror_hdcza,xy2=modeldata$wakeerror_l5hr6,d1,d2)
legend("topright",legend = c("HDCZA","L5+/-6"),col=c("blue","red"),lty=c(1,1),cex=0.8)
dev.off()



# investigate wake duration in relation to error in the estimation of SPT-window duration
modeldata$awakeduration = modeldata$dur_logaided * (1-modeldata$sleepeff_logaided)
fit.wakeduration_hdcza = lme(durerror_hdcza ~ awakeduration, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)

# aggretate per individual to faciltiate analyses at participant level
d_expl_BMI_auc = aggregate(modeldata,by = list(modeldata$id),mean)
print("Regression modelling")
for (domodel in c("hdcza","l5hr6")) {    # c("hdcza","l5hr6")
  print("-----------------------------------")
  print(domodel)
  ndigits = 1
  getcoef = function(x,ndigits) {
    tmp = as.numeric(summary(x)$coefficients$fixed) * 60
    tmp[3] = tmp[3] * 10
    tmp[4] = tmp[4] * 5
    tmp = round(tmp,digits=ndigits)
    se = as.numeric(summary(x)$tTable[,2])  * 60
    se[3] = se[3] * 10
    se[4] = se[4] * 5
    se = round(se,digits=ndigits)
    tmp = paste(tmp,"(",se,")",sep="")
    
    tmp = c(tmp,round(as.numeric(VarCorr(x)[4,2])*60,digits=ndigits))
    tmp = c(tmp,round(as.numeric(VarCorr(x)[5,2])*60,digits=ndigits))
    return(tmp)
  }
  if (domodel == "hdcza") {
    fit.dur = lme(durerror_hdcza ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    fit.wake = lme(wakeerror_hdcza ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    fit.onset = lme(onseterror_hdcza ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    matrixpos = 2
    sderror_dur = sd(modeldata$durerror_hdcza)
    sderror_wake = sd(modeldata$wakeerror_hdcza)
    sderror_onset = sd(modeldata$onseterror_hdcza)
  } else if (domodel == "l5hr6") {
    fit.dur = lme(durerror_l5hr6 ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    fit.wake = lme(wakeerror_l5hr6 ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    fit.onset = lme(onseterror_l5hr6 ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    matrixpos = 3
    sderror_dur = sd(modeldata$durerror_l5hr6)
    sderror_wake = sd(modeldata$wakeerror_l5hr6)
    sderror_onset = sd(modeldata$onseterror_l5hr6)
  }
  outputmatrix[matrixpos,] = c(getcoef(fit.dur,ndigits),summary(fit.dur)$AIC) # sderror_dur * 60)
  outputmatrix[matrixpos+3,] = c(getcoef(fit.wake,ndigits),summary(fit.wake)$AIC) #,sderror_wake * 60)
  outputmatrix[matrixpos+6,] = c(getcoef(fit.onset,ndigits),summary(fit.onset)$AIC) #,sderror_onset * 60)
  
  printsum2 = function(x,ndigits) {
    print(paste0("SD between", round(as.numeric(VarCorr(x)[4,2])*60,digits=ndigits)))
    print(paste0("SD within", round(as.numeric(VarCorr(x)[5,2])*60,digits=ndigits)))
    print(summary(x)$coefficients$fixed)
    tmp = as.numeric(summary(x)$coefficients$fixed) * 60
    tmp[3] = tmp[4] * 10
    tmp[4] = tmp[4] * 5
    print(round(tmp,digits=ndigits))
  }
  
  print(">>>>>>>>>>>>>>> SPT window duration")
  printsum2(fit.dur,ndigits)
  print(">>>>>>>>>>>>>>> wake up time")
  printsum2(fit.wake,ndigits)
  print(">>>>>>>>>>>>>>> onset time")
  printsum2(fit.onset,ndigits)
}

# Calculate MAE to facilitate some comparison with O'Donnell 2018
modeldata$wakeerror_hdcza_abs = abs(modeldata$wakeerror_hdcza) # calculate absolute error in wake time per night
modeldata$onseterror_hdcza_abs = abs(modeldata$onseterror_hdcza) # calculate absolute error in onset time per night
modeldata_mae = aggregate(modeldata,by = list(modeldata$id),mean) # aggregate both wake and onset per person
MAE = round(mean(c(modeldata$wakeerror_hdcza_abs,modeldata$onseterror_hdcza_abs)),digits=3) # calcualte mean acros individuals.
print(paste0("MAE = ", MAE * 60))


print("Correlations table 3")
print(cor.test(d_expl_BMI_auc$wake_hdcza,d_expl_BMI_auc$wake_logaided,paired=TRUE))
print(cor.test(d_expl_BMI_auc$onset_hdcza,d_expl_BMI_auc$onset_logaided,paired=TRUE))
print(cor.test(d_expl_BMI_auc$dur_hdcza,d_expl_BMI_auc$dur_logaided))
print(cor.test(d_expl_BMI_auc$wake_l5hr6,d_expl_BMI_auc$wake_logaided,paired=TRUE))
print(cor.test(d_expl_BMI_auc$onset_l5hr6,d_expl_BMI_auc$onset_logaided,paired=TRUE))
print(cor.test(d_expl_BMI_auc$dur_l5hr6,d_expl_BMI_auc$dur_logaided))

char_of_finalsample = d_expl_BMI_auc[which(is.na(d_expl_BMI_auc$age) == FALSE & is.na(d_expl_BMI_auc$BMI_uncorrected) == FALSE),]



write.csv(outputmatrix,file="/media/vincent/Exeter/table_2.csv")