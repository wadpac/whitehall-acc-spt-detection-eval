rm(list=ls())
graphics.off()

include_auc_calculation = FALSE # takes 5 minutes

# load the data
path_algorithm = "/media/vincent/Exeter/exeter_2aug/analysis_logFALSE_defnoc1"
path_logaided = "/media/vincent/Exeter/exeter_2aug/analysis_logTRUE_defnocempty"
path_window = "/media/vincent/Exeter/exeter_2aug/analysis_logFALSE_defnocempty"
Dalg = read.csv(paste0(path_algorithm,"/part4_nightsummary_sleep_cleaned.csv"))
Dlog = read.csv(paste0(path_logaided,"/part4_nightsummary_sleep_cleaned.csv"))
Dwin = read.csv(paste0(path_window,"/part4_nightsummary_sleep_cleaned.csv"))
demogra = read.csv("/media/vincent/Exeter/whitehall_pp_characteristics.csv")

# compare and remove non-matching days and individual
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
Dalg$id = as.integer(as.character(Dalg$id))
Dwin$id = as.integer(as.character(Dwin$id))
matcheddata = matchdata(Dalg,Dlog)
Dalg = matcheddata$x
Dlog = matcheddata$y
matcheddata = matchdata(Dalg,Dwin)
Dalg = matcheddata$x
Dwin = matcheddata$y
matcheddata = matchdata(Dlog,Dwin)
Dlog = matcheddata$x
Dwin = matcheddata$y

# fix waking time
fixwakingtime = function(x) {
  x$acc_wake[which(x$acc_wake < x$acc_onset)] = x$acc_wake[which(x$acc_wake < x$acc_onset)] + 24
  x$log_wake[which(x$log_wake < x$log_onset)] = x$log_wake[which(x$log_wake < x$log_onset)] + 24
  return(x)
}
Dalg = fixwakingtime(Dalg)
Dwin = fixwakingtime(Dwin)
Dlog = fixwakingtime(Dlog)
# only keep individuals with less than 33% indivalid data
removeinvalid =  function(x) x[which(x$fraction_night_invalid < 0.33),]
Dalg = removeinvalid(Dalg)
Dlog = removeinvalid(Dlog)
Dwin = removeinvalid(Dwin)


addweekhalf = function(x) {
  x$weekhalf = "weekenddays"
  x$weekhalf[which(x$weekday %in% c("Sunday","Monday","Tuesday","Wednesday","Thursday") == TRUE)] = "weekdays"
  return(x)
}
Dalg = addweekhalf(Dalg)
Dwin = addweekhalf(Dwin)
Dlog = addweekhalf(Dlog)

addseason = function(x) {
  x$month = sapply(x$calendardate,FUN = function(y) {unlist(strsplit(as.character(y),"/"))[2]})
  x$season = "summer"
  x$season[which(x$month %in% c(11:12,1:4) == TRUE)] = "winter"
  return(x)
}
Dalg = addseason(Dalg)
Dwin = addseason(Dwin)
Dlog = addseason(Dlog)

Dlogwin = merge(Dlog,Dwin,by=c("id","night"),suffixes = c(".log",".win"))
D = merge(Dlogwin,Dalg,by=c("id","night"))
D = D[-which(D$acc_onset == 0 & D$acc_wake ==0),]
data = data.frame(id = D$id, night=D$night,
                  durerror_log = D$acc_timeinbed.log - D$sleeplog_timeinbed.log,
                  wakeerror_log = D$acc_wake.log - D$sleeplog_wake.log,
                  onseterror_log = D$acc_onset.log -  D$sleeplog_onset.log,
                  
                  durerror_win = D$acc_timeinbed.win - D$sleeplog_timeinbed.log,
                  wakeerror_win = D$acc_wake.win - D$sleeplog_wake.log,
                  onseterror_win = D$acc_onset.win -  D$sleeplog_onset.log,

                  durerror_alg = D$acc_timeinbed - D$sleeplog_timeinbed.log,
                  wakeerror_alg = D$acc_wake - D$sleeplog_wake.log,
                  onseterror_alg = D$acc_onset -  D$sleeplog_onset.log,
                  weekhalf = D$weekhalf,season = D$season,
                  
                  dur_log = D$sleeplog_timeinbed.log,
                  dur_win = D$acc_timeinbed.win,
                  dur_alg = D$acc_timeinbed,
                  
                  onset_log = D$sleeplog_onset.log,
                  onset_win = D$acc_onset.win,
                  onset_alg = D$acc_onset,
                  
                  wake_log = D$sleeplog_wake.log,
                  wake_win = D$acc_wake.win,
                  wake_alg = D$acc_wake,
                  
                  sleepeff_log = D$acc_dur_noc.log / D$sleeplog_timeinbed.log,
                  sleepeff_win = D$acc_dur_noc.win / D$acc_timeinbed.win,
                  sleepeff_alg = D$acc_dur_noc / D$acc_timeinbed,
                  
                  n_noc_log = D$acc_n_noc.log,
                  n_noc_win = D$acc_n_noc.win,
                  n_noc_alg = D$acc_n_noc)

#--------------------------
# restructure data frame to only include relevant variables
# data = data.frame(id = Dlog$id, night=Dlog$night,durerror_log = Dlog$acc_timeinbed - Dlog$sleeplog_timeinbed,
#                   wakeerror_log = Dlog$acc_wake - Dlog$sleeplog_wake,
#                   onseterror_log = Dlog$acc_onset -  Dlog$sleeplog_onset,
#                   durerror_win = Dwin$acc_timeinbed - Dlog$sleeplog_timeinbed,
#                   wakeerror_win = Dwin$acc_wake - Dlog$sleeplog_wake,
#                   onseterror_win = Dwin$acc_onset -  Dlog$sleeplog_onset,
#                   durerror_alg = Dalg$acc_timeinbed - Dlog$sleeplog_timeinbed,
#                   wakeerror_alg = Dalg$acc_wake - Dlog$sleeplog_wake,
#                   onseterror_alg = Dalg$acc_onset -  Dlog$sleeplog_onset,
#                   weekhalf = Dlog$weekhalf,season = Dlog$season)

# remove duplicated rows to speed up analysis
data = data[!duplicated(data[,1:6]),]
modeldata = merge(x=data,y=demogra,by.x="id",by.y="STNO")
modeldata$SEX = modeldata$SEX - 1 # 0 is men (N = 3552), 1 is women (N=1328)
modeldata$BMI_uncorrected = modeldata$BMI
modeldata$age = modeldata$age - mean(modeldata$age,na.rm=TRUE)
modeldata$BMI = modeldata$BMI - mean(modeldata$BMI,na.rm=TRUE)

logincorrect = which(modeldata$wake_log < modeldata$onset_log)
if (length(logincorrect) > 0) {
  modeldata$wake_log[logincorrect] = modeldata$wake_log[logincorrect] + 24
}

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

modeldata$onseterror_alg = correct_morethan_24(modeldata$onseterror_alg)
modeldata$onseterror_win = correct_morethan_24(modeldata$onseterror_win)
modeldata$wakeerror_alg = correct_morethan_24(modeldata$wakeerror_alg)
modeldata$wakeerror_win = correct_morethan_24(modeldata$wakeerror_win)
modeldata$durerror_alg = correct_morethan_24(modeldata$durerror_alg)
modeldata$durerror_win = correct_morethan_24(modeldata$durerror_win)

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
    print(ci(roccurve))
    return(auc(roccurve))
  }
  rocdata_alg = data.frame(v=modeldata$onset_alg, y=modeldata$wake_alg, z=modeldata$onset_log, w=modeldata$wake_log)
  rocdata_win = data.frame(v=modeldata$onset_win, y=modeldata$wake_win, z=modeldata$onset_log, w=modeldata$wake_log)
  t0 = Sys.time()
  modeldata$auc_alg = apply(rocdata_alg,1,FUN=calc_rocauc)
  t1 = Sys.time()
  print(t1-t0)
  modeldata$auc_win = apply(rocdata_win,1,FUN=calc_rocauc)
  t2 = Sys.time()
  print(t2-t1)
}

require(nlme)
ctrl = lmeControl(opt="optim")

outputmatrix = matrix("",9,9)

jpeg("/media/vincent/Exeter/distributions_whitehall.jpeg",unit="in",res=400,width = 9,height=5)
par(mfrow=c(1,2))
d1 = density(modeldata$onseterror_alg)
d2 = density(modeldata$onseterror_win)
plot(d1,col="blue",xlim=c(-5,5),bty="l",main="Sleep onset",xlab="Difference with diary (hours)",lend=2)
lines(d2,col="red",lend=2)
legend("topright",legend = c("Algorithm","L5+/-6"),col=c("blue","red"),lty=c(1,1),cex=0.8)
d1 = density(modeldata$wakeerror_alg)
d2 = density(modeldata$wakeerror_win)
plot(d1,col="blue",xlim=c(-5,5),bty="l",main="Waking up",xlab="Difference with diary (hours)",lend=2)
lines(d2,col="red",lend=2)
legend("topright",legend = c("Algorithm","L5+/-6"),col=c("blue","red"),lty=c(1,1),cex=0.8)
dev.off()


d_expl_BMI_auc = aggregate(modeldata,by = list(modeldata$id),mean)

for (domodel in c("log","alg","win"))  {# "win") {  #c("log","alg","win")) { #
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
  if (domodel == "alg") {
    fit.dur = lme(durerror_alg ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    fit.wake = lme(wakeerror_alg ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    fit.onset = lme(onseterror_alg ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    matrixpos = 2
    sderror_dur = sd(modeldata$durerror_alg)
    sderror_wake = sd(modeldata$wakeerror_alg)
    sderror_onset = sd(modeldata$onseterror_alg)
  } else if (domodel == "win") {
    fit.dur = lme(durerror_win ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    fit.wake = lme(wakeerror_win ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    fit.onset = lme(onseterror_win ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    matrixpos = 3
    sderror_dur = sd(modeldata$durerror_win)
    sderror_wake = sd(modeldata$wakeerror_win)
    sderror_onset = sd(modeldata$onseterror_win)
  } else if (domodel == "log") {
    fit.dur = lme(durerror_log ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    fit.wake = lme(wakeerror_log ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    fit.onset = lme(onseterror_log ~ SEX + age + BMI + weekhalf + season, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
    matrixpos = 1
    sderror_dur = sd(modeldata$durerror_log)
    sderror_wake = sd(modeldata$wakeerror_log)
    sderror_onset = sd(modeldata$onseterror_log)
  }
  print(paste0("dur AIC ",AIC(fit.dur)," BIC ",BIC(fit.dur)))
  print(paste0("onset AIC ",AIC(fit.onset)," BIC ",BIC(fit.onset)))
  print(paste0("wake AIC ",AIC(fit.wake)," BIC ",BIC(fit.wake)))

  # if (domodel == "alg") {
  #   fit.dur = lme(durerror_alg ~ SEX, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
  #   fit.wake = lme(wakeerror_alg ~ SEX, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
  #   fit.onset = lme(onseterror_alg ~ SEX, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
  #   matrixpos = 2
  # } else if (domodel == "win") {
  #   fit.dur = lme(durerror_win ~ SEX, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
  #   fit.wake = lme(wakeerror_win ~ SEX, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
  #   fit.onset = lme(onseterror_win ~ SEX, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
  #   matrixpos = 3
  # } else if (domodel == "log") {
  #   fit.dur = lme(durerror_log ~ SEX, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
  #   fit.wake = lme(wakeerror_log ~ SEX, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
  #   fit.onset = lme(onseterror_log ~ SEX, random = ~1|night/id,data=modeldata,control=ctrl,na.action = na.omit)
  #   matrixpos = 1
  # }
  
  outputmatrix[matrixpos,] = c(getcoef(fit.dur,ndigits),sderror_dur * 60)
  outputmatrix[matrixpos+3,] = c(getcoef(fit.wake,ndigits),sderror_wake * 60)
  outputmatrix[matrixpos+6,] = c(getcoef(fit.onset,ndigits),sderror_onset * 60)
  
  printsum2 = function(x,ndigits) {
    print(paste0("SD between", round(as.numeric(VarCorr(x)[4,2])*60,digits=ndigits)))
    print(paste0("SD within", round(as.numeric(VarCorr(x)[5,2])*60,digits=ndigits)))
    print(summary(x)$coefficients$fixed)
    tmp = as.numeric(summary(x)$coefficients$fixed) * 60
    tmp[3] = tmp[4] * 10
    tmp[4] = tmp[4] * 5
    print(round(tmp,digits=ndigits))
  }
  
  print(">>>>>>>>>>>>>>> dur")
  printsum2(fit.dur,ndigits)
  print(">>>>>>>>>>>>>>> wake")
  printsum2(fit.wake,ndigits)
  print(">>>>>>>>>>>>>>> onset")
  printsum2(fit.onset,ndigits)
}

write.csv(outputmatrix,file="/media/vincent/Exeter/table_2.csv")