rm(list=ls()) # remove this line if you don't want your memory to be cleaned.
graphics.off()

#=============================================================================
# The analyses are done based on the csv output from R package GGIR named part4_....csv.
# which I created three times, with three configurations, the results are in the following folders
# as defined below (update with your own directories):
#=============================================================
# USER INPUT NEEDED:

# the new algorithm (hdcza)

# datapath = "/media/vincent/Exeter/exeter_2aug2017"
datapath = "/media/vincent/Exeter/exeter_11apr2018"

path_hdcza = paste0(datapath,"/analysis_logFALSE_defnoc1")
# SPT-window detection as described in the 2015 PLoSONE paper, van Hees, Sabia, et al.
path_logaided = paste0(datapath,"/analysis_logTRUE_defnocempty") #<= not used in paper
# the L5+6 approach
path_l5hr6 = paste0(datapath,"/analysis_logFALSE_defnocempty")

# path to demographic data file
path_demogra = "/media/vincent/Exeter/whitehall_pp_characteristics.csv"
# whether or not to do the auc calculation (can take 5 mintues)
include_auc_calculation = FALSE

senan_out = matrix(0,31,2)
# Disclaimer: The term timeined bed is used in this script, which actually refers to the SPT-window
for (senanid in  0:30) {
  #=================================
  # load the data
  # Dhdcza = read.csv(paste0(path_hdcza,"/part4_nightsummary_sleep_cleaned.csv"))
  path_part4_sensitivityanalysis = "/media/vincent/Exeter/whitehall_sensitivity/output_whitehallsensitivity/results"
  Dhdcza = read.csv(paste0(path_part4_sensitivityanalysis,"/part4_nightsummary_sleep_cleaned_id",senanid,".csv"))
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

  # investigate wake duration in relation to error in the estimation of SPT-window duration
  modeldata$awakeduration = modeldata$dur_logaided * (1-modeldata$sleepeff_logaided)
  # Calculate MAE to facilitate some comparison with O'Donnell 2018
  modeldata$wakeerror_hdcza_abs = abs(modeldata$wakeerror_hdcza) # calculate absolute error in wake time per night
  modeldata$onseterror_hdcza_abs = abs(modeldata$onseterror_hdcza) # calculate absolute error in onset time per night
  modeldata_mae = aggregate(modeldata,by = list(modeldata$id),mean) # aggregate both wake and onset per person
  MAE = round(mean(c(modeldata$wakeerror_hdcza_abs,modeldata$onseterror_hdcza_abs)),digits=3) # calcualte mean acros individuals.
  print(paste0("id: ",senanid," MAE hdcza: ", MAE * 60))
  senan_out[senanid+1,] = c(senanid,MAE)
}

write.csv(senan_out,"/media/vincent/Exeter/sensitivity_output.csv",row.names = FALSE)