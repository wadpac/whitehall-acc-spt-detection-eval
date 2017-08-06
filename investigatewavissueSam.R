# Description: use this R-script to specify and run all your analyses with function g.shell.GGIR from package GGIR
# By: Vincent van Hees, April 2015
#====================================================
# no input needed
rm(list=ls())
graphics.off()
#==================================================================
# INPUT NEEDED:
# specify file number to start and end with, fill in c() if unknown
f0 = c() #file to start with if used in serial analyses
f1 = c() #file to end with if used in serial analyses (modify accordingly, if infinite then it will process until last file)
#======================================================

################################################
# INPUT NEEDED:
mode= c(1) #What part of the analysis needs to be done (options: 1,2,3,4 and 5)

datadir= "/media/windows-share/Exeter/problematicwavfiles" #"Q:/data/ucl example bins" #c() #Where is the raw accelerometer data? (leave as c() if you work with milestone data and mode > 1
outputdir= "/media/windows-share/Exeter"#"Q:/scripts/GGIR/test_version14" #Name directory where output needs to be stored
studyname="problematicwavfiles" #name of sutdy, only needed if datadir is a list of filenames

#=====================================================================================
# load functions from functions folder (replace by require(GGIR) once package is updated)
ffnames = dir("/home/vincent/GGIR/mcs-acc/R") # creating list of filenames of scriptfiles to load
for (i in 1:length(ffnames)) {
  source(paste("/home/vincent/GGIR/mcs-acc/R/",ffnames[i],sep="")) #loading scripts for reading geneactiv data
}


g.shell.GGIR(#=======================================
             # INPUT NEEDED:
             #-------------------------------
             # General parameters
             #-------------------------------
             mode=mode, #specify above
             datadir=datadir, #specify above
             outputdir=outputdir, #specify above
             studyname=studyname, #specify above
             f0=f0, #specify above
             f1=f1, #specify above
             overwrite = TRUE, #overwrite previous milestone data?
             do.imp=TRUE, # Do imputation? (recommended)
             idloc=1, #id location (1 = file header, 2 = filename)Rcpp::
             print.filename=TRUE,
             storefolderstructure = FALSE,
             #-------------------------------
             # Part 1 parameters:
             #-------------------------------
             # Key functions: reading file, auto-calibration, and extracting features
             windowsizes = c(5,900,3600), #Epoch length, non-wear detection resolution, non-wear detection evaluation window
             do.cal= TRUE, # Apply autocalibration? (recommended)
             do.enmo = TRUE, #Needed for physical activity analysis
             do.anglez=TRUE, #Needed for sleep detection
             chunksize=1, #size of data chunks to be read (value = 1 is maximum)
             printsummary=TRUE,
             #-------------------------------
             # Part 2 parameters:
             #-------------------------------
             # Key functions: Non-wear detection, imputation, and basic descriptives
             strategy = 1, #Strategy (see tutorial for explanation)
             ndayswindow=7, #only relevant when strategy = 3
             hrs.del.start = 0, # Only relevant when strategy = 2. How many HOURS need to be ignored at the START of the measurement?
             hrs.del.end = 0, # Only relevant when strategy = 2. How many HOURS need to be ignored at the END of the measurement?
             maxdur = 7, # How many DAYS of measurement do you maximumally expect?
             includedaycrit = 1, # number of minimum valid hours in a day to attempt physical activity analysis
             L5M5window = c(0,24), #window over which to calculate L5 and M5
             M5L5res = 10, #resolution in minutes of M5 and L5 calculation
             winhr = c(5,10), # size of M5 and L5 (5 hours by default)
             qlevels = c(c(1380/1440),c(1410/1440)), #quantiles to calculate, set value at c() if you do not want quantiles
             qwindow=c(0,24), #window over which to calculate quantiles
             ilevels = c(seq(0,400,by=50),8000), #acceleration values (metric ENMO) from which a frequency distribution needs to be derived, set value at c() if you do not want quantiles
             mvpathreshold =c(100,120), #MVPA (moderate and vigorous physical activity threshold
             bout.metric = 4,
             closedbout=FALSE,
             #-------------------------------
             # Part 3 parameters:
             #-------------------------------
             # Key functions: Sleep detection
             timethreshold= c(5,10,15,20), #10
             anglethreshold=5,
             ignorenonwear = TRUE, # if TRUE non-wear is not detected as sleep (if FALSE then it will work with imputed data)
             #-------------------------------
             # Part 4 parameters:
             #-------------------------------
             # Key functions: Integrating sleep log (if available) with sleep detection, storing day and person specific summaries of sleep
             excludefirstlast = TRUE, # Exclude first and last night for sleep analysis?
             includenightcrit = 16, # number of minimum valid hours in a day to attempt sleep analysis
             def.noc.sleep = c(),
             # If sleep log is available:
             loglocation= "/media/windows-share/London/sleeplog_corrected_v5.csv", # full directory and name of the log (if available, otherwise leave value as c() )
             outliers.only = TRUE,
             criterror = 4,
             relyonsleeplog = FALSE,
             sleeplogidnum = TRUE, # Is the participant in the sleep log stored as a number (TRUE) or as a character (FALSE)
             colid=1, #colomn in which the participant id or filename is stored
             coln1=2, #column number for first day
             do.visual = TRUE,
             nnights = 9, #number of nights in the sleep log
             #-------------------------------
             # Part 5 parameters:
             #-------------------------------
             # Key functions: Merging physical activity with sleep analyses
             threshold.lig = c(30), #40 #threshold(s) for inactivity (can be more than one number)
             threshold.mod = c(100), #100 120 #threshold(s) for moderate activity (can be more than one number)
             threshold.vig = c(400), #500 #threshold(s) for vigorous activity (can be more than one number)
             boutcriter = 0.8,
             boutcriter.in = 0.9, #0.8 #fraction of an inactivity bout that needs to be below the threshold (needs to be 1 number)
             boutcriter.lig = 0.8, #fraction of an light activity bout that needs to be between the thresholds (needs to be 1 number)
             boutcriter.mvpa = 0.8, #fraction of an light activity bout that needs to be above the threshold (needs to be 1 number)
             boutdur.in = c(1,10,30), # duration of bouts to be calculated
             boutdur.lig = c(1,10), # duration of bouts to be calculated
             boutdur.mvpa = c(1), # duration of bouts to be calculated
             timewindow = c("WW","MM"), #,
             #-----------------------------------
             # Report generation
             #-------------------------------
             # Key functions: Generating reports based on meta-data
             do.report=c(2), #for what parts does and report need to be generated? (option: 2, 4 and 5)
             visualreport=FALSE,
             dofirstpage = TRUE, #first page of pdf-report with simple summary histograms
             viewingwindow=1) #viewingwindow of visual report: 1 centres at day and 2 centers at night
