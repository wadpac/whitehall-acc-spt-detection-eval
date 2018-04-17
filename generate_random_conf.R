rm(list=ls())

set.seed(300)

for (i in 1:30) {
  A = matrix(0,4,1)
  A[1] = round(runif(n = 1,min = 0.05,max= 0.15),digits=2)
  A[2] = round(runif(n = 1,min = 10,max= 20),digits=0)
  A[3] = round(runif(n = 1,min = 15,max= 45),digits=0)
  A[4] = round(runif(n = 1,min = 30,max= 90),digits=0)
  cat(paste0("perc = ",A[1],"; inbedthreshold=",A[2],
               "; bedblocksize =",A[3],"; outofbedsize=",A[4],"  #id=",i))
  cat("\n")
}