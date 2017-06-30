options(echo=TRUE)
args = commandArgs(TRUE)
if(length(args) > 0) {
  for (i in 1:length(args)) {
    eval(parse(text = args[[i]]))
  }
}

A = matrix(0,5,10)
write.csv(A,paste0("/users/vv233/test",f0,"_",f1,".csv")
