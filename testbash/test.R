options(echo=TRUE)
args = commandArgs(TRUE)
if(length(args) > 0) {
  for (i in 1:length(args)) {
    eval(parse(text = args[[i]]))
  }
}
print("step A")
A = matrix(0,5,10)
print("step B")
write.csv(A,paste0("/users/vv233/whitehall-acc/testbash/test",f0,"_",f1,".csv")
