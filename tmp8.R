cache_file = "test_error.txt"

tryCatch({
  if (FALSE){
    for (i in 1:20){
      gc()
      if (i == 10) stop("too many iterations")
    }
  }
  

}, error=function(e, f = filename){
  cat("ERROR :",conditionMessage(e), "\n")
  print(paste("in ",filename,sep=""))
  write(c(conditionMessage(e), i), 
        file = cache_file)
})