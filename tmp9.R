num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)



  tryCatch({
    a = parLapply(cl, c("a",2:4),
              function(text){
                text^2
                fileConn = file("test.txt")
                write(text, file = "test.txt", append = TRUE)
                close(fileConn)
              }
                )
    
  }, error=function(e, f = filename){
    cat("ERROR :",conditionMessage(e), "\n")
    print(paste("in ",filename,sep=""))
    })

stopCluster(cl)