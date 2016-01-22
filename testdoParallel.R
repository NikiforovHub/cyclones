library(foreach)
library(doParallel)
library(geosphere)

num_cores = detectCores() - 1
cl<-makeCluster(num_cores)
registerDoParallel(cl)
clusterEvalQ(cl, library(geosphere))

a = 5

foreach(x=list("a","b","c"))  %dopar%  
{
  tryCatch({
    distCosine(c(10,10),c(20,20))
    fileConn = file(paste(x,".txt",sep = ""))
    write(distCosine(c(10,10),c(20,20)), file = paste(x,".txt",sep = ""), append = TRUE)
    close(fileConn)
  }, error = function(e) return(paste0("The variable '", x, "'", 
                                       " caused the error: '", e, "'")))
}
  
stopImplicitCluster()




# foreach(x=list(3, 4, "a"))  %dopar%  
# {
#   tryCatch({
#     c(1/x, x, 2^x)
#   }, error = function(e) return(paste0("The variable '", x, "'", 
#                                        " caused the error: '", e, "'")))
# }


# test <- function (exponent) {
#   foreach(exponent = 2:4, 
#           .combine = c)  %dopar%  
#     base^exponent
# }
# test()
# 
# 
# test2 <- function(x){
#   x*a
# }