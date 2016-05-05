library(foreach)
library(doParallel)
library(geosphere)

num_cores = detectCores() - 1
cl<-makeCluster(num_cores)
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(geosphere)
  library(lubridate)})

p1 = c(0,0)
p2 = c(45,45)
filenames = list("a","b","c")
foreach(filename = filenames)  %dopar%  
{
  y = rep(distCosine(p1,p2),5)
  save(y, file = paste0("other/",filename))
  z = tz(date())
  save(z, file = paste0("other/",filename,"date"))
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