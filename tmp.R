grad_track = list()

for (k in 1:10){
  grad_tmp = list()
  for (i in 1:8){
    grad = 1:i
    grad = list(grad)
    grad_tmp = c(grad_tmp,grad)
  }
  names(grad_tmp) = c("SW", "W", "NW", "S", "N", "SE", "E", "NE")
  grad_tmp = grad_tmp
  grad_tmp = list(grad_tmp)
  grad_track <- c(grad_track, grad_tmp)
  
}
