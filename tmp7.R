find_contour = function(contours, center_lon_ind, check_lat_ind){
  l = length(contours)
  contour = NULL
  if (l){
    contour = NULL
    for (i in 1:l){
#       contours[[i]]$x = sapply(contours[[i]]$x,round)
#       contours[[i]]$y = sapply(contours[[i]]$y,round)
      bool = ((contours[[i]]$x < center_lon_ind + 1) & 
              (contours[[i]]$x > center_lon_ind - 1))
      lat_ind_vector = contours[[i]]$y[bool]
      m = length(lat_ind_vector)
      if (m){
        for (k in 1:m){
          if ((lat_ind_vector[k] < check_lat_ind + 1) &
              (lat_ind_vector[k] > check_lat_ind - 1)){
            level = list(level = contours[[i]]$level)
            x = list(x = contours[[i]]$x)
            y = list(y = contours[[i]]$y)
            contour = list(c(level, x, y))
            break
          }
        }
      }
      if (!is.null(contour)) break
    }
  }
  if (is.null(contour)){
    if (l){
      level = list(level = contours[[1]]$level)
    }else{
      level = list(level = 1)
    }
    x = c(1:10)
    y = c(1:10)
    x = list(x = x)
    y = list(y = y)
    contour = list(c(level, x, y))
  }
  return(contour)
}