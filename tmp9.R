get_radiuses = function(contour, data_tmp){
  center_point = c(contour$geom_center_lon, contour$geom_center_lat)
  contour$x = round(contour$x)
  contour$y = round(contour$y)
  lon = data_tmp$lon[contour$x]
  lat = data_tmp$lat[contour$y]
  radiuses = NULL
  for (i in 1:length(contour$x)){
    isobar_point = c(lon[i], lat[i])
    radiuses[i] = distCosine(center_point,isobar_point)
  }
  max_r = max(radiuses)/1000
  max_index = which.max(radiuses)
  max_point = c(lon[max_index], lat[max_index])
  brng = bearing(center_point, max_point) + 90
  grCircle = greatCircleBearing(center_point, brng, n = 1080)
  bool = logical(nrow(grCircle))
  for (i in 1:nrow(grCircle)){
    for (j in 1:length(lon)){
      if ((grCircle[i,1] > lon[j] - 0.25) & (grCircle[i,1] < lon[j] + 0.25)){
        bool[i] = TRUE
      }
    }
  }
  grCircle_sub = matrix(grCircle[bool], ncol = 2)
  bool = logical(length(lat))
  for (i in 1:nrow(grCircle_sub)){
    for (j in 1:length(lat)){
      if ((grCircle_sub[i,1] > lon[j] - 0.25) & (grCircle_sub[i,1] < lon[j] + 0.25) &
        (grCircle_sub[i,2] > lat[j] - 0.25) & (grCircle_sub[i,2] < lat[j] + 0.25)){
        bool[j] = TRUE
      }
    }
  }
  lon_prob = lon[bool]
  lat_prob = lat[bool]
  probs = matrix(c(lon_prob, lat_prob), ncol = 2)
  d = NULL
  for (i in 1:nrow(probs)){
    point_prob = c(probs[i,1],probs[i,2])
    d[i] = distCosine(center_point, point_prob)/1000
  }
  min_r = min(d[i])
  radiuses = matrix(c(min_r[1], max_r), ncol = 2)
  colnames(radiuses) = c("Rx","Ry")
  return(radiuses)
}