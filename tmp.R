isobars = list()
if (length(cyclone_centers)){
  maxLatInd = length(data_tmp$lat)
  maxLonInd = length(data_tmp$lon)
  for (i in 1:nrow(cyclone_centers)){
    level_found = FALSE
    center_lat_ind = cyclone_centers$lat_ind[i]
    center_lon_ind = cyclone_centers$lon_ind[i]
    if ((maxLatInd - 1) > (maxLatInd - center_lat_ind)){
      closest_lat_edge = 1
      stepLat = -1
    }else{
      closest_lat_edge = maxLatInd
      stepLat = 1
    } 
    if ((maxLonInd - 1) > (maxLonInd - center_lon_ind)){
      closest_lon_edge = 1
      stepLon = -1
    }else{
      closest_lon_edge = maxLonInd
      stepLon = 1
    }
    k = center_lat_ind
    while((k >= 1) & (k <= maxLatInd) & (k + stepLat >= 1) & (k + stepLat <= maxLatInd)){
      grad = data_tmp$values[center_lon_ind,k + stepLat] - data_tmp$values[center_lon_ind,k]
      if (grad < 0){
        level_found = TRUE
        level = data_tmp$values[center_lon_ind,k]
        break
      }
    }
    if (level_found){
      contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, 
                              z = data_tmp$values, level = level)
      contour = find_contour(contours, center_lon_ind, k)
      isobars = c(isobars, contour)
    } 
  }
}