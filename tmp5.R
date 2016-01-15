find_isobars_by_alpha = function(data_tmp, cyclone_centers, grad_limit){
  isobars = list()
  if (length(cyclone_centers)){ 
    maxLatInd = length(data_tmp$lat)
    maxLonInd = length(data_tmp$lon)
    min_pressure = min(data_tmp$values)
    max_pressure = max(data_tmp$values)
    level_pressure = min_pressure + (max_pressure - min_pressure)*0.3
    for (i in 1:nrow(cyclone_centers)){
      center_lat_ind = cyclone_centers$lat_ind[i]
      center_lon_ind = cyclone_centers$lon_ind[i]
      k = center_lat_ind
      l = center_lon_ind
      #level_found = logical()
      level_found = logical(8)
      level = numeric()
      level_lat_ind = integer()
      level_lon_ind = integer()
      distance_to_center = numeric()
      grad = numeric()
      count = 0
      for(p1 in c(-1,0,1)){
        # order of directions: NW,N,NE, W,E, SW,S,SE
        for(p2 in c(-1,0,1)){
          if((p1 == 0) & (p2 == 0)) next
          count = count + 1
          k=center_lat_ind
          l=center_lon_ind
          d = 1
          while((k >= 1) & (k <= maxLatInd) & (l>=1) & (l<= maxLonInd)){
            if  ((data_tmp$values[l,k] < level_pressure + 0.1) & 
                 (data_tmp$values[l,k] > level_pressure - 0.1)){
              level_found[count] = TRUE
              level[count] = data_tmp$values[l,k]
              level_lat_ind[count] = k
              level_lon_ind[count] = l
              lat1 = data_tmp$lat[center_lat_ind]
              lon1 = data_tmp$lon[center_lon_ind]
              point1 = c(lon1, lat1)
              lat2 = data_tmp$lat[k]
              lon2 = data_tmp$lon[l]
              point2 = c(lon2, lat2)
              distance_to_center[count] = distCosine(point1,point2)/1000
            }
            k = k + 1
            l = l + 1
          }
        } 
      }
      ## ----------------------------- ##
      #temporary block
      for (j in 1:8){
        if (level_found[j] == FALSE){
          level[count] = 1
          level_lat_ind[count] = 1
          level_lon_ind[count] = 1
          distance_to_center[count] = 100000
        }
      }
      ## ----------------------------- ##
      if (length(level_found)){
        for (m in 1:length(level_found)){
          contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, 
                                  z = data_tmp$values, level = level[m])
          contour = find_contour(contours, level_lon_ind[m], level_lat_ind[m])
          isobars = c(isobars, contour)
        }
      }
    }
  }
  return(isobars)
}