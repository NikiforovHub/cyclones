find_isobars_by_area = function(data_tmp, cyclone_centers, rateMax){
  step = 25 # шаг, в точках сетки
  Lmin = 50
  isobars = list()
  if (length(cyclone_centers)){ 
    maxLatInd = length(data_tmp$lat)
    maxLonInd = length(data_tmp$lon)
    for (i in 1:nrow(cyclone_centers)){
      center_lat_ind = cyclone_centers$lat_ind[i]
      center_lon_ind = cyclone_centers$lon_ind[i]
      
      
      if (k > maxLatInd/2){
        p1 = -1
      }else{p1 = 1}
      if (l > maxLonInd/2){
        p2 = -1
      }else{p2 = 1}
      
      
      k = center_lat_ind
      l = center_lon_ind
      d = 1
      # делаем, чтобы расстояние от центра до первой проверочной точки было не меньше Lmin
      repeat{
        lat1 = data_tmp$lat[center_lat_ind]
        lon1 = data_tmp$lon[center_lon_ind]
        point1 = c(lon1, lat1)
        lat2 = data_tmp$lat[k + d]
        lon2 = data_tmp$lon[l + d]
        point2 = c(lon2, lat2)
        distance_to_center = distCosine(point1,point2)/1000
        if (distance_to_center >= Lmin) break
        d = d + 1
        if (((k + p1*d) < 1) | (k + p1*d > maxLatInd) |
            (l + p2*d < 1) | (l + p2*d > maxLonInd)) break
      }
      if (((k + p1*d) < 1) | (k + p1*d > maxLatInd) |
          (l + p2*d < 1) | (l + p2*d > maxLonInd)){
        d = d - 1
      }
      k = k + p1*d
      l = l + p2*d
      
      
      current_contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, z = data_tmp$values, 
                                      level = data_tmp$values[l, k])
      current_contour = find_contour(current_contours, center_lon_ind, k)
      contour_result = NULL
      k = k + p1*step
      l = l + p2*step
      while (((k + p1*d) < 1) | (k + p1*d > maxLatInd) |
             (l + p2*d < 1) | (l + p2*d > maxLonInd)){
        
        next_contour = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, 
                                    z = data_tmp$values, level = data_tmp$values[l,k])
        if ( areaPolygon(next_contour)/areaPolygon(current_contour) > rateMax ){
          contour_result = current_contour
          break
        }else{contour_result = next_contour}
        k = k + p1*25
        l = l + p2*25
      }
    }
    if (is.null(contour_result)) contour_result = current_contour
    isobars = c(isobars, contour_result)
  }
  return(isobars)
}