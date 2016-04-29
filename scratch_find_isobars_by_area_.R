contour_complementing = function(contour, maxLonInd, maxLatInd){
  lon_start = contour$x[1]
  lat_start = contour$y[1]
  lon_end = contour$x[length(contour$x)]
  lat_end = contour$y[length(contour$y)]
  
  if (lat_start == lat_end){
    lat_add = lat_start
    lon_add = round((lon_start + lon_end)/2)
    contour_closed = contour
    contour_closed$x = c(lon_add,contour_closed$x,lon_add)
    contour_closed$y = c(lat_add,contour_closed$y,lat_add)
    return(contour_closed)
  }
  
  if (lon_start == lon_end){
    lat_add = round((lat_start + lat_end)/2) 
    lon_add = lon_start
    contour_closed = contour
    contour_closed$x = c(lon_add,contour_closed$x,lon_add)
    contour_closed$y = c(lat_add,contour_closed$y,lat_add)
    return(contour_closed)
  }
  
  if (lon_start == lon_end){
    lat_add = round((lat_start + lat_end)/2) 
    lon_add = lon_start
    contour_closed = contour
    contour_closed$x = c(lon_add,contour_closed$x,lon_add)
    contour_closed$y = c(lat_add,contour_closed$y,lat_add)
    return(contour_closed)
  }
  
}










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
      k = center_lat_ind
      l = center_lon_ind
      if (k > maxLatInd/2){
        p1 = -1
      }else{p1 = 1}
      if (l > maxLonInd/2){
        p2 = -1
      }else{p2 = 1}
      
      d = 1
      # делаем, чтобы расстояние от центра до первой проверочной точки было не меньше Lmin
      {
      lat1 = data_tmp$lat[center_lat_ind]
      lon1 = data_tmp$lon[center_lon_ind]
      point1 = c(lon1, lat1)
      lat2 = data_tmp$lat[k + p1*d]
      lon2 = data_tmp$lon[l + p2*d]
      point2 = c(lon2, lat2)
      distance_to_center = distCosine(point1,point2)/1000
      while(  (k >= 1) & (k <= maxLatInd) & (l>=1) & (l<= maxLonInd) &
              distance_to_center < Lmin ){
        lat2 = data_tmp$lat[k + p1*d]
        lon2 = data_tmp$lon[l + p2*d]
        point2 = c(lon2, lat2)
        distance_to_center = distCosine(point1,point2)/1000
        k = k + p1*d
        l = l + p2*d
      }
      }
      
      
      
      
      
      current_contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, z = data_tmp$values, 
                                      level = data_tmp$values[l, k])
      current_contour = find_contour(current_contours, l, k)
      contour_result = NULL
      k = k + p1*step
      l = l + p2*step
      while ( (k >= 1) & (k <= maxLatInd) & (l>=1) & (l<= maxLonInd) ){
        next_contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, 
                                     z = data_tmp$values, level = data_tmp$values[l,k])
        next_contour = find_contour(next_contours, l, k)
        
        # эти преобразования необходимы для расчёта площади в функции areaPolygon()
        next_contour_lon = data_tmp$lon[round(next_contour[[1]]$x)]
        next_contour_lat = data_tmp$lat[round(next_contour[[1]]$y)]
        next_contour_matrix = matrix(c(next_contour_lon, next_contour_lat), ncol = 2)
        current_contour_lon = data_tmp$lon[round(current_contour[[1]]$x)]
        current_contour_lat = data_tmp$lat[round(current_contour[[1]]$y)]
        current_contour_matrix = matrix(c(current_contour_lon, current_contour_lat), ncol = 2)
        # ------------------------------------------------------------------- #
        
        rate = areaPolygon(next_contour_matrix)/ areaPolygon(current_contour_matrix)
        if (rate > rateMax){
          contour_result = current_contour
          break
        }else{current_contour = next_contour}
        k = k + p1*step
        l = l + p2*step
      }
      if (is.null(contour_result)) contour_result = current_contour
      isobars = c(isobars, contour_result)
    }
  }
  return(isobars)
}