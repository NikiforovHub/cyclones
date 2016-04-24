find_isobars_binary = function(data_tmp, cyclone_centers){
  isobars = list()
  min = min(data_tmp$values)
  max = max(data_tmp$values)
  data_tmp$values = (data_tmp$values-min)/(max-min)
  values = (data_tmp$values-min)/(max-min)
  threshold = 0.2
  values[values >= threshold] = 0
  maxLatInd = length(data_tmp$lat)
  maxLonInd = length(data_tmp$lon)
  for(icycl in 1:nrow(cyclone_centers)){
    center_lon_ind = cyclone_centers$lon_ind[icycl]
    center_lat_ind = cyclone_centers$lat_ind[icycl]
    if (center_lat_ind > maxLatInd/2){
      p1 = -1
    }else{p1 = 1}
    if (center_lon_ind > maxLonInd/2){
      p2 = -1
    }else{p2 = 1}
    k = center_lat_ind
    l = center_lon_ind
    
    while(data_tmp$values[l,k]&
          (k!=1|p1==1)&(k!=maxLatInd|p1==-1)&
          (l!=1|p2==1)&(l!=maxLonInd|p2==-1)){
      k = k + p1
      l = l + p2
    }
    contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, z = data_tmp$values, 
                                    level = data_tmp$values[l, k])
    contour = find_contour(contours, l, k)
    isobars = c(isobars, contour)
  }
  return(isobars)
}