find_isobars_by_max = function(data_tmp, cyclone_centers, Lmax){
  isobars = list()
  nCenters = nrow(cyclone_centers)
  maxLatInd = length(data_tmp$lat)
  maxLonInd = length(data_tmp$lon)
  for (icycl in 1:nCenters){
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
    test_vector = numeric()
    L_to_center = 0
    i = 1
    max = 0
    while((L_to_center < Lmax) &
          (k!=1|p1==1)&(k!=maxLatInd|p1==-1)&
          (l!=1|p2==1)&(l!=maxLonInd|p2==-1)){
      if (data_tmp$values[l,k] > max){
        max = data_tmp$values[l,k]
        kmax = k
        lmax = l
      }
      k = k + p1
      l = l + p2
      lat1 = data_tmp$lat[center_lat_ind]
      lon1 = data_tmp$lon[center_lon_ind]
      point1 = c(lon1, lat1)
      lat2 = data_tmp$lat[k]
      lon2 = data_tmp$lon[l]
      point2 = c(lon2, lat2)
      L_to_center = distCosine(point1,point2)/1000
    }
    contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, z = data_tmp$values, 
                            level = data_tmp$values[lmax, kmax])
    contour = find_contour(contours, lmax, kmax)
    isobars = c(isobars, contour)
  }
  return(isobars)
}