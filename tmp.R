isobars = list()
if (length(cyclone_centers)){
  maxLatInd = length(data_tmp$lat)
  maxLonInd = length(data_tmp$lon)
  for (i in 1:nrow(cyclone_centers)){
    center_lat_ind = cyclone_centers$lat_ind[i]
    center_lon_ind = cyclone_centers$lon_ind[i]
    k = center_lat_ind
    l = center_lon_ind
    level_found = logical()
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
        while((k >= 1) & (k <= maxLatInd) & (l>=1) & (l<= maxLonInd) &
              (k + p1*d >= 1) & (k + p1*d <= maxLatInd) &
              (l + p2*d >= 1) & (l + p2*d <= maxLonInd)){
          d = 1
          repeat{
            # Calculating distance between two points
            # https://ru.wikipedia.org/wiki/%D0%A1%D1%84%D0%B5%D1%80%D0%B0 
            # "Расстояние между двумя точками на сфере"
            theta1 = data_tmp$lat[k + p1*d]
            theta2 = data_tmp$lat[k]
            phi1 = data_tmp$lon[l + p2*d]
            phi2 = data_tmp$lon[l]
            theta1 = theta1/180*pi
            theta2 = theta2/180*pi
            phi1 = phi1/180*pi
            phi2 = phi2/180*pi
            L = R*acos(sin(theta1)*sin(theta2) + 
                         cos(theta1)*cos(theta2)*cos(phi1-phi2))
            if (L >= Lmin) break
            d = d + 1
            if (((k + p1*d) < 1) | (k + p1*d > maxLatInd) |
                (l + p2*d < 1) | (l + p2*d > maxLonInd)) break
          }
          if (((k + p1*d) < 1) | (k + p1*d > maxLatInd) |
              (l + p2*d < 1) | (l + p2*d > maxLonInd)) break
          
          grad = (data_tmp$values[l + p2*d, k + p1*d] - data_tmp$values[l,k])/L*100
          if (grad < grad_limit){
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
            distance_to_center[count] = distCosine(point1,point2)
            break
          }
          k = k + p1*d
          l = l + p2*d
        }
      } 
    }
    min_dist_ind = which.min(distance_to_center)
    isobar_level = level[min_dist_ind]
    contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, 
                            z = data_tmp$values, level = isobar_level)
    contour = find_contour(contours, level_lon_ind[min_dist_ind], level_lat_ind[min_dist_ind])
    isobars = c(isobars, contour)
  }
}