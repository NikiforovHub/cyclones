nCenters = nrow(cyclone_centers)
maxLatInd = length(data_tmp$lat)
maxLonInd = length(data_tmp$lon)
values_data = list()
values_data_count = 1
for (i in 1:nCenters){
  center_lat_ind = cyclone_centers$lat_ind[i]
  center_lon_ind = cyclone_centers$lon_ind[i]
  values_lines_count = 0
  for(p1 in c(-1,0,1)){
    values_lines = list()
    for(p2 in c(-1,0,1)){
      values_count = 1
      if((p1 == 0) & (p2 == 0)) next
      values_lines_count = values_lines_count + 1
      k=center_lat_ind
      l=center_lon_ind
      values_lines[[values_lines_count]] = NA
      while((k >= 1) & (k <= maxLatInd) & (l>=1) & (l<= maxLonInd)){
        values_lines[[values_lines_count]][values_count] = data_tmp$values[l,k]
        values_count = values_count + 1
        k = k + p1
        l = l + p2
      }
    } 
  }
  attributes(values_lines) = list(center_lat_ind = center_lat_ind, 
                                  center_lon_ind = center_lon_ind,
                                  center_lat = cyclone_centers$lat[i],
                                  center_lon = cyclone_centers$lon[i])
  names(values_lines) = c("SW", "W", "NW", "S", "N", "SE", "E", "NE")
  values_data[[values_data_count]] = values_lines
  values_data_count = values_data_count + 1
}