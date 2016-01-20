get_cyclones_base_lines = function(cyclone_centers, cyclones_base_lines_previous, 
                                   closest_isobars, DBC, maxID){
  t = 6 # time between stamps
  cyclones_base_lines = data.table()
  ncyclones_current = nrow(cyclone_centers)
  ncyclones_previous = nrow(cyclones_base_lines_previous)
  for(i in 1:ncyclones_current){
    ID = NA
    speed = NA
    center_lon = cyclone_centers$lon[i]
    center_lat = cyclone_centers$lat[i]
    for(k in 1:length(closest_isobars)){
      if ((cyclone_centers$lat[i] == closest_isobars[[k]]$center_lat) &
          (cyclone_centers$lon[i] == closest_isobars[[k]]$center_lon) &
          length(closest_isobars)){
        geom_center_lon = closest_isobars[[k]]$geom_center_lon
        geom_center_lat = closest_isobars[[k]]$geom_center_lat
        area = closest_isobars[[k]]$area
        min_r = closest_isobars[[k]]$min_r
        max_r = closest_isobars[[k]]$max_r
      }else{
        geom_center_lon = NA
        geom_center_lat = NA
        area = NA
        min_r = NA
        max_r = NA
      }
    }
    if (ncyclones_previous){
      for(j in 1:ncyclones_previous){
        if (!is.na(geom_center_lon) & !is.na(cyclones_base_lines_previous$geom_center_lon[j])){
          point1 = c(geom_center_lon, geom_center_lat)
          point2 = c(cyclones_base_lines_previous$geom_center_lon[j], 
                     cyclones_base_lines_previous$geom_center_lat[j])
          distance_between_geom_centers = distCosine(point1,point2)/1000
          if(distance_between_geom_centers <= DBC){
            ID = cyclones_base_lines_previous$ID[j]
            speed = distance_between_geom_centers/t # speed in km/hr
            break
          }
        }
      }
      for(j in 1:ncyclones_previous){
        if (is.na(ID)){
          point1 = c(center_lon, center_lat)
          point2 = c(cyclones_base_lines_previous$center_lon[j], 
                     cyclones_base_lines_previous$center_lat[j])
          distance_between_centers = distCosine(point1,point2)/1000
          if(distance_between_centers <= DBC){
            ID = cyclones_base_lines_previous$ID[j]
            speed = distance_between_centers/t # speed in km/hr
          }
        }
      }
    }
    if (is.na(ID)){
      ID = maxID + 1
      maxID = ID
    }
    year = date[1]
    month = date[2]
    day = date[3]
    hour = date[4]
    line = data.table(ID, year, month, day, hour, geom_center_lon, geom_center_lat, center_lon, center_lat, min_r, max_r, speed, area)
    cyclones_base_lines = rbindlist(list(cyclones_base_lines, line))
  }
  return(cyclones_base_lines)
}