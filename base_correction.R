base_correction = function(cyclones_base, DBC){
  init_date = cyclones_base$date[1]
  end_date = cyclones_base$date[nrow(cyclones_base)]
  end_date = as.POSIXct(paste(data.table::year(end_date), data.table::month(end_date), lubridate::day(end_date)), 
             tz = "GMT", "%Y %m %d")
  t = 6
  cyclones_base_correct = data.table()
  
  
  base_tmp1 = cyclones_base[date == init_date]
  for (i in 1:nrow(base_tmp1)){
    base_tmp1$ID[i] = i
  }
  cyclones_base_correct = rbind(cyclones_base_correct, base_tmp1)
  date2 = init_date + 3600*6
  base_tmp2 = cyclones_base[date == date2]
  
  
  while(date2 != (end_date + 3600*6)){
    ncyclones1 = nrow(base_tmp1)
    ncyclones2 = nrow(base_tmp2)
    if (ncyclones1){
      for(i in 1:ncyclones1){
        point1 = c(base_tmp1$geom_center_lon[i], base_tmp1$geom_center_lat[i])
        if (ncyclones2){
          for (j in 1:ncyclones2){
            if (!is.na(base_tmp1$geom_center_lon[i]) & !is.na(base_tmp2$geom_center_lon[j])){
              point2 = c(base_tmp2$geom_center_lon[j], base_tmp2$geom_center_lat[j])
              distance_between_geom_centers = distCosine(point1,point2)/1000
              if(distance_between_geom_centers <= DBC){
                ID = base_tmp1$ID[i]
                speed = distance_between_geom_centers/t # speed in km/hr
                base_tmp2$ID[j] = base_tmp1$ID[i]
                base_tmp2$speed[j] = speed
                break
              }
            }
          }
        }
      }
    }
    if (ncyclones2){
      for(i in 1:ncyclones2){
        if (base_tmp2$ID[i] == -Inf){
          base_tmp2$ID[i] = max(base_tmp2$ID, cyclones_base_correct$ID,na.rm = TRUE) + 1
        }
      }
    }
    cyclones_base_correct = rbind(cyclones_base_correct, base_tmp2) 
    date1 = date2
    base_tmp1 = base_tmp2
    date2 = date2 + 3600*6
    base_tmp2 = cyclones_base[date == date2]
  }
  return(cyclones_base_correct)
}