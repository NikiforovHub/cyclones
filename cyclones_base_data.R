# These functions needed for getting cyclone base and clusterization data
# ----------------------------------------------------------- #
# ----------------------------------------------------------- #


get_min_max_radiuses = function(contour, data_tmp){
  center_point = c(contour$geom_center_lon, contour$geom_center_lat)
  contour$x = round(contour$x)
  contour$y = round(contour$y)
  lat = data_tmp$lat[contour$y]
  lon = data_tmp$lon[contour$x]
  radiuses = NULL
  for (i in 1:length(contour$x)){
    isobar_point = c(lon[i], lat[i])
    radiuses[i] = distCosine(center_point,isobar_point)
  }
  min_max_r = matrix(c(min(radiuses), max(radiuses))/1000, ncol = 2)
  colnames(min_max_r) = c("min", "max")
  return(min_max_r)
}


get_radiuses = function(contour, data_tmp){
  center_point = c(contour$geom_center_lon, contour$geom_center_lat)
  contour$x = round(contour$x)
  contour$y = round(contour$y)
  lon = data_tmp$lon[contour$x]
  lat = data_tmp$lat[contour$y]
  radiuses = NULL
  for (i in 1:length(contour$x)){
    isobar_point = c(lon[i], lat[i])
    radiuses[i] = distCosine(center_point,isobar_point)
  }
  max_r = max(radiuses)/1000
  max_index = which.max(radiuses)
  max_point = c(lon[max_index], lat[max_index])
  brng = bearing(center_point, max_point) + 90
  grCircle = greatCircleBearing(center_point, brng, n = 1080)
  bool = logical(nrow(grCircle))
  for (i in 1:nrow(grCircle)){
    for (j in 1:length(lon)){
      if ((grCircle[i,1] > lon[j] - 0.25) & (grCircle[i,1] < lon[j] + 0.25)){
        bool[i] = TRUE
      }
    }
  }
  grCircle_sub = matrix(grCircle[bool], ncol = 2)
  bool = logical(length(lat))
  for (i in 1:nrow(grCircle_sub)){
    for (j in 1:length(lat)){
      if ((grCircle_sub[i,1] > lon[j] - 0.25) & (grCircle_sub[i,1] < lon[j] + 0.25) &
          (grCircle_sub[i,2] > lat[j] - 0.25) & (grCircle_sub[i,2] < lat[j] + 0.25)){
        bool[j] = TRUE
      }
    }
  }
  lon_prob = lon[bool]
  lat_prob = lat[bool]
  probs = matrix(c(lon_prob, lat_prob), ncol = 2)
  d = NULL
  if (length(probs)){
    for (i in 1:nrow(probs)){
      point_prob = c(probs[i,1],probs[i,2])
      d[i] = distCosine(center_point, point_prob)/1000
    }
  }
  min_r = min(d[i])
  radiuses = matrix(c(min_r[1], max_r), ncol = 2)
  colnames(radiuses) = c("Rx","Ry")
  return(radiuses)
}

get_contour_area = function(contour, data_tmp){
  l = length(contour)
  if (l){
    for (i in 1:l){
      contour$x = round(contour$x)
      contour$y = round(contour$y)
      lat = data_tmp$lat[contour$y]
      lon = data_tmp$lon[contour$x]
      x = matrix(c(lon,lat),ncol = 2)
      area = areaPolygon(x)/10e+6 # conversion sq meters to sq kilometers
    }
  }
  return(area)
}


get_geom_center = function(contour, data_tmp){
  contour$x = round(contour$x)
  contour$y = round(contour$y)
  lat = data_tmp$lat[contour$y]
  lon = data_tmp$lon[contour$x]
  x = matrix(c(lon,lat),ncol = 2)
  geom_center = centroid(x)
  return(geom_center)
}


get_cyclones_base_lines = function(cyclone_centers, cyclones_base_lines_previous, 
                                   closest_isobars, DBC, maxID, date){
  t = 6 # time between stamps in hr
  cyclones_base_lines = data.table()
  ncyclones_current = nrow(cyclone_centers)
  ncyclones_previous = nrow(cyclones_base_lines_previous)
  for(i in 1:ncyclones_current){
    ID = NA
    speed = NA
    center_lon = cyclone_centers$lon[i]
    center_lat = cyclone_centers$lat[i]
    center_pressure = cyclone_centers$pressure[i]
    if (length(closest_isobars)){
      for(k in 1:length(closest_isobars)){
        if ((cyclone_centers$lat[i] == closest_isobars[[k]]$center_lat) &
            (cyclone_centers$lon[i] == closest_isobars[[k]]$center_lon)){
          geom_center_lon = closest_isobars[[k]]$geom_center_lon
          geom_center_lat = closest_isobars[[k]]$geom_center_lat
          area = closest_isobars[[k]]$area
          min_r = closest_isobars[[k]]$min_r
          max_r = closest_isobars[[k]]$max_r
          break
        }else{
          geom_center_lon = NA
          geom_center_lat = NA
          area = NA
          min_r = NA
          max_r = NA
        }
      }
    }else{
      geom_center_lon = NA
      geom_center_lat = NA
      area = NA
      min_r = NA
      max_r = NA
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
      if (is.na(ID)){
        for(j in 1:ncyclones_previous){
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
    
    line = data.table(ID, date, center_pressure, geom_center_lon, geom_center_lat, 
                      center_lon, center_lat, min_r, max_r, speed, area)
    cyclones_base_lines = rbindlist(list(cyclones_base_lines, line))
  }
  return(cyclones_base_lines)
}


get_cleared_base = function(cyclones_base){
  cyclones_base = cyclones_base[!is.na(geom_center_lon)]
  chain_len = 1
  bool = logical(nrow(cyclones_base))
  for (i in 1:(nrow(cyclones_base)-1)){
    if (cyclones_base$ID[i] == cyclones_base$ID[i+1]){
      chain_len = chain_len + 1
    }else{
      if (chain_len > 2){
        bool[(i - chain_len + 1):i] =  TRUE
      }
      chain_len = 1
    }
  }
  if (chain_len > 2){
    bool[(nrow(cyclones_base) - chain_len + 1):nrow(cyclones_base)] =  TRUE
  }
  chain_len = 1
  cyclones_base_cleared = cyclones_base[bool]
  for(i in 1:nrow(cyclones_base_cleared)){
    if (is.infinite(cyclones_base_cleared$min_r[i])){
      cyclones_base_cleared$min_r[i] = NA
    }
  }
  return(cyclones_base_cleared)
}


get_database_all = function(cache_folder){
  files_real_data = list.files(cache_folder, pattern = "Real data", recursive = F, full.names = F)
  cyclones_base_all = data.table()
  maxID = 0
  for (file in files_real_data){
    load(paste0(cache_folder,file))
    cyclones_base$ID = cyclones_base$ID + maxID
    maxID = max(cyclones_base$ID)
    cyclones_base = get_cleared_base(cyclones_base)
    cyclones_base_all = rbindlist(list(cyclones_base_all, cyclones_base))
  }
  return(cyclones_base_all)
}


get_clusterization_data = function(cyclones_base){
  clusterization_data = data.table()
  chain_len = 1
  for (i in 1:(nrow(cyclones_base)-1)){
    if (cyclones_base$ID[i] == cyclones_base$ID[i+1]){
      chain_len = chain_len + 1
    }else{
      if (chain_len > 2){
        cyclone_start_point = c(cyclones_base$geom_center_lon[i - chain_len + 1], 
                                cyclones_base$geom_center_lat[i - chain_len + 1])
        cyclone_end_point = c(cyclones_base$geom_center_lon[i], cyclones_base$geom_center_lat[i])
        
        ID = cyclones_base$ID[i]
        mean_pressure = mean(cyclones_base$center_pressure[(i - chain_len + 1):i])
        geom_center_lon = cyclones_base$geom_center_lon[i - chain_len + 1]
        geom_center_lat = cyclones_base$geom_center_lat[i - chain_len + 1]
        brng = bearing(cyclone_start_point, cyclone_end_point)
        mean_area = mean(cyclones_base$area[(i - chain_len + 1):i])
        mean_speed = mean(cyclones_base$speed[(i - chain_len + 1):i], na.rm = TRUE)
        mean_min_r = mean(cyclones_base$min_r[(i - chain_len + 1):i], na.rm = TRUE)
        mean_max_r = mean(cyclones_base$max_r[(i - chain_len + 1):i])
        
        data_line = data.table(ID, mean_pressure, geom_center_lon, geom_center_lat, brng, 
                               mean_area, mean_speed, mean_min_r, mean_max_r)
        clusterization_data = rbindlist(list(clusterization_data, data_line))
      }
      chain_len = 1
    }
  }
  if (chain_len > 2){
    ID = cyclones_base$ID[nrow(cyclones_base)]
    mean_pressure = mean(cyclones_base$center_pressure[(nrow(cyclones_base) - chain_len + 1):nrow(cyclones_base)])
    geom_center_lon = cyclones_base$geom_center_lon[nrow(cyclones_base) - chain_len + 1]
    geom_center_lat = cyclones_base$geom_center_lat[nrow(cyclones_base) - chain_len + 1]
    brng = bearing(cyclone_start_point, cyclone_end_point)
    mean_area = mean(cyclones_base$area[(nrow(cyclones_base) - chain_len + 1):nrow(cyclones_base)])
    mean_speed = mean(cyclones_base$speed[(nrow(cyclones_base) - chain_len + 1):nrow(cyclones_base)])
    mean_min_r = mean(cyclones_base$min_r[(nrow(cyclones_base) - chain_len + 1):nrow(cyclones_base)], na.rm = TRUE)
    mean_max_r = mean(cyclones_base$max_r[(nrow(cyclones_base) - chain_len + 1):nrow(cyclones_base)])
    
    data_line = data.table(ID, mean_pressure, geom_center_lon, geom_center_lat, brng, 
                           mean_area, mean_speed, mean_min_r, mean_max_r)
    clusterization_data = rbindlist(list(clusterization_data, data_line))
  }
  chain_len = 1
  clusterization_data = clusterization_data[!is.na(clusterization_data$mean_min_r)]
  return(clusterization_data)
}


convert_to_dates = function(cyclones_base, IDs){
  dates = vector()
  for (ID_check in IDs){
    cyclones_base_sub = cyclones_base[ID == ID_check]
    if (nrow(cyclones_base_sub)){
      for (i in 1:length(cyclones_base_sub)){
        year = cyclones_base_sub$year
        month = cyclones_base_sub$month
        day = cyclones_base_sub$day
        hour = cyclones_base_sub$hour
        date = paste(year, month, day, sep = "-")
        time = paste0(hour,":00:00")
        date = paste(date,time,sep = " ")
        dates = c(dates, date)
      }
    }
  }
  return(dates)
}


convert_to_timestamps = function(dates, data){
  timestamps = vector()
  for (i in 1:length(data$time)){
    for (j in 1:length(dates)){
      if (data$time[i] == dates[j]){
        timestamps = c(timestamps, i)
      }
    }
  }
  return(timestamps)
}


normalize_clust_data = function(clust_data){
  for (i in 1:length(clust_data)){
    clust_data[[i]] = clust_data[[i]]/(max(clust_data[[i]]) - min(clust_data[[i]]))
  }
  return(clust_data)
}
