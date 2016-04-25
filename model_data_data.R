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
                                   closest_isobars, DBC, maxID){
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
    year = date[1]
    month = date[2]
    day = date[3]
    hour = date[4]
    line = data.table(ID, year, month, day, hour, pressure, geom_center_lon, geom_center_lat, 
                      center_lon, center_lat, min_r, max_r, speed, area)
    cyclones_base_lines = rbindlist(list(cyclones_base_lines, line))
  }
  return(cyclones_base_lines)
}


get_model_frame = function(matrix, cyclone_centers, grad_limit){
  nCenters = nrow(cyclone_centers)
  maxLatInd = length(data_tmp$lat)
  maxLonInd = length(data_tmp$lon)
  model_frame_total = data.frame()
  for (i in 1:nCenters){
    center_lat_ind = cyclone_centers$lat_ind[i]
    center_lon_ind = cyclone_centers$lon_ind[i]
    ## collecting pressure values in 8 directions 
    for(p1 in c(-1,0,1)){
      # order of directions: NW,N,NE, W,E, SW,S,SE
      for(p2 in c(-1,0,1)){
        if((p1 == 0) & (p2 == 0)) next
        model_frame = data.frame()
        Lcount = 0
        d = 1
        k1 = center_lat_ind
        l1 = center_lon_ind
        k2 = NULL
        l2 = NULL
        point1 = NULL
        point2 = NULL
        while((k1 >= 1) & (k1 <= maxLatInd) & (l1>=1) & (l1<= maxLonInd) &
              (k1 + p1*d >= 1) & (k1 + p1*d <= maxLatInd) &
              (l1 + p2*d >= 1) & (l1 + p2*d <= maxLonInd)){
          Lcount = Lcount + 1
          d = 1
          repeat{
            k2 = k1 + p1*d
            l2 = l1 + p2*d
            lat_center = data_tmp$lat[center_lat_ind]
            lon_center = data_tmp$lon[center_lon_ind]
            lat1 = data_tmp$lat[k1]
            lon1 = data_tmp$lon[l1]
            lat2 = data_tmp$lat[k2]
            lon2 = data_tmp$lon[l2]
            point_center = c(lon_center,lat_center)
            point1 = c(lon1,lat1)
            point2 = c(lon2,lat2)
            Lcenter = distCosine(point_center,point2)/1000 # divided by 1000 for conversion from meters to kilometers
            if (Lcenter >= Lcount*Lmin) break
            d = d + 1
            if (((k1 + p1*d) < 1) | (k1 + p1*d > maxLatInd) |
                (l1 + p2*d < 1) | (l1 + p2*d > maxLonInd)) break
          }
          if (is.null(point1) | is.null(point2)) break
          L = distCosine(point1,point2)/1000 # divided by 1000 for conversion from meters to kilometers
          grad = (data_tmp$values[l2, k2] - data_tmp$values[l1,k1])/L*100
          center_p = data_tmp$values[center_lon_ind,center_lat_ind]
          Delta_p = data_tmp$values[l2,k2] - 
            data_tmp$values[center_lon_ind,center_lat_ind]
          p = data_tmp$values[l2,k2]
          frame_line = data.frame(center_p,Lcenter,p,Delta_p,NA)
          model_frame = rbind.data.frame(model_frame,frame_line)
          if (grad < grad_limit){
            dmax_row = rep(model_frame[nrow(model_frame),2],nrow(model_frame))
            model_frame[,5] = dmax_row
            break
          }
          k1 = k2
          l1 = l2
        }
        if (nrow(model_frame)){
          if (is.na(model_frame[nrow(model_frame),5])){
            dmax_row = rep(model_frame[nrow(model_frame),2],nrow(model_frame))
            model_frame[,5] = dmax_row
          }
        }
        model_frame_total = rbind.data.frame(model_frame_total,model_frame)
      }
    }
  }
  names(model_frame_total) = c("Pcenter","d","P","Delta_P","dmax")
  return(model_frame_total)
}