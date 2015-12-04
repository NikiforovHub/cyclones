# These functions needed for finding centers of cyclones, closest isobars of these cyclones
# ----------------------------------------------------------- #
# ----------------------------------------------------------- #

find_cyclones = function(data_tmp,centers_prob,D,G,N,Lmin){
  nCenters = nrow(centers_prob)
  maxLatInd = length(data_tmp$lat)
  maxLonInd = length(data_tmp$lon)
  cyclone_centers = NULL
  track = NULL # creating track object for gradient maximums
  grad_track = list() # creating track list for gradient
  for (i in 1:nCenters){
    center_lat_ind = centers_prob$lat_ind[i]
    center_lon_ind = centers_prob$lon_ind[i]
    track_line = c(data_tmp$lat[center_lat_ind],data_tmp$lon[center_lon_ind])
    Ntest = 0
    grad_tmp = list()
    ## computating gradient in 8 directions 
    for(p1 in c(-1,0,1)){
      # order of directions: NW,N,NE, W,E, SW,S,SE
      for(p2 in c(-1,0,1)){
        if((p1 == 0) & (p2 == 0)) next
        k=center_lat_ind
        l=center_lon_ind
        grad = NULL
        grad_count = 1
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
          
          grad[grad_count] = (data_tmp$values[l + p2*d, k + p1*d] - data_tmp$values[l,k])/L*100
          grad_count = grad_count + 1
          k = k + p1*d
          l = l + p2*d
        }
        track_line = c(track_line, max(grad, na.rm=TRUE))
        if ((max(grad, na.rm=TRUE) > G) | (max(grad, na.rm=TRUE) == -Inf)) Ntest = Ntest + 1
#        if (is.null(grad))  grad_tmp = c(grad_tmp,"NULL")
        grad = list(grad)
        grad_tmp = c(grad_tmp,grad)
      } 
    }
    names(grad_tmp) = c("NW","N","NE","W","E","SW","S","SE")
    grad_tmp = list(grad_tmp)
    grad_track = c(grad_track, grad_tmp)
    track = rbind(track,track_line,deparse.level = 0)
    if (Ntest >= N) {
      center_lat = data_tmp$lat[center_lat_ind]
      center_lon = data_tmp$lon[center_lon_ind]
      cyclone_center = c(center_lat, center_lon, center_lat_ind,center_lon_ind,1)
      cyclone_centers = rbind(cyclone_centers,cyclone_center,deparse.level = 0)
    }
  }
  colnames(track) = c("lat","lon","SW", "W", "NW", "S", "N", "SE", "E", "NE")
  track_t = t(track)
  print(track_t,digits = 2)
  fileConn = file("track_log.csv")
  write.table(format(track_t, digits=2), file = "track_log.csv", 
              append = TRUE, sep='\t',quote = FALSE)
  a = paste("day")
  write.table(a, file = "track_log.csv", 
              append = TRUE, sep = " ", quote = FALSE)
  close(fileConn)
  
#   fileConn = file("grad_track.data")
#   write.table(format(grad_tmp, digits=2), file = "grad_track.data", 
#               append = TRUE, sep='\t',quote = FALSE)
#   a = paste("timestamp")
#   write.table(a, file = "grad_track.data", 
#               append = TRUE, sep = " ", quote = FALSE)
#   close(fileConn)
  save(grad_track, file = "grad_track.data")
  
  cyclone_centers = as.data.frame(cyclone_centers)
  if(length(cyclone_centers)){
    names(cyclone_centers) = c("lat","lon", "lat_ind", "lon_ind", "values")
  }
  return(cyclone_centers)
}


find_loc_mins = function(matrix, data_with_latlon, nIntervLon, nIntervLat){
  maxLonInd = nrow(matrix)
  maxLatInd = ncol(matrix)
  intervLon = ceiling(maxLonInd/nIntervLon)
  intervLat = ceiling(maxLatInd/nIntervLat)
  min_ind_list = list()
  matrix_mins = matrix(NA, nIntervLon, nIntervLat)
  matrix_lat =  matrix(NA, nIntervLon, nIntervLat)
  matrix_lon =  matrix(NA, nIntervLon, nIntervLat)
  matrix_lat_ind =  matrix(NA, nIntervLon, nIntervLat)
  matrix_lon_ind =  matrix(NA, nIntervLon, nIntervLat)
  for (i in 1:nIntervLon){ 
    for (j in 1:nIntervLat){
      nlon = intervLon*i
      nlat = intervLat*j
      if ((nlon <= maxLonInd) & (nlat <= maxLatInd)) {
        m = matrix[(intervLon*(i-1)+1):(intervLon*i),
                   (intervLat*(j-1)+1):(intervLat*j)]
      }else if ((nlon <= maxLonInd) & (nlat > maxLatInd)) {
        m = matrix[(intervLon*(i-1)+1):(intervLon*i),
                   (intervLat*(j-1)+1): maxLatInd      ]
      }else if ((nlon  > maxLonInd) & (nlat <= maxLatInd)){
        m = matrix[(intervLon*(i-1)+1): maxLonInd,
                   (intervLat*(j-1)+1):(intervLat*j)]
      }else if ((nlon  > maxLonInd) & (nlat > maxLatInd)){
        m = matrix[(intervLon*(i-1)+1):maxLonInd,
                   (intervLat*(j-1)+1):maxLatInd]
      }
        min_ind = NULL
        min_ind = which(m == min(m), arr.ind = TRUE)
        min_ind[,1] = min_ind[,1] + intervLon*(i-1)
        min_ind[,2] = min_ind[,2] + intervLat*(j-1)
        matrix_mins[i, j] = min(m)
        lat_ind = min_ind[1,2]
        lat = data$lat[lat_ind]
        matrix_lat_ind[i,j] = lat_ind
        matrix_lat[i, j] = lat
        lon_ind = min_ind[1,1]
        lon = data$lon[lon_ind]
        matrix_lon_ind[i,j] = lon_ind
        matrix_lon[i, j] = lon
        }
  }
  min_list = list(values = matrix_mins, lat = matrix_lat, lon = matrix_lon,
                  lat_ind = matrix_lat_ind, lon_ind = matrix_lon_ind)
  return(min_list)
}

loc_mins = function(min_ind_list){
  n = length(min_ind_list)
  min_points = NULL
  for (i in 1:n){
    m = nrow(min_ind_list[[i]])
    for (j in 1:m){
      lat_ind = min_ind_list[[i]][j,2]
      lat = data$lat[lat_ind]
      lon_ind = min_ind_list[[i]][j,1]
      lon = data$lon[lon_ind]
      tmp = c(lat,lon,1)
      min_points = rbind(min_points,tmp,deparse.level = 0)
    }
  }
  min_points = as.data.frame(min_points)
  names(min_points) = c("lat","lon","values")
  return(min_points)
}


min_list_to_frame = function(min_list){
  lat = min_list$lat
  dim(lat) = NULL
  lon = min_list$lon
  dim(lon) = NULL
  values = min_list$values
  dim(values) = NULL
  data.frame(lat = lat, lon = lon, values = values)
}


find_possible_centers = function(min_list){
  n = nrow(min_list$values)
  m = ncol(min_list$values)
  matrix_bool = matrix(FALSE,nrow = n,ncol = m)
  for (i in 1:n){ 
    for (j in 1:m){
      min_count = 0
      count_edge_cells = 0
      for (k in c(-1,0,1)){ 
        for (l in c(-1,0,1)){
          if ((i+k < 1) | (i+k > n) | (j+l < 1) | (j+l > m)){
            count_edge_cells = count_edge_cells + 1
          }else{
            if (min_list$values[i,j] <= min_list$values[i+k,j+l]) min_count = min_count + 1
          }
        }
      }
      if (min_count + count_edge_cells == 9) matrix_bool[i,j] = TRUE
    }
  }
  possible_centers = data.frame(lat = min_list$lat[matrix_bool], lon = min_list$lon[matrix_bool],
             lat_ind = min_list$lat_ind[matrix_bool], lon_ind = min_list$lon_ind[matrix_bool],
             values = min_list$values[matrix_bool])
  return(possible_centers)
}


find_closest_isobars = function(data_tmp, cyclone_centers){
  closest_isobars = list()
  if (length(cyclone_centers)){
    step = 40
    maxLatInd = length(data_tmp$lat)
    maxLonInd = length(data_tmp$lon)
    for (i in 1:nrow(cyclone_centers)){
      k = 0
      center_lat_ind = cyclone_centers$lat_ind[i]
      center_lon_ind = cyclone_centers$lon_ind[i]
      if (center_lat_ind <= 2 | center_lat_ind >= maxLatInd-1 | 
          center_lon_ind <= 2 | center_lon_ind >= maxLonInd-1 ) next
      check_lat_ind = center_lat_ind + step
      if (check_lat_ind > maxLatInd) check_lat_ind = maxLatInd
      current_level = data_tmp$values[center_lon_ind, check_lat_ind]
      current_contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, z = data_tmp$values, 
                                      level = current_level)
      current_contour = find_contour(current_contours, center_lon_ind, check_lat_ind)
      current_closed = is_closed(current_contour)
      repeat{
        if (current_closed){
          if (check_lat_ind == maxLatInd) break
          check_lat_ind = check_lat_ind + step
          if(check_lat_ind > maxLatInd) check_lat_ind = maxLatInd
          current_level = data_tmp$values[center_lon_ind, check_lat_ind]
          current_contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, z = data_tmp$values, 
                                          level = current_level)
          current_contour = find_contour(current_contours, center_lon_ind, check_lat_ind)
          current_closed = is_closed(current_contour)
        }else break
      }
      if ((check_lat_ind == maxLatInd) & (current_closed)){
        closest_isobars = c(closest_isobars, current_contour)
        next
      }
      step = ceiling((check_lat_ind - center_lat_ind)/2)
      check_lat_ind = center_lat_ind + step
      next_level = data_tmp$values[center_lon_ind, check_lat_ind]
      next_contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, 
                                   z = data_tmp$values, level = next_level)
      next_contour = find_contour(next_contours, center_lon_ind, check_lat_ind)
      next_closed = is_closed(next_contour)
      repeat{
        step = ceiling(step/2)
        if (current_closed){
          check_lat_ind = check_lat_ind + step
          next_level = data_tmp$values[center_lon_ind, check_lat_ind]
          next_contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, 
                                       z = data_tmp$values, level = next_level)
          next_contour = find_contour(next_contours, center_lon_ind, check_lat_ind)
          next_closed = is_closed(next_contour)
        }else{
          check_lat_ind = check_lat_ind - step
          next_level = data_tmp$values[center_lon_ind, check_lat_ind]
          next_contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, 
                                       z = data_tmp$values, level = next_level)
          next_contour = find_contour(next_contours, center_lon_ind, check_lat_ind)
          next_closed = is_closed(next_contour)
        }
        if (current_closed & !next_closed & step == 1){
          closest_isobars = c(closest_isobars, current_contour)
          break
        }
        if (!current_closed & next_closed & step == 1){
          closest_isobars = c(closest_isobars, next_contour)
          break
        }
        current_contour = next_contour
        current_closed = next_closed
        k = k + 1
        if (k > 100) break
      }
    }
  }
  return(closest_isobars)
}


find_isobars = function(data_tmp, cyclone_centers, grad_limit){
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
  return(isobars)
}


get_isobars_frame = function(closest_isobars, data_tmp){
  closest_isobars_frame = data.frame()
  l = length(closest_isobars)
  if (l){
    for (i in 1:l){
      closest_isobars[[i]]$x = round(closest_isobars[[i]]$x)
      closest_isobars[[i]]$y = round(closest_isobars[[i]]$y)
      lat = data_tmp$lat[closest_isobars[[i]]$y]
      lon = data_tmp$lon[closest_isobars[[i]]$x]
      tmp = data.frame(lat = lat, lon = lon)
      closest_isobars_frame = rbind(closest_isobars_frame, tmp)
    }
  }
  return(closest_isobars_frame)
}


# select a contour, that assigned to exact cyclone with center_lon_ind and check_lat_ind, from couple of contours
# couple of contours creates by R function contourLines
find_contour = function(contours, center_lon_ind, check_lat_ind){
  l = length(contours)
  contour = NULL
  if (l){
    contour = NULL
    for (i in 1:l){
      contours[[i]]$x = sapply(contours[[i]]$x,round)
      contours[[i]]$y = sapply(contours[[i]]$y,round)
      bool = contours[[i]]$x == center_lon_ind
      lat_ind_vector = contours[[i]]$y[bool]
      m = length(lat_ind_vector)
      if (m){
        for (k in 1:m){
          if (lat_ind_vector[k] == check_lat_ind){
            level = list(level = contours[[i]]$level)
            x = list(x = contours[[i]]$x)
            y = list(y = contours[[i]]$y)
            contour = list(c(level, x, y))
            break
          }
        }
      }
      if (!is.null(contour)) break
    }
  }
  if (is.null(contour)){
    if (l){
      level = list(level = contours[[1]]$level)
    }else{
      level = list(level = 1)
    }
    x = c(1:10)
    y = c(1:10)
    x = list(x = x)
    y = list(y = y)
    contour = list(c(level, x, y))
  }
  return(contour)
}


# check if contour closed or not
is_closed = function(contour, maxLonInd, maxLatInd){
  lx = length(contour[[1]]$x)
  ly = length(contour[[1]]$y)
  bool =  ((contour[[1]]$x[1] == contour[[1]]$x[lx]) & (contour[[1]]$y[1] == contour[[1]]$y[ly]))
  return(bool)
}


# get_values_data returns pressure values near cyclone centers in eight directions
get_values_data = function(data_tmp,cyclone_centers,date){
  nCenters = nrow(cyclone_centers)
  maxLatInd = length(data_tmp$lat)
  maxLonInd = length(data_tmp$lon)
  values_data = list()
  values_data_count = 1
  for (i in 1:nCenters){
    center_lat_ind = cyclone_centers$lat_ind[i]
    center_lon_ind = cyclone_centers$lon_ind[i]
    values_lines = list()
    values_lines_count = 0
    for(p1 in c(-1,0,1)){
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
    attributes(values_lines) = list(date = date,
                                    center_lat_ind = center_lat_ind, 
                                    center_lon_ind = center_lon_ind,
                                    center_lat = cyclone_centers$lat[i],
                                    center_lon = cyclone_centers$lon[i])
    names(values_lines) = c("NW","N","NE","W","E","SW","S","SE")
    values_data[[values_data_count]] = values_lines
    values_data_count = values_data_count + 1
  }
  return(values_data)
}
