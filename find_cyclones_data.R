# These functions needed for finding centers of cyclones, closest isobars of these cyclones
# ----------------------------------------------------------- #
# ----------------------------------------------------------- #


library(geosphere)

find_cyclones = function(data_tmp, centers_prob,D,G,N,Lmin){
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
            # "���������� ����� ����� ������� �� �����"
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
          # creating points to calculating Dtest 
          {lat_center = data_tmp$lat[center_lat_ind]
          lon_center = data_tmp$lon[center_lon_ind]
          lat1 = data_tmp$lat[k]
          lon1 = data_tmp$lon[l]
          point_center = c(lon_center,lat_center)
          point1 = c(lon1, lat1)} 
          Dtest = distCosine(point_center,point1)/1000 # divided by 1000 for conversion from meters to kilometers
          if (Dtest > D) break
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
      pressure = data_tmp$values[center_lon_ind, center_lat_ind]
      cyclone_center = c(center_lat, center_lon, center_lat_ind,center_lon_ind, pressure)
      cyclone_centers = rbind(cyclone_centers,cyclone_center,deparse.level = 0)
    }
  }
  colnames(track) = c("lat","lon","SW", "W", "NW", "S", "N", "SE", "E", "NE")
  track_t = t(track)
  print(track_t,digits = 2)
#   fileConn = file("track_log.csv")
#   write.table(format(track_t, digits=2), file = "track_log.csv", 
#               append = TRUE, sep='\t',quote = FALSE)
#   a = paste("day")
#   write.table(a, file = "track_log.csv", 
#               append = TRUE, sep = " ", quote = FALSE)
#   close(fileConn)
  
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
    names(cyclone_centers) = c("lat","lon", "lat_ind", "lon_ind", "pressure")
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
      lat = data_with_latlon$lat[lat_ind]
      matrix_lat_ind[i,j] = lat_ind
      matrix_lat[i, j] = lat
      lon_ind = min_ind[1,1]
      lon = data_with_latlon$lon[lon_ind]
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
    step = 25
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
      current_closed = is_closed(current_contour[[1]])
      repeat{
        if (current_closed){
          if (check_lat_ind == maxLatInd) break
          check_lat_ind = check_lat_ind + step
          if(check_lat_ind > maxLatInd) check_lat_ind = maxLatInd
          current_level = data_tmp$values[center_lon_ind, check_lat_ind]
          current_contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, z = data_tmp$values, 
                                          level = current_level)
          current_contour = find_contour(current_contours, center_lon_ind, check_lat_ind)
          current_closed = is_closed(current_contour[[1]])
        }else break
      }
      if ((check_lat_ind == maxLatInd) & (current_closed)){
        current_contour[[1]]$center_lat_ind = center_lat_ind
        current_contour[[1]]$center_lon_ind = center_lon_ind
        current_contour[[1]]$center_lat = data_tmp$lat[center_lat_ind]
        current_contour[[1]]$center_lon = data_tmp$lon[center_lon_ind]
        closest_isobars = c(closest_isobars, current_contour)
        next
      }
      step = ceiling((check_lat_ind - center_lat_ind)/2)
      check_lat_ind = center_lat_ind + step
      next_level = data_tmp$values[center_lon_ind, check_lat_ind]
      next_contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, 
                                   z = data_tmp$values, level = next_level)
      next_contour = find_contour(next_contours, center_lon_ind, check_lat_ind)
      next_closed = is_closed(next_contour[[1]])
      repeat{
        step = ceiling(step/2)
        if (current_closed){
          check_lat_ind = check_lat_ind + step
          next_level = data_tmp$values[center_lon_ind, check_lat_ind]
          next_contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, 
                                       z = data_tmp$values, level = next_level)
          next_contour = find_contour(next_contours, center_lon_ind, check_lat_ind)
          next_closed = is_closed(next_contour[[1]])
        }else{
          check_lat_ind = check_lat_ind - step
          next_level = data_tmp$values[center_lon_ind, check_lat_ind]
          next_contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, 
                                       z = data_tmp$values, level = next_level)
          next_contour = find_contour(next_contours, center_lon_ind, check_lat_ind)
          next_closed = is_closed(next_contour[[1]])
        }
        if (current_closed & !next_closed & step == 1){
          current_contour[[1]]$center_lat_ind = center_lat_ind
          current_contour[[1]]$center_lon_ind = center_lon_ind
          current_contour[[1]]$center_lat = data_tmp$lat[center_lat_ind]
          current_contour[[1]]$center_lon = data_tmp$lon[center_lon_ind]
          closest_isobars = c(closest_isobars, current_contour)
          break
        }
        if (!current_closed & next_closed & step == 1){
          next_contour[[1]]$center_lat_ind = center_lat_ind
          next_contour[[1]]$center_lon_ind = center_lon_ind
          next_contour[[1]]$center_lat = data_tmp$lat[center_lat_ind]
          next_contour[[1]]$center_lon = data_tmp$lon[center_lon_ind]
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
  Lmin = 100
  isobars = list()
  if (length(cyclone_centers)){ 
    maxLatInd = length(data_tmp$lat)
    maxLonInd = length(data_tmp$lon)
    for (i in 1:nrow(cyclone_centers)){
      center_lat_ind = cyclone_centers$lat_ind[i]
      center_lon_ind = cyclone_centers$lon_ind[i]
      k = center_lat_ind
      l = center_lon_ind
      #level_found = logical()
      level_found = logical(8)
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
              # "���������� ����� ����� ������� �� �����"
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
              distance_to_center[count] = distCosine(point1,point2)/1000
            }
            k = k + p1*d
            l = l + p2*d
          }
        } 
      }
      ## ----------------------------- ##
      #temporary block
      for (j in 1:8){
        if (level_found[j] == FALSE){
          level[j] = 1
          level_lat_ind[j] = 1
          level_lon_ind[j] = 1
          distance_to_center[j] = 100000
        }
      }
      ## ----------------------------- ##
      if (length(level_found)){
        for (m in 1:length(level_found)){
          if (distance_to_center[m] <= 200){
            distance_to_center[m] = 100000
          }
          
        }
      }
    }
    min_distance_index = which.min(distance_to_center)
    contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, 
                            z = data_tmp$values, level = level[min_distance_index])
    contour = find_contour(contours, level_lon_ind[min_distance_index], level_lat_ind[min_distance_index])
    isobars = c(isobars, contour)
  }
  return(isobars)
}


# this function is under testing
# it should find borders of cyclones by level alpha
find_isobars_by_alpha = function(data_tmp, cyclone_centers, grad_limit){
  isobars = list()
  if (length(cyclone_centers)){ 
    maxLatInd = length(data_tmp$lat)
    maxLonInd = length(data_tmp$lon)
    min_pressure = min(data_tmp$values)
    max_pressure = max(data_tmp$values)
    level_pressure = min_pressure + (max_pressure - min_pressure)*0.3
    for (i in 1:nrow(cyclone_centers)){
      center_lat_ind = cyclone_centers$lat_ind[i]
      center_lon_ind = cyclone_centers$lon_ind[i]
      k = center_lat_ind
      l = center_lon_ind
      #level_found = logical()
      level_found = logical(8)
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
          while((k >= 1) & (k <= maxLatInd) & (l>=1) & (l<= maxLonInd)){
            if  ((data_tmp$values[l,k] < level_pressure + 0.1) & 
                 (data_tmp$values[l,k] > level_pressure - 0.1)){
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
              distance_to_center[count] = distCosine(point1,point2)/1000
            }
            k = k + 1
            l = l + 1
          }
        } 
      }
      ## ----------------------------- ##
      #temporary block
      for (j in 1:8){
        if (level_found[j] == FALSE){
          level[count] = 1
          level_lat_ind[count] = 1
          level_lon_ind[count] = 1
          distance_to_center[count] = 100000
        }
      }
      ## ----------------------------- ##
      if (length(level_found)){
        for (m in 1:length(level_found)){
          contours = contourLines(x = 1:maxLonInd, y = 1:maxLatInd, 
                                  z = data_tmp$values, level = level[m])
          contour = find_contour(contours, level_lon_ind[m], level_lat_ind[m])
          isobars = c(isobars, contour)
        }
      }
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
      #       contours[[i]]$x = sapply(contours[[i]]$x,round)
      #       contours[[i]]$y = sapply(contours[[i]]$y,round)
      bool = ((contours[[i]]$x < center_lon_ind + 1) & 
                (contours[[i]]$x > center_lon_ind - 1))
      lat_ind_vector = contours[[i]]$y[bool]
      m = length(lat_ind_vector)
      if (m){
        for (k in 1:m){
          if ((lat_ind_vector[k] < check_lat_ind + 1) &
              (lat_ind_vector[k] > check_lat_ind - 1)){
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
  lx = length(contour$x)
  ly = length(contour$y)
  bool =  ((contour$x[1] == contour$x[lx]) & (contour$y[1] == contour$y[ly]))
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
        plot_values_count = 1
        if((p1 == 0) & (p2 == 0)) next
        values_lines_count = values_lines_count + 1
        k=center_lat_ind
        l=center_lon_ind
        plot_values = list()
        plot_values_x = numeric()
        plot_values_y = numeric()
        while((k >= 1) & (k <= maxLatInd) & (l>=1) & (l<= maxLonInd)){
          lat1 = data_tmp$lat[center_lat_ind]
          lon1 = data_tmp$lon[center_lon_ind]
          lat2 = data_tmp$lat[k]
          lon2 = data_tmp$lon[l]
          point1 = c(lon1,lat1)
          point2 = c(lon2,lat2)
          L = distCosine(point1,point2)/1000 # divided by 1000 for conversion from meters to kilometers
          plot_values_x[plot_values_count] = L
          plot_values_y[plot_values_count] = data_tmp$values[l,k]
          plot_values_count = plot_values_count + 1
          k = k + p1
          l = l + p2
        }
        plot_values = list(plot_values_x, plot_values_y)
        names(plot_values) = c("x","y")
        values_lines[[values_lines_count]] = plot_values
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


# get_grad_data returns pressure grad values near cyclone centers in eight directions
get_grad_data = function(data_tmp,cyclone_centers,date, Lmin){
  nCenters = nrow(cyclone_centers)
  maxLatInd = length(data_tmp$lat)
  maxLonInd = length(data_tmp$lon)
  grad_data = list()
  grad_data_count = 1
  for (i in 1:nCenters){
    center_lat_ind = cyclone_centers$lat_ind[i]
    center_lon_ind = cyclone_centers$lon_ind[i]
    center_lat = data_tmp$lat[center_lat_ind]
    center_lon = data_tmp$lon[center_lon_ind]
    point_center = c(center_lon, center_lat)
    grad_lines = list()
    grad_lines_count = 0
    for(p1 in c(-1,0,1)){
      for(p2 in c(-1,0,1)){
        plot_count = 1
        if((p1 == 0) & (p2 == 0)) next
        grad_lines_count = grad_lines_count + 1
        k1=center_lat_ind
        l1=center_lon_ind
        k2 = NULL
        l2 = NULL
        grad = list()
        grad_x = NULL
        grad_y = NULL
        while((k1 >= 1) & (k1 <= maxLatInd) & (l1>=1) & (l1<= maxLonInd) &
              (k1 + p1 >= 1) & (k1 + p1 <= maxLatInd) & (l1 + p2 >=1) & (l1 + p2 <= maxLonInd)){
          k2 = NULL
          l2 = NULL
          d = 1
          repeat{
            k_tmp = k1 + p1*d
            l_tmp = l1 + p2*d
            lat1 = data_tmp$lat[k1]
            lon1 = data_tmp$lon[l1]
            lat_tmp = data_tmp$lat[k_tmp]
            lon_tmp = data_tmp$lon[l_tmp]
            point1 = c(lon1,lat1)
            point_tmp = c(lon_tmp,lat_tmp)
            L_tmp = distCosine(point1,point_tmp)/1000 # divided by 1000 for conversion from meters to kilometers
            if (L_tmp >= Lmin){
              k2 = k_tmp
              l2 = l_tmp
              point2 = point_tmp
              L = L_tmp
              break
            }
            d = d + 1
            if (((k1 + p1*d) < 1) | (k1 + p1*d > maxLatInd) |
                (l1 + p2*d < 1) | (l1 + p2*d > maxLonInd)) break
          }
          if (!is.null(k2)){
            Lcenter = distCosine(point2, point_center)/1000 # divided by 1000 for conversion from meters to kilometers
            grad_x[plot_count] = Lcenter
            grad_y[plot_count] = (data_tmp$values[l2,k2] - data_tmp$values[l1,k1])/L*100
            plot_count = plot_count + 1
            k1 = k2
            l1 = l2
          }else{
            k1 = k_tmp
            l1 = l_tmp
          }
        }
        if (is.null(grad_x)){
          grad_x = 1000
          grad_y = 1000
        } 
        grad = list(grad_x, grad_y)
        names(grad) = c("x","y")
        grad_lines[[grad_lines_count]] = grad
      }
    }
    attributes(grad_lines) = list(date = date,
                                  center_lat_ind = center_lat_ind, 
                                  center_lon_ind = center_lon_ind,
                                  center_lat = cyclone_centers$lat[i],
                                  center_lon = cyclone_centers$lon[i])
    names(grad_lines) = c("NW","N","NE","W","E","SW","S","SE")
    grad_data[[grad_data_count]] = grad_lines
    grad_data_count = grad_data_count + 1
  }
  return(grad_data)
}