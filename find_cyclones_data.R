find_cyclones = function(data_tmp,min_ind_list,D,G,N){
  n = length(min_ind_list)
  maxLatInd = length(data_tmp$lat)
  maxLonInd = length(data_tmp$lon)
  cyclon_centers = NULL
  track = NULL
  for (i in 1:n){
    m = nrow(min_ind_list[[i]])
    for (j in 1:m){ 
      lat_ind = min_ind_list[[i]][j,2]
      lon_ind = min_ind_list[[i]][j,1]
      track_line = c(data_tmp$lat[lat_ind],data_tmp$lon[lon_ind])
      Ntest = 0
      grad = NULL
      
      ## gradient to north
      grad = NULL
      for (k in lat_ind:2){ 
        theta1 = data_tmp$lat[k-1]
        theta2 = data_tmp$lat[k]
        phi1 = data_tmp$lon[lon_ind]
        phi2 = data_tmp$lon[lon_ind]
        theta1 = theta1/180*pi
        theta2 = theta2/180*pi
        phi1 = phi1/180*pi
        phi2 = phi2/180*pi
        L = R*acos(sin(theta1)*sin(theta2) + 
                     cos(theta1)*cos(theta2)*cos(phi1-phi2))
        grad[k-1] = (data_tmp$value[lon_ind,k-1]-data_tmp$value[lon_ind,k])/L*100
      }
      track_line = c(track_line, max(grad, na.rm=TRUE))
      if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
      
      ## gradient to south
      grad = NULL
      for (k in lat_ind:length(data_tmp$lat)){ 
        theta1 = data_tmp$lat[k-1]
        theta2 = data_tmp$lat[k]
        phi1 = data_tmp$lon[lon_ind]
        phi2 = data_tmp$lon[lon_ind]
        theta1 = theta1/180*pi
        theta2 = theta2/180*pi
        phi1 = phi1/180*pi
        phi2 = phi2/180*pi
        L = R*acos(sin(theta1)*sin(theta2) + 
                     cos(theta1)*cos(theta2)*cos(phi1-phi2))
        grad[k] = (data_tmp$value[lon_ind,k]-data_tmp$value[lon_ind,k-1])/L*100
      }
      track_line = c(track_line, max(grad, na.rm=TRUE))
      if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
      
      ## gradient to west
      grad = NULL
      for (k in lon_ind:2){ 
        theta1 = data_tmp$lat[lat_ind]
        theta2 = data_tmp$lat[lat_ind]
        phi1 = data_tmp$lon[k-1]
        phi2 = data_tmp$lon[k]
        theta1 = theta1/180*pi
        theta2 = theta2/180*pi
        phi1 = phi1/180*pi
        phi2 = phi2/180*pi
        L = R*acos(sin(theta1)*sin(theta2) + 
                     cos(theta1)*cos(theta2)*cos(phi1-phi2))
        grad[k-1] = (data_tmp$value[k-1,lat_ind]-data_tmp$value[k,lat_ind])/L*100
      }
      track_line = c(track_line, max(grad, na.rm=TRUE))
      if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
      
      ## gradient to east
      grad = NULL
      for (k in lat_ind:2){ 
        theta1 = data_tmp$lat[lat_ind]
        theta2 = data_tmp$lat[lat_ind]
        phi1 = data_tmp$lon[k-1]
        phi2 = data_tmp$lon[k]
        theta1 = theta1/180*pi
        theta2 = theta2/180*pi
        phi1 = phi1/180*pi
        phi2 = phi2/180*pi
        L = R*acos(sin(theta1)*sin(theta2) + 
                     cos(theta1)*cos(theta2)*cos(phi1-phi2))
        grad[k-1] = (data_tmp$value[k,lat_ind]-data_tmp$value[k-1,lat_ind])/L*100
      }
      track_line = c(track_line, max(grad, na.rm=TRUE))
      if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
      
      ## gradient to north-west
      grad = NULL
      k=lat_ind
      l=lon_ind
      while((k>=2)&(l>=2)){
        theta1 = data_tmp$lat[k-1]
        theta2 = data_tmp$lat[k]
        phi1 = data_tmp$lon[l-1]
        phi2 = data_tmp$lon[l]
        theta1 = theta1/180*pi
        theta2 = theta2/180*pi
        phi1 = phi1/180*pi
        phi2 = phi2/180*pi
        L = R*acos(sin(theta1)*sin(theta2) + 
                     cos(theta1)*cos(theta2)*cos(phi1-phi2))
        grad[k-1] = (data_tmp$value[l-1,k-1]-data_tmp$value[l,k])/L*100
        k = k-1
        l = l-1
      }
      track_line = c(track_line, max(grad, na.rm=TRUE))
      if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
      
      ## gradient to north-east
      grad = NULL
      k=lat_ind
      l=lon_ind
      while((k>=2)&(l<=maxLonInd)){
        theta1 = data_tmp$lat[k-1]
        theta2 = data_tmp$lat[k]
        phi1 = data_tmp$lon[l-1]
        phi2 = data_tmp$lon[l]
        theta1 = theta1/180*pi
        theta2 = theta2/180*pi
        phi1 = phi1/180*pi
        phi2 = phi2/180*pi
        L = R*acos(sin(theta1)*sin(theta2) + 
                     cos(theta1)*cos(theta2)*cos(phi1-phi2))
        grad[k-1] = (data_tmp$value[l,k-1]-data_tmp$value[l-1,k])/L*100
        k = k-1
        l = l+1
      }
      track_line = c(track_line, max(grad, na.rm=TRUE))
      if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
      
      ## gradient to south-east
      grad = NULL
      k=lat_ind
      l=lon_ind
      while((k<=maxLatInd)&(l<=maxLonInd)){
        theta1 = data_tmp$lat[k-1]
        theta2 = data_tmp$lat[k]
        phi1 = data_tmp$lon[l-1]
        phi2 = data_tmp$lon[l]
        theta1 = theta1/180*pi
        theta2 = theta2/180*pi
        phi1 = phi1/180*pi
        phi2 = phi2/180*pi
        L = R*acos(sin(theta1)*sin(theta2) + 
                     cos(theta1)*cos(theta2)*cos(phi1-phi2))
        grad[k-1] = (data_tmp$value[l,k]-data_tmp$value[l-1,k-1])/L*100
        k = k+1
        l = l+1
      }
      track_line = c(track_line, max(grad, na.rm=TRUE))
      if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
      
      ## gradient to south-west
      grad = NULL
      k=lat_ind
      l=lon_ind
      while((k<=maxLatInd)&(l>=2)){
        theta1 = data_tmp$lat[k-1]
        theta2 = data_tmp$lat[k]
        phi1 = data_tmp$lon[l-1]
        phi2 = data_tmp$lon[l]
        theta1 = theta1/180*pi
        theta2 = theta2/180*pi
        phi1 = phi1/180*pi
        phi2 = phi2/180*pi
        L = R*acos(sin(theta1)*sin(theta2) + 
                     cos(theta1)*cos(theta2)*cos(phi1-phi2))
        grad[k-1] = (data_tmp$value[l-1,k]-data_tmp$value[l,k-1])/L*100
        k = k+1
        l = l-1
      }
      track_line = c(track_line, max(grad, na.rm=TRUE))
      if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
      track = rbind(track,track_line,deparse.level = 0)
      if (Ntest > N) {
        tmp = c(lat_ind,lon_ind,1)
        cyclon_centers = rbind(cyclon_centers,tmp,deparse.level = 0)
      }
    }
  }
  colnames(track) = c("lat","lon","N","S","W","E","NW","NE","SE","SW")
  track_t = t(track)
  print(track_t,digits = 2)
  fileConn = file("track_log.csv")
  write.table(format(track_t, digits=2), file = "track_log.csv", 
              append = TRUE, sep='\t',quote = FALSE)
  a = paste("day")
  write.table(a, file = "track_log.csv", 
              append = TRUE, sep = " ", quote = FALSE)
  close(fileConn)
  return(cyclon_centers)
}

find_loc_mins = function(matrix, nIntervLon, nIntervLat){
  maxlon = nrow(matrix)
  maxlat = ncol(matrix)
  intervLon = ceiling(maxlon/nIntervLon)
  intervLat = ceiling(maxlat/nIntervLat)
  min_points = matrix(NA,nIntervLon,nIntervLat)
  min_ind_list = list()
  for (i in 1:nIntervLon){ 
    for (j in 1:nIntervLat){
      nlon = intervLon*i
      nlat = intervLat*j
      if ((nlon <= maxlon) & (nlat <= maxlat)) {
        m = matrix[(intervLon*(i-1)+1):(intervLon*i),
                   (intervLat*(j-1)+1):(intervLat*j)]
      }else if ((nlon <= maxlon) & (nlat > maxlat)) {
        m = matrix[(intervLon*(i-1)+1):(intervLon*i),
                   (intervLat*(j-1)+1): maxlat      ]
      }else if ((nlon  > maxlon) & (nlat <= maxlat)){
        m = matrix[(intervLon*(i-1)+1): maxlon,
                   (intervLat*(j-1)+1):(intervLat*j)]
      }else if ((nlon  > maxlon) & (nlat > maxlat)){
        m = matrix[(intervLon*(i-1)+1):maxlon,
                   (intervLat*(j-1)+1):maxlat]
      }
        # min_points[i,j] = min(m)
        min_ind = NULL
        min_ind = which(m == min(m), arr.ind = TRUE)
        min_ind[,1] = min_ind[,1] + intervLon*(i-1)
        min_ind[,2] = min_ind[,2] + intervLat*(j-1)
        min_ind_list[[(j-1)*nIntervLon+i]] = min_ind
      
    } 
  }
  return(min_ind_list)
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