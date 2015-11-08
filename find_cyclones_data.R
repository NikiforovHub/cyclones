find_cyclones = function(D,G,N){
  min_points = find_loc_mins(matrix)
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
  names(min_points) = c("lat","lon","value")
  return(min_points)
}