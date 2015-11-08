# timestamps = length(data$time)
# data_tmp = list(lat = data$lat,lon = data$lon, value = data$values[,,1])
# frame = get_map_frame(data_tmp)
# map = ggmap_map_frame(frame)
# plot(map)
# cycl_centers = map + geom_point(data = min_points, 
#                        aes(x = X2, y = X1), color = "black", size = 10 )

##----------------------------------------##

# nIntervLon = 4
# nIntervLat = 3
# matrix = data$values[,,1]
# min_ind_list = find_loc_mins(matrix, nIntervLon, nIntervLat)
# min_points = loc_mins(min_ind_list)
# map_mins = map + geom_point(data = min_points, 
#                             aes(x = lon, y = lat), color = "black", size = 2)
# plot(map_mins)

## ------------------------------------------------------##

R = 6400 # radius of Earth in km
G = 1.5  # maximum value of average pressure gradient in hPa
N = 6    # amount of directions on which G is achieved

n = length(min_ind_list)
data_tmp = list(lat = data$lat, lon = data$lon, values = data$values[,,1])
data_tmp$values = data_tmp$values/100
maxLatInd = length(data_tmp$lat)
maxLonInd = length(data_tmp$lon)
cyclon_centers = NULL
for (i in 1:n){
  m = nrow(min_ind_list[[i]])
  for (j in 1:m){ 
    lat_ind = min_ind_list[[i]][j,2]
    lon_ind = min_ind_list[[i]][j,1]
    print(c("lat",data_tmp$lat[lat_ind])) 
    print(c("lon",data_tmp$lon[lon_ind])) 
    Ntest = 0
    grad = NULL
    
    ## gradient to north
    grad = NULL
    for (k in lat_ind:2){ 
      theta1 = data_tmp$lat[k-1]
      theta2 = data_tmp$lat[k]
      phi1 = data_tmp$lon[lon_ind]
      phi2 = data_tmp$lon[lon_ind]
      L = R*acos(sin(theta1)*sin(theta2) + 
                   cos(theta1)*cos(theta2)*cos(phi1-phi2))
      grad[k-1] = (data_tmp$value[lon_ind,k]-data_tmp$value[lon_ind,k-1])/L*100
    }
    print(max(grad, na.rm=TRUE))
    if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
    
    ## gradient to south
    grad = NULL
    for (k in lat_ind:length(data_tmp$lat)){ 
      theta1 = data_tmp$lat[k-1]
      theta2 = data_tmp$lat[k]
      phi1 = data_tmp$lon[lon_ind]
      phi2 = data_tmp$lon[lon_ind]
      L = R*acos(sin(theta1)*sin(theta2) + 
                   cos(theta1)*cos(theta2)*cos(phi1-phi2))
      grad[k] = (data_tmp$value[lon_ind,k]-data_tmp$value[lon_ind,k-1])/L*100
    }
    print(max(grad, na.rm=TRUE))
    if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
    
    ## gradient to west
    grad = NULL
    for (k in lon_ind:2){ 
      theta1 = data_tmp$lat[lat_ind]
      theta2 = data_tmp$lat[lat_ind]
      phi1 = data_tmp$lon[k-1]
      phi2 = data_tmp$lon[k]
      L = R*acos(sin(theta1)*sin(theta2) + 
                   cos(theta1)*cos(theta2)*cos(phi1-phi2))
      grad[k-1] = (data_tmp$value[k,lat_ind]-data_tmp$value[k-1,lat_ind])/L*100
    }
    print(max(grad, na.rm=TRUE))
    if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
    
    ## gradient to east
    grad = NULL
    for (k in lat_ind:2){ 
      theta1 = data_tmp$lat[lat_ind]
      theta2 = data_tmp$lat[lat_ind]
      phi1 = data_tmp$lon[k-1]
      phi2 = data_tmp$lon[k]
      L = R*acos(sin(theta1)*sin(theta2) + 
                   cos(theta1)*cos(theta2)*cos(phi1-phi2))
      grad[k-1] = (data_tmp$value[k,lat_ind]-data_tmp$value[k-1,lat_ind])/L*100
    }
    print(max(grad, na.rm=TRUE))
    if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
    
    ## gradient to north-west
    grad = NULL
    for (k in lat_ind:2){
      for (l in lon_ind:2){
        theta1 = data_tmp$lat[k-1]
        theta2 = data_tmp$lat[k]
        phi1 = data_tmp$lon[l-1]
        phi2 = data_tmp$lon[l]
        L = R*acos(sin(theta1)*sin(theta2) + 
                     cos(theta1)*cos(theta2)*cos(phi1-phi2))
        if (k == l){
          grad[k] = (data_tmp$value[l,k]-data_tmp$value[l-1,k-1])/L*100
        }
      }
    }
    print(max(grad, na.rm=TRUE))
    if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
    
    ## gradient to north-east
    grad = NULL
    for (k in lat_ind:2){
      for (l in lon_ind:length(data_tmp$lon)){
        theta1 = data_tmp$lat[k-1]
        theta2 = data_tmp$lat[k]
        phi1 = data_tmp$lon[l-1]
        phi2 = data_tmp$lon[l]
        L = R*acos(sin(theta1)*sin(theta2) + 
                     cos(theta1)*cos(theta2)*cos(phi1-phi2))
        if (k == l){
          grad[k-1] = (data_tmp$value[l,k]-data_tmp$value[l-1,k-1])/L*100
        }
      }
    }
    print(max(grad, na.rm=TRUE))
    if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
    
    ## gradient to south-east
    grad = NULL
    for (k in lat_ind:length(data_tmp$lat)){
      for (l in lon_ind:length(data_tmp$lon)){
        theta1 = data_tmp$lat[k-1]
        theta2 = data_tmp$lat[k]
        phi1 = data_tmp$lon[l-1]
        phi2 = data_tmp$lon[l]
        L = R*acos(sin(theta1)*sin(theta2) + 
                     cos(theta1)*cos(theta2)*cos(phi1-phi2))
        if (k == l){
          grad[k] = (data_tmp$value[l,k]-data_tmp$value[l-1,k-1])/L*100
        }
      }
    }
    print(max(grad, na.rm=TRUE))
    if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
    
    ## gradient to south-west
    grad = NULL
    for (k in lat_ind:length(data_tmp$lat)){
      for (l in lon_ind:2){
        theta1 = data_tmp$lat[k-1]
        theta2 = data_tmp$lat[k]
        phi1 = data_tmp$lon[l-1]
        phi2 = data_tmp$lon[l]
        L = R*acos(sin(theta1)*sin(theta2) + 
                     cos(theta1)*cos(theta2)*cos(phi1-phi2))
        if (k == l){
          grad[k] = (data_tmp$value[l,k]-data_tmp$value[l-1,k-1])/L*100
        }
      }
    }
    print(max(grad, na.rm=TRUE))
    if (max(grad, na.rm=TRUE) > G) Ntest = Ntest + 1
    
    if (Ntest > N) {
      tmp = c(lat_ind,lon_ind,1)
      cyclon_centers = rbind(cyclon_centers,tmp,deparse.level = 0)
    } 
  }
}