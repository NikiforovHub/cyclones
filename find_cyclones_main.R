source("data.r")
source("find_cyclones.r")
R = 6400 # radius of Earth in km

D = 1000 # distance from center of cyclone in km
G = 1.5  # maximum value of average pressure gradient in hPa
N = 6    # amount of directions on which G is achieved
nIntervLon = 4
nIntervLat = 3


timestamps = 10 #timestamps = length(data$time)
for (i in 1:timestamps){
  data_tmp = list(lat = data$lat,lon = data$lon, value = data$values[,,i])
  frame = get_map_frame(data_tmp)
  map = ggmap_map_frame(frame)
  matrix = data$values[,,i]
  min_ind_list = find_loc_mins(matrix, nIntervLon, nIntervLat)
  min_points = loc_mins(min_ind_list)
  map_mins = map + geom_point(data = min_points, 
                              aes(x = X2, y = X1), color = "black", size = 2)
  cyclone_centers = find_cyclones(min_ind_list,D,G,N)
  map_cyclones
  
}