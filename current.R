R = 6400 # radius of Earth in km
G = 1.5  # maximum value of average pressure gradient in hPa
N = 6    # amount of directions on which G is achieved
images_folder = "images/"
nIntervLon = 4
nIntervLat = 3



centers_list = list()
timestamps = length(data$time)

year = 1957
month = 9
unlink("track_log.csv")
for (i in 1:timestamps){
  hour_count = i
  year = 1957
  image_path = paste(images_folder, year,"_",month,"_",hour_count, ".png", sep="")  
  
  data_tmp = list(lat = data$lat, lon = data$lon, values = data$values[,,i])
  data_tmp$values = data_tmp$values/100
  frame = get_map_frame(data_tmp)
  map = ggmap_map_frame(frame)
  matrix = data$values[,,i]
  min_ind_list = find_loc_mins(matrix, nIntervLon, nIntervLat)
  min_points = loc_mins(min_ind_list)
  map_mins = map + geom_point(data = min_points, 
                              aes(x = lon, y = lat), color = "black", size = 2)
  print(paste("hour_count",i))
  cyclon_centers = find_cyclones(data_tmp,min_ind_list,D,G,N)
  cyclon_centers[,1] = data_tmp$lat[cyclon_centers[,1]]
  cyclon_centers[,2] = data_tmp$lon[cyclon_centers[,2]]
  cyclon_centers = as.data.frame(cyclon_centers)
  if (ncol(cyclon_centers) == 3){
    names(cyclon_centers) = c("lat","lon","values")
    map_cyclones = map_mins + geom_point(data = cyclon_centers, 
                                         aes(x = lon, y = lat), color = "red", size = 2)
    png(file=image_path, width=2000,height=1400,res=150)
    plot(map_cyclones)
    dev.off()
  }else{
    png(file=image_path, width=2000,height=1400,res=150)
    map_mins = map_mins + geom_point(data = tick_mark,
                           aes(x = lon, y = lat), color = "yellow", size = 5)
    plot(map_mins)
    dev.off()
  }
  centers_list[[i]] = cyclon_centers
}


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