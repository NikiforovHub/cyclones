library(ggmap)
library(lubridate)
library(geosphere)
# library(directlabels)

source("data.r")
source("plot_tmp.r")
source("find_cyclones_data.r")




  R = 6400 # radius of Earth in km
  ##------------------------------------------##
  
  G = 1.5  # maximum value of average pressure gradient in hPa/100 km
  Lmin = 50 # minimum distance between neighbour points in km
  N = 6    # amount of directions on which G is achieved
  D = 1000 # distance of cyclone in km
  



#find_cyclones_main = function(){
  nIntervLon = 8
  nIntervLat = 6
#  data_folder = "\\\\192.168.13.1\\share\\Dudko\\data\\ERA-40\\data"
#  data_folder = "I:\\ERA-40\\ERA-40\\data"
 data_folder = "C:/R/cyclones/data/data"

  images_folder = "images/"
#  files = c("netcdf_1957.nc", "netcdf_1970.nc", "netcdf_1971.nc", "netcdf_2001.nc")
  files = c("netcdf_1957.nc")
  centers_list = list()
  unlink("track_log.csv")
  europe_map = get_map(location = "europe", maptype = "terrain", zoom = 3)
  
  
  start = Sys.time()
  for(filename in files){
    data_filename = paste(data_folder,filename, sep='/')
    year = na.omit(as.numeric(unlist(strsplit(filename, "[^0-9]+"))))
    data = read_nc_file(data_filename)
    timestamps = length(data$time)
    for (i in 1:timestamps){
      year = year(data$time[i])
      month = month(data$time[i])
      day = day(data$time[i])
      hour = hour(data$time[i])
      image_path = paste(images_folder, year,"_",month,"_",day,"_",hour, ".png", sep="")
      if(!file.exists(image_path)){
        data_tmp = list(lat = data$lat, lon = data$lon, values = data$values[,,i])
        data_tmp$values = data_tmp$values/100
        frame = get_map_frame(data_tmp)
        
        matrix = data$values[,,i]
        min_list = find_loc_mins(matrix, data_tmp, nIntervLon, nIntervLat)
        min_points = min_list_to_frame(min_list)
       
        centers_prob = find_possible_centers(min_list)
        
        print(Sys.time())
        print(paste("hour_count",i))
        cyclone_centers = find_cyclones(data_tmp,centers_prob,D,G,N,Lmin)
        closest_isobars = find_closest_isobars(data_tmp, cyclone_centers)
        closest_isobars_frame = get_isobars_frame(closest_isobars, data_tmp)
        names(frame) = c("lat", "lon", "values")
        
        map = ggmap_map_frame(frame,europe_map)
        map_mins = map + geom_point(data = min_points, 
                                    aes(x = lon, y = lat), color = "black", size = 2)
        map_probs = map_mins + geom_point(data = centers_prob, 
                                          aes(x = lon, y = lat), color = "green", size = 2)
        
        if (length(cyclone_centers)){
          if (length(closest_isobars_frame) == 0){
            map_final = map + geom_point(data = cyclone_centers, 
                                         aes(x = lon, y = lat), color = "blue", size = 2) +
              geom_contour(data = frame, aes(x = lon, y = lat, z = values),
                           color = "black", binwidth = 2)
          }else{
            map_final = map + geom_point(data = cyclone_centers, 
                                         aes(x = lon, y = lat), color = "blue", size = 2) +
              geom_contour(data = frame, aes(x = lon, y = lat, z = values),
                           color = "black", binwidth = 2) +
              geom_point(data = closest_isobars_frame, aes(x = lon, y = lat), color = "blue", size = 1)

          }
          png(file=image_path, width=2000,height=1400,res=150)
          plot(map_final)
          dev.off()
        }else{
          map_final = map + geom_contour(data = frame, aes(x = lon, y = lat, z = values), binwidth = 2)
          direct.label(map_final,"top.points")
          png(file=image_path, width=2000,height=1400,res=150)
          plot(map_final)
          dev.off()
        }
      }
    }
  }
#}
  
end = Sys.time()
print("Exexution time", end - start)

