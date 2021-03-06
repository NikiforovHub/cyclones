library(ggmap)
#library(data.table)
library(lubridate)
library(geosphere)
# library(foreach)
# library(doParallel)

source("data.r")
source("plot.r")
source("find_cyclones_data.r")
source("model_data_data.r")

# setwd("//192.168.13.1/share/Nikiforov Sergey/R/cyclones")

R = 6400 # radius of Earth in km
##------------------------------------------##

G = 1.5  # maximum value of average pressure gradient in hPa/100 km
Lmin = 50 # minimum distance between neighbour points in km
N = 6    # amount of directions on which G is achieved
D = 1000 # distance of cyclone in km



grad_limit = 0.2    # limit value of gradient pressure for finding isobars in hPa/100 km
nIntervLon = 8
nIntervLat = 6
data_folder = "//192.168.13.1/share/Dudko/data/ERA-40/data"
# data_folder = "C:/R/cyclones/data/data"
images_folder = "images/"
graphs_folder = "graphs/"
cache_folder = "cache/model/"
# files = c("netcdf_1957.nc", "netcdf_1970.nc", "netcdf_1971.nc", "netcdf_2001.nc")
# files = c("netcdf_1957.nc")
files = list.files(data_folder)
centers_list = list()
#unlink("track_log.csv")
#unlink("grad_track.data")
values_data = list()





for(filename in files[length(files):30]){
  model_frame_total = data.frame()
  data_filename = paste(data_folder,filename, sep='/')
  year = na.omit(as.numeric(unlist(strsplit(filename, "[^0-9]+"))))
  cache_path = paste0(cache_folder, year, "model.cache")
  data = read_nc_file(data_filename)
  timestamps = length(data$time)
  start = 1
  for (i in seq(from = 100,to = 1400, by = 100)){
    file = paste0(cache_path, " timestamp ", i)
    if(!file.exists(cache_path)){
      if(file.exists(paste0(cache_path, " timestamp ", i))){
        load(model_frame_total, file = paste0(cache_path, " timestamp ", i))
        start = i + 1
      }
    }
  }
  if(!file.exists(cache_path)){
    for (i in start:timestamps){
      year = year(data$time[i])
      month = month(data$time[i])
      day = day(data$time[i])
      hour = hour(data$time[i])
      image_path = paste(images_folder, year,"_",month,"_",day,"_",hour, ".png", sep="")
      #    if(!file.exists(image_path)){
      data_tmp = list(lat = data$lat, lon = data$lon, values = data$values[,,i])
      data_tmp$values = data_tmp$values/100
      #       frame = get_map_frame(data_tmp)
      #       map = ggmap_map_frame(frame, europe_map)
      matrix = data$values[,,i]
      min_list = find_loc_mins(matrix, data_tmp, nIntervLon, nIntervLat)
      min_points = min_list_to_frame(min_list)
      #       map_mins = map + geom_point(data = min_points, 
      #                                   aes(x = lon, y = lat), color = "black", size = 2)
      centers_prob = find_possible_centers(min_list)
      #       map_probs = map_mins + geom_point(data = centers_prob, 
      #                                         aes(x = lon, y = lat), color = "green", size = 2)
      print(paste("hour_count",i))
      cyclone_centers = find_cyclones(data_tmp,centers_prob,D,G,N,Lmin)
      date = paste(year = year, month = month, day = day, hour = hour, sep = "-")
      date = as.POSIXlt(date, tz = "GMT","%Y-%m-%d-%H")
      if (nrow(cyclone_centers)){
        model_frame = get_model_frame(matrix,cyclone_centers,date,grad_limit)
        model_frame_total = rbind.data.frame(model_frame_total,model_frame)
      }
      if (i %% 100 == 0){
        save(model_frame_total, file = paste0(cache_path, " timestamp ", i))
      }
      #      values_data[[i]] = get_values_data(data_tmp, cyclone_centers, date)
      #       for (cyclone_number in 1:length(values_data[[i]])){
      #           for (j in 1:8){
      #             direction = names(values_data[[i]][[cyclone_number]][j])
      #             graph_name = paste(year,month,day,hour,"cyclone",i,"direction",direction,sep="_")
      #             graph_path = paste(graphs_folder,graph_name,".png",sep="")
      #             if (!file.exists(graph_path))
      #             png(file=graph_path, width=2000,height=1400,res=150)
      #             plot(x = values_data[[i]][[cyclone_number]][[j]]$x, 
      #                  y = values_data[[i]][[cyclone_number]][[j]]$y,
      #                  type = "l")
      #             dev.off()
      #           }
      #       }  
      
      
      
      
      #       isobars = find_isobars(data_tmp, cyclone_centers, grad_limit)
      #       isobars_frame = get_isobars_frame(isobars, data_tmp)
      #       names(frame) = c("lat", "lon", "values")
      
      #       if (length(cyclone_centers)){
      #         if (length(isobars_frame) == 0){
      #           map_final = map + geom_point(data = cyclone_centers, 
      #                                        aes(x = lon, y = lat), color = "blue", size = 2) +
      #             stat_contour(data = frame, aes(x = lon, y = lat, z = values), binwidth = 2)
      #         }else{
      #           map_final = map + geom_point(data = cyclone_centers, 
      #                                        aes(x = lon, y = lat), color = "blue", size = 2) +
      #             stat_contour(data = frame, aes(x = lon, y = lat, z = values), binwidth = 2) +
      #             geom_point(data = isobars_frame, aes(x = lon, y = lat), color = "blue", size = 1)
      #         }
      #         png(file=image_path, width=2000,height=1400,res=150)
      #         plot(map_final)
      #         dev.off()
      #       }else{
      #         map_final = map + stat_contour(data = frame, aes(x = lon, y = lat, z = values), binwidth = 2)
      #         png(file=image_path, width=2000,height=1400,res=150)
      #         plot(map_final)
      #         dev.off()
      #       }
      #    } commented if(!file.exists(image_path)){
    }
    save(model_frame_total, file = cache_path)
  }
  rm(data)
  gc()
}