library(ggmap)
library(lubridate)
library(data.table)
library(geosphere)

source("data.r")
source("plot.r")
source("find_cyclones_data.r")
source("model_data_data.r")
source("cyclones_base_data.R")

#setwd("//192.168.13.1/share/Nikiforov Sergey/R/cyclones")


R = 6400 # radius of Earth in km
##------------------------------------------##

G = 1.5  # maximum value of average pressure gradient in hPa/100 km
Lmin = 50 # minimum distance between neighbour points in km
N = 6   # amount of directions on which G is achieved
D = 1000 # distance of cyclone in km
DBC = 500 # distance between centers, which assumed as one cyclone





starttime <- proc.time()
#find_cyclones_main = function(){
nIntervLon = 8
nIntervLat = 6
data_folder = "//192.168.13.1/share/Dudko/data/ERA-40/data"
# data_folder = "\\\\192.168.13.1\\share\\Nikiforov Sergey\\ERA Interim"
#  data_folder = "C:/R/cyclones/data/data"
# cache_folder = "cache/ERA Interim/"
cache_folder = "cache/ERA-40/"
images_folder = "images/"

#files_real_data = list.files("cache/ERA Interim ver 1.0/", pattern = "Real data", recursive = F, full.names = F)
#files_prognoses = list.files("cache/ERA Interim ver 1.0/", pattern = "Prognoses", recursive = F, full.names = F)
# ##---------------------##
# ## temporary block
# files_real_data = substr(files_real_data,1,37)
# files_prognoses = substr(files_prognoses,1,37)
# files_real_data1 = files_real_data[1:8]
# files_real_data2 = files_real_data[9:16]
# files_prognoses1 = files_prognoses[1:8]
# files_prognoses2 = files_prognoses[9:16]
# files1 = c(files_real_data1,files_prognoses1)
# files2 = c(files_real_data2,files_prognoses2)
# 
# ##---------------------##

files = list.files(data_folder)

#unlink("track_log.csv")
# europe_map = get_map(location = "europe", maptype = "terrain", zoom = 3)
#netcdf_1958.nc

for(filename in files[2:length(files)]){
   tryCatch({
    centers_list = list()
    cyclones_base = data.table()
    cyclones_centers_lines_previous = data.table()
    data_filename = paste(data_folder,filename, sep='/')
    data = read_nc_file(data_filename)
    gc()
    timestamps = length(data$time)
    cyclones_base_lines_previous = data.table()
    maxID = 0
    cache_path = paste0(cache_folder, filename, ".cache")
    if(!file.exists(cache_path)){
      for (i in 1:timestamps){
        maxID = max(cyclones_base$ID)
        year = lubridate::year(data$time[i])
        month = lubridate::month(data$time[i])
        day = lubridate::day(data$time[i])
        hour = lubridate::hour(data$time[i])
        image_path = paste0(images_folder, year,"_",month,"_",day,"_",hour, ".png")
        date = as.POSIXlt(data$time[i])
        # if(!file.exists(image_path)){
        data_tmp = list(lat = data$lat, lon = data$lon, values = data$values[,,i])
        data_tmp$values = data_tmp$values/100
        frame = get_map_frame(data_tmp)
        # map = ggmap_map_frame(frame,europe_map)
        matrix = data$values[,,i]
        min_list = find_loc_mins(matrix, data_tmp, nIntervLon, nIntervLat)
        min_points = min_list_to_frame(min_list)
        #       map_mins = map + geom_point(data = min_points, 
        #                                   aes(x = lon, y = lat), color = "black", size = 2)
        centers_prob = find_possible_centers(min_list)
        #       map_probs = map_mins + geom_point(data = centers_prob, 
        #                                         aes(x = lon, y = lat), color = "green", size = 2)
        sink("sink-examp0.txt") # suppress output of find_cyclones
        print(paste("hour_count",i))
        cyclone_centers = find_cyclones(data_tmp,centers_prob,D,G,N,Lmin)
        sink() # end suppress output of find_cyclones
        closest_isobars = find_closest_isobars(data_tmp, cyclone_centers)
        nisobars = length(closest_isobars)
        if (nisobars){
          for (k in 1:nisobars){
            if (is_Polygon(closest_isobars[[k]])){
              geom_center = get_geom_center(closest_isobars[[k]], data_tmp)
              closest_isobars[[k]]$area = get_contour_area(closest_isobars[[k]],data_tmp)
              closest_isobars[[k]]$geom_center_lon = geom_center[1]
              closest_isobars[[k]]$geom_center_lat = geom_center[2]
              radiuses = get_radiuses(closest_isobars[[k]], data_tmp)
              closest_isobars[[k]]$min_r = radiuses[1]
              closest_isobars[[k]]$max_r = radiuses[2]
            }else{
              closest_isobars[[k]]$area = NA
              closest_isobars[[k]]$geom_center_lon = NA
              closest_isobars[[k]]$geom_center_lat = NA
              closest_isobars[[k]]$min_r = NA
              closest_isobars[[k]]$max_r = NA
            }
          }
          cyclones_base_lines_current = get_cyclones_base_lines(cyclone_centers, cyclones_base_lines_previous, 
                                                                closest_isobars, DBC, maxID, date)
          cyclones_base = rbindlist(list(cyclones_base, cyclones_base_lines_current))
          cyclones_base_lines_previous = cyclones_base_lines_current
          maxID = max(cyclones_base$ID)
        }
        
        #       closest_isobars_frame = get_isobars_frame(closest_isobars, data_tmp)
        #       names(frame) = c("lat", "lon", "values")
        #       
        #       if (length(cyclone_centers)){
        #         if (length(closest_isobars_frame) == 0){
        #           map_final = map + geom_point(data = cyclone_centers, 
        #                                        aes(x = lon, y = lat), color = "blue", size = 2) +
        #             stat_contour(data = frame, aes(x = lon, y = lat, z = values), binwidth = 2)
        #         }else{
        #           map_final = map + geom_point(data = cyclone_centers, 
        #                                        aes(x = lon, y = lat), color = "blue", size = 2) +
        #             stat_contour(data = frame, aes(x = lon, y = lat, z = values), binwidth = 2) +
        #             geom_point(data = closest_isobars_frame, aes(x = lon, y = lat), color = "blue", size = 1)
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
        # }
        if (i %% 50 == 0){
          save(cyclones_base, file = paste0(cache_folder, filename, ".cache"))
          write(i, file = paste0(cache_folder, filename, " index.txt"))
          gc()
        }
      }
      setkey(cyclones_base, ID)
      save(cyclones_base, file = paste(cache_folder, filename, ".cache", sep = ""))
      write(paste0("cyclone base data for ", year, "year is ready"), 
            file = paste0(cache_folder, filename, " ready.txt"))
      stoptime <- proc.time()
      print("for", data_filename)
      print(stoptime - starttime)
      rm(data)
      gc()
    }
  }, error=function(e, f = filename){
    cat("ERROR :",conditionMessage(e), "\n")
    print(paste("in ",filename,sep=""))
    write(conditionMessage(e), file = paste0(cache_folder, filename, " error.txt"))
  })
}
#}
