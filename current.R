library(ggmap)
library(lubridate)

source("data.r")
source("plot.r")
source("find_cyclones_data.r")


R = 6400 # radius of Earth in km
##------------------------------------------##

G = 1.5  # maximum value of average pressure gradient in hPa/100 km
Lmin = 50 # minimum distance between neighbour points
N = 6    # amount of directions on which G is achieved
D = 1000 # distance of cyclone in km


nIntervLon = 8
nIntervLat = 6
# data_folder = "\\\\192.168.13.1\\share\\Dudko\\data\\ERA-40\\data"
data_folder = "C:/R/cyclones/data/data"
images_folder = "images/graphs/"
# files = c("netcdf_1957.nc", "netcdf_1970.nc", "netcdf_1971.nc", "netcdf_2001.nc")
files = c("netcdf_1957.nc")
centers_list = list()
# unlink("track_log.csv")
# unlink("grad_track.data")

for(filename in files){
  data_filename = paste(data_folder,filename, sep='/')
  year = na.omit(as.numeric(unlist(strsplit(filename, "[^0-9]+"))))
  data = read_nc_file(data_filename)
  timestamps = length(data$time)
  for (i in 1:1){
    year = year(data$time[i])
    month = month(data$time[i])
    day = day(data$time[i])
    hour = hour(data$time[i])
    image_path = paste(images_folder, year,"_",month,"_",day,"_",hour, ".png", sep="")
    if(!file.exists(image_path)){2
      data_tmp = list(lat = data$lat, lon = data$lon, values = data$values[,,i])
      data_tmp$values = data_tmp$values/100
      frame = get_map_frame(data_tmp)
      matrix = data$values[,,i]
      min_list = find_loc_mins(matrix, data_tmp, nIntervLon, nIntervLat)
      min_points = min_list_to_frame(min_list)
      centers_prob = find_possible_centers(min_list)
      print(paste("hour_count",i))
      cyclone_centers = find_cyclones(data_tmp,centers_prob,D,G,N,Lmin)
      names(frame) = c("lat", "lon", "values")
      values_data <- get_values_data(data_tmp, cyclone_centers)
    }
  }
}