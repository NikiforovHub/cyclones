<<<<<<< HEAD
# install.packages("lubridate")

library(ggmap)
library(lubridate)
library(geosphere)

source("data.r")
source("plot.r")
source("find_cyclones_data.r")




R = 6400 # radius of Earth in km
##------------------------------------------##

G = 1.5  # maximum value of average pressure gradient in hPa/100 km
Lmin = 50 # minimum distance between neighbour points in km
N = 6    # amount of directions on which G is achieved
D = 1000 # distance of cyclone in km

{
nIntervLon = 8
nIntervLat = 6
data_folder = "\\\\192.168.13.1\\share\\Dudko\\data\\ERA-40\\data"
#  data_folder = "I:\\ERA-40\\ERA-40\\data"
images_folder = "images_for_presentation/"
#  files = c("netcdf_1957.nc", "netcdf_1970.nc", "netcdf_1971.nc", "netcdf_2001.nc")
files = c("netcdf_1957.nc")
centers_list = list()
unlink("track_log.csv")
europe_map = get_map(location = "europe", maptype = "terrain", zoom = 3)

for(filename in files){
  data_filename = paste(data_folder,filename, sep='/')
  year = na.omit(as.numeric(unlist(strsplit(filename, "[^0-9]+"))))
  data = read_nc_file(data_filename)
  timestamps = length(data$time)
  for (i in 1:20){
    year = year(data$time[i])
    month = month(data$time[i])
    day = day(data$time[i])
    hour = hour(data$time[i])
    image_path = paste(images_folder, year,"_",month,"_",day,"_",hour, ".png", sep="")
    if(TRUE){
      data_tmp = list(lat = data$lat, lon = data$lon, values = data$values[,,i])
      data_tmp$values = data_tmp$values/100
      frame = get_map_frame(data_tmp)
      
      map = ggmap_map_frame(frame,europe_map)
      names(frame) = c("lat", "lon", "values")
      map = map + stat_contour(data = frame, aes(x = lon, y = lat, z = values), binwidth = 2)
      png(file=image_path, width=2000,height=1400,res=150)
      plot(map)
      dev.off()
    }
  }
}
}
=======
n = 10
m = 10
x = 1:100
y = rep(34,length(x))
plot(x,y, type = "l", xlim = c(0,100), ylim = c(0,35))

# cl = rgb(1,1,0,maxColorValue = 255)
# plot(x,y, type = "l", xlim = c(0,100), ylim = c(0,17))
# for (i in 1:n){
#     cl = rgb(i/n,i/n,i/n)
#     y = rep(i,length(x))
#     lines(x = x, y = y, type = "l", col = cl, lwd = 3)
#   
#   
# }

for (k in 1:4){
  for (j in 1:8){
    c1 = k %% 2
    c2 = ((k - c1)%%4)/2
    c3 = ((k - c1 - c2*2)%%8)/4
    if (k%%8 == 0){
      c1 = 1
      c2 = 1
      c3 = 0
    }
    p = 13
    cl = rgb(c1*(j+(p-8))/p,c2*(j+(p-8))/p,c3*(j+(p-8))/p)
    y = rep((k-1)*8 + j,length(x))
    lines(x = x, y = y, type = "l", col = cl, lwd = 3)
  }
}



# for (i in 1:n){
#     
#     c1 = i %% 2
#     c2 = ((i - c1)%%4)/2
#     c3 = ((i - c1 - c2*2)%%8)/4
#     if (i%%8 == 0){
#       c1 = 1
#       c2 = 1
#       c3 = 0
#     }
#     print(c(c1,c2,c3))
#     print(" ")
# }



# 
# i = 2
# 
# plot(x,y, type = "l", xlim = c(0,100), ylim = c(0,17))
# cl1 = rgb(255,0,0,maxColorValue = 255)
# y = rep(1,length(x))
# lines(x = x, y = y, type = "l", col = cl1, lwd = 3)
# y = rep(3,length(x))
# cl2 = rgb(255,50,100,maxColorValue = 255)
# lines(x = x, y = y, type = "l", col = cl2, lwd = 3)
# 
# 
>>>>>>> origin/master


