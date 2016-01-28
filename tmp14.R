library(ggmap)
library(lubridate)
library(data.table)
library(geosphere)

source("data.r")
source("plot.r")
source("find_cyclones_data.r")




##------------------------------------##
## constants
nlat = 325
nlon = 577




##------------------------------------##


data_folder = '//192.168.13.1/share/Nikiforov Sergey/ERA Iterim'
images_folder = 'images_clust/'



files = list.files(data_folder, recursive = F, full.names = F)

clust4_sum_list = list()
for (i in 1:length(clust4_IDs)){
  clust4_sum_list[[i]] = matrix(0, nrow = nlon, ncol = nlat)
}

for(filename in files){
  data_filename = paste(data_folder,filename, sep='/')
  year = na.omit(as.numeric(unlist(strsplit(filename, "[^0-9]+"))))
  # image_name = paste0(images_folder, )
  data = read_nc_file(data_filename)
  for (i in 1:length(clust4_IDs)){
    dates = convert_to_dates(IDs = clust4_IDs[[i]])
    timestamps = convert_to_timestamps(dates, data)
    summary = matrix(0, nrow = length(data$lon), ncol = length(data$lat))
    for (t in timestamps){
      matrix = data$values[ , ,t]
      matrix = normalize_matrix(matrix)
      summary = summary + matrix
    }
    clust4_sum_list[[i]] = clust4_sum_list[[i]] + summary
  }
  
  
  
  
    

  
  
  
  
  
  
  
}

image_path = paste0(image_path, image_name)


if(!file.exists(image_path)){
  for(i in 1:length(clust4_IDs)){
    data_tmp = list(lat = data$lat, lon = data$lon, sum = clust4_sum_list[[i]])
    map = get_map_frame(data_tmp)
    
    
  }
  
  data$summary = get_summary(data)
  data$summary = normalize_matrix(data$summary)
  frame = get_map_frame(data)
  rm(data)
  save(frame, file=cache_filename)
}else{
  load(cache_filename)
}
# frame$X3 = round(frame$X3*100)
map = ggmap_map_frame(frame)
# pressure_plot = ggplot_map_frame(frame)
# png(file=paste(images_folder, year, ".png", sep=""), width=2000,height=1400,res=150)
plot(map)
# dev.off()
rm(frame)



