library(ggmap)
library(lubridate)
library(data.table)
library(geosphere)
library(foreach)
library(doParallel)

source("data.r")
source("plot.r")
source("find_cyclones_data.r")
source("cyclones_base_data.R")



##------------------------------------##
## constants
nlat = 325
nlon = 577
##------------------------------------##

# setwd("//192.168.13.1/share/Nikiforov Sergey/R/cyclones")
# 
# 
data_folder = '//192.168.13.1/share/Nikiforov Sergey/ERA Iterim/'
images_folder = 'images_clust_foreach_test/'
cache_folder = "cache/ERA Interim/"
clust4_cache_folder = "cache/clusterization/clust4_foreach/"
clust5_cache_folder = "cache/clusterization/clust5_foreach/"



# getting database files and clusters data
{
cyclones_base_all = get_database_all(cache_folder)
clust_data_all = get_clusterization_data(cyclones_base_all)
clust_data = clust_data_all[,c(2,5:9),with = FALSE]
clust_data = normalize_clust_data(clust_data)
cl4 = kmeans(clust_data, centers = 4)
cl5 = kmeans(clust_data, centers = 5)


clust4_IDs = list()
for (i in 1:4){
  clust4_IDs[[i]] = clust_data_all$ID[(cl4$cluster == i)] 
}
clust5_IDs = list()
for (i in 1:5){
  clust5_IDs[[i]] = clust_data_all$ID[(cl5$cluster == i)] 
}
}
## ------------------------------------ ##



files_real_data = list.files("cache/ERA Interim/", pattern = "Real data", recursive = F, full.names = F)
files_real_data = substr(files_real_data,1,37)

start_time = proc.time()

num_cores = detectCores() - 4
cl <- makeCluster(num_cores)
registerDoParallel(cl)

library(ggmap)
library(lubridate)
library(data.table)
library(geosphere)
library(foreach)
library(doParallel)

foreach(filename = files_real_data[9:13], .packages=c("ggmap", "lubridate",'data.table', 
                                                "geosphere", "foreach", "doParallel")) %dopar% {
  # initializing lists with sums
  {
  for (i in 1:length(clust4_IDs)){
    clust4_sum_list[[i]] = matrix(0, nrow = nlon, ncol = nlat)
  }
  
  for (i in 1:length(clust5_IDs)){
    clust5_sum_list[[i]] = matrix(0, nrow = nlon, ncol = nlat)
  }
  }
  
  
  cache_name4 = paste0("until ",filename,".cache")
  cache_path4 = paste0(clust4_cache_folder, cache_name4)
  cache_name5 = paste0("clusterize_data ",filename,".cache")
  cache_path5 = paste0(clust5_cache_folder, cache_name5)
  
  if ((!file.exists(cache_path4)) || (!file.exists(cache_path5))){
    print(paste0("Start work with ", filename))
    data_filename = paste0(data_folder,filename)
    data = read_nc_file(data_filename)
    if ((!file.exists(cache_path4))){
      for (i in 1:length(clust4_IDs)){
        dates = convert_to_dates(cyclones_base = cyclones_base_all, IDs = clust4_IDs[[i]])
        dates_unique = unique(dates)
        year_start = substr(filename, 11, 14)
        year_end = substr(filename, 25, 28)
        dates_final = c(grep(year_start, dates_unique, value = TRUE),
                        grep(year_end, dates_unique, value = TRUE))
        timestamps = convert_to_timestamps(dates_final, data)
        summary = matrix(0, nrow = length(data$lon), ncol = length(data$lat))
        for (t in timestamps){
          matrix = data$values[ , ,t]
          matrix = normalize_matrix(matrix)
          summary = summary + matrix
        }
        clust4_sum_list[[i]] = clust4_sum_list[[i]] + summary
      }
      save(clust4_sum_list, file = cache_path4)
      print(paste0(cache_path4," saved"))
    }
    
    if ((!file.exists(cache_path5))){
      for (i in 1:length(clust5_IDs)){
        dates = convert_to_dates(cyclones_base = cyclones_base_all, IDs = clust5_IDs[[i]])
        dates_unique = unique(dates)
        year_start = substr(filename, 11, 14)
        year_end = substr(filename, 25, 28)
        dates_final = c(grep(year_start, dates_unique, value = TRUE),
                        grep(year_end, dates_unique, value = TRUE))
        timestamps = convert_to_timestamps(dates_final, data)
        summary = matrix(0, nrow = length(data$lon), ncol = length(data$lat))
        for (t in timestamps){
          matrix = data$values[ , ,t]
          matrix = normalize_matrix(matrix)
          summary = summary + matrix
        }
        clust5_sum_list[[i]] = clust5_sum_list[[i]] + summary
      }
      save(clust5_sum_list, file = cache_path5)
      print(paste0(cache_path5," saved"))
      
    }
  }
}
stopCluster(cl)
stop_time = proc.time()
print(stop_time - start_time)


cache4_files_list = list.files(clust4_cache_folder)  
cache5_files_list = list.files(clust5_cache_folder)


#initializing sum lists for all files
{
clust4_sum_list_all = list()
clust5_sum_list_all = list()

for (i in 1:length(clust4_IDs)){
  clust4_sum_list_all[[i]] = matrix(0, nrow = nlon, ncol = nlat)
}
for (i in 1:length(clust5_IDs)){
  clust5_sum_list_all[[i]] = matrix(0, nrow = nlon, ncol = nlat)
}
}


for (filename in cache4_files_list){
  cache_path = paste0(clust4_cache_folder,filename)
  load(cache_path)
  for(i in 1:length(clust4_IDs)){
    clust4_sum_list_all[[i]] = clust4_sum_list_all[[i]] + clust4_sum_list[[i]]
  }
}


for (filename in cache5_files_list){
  cache_path = paste0(clust5_cache_folder,filename)
  load(cache_path)
  for(i in 1:length(clust5_IDs)){
    clust5_sum_list_all[[i]] = clust5_sum_list_all[[i]] + clust5_sum_list[[i]]
  }
}

# extracting latitudes and lontitudes for plotting
data = read_nc_file("//192.168.13.1/share/Nikiforov Sergey/ERA Iterim/Real data 1981-09-01 to 1982-03-01.nc")
latitudes = data$lat
lontitudes = data$lon


europe_map = get_map(location = "europe", maptype = "terrain", zoom = 3)

for (i in 1:length(clust4_sum_list_all)){
  image_name = paste0("clust4_",i,".png")
  image_path = paste0(images_folder, image_name)
  if(!file.exists(image_path)){
    data_tmp = list(lat = latitudes, lon = lontitudes, sum = clust4_sum_list_all[[i]])
    frame = get_map_frame(data_tmp)
    map = ggmap_map_frame(frame,europe_map)
    png(file=image_path, width=2000,height=1400,res=150)
    plot(map)
    dev.off()
  }
}

for (i in 1:length(clust5_sum_list_all)){
  image_name = paste0("clust5_",i,".png")
  image_path = paste0(images_folder, image_name)
  if(!file.exists(image_path)){
    data_tmp = list(lat = latitudes, lon = lontitudes, sum = clust5_sum_list_all[[i]])
    frame = get_map_frame(data_tmp)
    map = ggmap_map_frame(frame,europe_map)
    png(file=image_path, width=2000,height=1400,res=150)
    plot(map)
    dev.off()
  }
}