library(ggmap)
library(lubridate)
library(data.table)
library(geosphere)

source("data.r")
source("plot.r")
source("find_cyclones_data.r")
source("cyclones_base_data.R")


cache_folder = "cache/ERA Interim/"

cyclones_base_all = get_database_all(cache_folder)
clust_data_all = get_clusterization_data(cyclones_base_all)
clust_data = clust_data_all[,c(2,5:9),with = FALSE]
clust_data = normalize_clust_data(clust_data)
# clust_data$brng = clust_data$brng*4
cl4 = kmeans(clust_data, centers = 4)
cl5 = kmeans(clust_data, centers = 5)

plot(clust_data, col = cl4$cluster)
plot(clust_data, col = cl5$cluster)



clust4_IDs = list()
for (i in 1:4){
  clust4_IDs[[i]] = clust_data_all$ID[(cl4$cluster == i)]
}


map = get_map(location = "europe", maptype = "terrain", zoom = 3)
map = ggmap(map)

for (i in 1:4){
  for (j in 1:length(clust4_IDs[[i]])){
    sub_data = cyclones_base_all[ ID == clust4_IDs[[i]][j] ]
    map = map + geom_point(data = sub_data, aes(x = geom_center_lon, y = geom_center_lat), 
                           color = 1+i, size = 4, alpha = 0.3) + 
      #       geom_line(data = sub_data, aes(x = geom_center_lon, y = geom_center_lat), 
      #                 color = 1+i, size = 1)
      rm(sub_data)
  }
  
}
plot(map)













image_name = paste0("clust4_",i,".png")
image_path = paste0(images_folder, image_name)

if(!file.exists(image_path)){
  
  png(file=image_path, width=2000,height=1400,res=150)
  plot(map)
  dev.off()
}