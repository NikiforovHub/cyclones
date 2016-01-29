cyclones_base_all = get_database_all(cache_folder)
clust_data_all = get_clusterization_data(cyclones_base_all)
clust_data = clust_data_all[,c(2,5:9),with = FALSE]
clust_data = normalize_clust_data(clust_data)
# clust_data$brng = clust_data$brng*4
cl4 = kmeans(clust_data, centers = 4)
cl5 = kmeans(clust_data, centers = 5)

plot(clust_data, col = cl4$cluster)
plot(clust_data, col = cl5$cluster)


images_folder = "images_points/"
clust4_IDs = list()
for (i in 1:4){
  clust4_IDs[[i]] = clust_data_all$ID[(cl4$cluster == i)]
}

clust5_IDs = list()
for (i in 1:5){
  clust5_IDs[[i]] = clust_data_all$ID[(cl5$cluster == i)] 
}


cyclones_base_plot = cyclones_base_all
norm_area_coef = max(cyclones_base_plot$area) - min(cyclones_base_plot$area)
cyclones_base_plot$area = cyclones_base_plot$area/norm_area_coef*5 + 1



europe_map = get_map(location = "europe", maptype = "terrain", zoom = 3)
for (i in 1:5){
  map = ggmap(europe_map)
  for (j in 1:length(clust5_IDs[[i]])){
    sub_data = cyclones_base_plot[ ID == clust5_IDs[[i]][j] ]
    map = map + geom_point(data = sub_data, aes(x = geom_center_lon, y = geom_center_lat, size = area), 
                           color = 1+i, alpha = 0.3)
#       geom_line(data = sub_data, aes(x = geom_center_lon, y = geom_center_lat), 
#                 color = 1+i, size = 1)
    rm(sub_data)
  }
  image_name = paste0("clust5_",i,".png")
  image_path = paste0(images_folder, image_name)
  png(file=image_path, width=2000,height=1400,res=150)
  plot(map)
  dev.off()
}








  
  

image_name = paste0("clust4_",i,".png")
image_path = paste0(images_folder, image_name)

if(!file.exists(image_path)){
  
  png(file=image_path, width=2000,height=1400,res=150)
  plot(map)
  dev.off()
}
