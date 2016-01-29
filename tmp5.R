library(ggmap)
library(lubridate)
library(data.table)
library(geosphere)
library(foreach)
library(doParallel)

# cyclones_base_all = get_database_all(cache_folder)
# clust_data_all = get_clusterization_data(cyclones_base_all)
# clust_data = clust_data_all[,c(2,5:9),with = FALSE]
# clust_data = normalize_clust_data(clust_data)
# cl4 = kmeans(clust_data, centers = 4)
# cl5 = kmeans(clust_data, centers = 5)


a = data.table(1:20,21:40)
c = 1:10
num_cores = detectCores() - 2
cl <- makeCluster(num_cores)
registerDoParallel(cl)




foreach(i = 1:10, .packages=c("lubridate",'data.table')) %dopar% {
  b = a[V2 < 30]
  print(c[i])
  
}
stopCluster(cl)