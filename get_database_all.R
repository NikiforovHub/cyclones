get_database_all = function(){
  cache_folder = "cache/ERA Interim/"
  files_real_data = list.files(cache_folder, pattern = "Real data", recursive = F, full.names = F)
  cyclones_base_all = data.table()
  maxID = 0
  for (file in files_real_data){
    load(paste0(cache_folder,file))
    cyclones_base$ID = cyclones_base$ID + maxID
    maxID = max(cyclones_base$ID)
    cyclones_base = get_cleared_base(cyclones_base)
    cyclones_base_all = rbindlist(list(cyclones_base_all, cyclones_base))
  }
  return(cyclones_base_all)
}


get_clusterization_data = function(cyclones_base){
  clusterization_data = data.table()
  chain_len = 1
  for (i in 1:(nrow(cyclones_base)-1)){
    if (cyclones_base$ID[i] == cyclones_base$ID[i+1]){
      chain_len = chain_len + 1
    }else{
      if (chain_len > 2){
        cyclone_start_point = c(cyclones_base$geom_center_lon[i - chain_len + 1], 
                                cyclones_base$geom_center_lat[i - chain_len + 1])
        cyclone_end_point = c(cyclones_base$geom_center_lon[i], cyclones_base$geom_center_lat[i])
        
        ID = cyclones_base$ID[i]
        mean_pressure = mean(cyclones_base$center_pressure[(i - chain_len + 1):i])
        geom_center_lon = cyclones_base$geom_center_lon[i - chain_len + 1]
        geom_center_lat = cyclones_base$geom_center_lat[i - chain_len + 1]
        brng = bearing(cyclone_start_point, cyclone_end_point)
        mean_area = mean(cyclones_base$area[(i - chain_len + 1):i])
        mean_speed = mean(cyclones_base$speed[(i - chain_len + 1):i], na.rm = TRUE)
        mean_min_r = mean(cyclones_base$min_r[(i - chain_len + 1):i], na.rm = TRUE)
        mean_max_r = mean(cyclones_base$max_r[(i - chain_len + 1):i])
        
        data_line = data.table(ID, mean_pressure, geom_center_lon, geom_center_lat, brng, 
                               mean_area, mean_speed, mean_min_r, mean_max_r)
        clusterization_data = rbindlist(list(clusterization_data, data_line))
      }
      chain_len = 1
    }
  }
  if (chain_len > 2){
    ID = cyclones_base$ID[nrow(cyclones_base)]
    mean_pressure = mean(cyclones_base$center_pressure[(nrow(cyclones_base) - chain_len + 1):nrow(cyclones_base)])
    geom_center_lon = cyclones_base$geom_center_lon[nrow(cyclones_base) - chain_len + 1]
    geom_center_lat = cyclones_base$geom_center_lat[nrow(cyclones_base) - chain_len + 1]
    brng = bearing(cyclone_start_point, cyclone_end_point)
    mean_area = mean(cyclones_base$area[(nrow(cyclones_base) - chain_len + 1):nrow(cyclones_base)])
    mean_speed = mean(cyclones_base$speed[(nrow(cyclones_base) - chain_len + 1):nrow(cyclones_base)])
    mean_min_r = mean(cyclones_base$min_r[(nrow(cyclones_base) - chain_len + 1):nrow(cyclones_base)], na.rm = TRUE)
    mean_max_r = mean(cyclones_base$max_r[(nrow(cyclones_base) - chain_len + 1):nrow(cyclones_base)])
    
    data_line = data.table(ID, mean_pressure, geom_center_lon, geom_center_lat, brng, 
                           mean_area, mean_speed, mean_min_r, mean_max_r)
    clusterization_data = rbindlist(list(clusterization_data, data_line))
  }
  chain_len = 1
  return(clusterization_data)
}