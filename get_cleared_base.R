get_cleared_base = function(cyclones_base){
  cyclones_base = cyclones_base[!is.na(geom_center_lon)]
  chain_len = 1
  bool = logical(nrow(cyclones_base))
  for (i in 1:(nrow(cyclones_base)-1)){
    if (cyclones_base$ID[i] == cyclones_base$ID[i+1]){
      chain_len = chain_len + 1
    }else{
      if (chain_len > 2){
        bool[(i - chain_len + 1):i] =  TRUE
        chain_len = 1
      }
    }
  }
  if (chain_len > 2){
    bool[(nrow(cyclones_base) - chain_len + 1):nrow(cyclones_base)] =  TRUE
    chain_len = 1
  }
  cyclones_base_cleared = cyclones_base[bool]
  for(i in 1:nrow(cyclones_base_cleared)){
    if (is.infinite(cyclones_base_cleared$min_r[i])){
      cyclones_base_cleared$min_r[i] = NA
    }
  }
  
  
  return(cyclones_base_cleared)
}


get_clusterization_data = function(cyclones_base){
  chain_len = 1
  for (i in 1:(nrow(cyclones_base)-1)){
    if (cyclones_base$ID[i] == cyclones_base$ID[i+1]){
      chain_len = chain_len + 1
    }else{
      point1 = c(cyclones_base$geom_center_lon[i - chain_len + 1], 
                 cyclones_base$geom_center_lat[i - chain_len + 1])
      point2 = c(cyclones_base$geom_center_lon[i], 
                 cyclones_base$geom_center_lat[i])
      
      
      
      ID = cyclones_base$ID[i]
      brng = bearing(poin1, point2)
      mean_center_pressure = mean(cyclones_base$center_pressure[(i - chain_len + 1):i])
      geom_center_lon = cyclones_base$geom_center_lon[i - chain_len + 1]
      geom_center_lat = cyclones_base$geom_center_lat[i - chain_len + 1]
      mean_speed = mean(cyclones_base$speed[(i - chain_len + 1):i])
      mean_area = mean(cyclones_base$area[(i - chain_len + 1):i])
      mean_area = mean(cyclones_base$area[(i - chain_len + 1):i])
      
      
      
      clusterization_data = data.table(ID, geom_center_lon, geom_center_lat, brng)
      chain_len = 1
    }
  }
#   if (chain_len != 1){
#     point1 = c(cyclones_base$geom_center_lon[nrow(cyclones_base) - chain_len + 1], 
#                cyclones_base$geom_center_lat[nrow(cyclones_base) - chain_len + 1])
#     point2 = c(cyclones_base$geom_center_lon[nrow(cyclones_base)], 
#                cyclones_base$geom_center_lat[nrow(cyclones_base)])
#     brng = bearing(poin1, point2)
#   }
  
}