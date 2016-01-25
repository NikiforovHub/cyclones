load("get_cleared_base.R")

get_database_all = function(cyclones_base){
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