source("data.r")

data_filename = 'data/2001.nc'
year = na.omit(as.numeric(unlist(strsplit(data_filename, "[^0-9]+"))))
cache_filename = paste("frame_",year,".cache",sep="")
if(!file.exists(cache_filename)){
  data = read_nc_file(data_filename)
  data$summary = get_summary(data)
  data$summary = normalize_matrix(data$summary)
  frame = get_map_frame(data)
  rm(data)
  save(frame, file=cache_filename)
}else{
  load(cache_filename)
}
ggplot_map_frame(frame)
rm(frame)