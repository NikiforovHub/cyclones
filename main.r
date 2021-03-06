source("data.r")
source("plot.r")

data_folder = 'D:/ERA-40/sample'
images_folder = 'images/'
files = list.files(data_folder, recursive = F, full.names = F)
for(filename in files){
  data_filename = paste(data_folder,filename, sep='/')
  year = na.omit(as.numeric(unlist(strsplit(filename, "[^0-9]+"))))
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
  # frame$X3 = round(frame$X3*100)
  map = ggmap_map_frame(frame)
  # pressure_plot = ggplot_map_frame(frame)
  # png(file=paste(images_folder, year, ".png", sep=""), width=2000,height=1400,res=150)
  plot(map)
  # dev.off()
  rm(frame)
}

data_folder = '//192.168.13.1/share/Dudko/data/ERA-40/data'
images_folder = 'images/'
cache_folder = 'cache/'
files = list.files(data_folder, recursive = F, full.names = F)
for(filename in files){
  data_filename = paste(data_folder,filename, sep='\\')
  year = na.omit(as.numeric(unlist(strsplit(filename, "[^0-9]+"))))
  data_tmp = read_nc_file(data_filename)
  for(month in 1:12){
    cache_path = paste(cache_folder,"frame_",year,"_",month,".cache",sep="")
    if(!file.exists(cache_path)){
      data$values = normalize_values(data$values)
      data$summary = get_summary(data)
      data$summary = normalize_matrix(data$summary)
      data = list(data$lat,data$lon,data$summary)
      names(data) =  c("lat","lon","value")
      frame = get_map_frame(data)
      rm(data)
      save(frame, file=cache_path)
    }else{
      load(cache_path)
    }
    image_path = paste(images_folder, year,"_",month, ".png", sep="")  
    if (!file.exists(image_path)){
      # frame$X3 = round(frame$X3*100)
      map = ggmap_map_frame(frame)
      # pressure_plot = ggplot_map_frame(frame)
      png(file=image_path, width=2000,height=1400,res=150)
      plot(map)
      dev.off()
      rm(map)
      rm(frame)
    }
  }
}
