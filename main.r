source("data.r")

data_folder = '\\\\192.168.13.1\\share\\Dudko\\data\\ERA-40\\data'
images_folder = 'images\\'
cache_folder = 'cache/'
files = list.files(data_folder, recursive = F, full.names = F)
for(filename in files){
  data_filename = paste(data_folder,filename, sep='\\')
  year = na.omit(as.numeric(unlist(strsplit(filename, "[^0-9]+"))))
  data_tmp = read_nc_file(data_filename)
  for(month in 1:12){
    cache_path = paste(cache_folder,"frame_",year,"_",month,".cache",sep="")
    if(!file.exists(cache_path)){
      data = data_tmp
      data$summary = get_summary(data,month)
      data$summary = normalize_matrix(data$summary)
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
      rm(frame)
    }
  }
}
