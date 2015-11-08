read_nc_file = function(file_path){
  library(ncdf)
  nc <- open.ncdf(file_path)
  lon <- get.var.ncdf(nc,"longitude")
  nlon <- dim(lon)
  lat <- get.var.ncdf(nc,"latitude")
  time <- get.var.ncdf(nc,"time")
  tunits <- att.get.ncdf(nc, "time", "units")
  dates <- as.POSIXct(time*3600, origin = "1900-01-01", tz="GMT")
  var <- get.var.ncdf(nc,"msl")
  
  result = list(lat=lat, lon=lon, time=dates, values=var)
  close.ncdf(nc)
  rm(nc)
  return(result)
}


normalize_values = function(values){
  timestamps = dim(values)[3]
  for(i in 1:timestamps){
    values[,,i] = normalize_matrix(values[,,i])
  }
  return(values)
}


normalize_matrix = function(matrix){
  min = min(matrix)
  matrix = matrix - min
  max = max(matrix)
  matrix = matrix / max
  return(matrix)
}


get_summary = function(data_list, month=NULL){
  timestamps = dim(data_list$values)[3]
  time = as.character(data_list$time)
  tmp = strsplit(time, "[^0-9]+")
  monthstamps = NULL
  for (i in 1:timestamps){
    monthstamps[i] = as.numeric(tmp[[i]][2])
  }
  lon = dim(data_list$values)[1]
  lat = dim(data_list$values)[2]
  result = matrix(sample(0, lon * lat, replace = TRUE),nrow=lon, ncol=lat)
    if (is.null(month)){
    for(i in 1:timestamps){
      result = result + data_list$values[,,i]
    }
  }else{
    for(i in 1:timestamps){
      if (monthstamps[i] == month) {
        result = result + data_list$values[,,i]
      }
    }  
  }
  return(result)
}




# get_map_frame transforms list with lat,lon and values to frame, which can be
# used to plot
get_map_frame = function(data_list){
  lon_length = length(data_list$lon)
  lat_length = length(data_list$lat)
  frame_matrix = matrix(0,lon_length*lat_length,3)
  result_frame = data.frame(lat=integer(),lon=integer(),value=numeric())
  for(i in 1:lat_length){
    for(j in 1:lon_length){
      frame_matrix[(i-1)*lon_length+j,1] = data_list[[1]][i]
      frame_matrix[(i-1)*lon_length+j,2] = data_list[[2]][j]
      frame_matrix[(i-1)*lon_length+j,3] = data_list[[3]][j,i]
    }
  }
  result_frame = data.frame(frame_matrix) 
  return(result_frame)
}


ggplot_map_frame = function(frame){
  library(ggplot2)
  p = ggplot(frame, 
             aes(x = frame$X2, y = frame$X1, z = frame$X3, fill = frame$X3)) + 
    scale_fill_gradient(low="red", high="white") +
    geom_tile()
  return(p)
}