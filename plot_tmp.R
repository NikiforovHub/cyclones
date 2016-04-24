ggmap_map_frame = function(frame,base_map){
  library(ggmap)
  map = ggmap(base_map, extent = "device", legend = "right") + 
    geom_point(data = frame, aes(x = lon, y = lat, colour = 1-values), size = 10) +
    scale_color_gradient(low = "blue", high = "red")
  return(map)
}

leaflet_map_frame = function(frame){
  library(leaflet)
  m = leaflet() %>% addTiles() %>% addCircles(data = frame, lat = ~ X1, lng = ~ X2, radius = 1, color = "red")
  return(m)
}