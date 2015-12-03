ggmap_map_frame = function(frame,base_map){
  library(ggmap)
  map = ggmap(base_map, extent = "device") + 
    geom_point(data = frame, aes(x = X2, y = X1, colour = 1-X3, alpha = 1-X3), size = 10)+
    scale_color_gradient(low = "white", high = "red", guide=FALSE) +
    scale_alpha(range = c(0, 0.03), guide = FALSE)
  return(map)
}

leaflet_map_frame = function(frame){
  library(leaflet)
  m = leaflet() %>% addTiles() %>% addCircles(data = frame, lat = ~ X1, lng = ~ X2, radius = 1, color = "red")
  return(m)
}