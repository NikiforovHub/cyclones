# model_frame_total$model_predict = predict(model, newdata=model_frame_total)

start_plot = 1
plot_count = 1
plot_folder = "graphs_for_presentation/plots/"

for (i in 1:nrow(model_frame_total)){
  image_path = paste(plot_folder,"plot_",plot_count,".png",sep="")
  if (model_frame_total$d[i] ==  model_frame_total$dmax[i]){
    png(file=image_path, width=2000,height=1400,res=150)
    plot(model_frame_total$Delta_P[start_plot:i], type="l",x = model_frame_total$d[start_plot:i],
         xlab="Distance", ylab="Pressure difference", lwd = 3, cex.lab = 1.8) + box(lwd = 3)
    lines(model_frame_total$model_predict[start_plot:i], col="red",x = model_frame_total$d[start_plot:i], lwd = 3)
    dev.off()
    start_plot = i+1
    plot_count = plot_count + 1
  }
}


# png(file="other/map_netherland.png", width=2000,height=1400,res=150)
# plot(netherland_map)
# dev.off()
# 
# 
# nIntervLon = 8
# nIntervLat = 6
# 
# 
# x = data_tmp$lon
# y = data_tmp$lat
# 
# 
# 
# hline1 = rep(65,length(x))
# 
# 
# 
# plot(mt)
# lines(1:577, hline1)
# 
# 
# cf = get_isobars_frame(c_tmp,data_tmp)
# mp = map + stat_contour(data = frame, aes(x = lon, y = lat, z = values), binwidth = 2) + 
#   geom_point(data = cf[1:1400,], aes(x = lon, y = lat), color = "yellow", size = 1)
# plot(mp)