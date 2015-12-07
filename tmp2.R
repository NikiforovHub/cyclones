start_plot = 1
plot_count = 1
plot_folder = "graphs/plots/"

for (i in 1:nrow(model_frame_total)){
  image_path = paste(plot_folder,"plot_",plot_count,".png",sep="")
  if (model_frame_total$d[i] ==  model_frame_total$dmax[i]){
    png(file=image_path, width=2000,height=1400,res=150)
    plot(model_frame_total$Delta_P[start_plot:i], type="l",x = model_frame_total$d[start_plot:i],
         xlab="Distance", ylab="Pressure difference")
    lines(model_frame_total$model_predict[start_plot:i], col="red",x = model_frame_total$d[start_plot:i])
    dev.off()
    start_plot = i+1
    plot_count = plot_count + 1
  }
}