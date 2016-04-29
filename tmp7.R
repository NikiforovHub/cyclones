Pmax = numeric()
k = 1
for ( i in 1:(nrow(model_frame_total) - 1) ){
  if (model_frame_total$dmax[i] == model_frame_total$dmax[i + 1]){
    k = k + 1
  }else{
    Pmax[(i - k + 1):i] = max(model_frame_total$P[(i - k + 1):i])
    k = 1
  }
}

Pmax[(i - k + 1):i] = max(model_frame_total$P[(i - k + 1):i])



m2 = m
m2$P = (m2$P - m2$Pcenter)/(m2$Pmax - m2$Pcenter)




