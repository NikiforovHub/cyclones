nCenters = nrow(cyclone_centers)
maxLatInd = length(data_tmp$lat)
maxLonInd = length(data_tmp$lon)

for (i in 1:nCenters){
  model_frame = data.frame()
  model_frame_total = data.frame()
  center_lat_ind = cyclone_centers$lat_ind[i]
  center_lon_ind = cyclone_centers$lon_ind[i]
  ## collecting pressure values in 8 directions 
  for(p1 in c(-1,0,1)){
    # order of directions: NW,N,NE, W,E, SW,S,SE
    for(p2 in c(-1,0,1)){
      if((p1 == 0) & (p2 == 0)) next
      Lcount = 0
      d = 1
      k1 = center_lat_ind
      l1 = center_lon_ind
      k2 = NULL
      l2 = NULL
      while((k1 >= 1) & (k1 <= maxLatInd) & (l1>=1) & (l1<= maxLonInd) &
            (k1 + p1*d >= 1) & (k1 + p1*d <= maxLatInd) &
            (l1 + p2*d >= 1) & (l1 + p2*d <= maxLonInd)){
        Lcount = Lcount + 1
        d = 1
        repeat{
          k2 = k1 + p1*d
          l2 = l1 + p2*d
          # Calculating distance between two points
          lat_center = data_tmp$lat[center_lat_ind]
          lon_center = data_tmp$lon[center_lon_ind]
          lat1 = data_tmp$lat[k1]
          lon1 = data_tmp$lon[l1]
          lat2 = data_tmp$lat[k2]
          lon2 = data_tmp$lon[l2]
          point_center = c (lon_center,lat_center)
          point1 = c(lon1,lat1)
          point2 = c(lon2,lat2)
          Lcenter = distCosine(point_center,point2)/1000 # divided by 1000 for conversion from meters to kilometers
          if (Lcenter >= Lcount*Lmin) break
          if (((k1 + p1*d) < 1) | (k1 + p1*d > maxLatInd) |
              (l1 + p2*d < 1) | (l1 + p2*d > maxLonInd)) break
          d = d + 1
        }
        L = distCosine(point1,point2)/1000 # divided by 1000 for conversion from meters to kilometers
        grad = (data_tmp$values[l2, k2] - data_tmp$values[l1,k1])/L*100
        center_p = data_tmp$values[center_lon_ind,center_lat_ind]
        Delta_p = data_tmp$values[l2,k2] - 
          data_tmp$values[center_lon_ind,center_lat_ind]
        frame_line = data.frame(center_p,Lcenter,Delta_p,NA)
        model_frame = rbindlist(list(model_frame,frame_line))
        if (grad < grad_limit){
          model_frame[,4] = Lcenter
          break
        }
        k1 = k2
        l1 = l2
      }
      model_frame_total = rbind(model_frame_total,model_frame)
      
    }
  }
}
names(model_frame_total) = c("Pcenter","d","Delta_P","dmax")