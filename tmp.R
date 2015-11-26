n = nrow(centers_prob)
maxLatInd = length(data_tmp$lat)
maxLonInd = length(data_tmp$lon)
cyclone_centers = NULL
track = NULL
for (i in 1:n){
  lat_ind = centers_prob$lat_ind[i]
  lon_ind = centers_prob$lon_ind[i]
  track_line = c(data_tmp$lat[lat_ind],data_tmp$lon[lon_ind])
  Ntest = 0
  
  ## gradient
  for(p1 in c(-1,0,1)){
    for(p2 in c(-1,0,1)){ # order: SW, W, NW, S, N, SE, E,  NE
      if((p1 == 0) & (p2 == 0)) next
      k=lat_ind
      l=lon_ind
      grad = NULL
      grad_count = 1
      d = 1
      while((k >= 1) & (k <= maxLatInd) & (l>=2) & (l<= maxLonInd) &
            (k + p1*d >= 1) & (k + p1*d <= maxLatInd) &
            (l + p2*d >= 1) & (l + p2*d <= maxLonInd)){
        d = 1
        repeat{
          theta1 = data_tmp$lat[k + p1*d]
          theta2 = data_tmp$lat[k]
          phi1 = data_tmp$lon[l + p2*d]
          phi2 = data_tmp$lon[l]
          theta1 = theta1/180*pi
          theta2 = theta2/180*pi
          phi1 = phi1/180*pi
          phi2 = phi2/180*pi
          L = R*acos(sin(theta1)*sin(theta2) + 
                       cos(theta1)*cos(theta2)*cos(phi1-phi2))
          if (L >= Lmin) break
          d = d + 1
          if (((k + p1*d) < 1) | (k + p1*d > maxLatInd) |
              (l + p2*d < 1) | (l + p2*d > maxLonInd)) break
        }
        if (((k + p1*d) < 1) | (k + p1*d > maxLatInd) |
            (l + p2*d < 1) | (l + p2*d > maxLonInd)) break
        
        grad[grad_count] = (data_tmp$values[l + p2*d, k + p1*d] - data_tmp$values[l,k])/L*100
        grad_count = grad_count + 1
        k = k + p1*d
        l = l + p2*d
      }
      track_line = c(track_line, max(grad, na.rm=TRUE))
      if ((max(grad, na.rm=TRUE) > G) | (max(grad, na.rm=TRUE) == -Inf)) Ntest = Ntest + 1
    } 
  }
  track = rbind(track,track_line,deparse.level = 0)
  if (Ntest >= N) {
    lat = data_tmp$lat[lat_ind]
    lon = data_tmp$lon[lon_ind]
    cyclone_center = c(lat, lon, lat_ind,lon_ind,1)
    cyclone_centers = rbind(cyclone_centers,cyclone_center,deparse.level = 0)
  }
}
colnames(track) = c("lat","lon","N","S","W","E","NW","NE","SE","SW")
track_t = t(track)
print(track_t,digits = 2)
fileConn = file("track_log.csv")
write.table(format(track_t, digits=2), file = "track_log.csv", 
            append = TRUE, sep='\t',quote = FALSE)
a = paste("day")
write.table(a, file = "track_log.csv", 
            append = TRUE, sep = " ", quote = FALSE)
close(fileConn)
cyclone_centers = as.data.frame(cyclone_centers)
names(cyclone_centers) = c("lat","lon", "lat_ind", "lon_ind", "values")