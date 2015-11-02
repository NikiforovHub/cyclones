source(data.r)
source(find_cyclones.r)

D = 1000 # distance from center of cyclone in km
G = 1.5  # maximum value of average pressure gradient in hPa
N = 6    # amount of directions on which G is achieved


timestamps = length(data$time)
for (i in 1:timestamps){
  matrix = data$values[,,i]
  find_cyclones(D,G,N)
  
}