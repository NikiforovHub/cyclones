find_loc_mins = function(matrix){
  n = nrow(matrix)
  m = ncol(matrix)
  loc_min_list = list(NULL)
  for (i in 2:(n-1)){
    for (j in 2:(m-1)){
      loc_min = min(matrix[i-1,j-1],matrix[i-1,j],matrix[i-1,j+1],
        matrix[i,j-1],  matrix[i,j],  matrix[i,j+1],
        matrix[i+1,j-1],matrix[i+1,j],matrix[i+1,j+1])
      if (loc_min == matrix[i,j]) loc_min_list = c(loc_min_list,c(i,j))
    }
  }
  return(loc_min_list)
}

find_cyclones = function(D,G,N){
  min_points = find_loc_mins(matrix)
}

