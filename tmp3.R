n = 10
m = 10
x = 1:100
y = rep(34,length(x))
plot(x,y, type = "l", xlim = c(0,100), ylim = c(0,35))

# cl = rgb(1,1,0,maxColorValue = 255)
# plot(x,y, type = "l", xlim = c(0,100), ylim = c(0,17))
# for (i in 1:n){
#     cl = rgb(i/n,i/n,i/n)
#     y = rep(i,length(x))
#     lines(x = x, y = y, type = "l", col = cl, lwd = 3)
#   
#   
# }

for (k in 1:4){
  for (j in 1:8){
    c1 = k %% 2
    c2 = ((k - c1)%%4)/2
    c3 = ((k - c1 - c2*2)%%8)/4
    if (k%%8 == 0){
      c1 = 1
      c2 = 1
      c3 = 0
    }
    p = 13
    cl = rgb(c1*(j+(p-8))/p,c2*(j+(p-8))/p,c3*(j+(p-8))/p)
    y = rep((k-1)*8 + j,length(x))
    lines(x = x, y = y, type = "l", col = cl, lwd = 3)
  }
}



# for (i in 1:n){
#     
#     c1 = i %% 2
#     c2 = ((i - c1)%%4)/2
#     c3 = ((i - c1 - c2*2)%%8)/4
#     if (i%%8 == 0){
#       c1 = 1
#       c2 = 1
#       c3 = 0
#     }
#     print(c(c1,c2,c3))
#     print(" ")
# }



# 
# i = 2
# 
# plot(x,y, type = "l", xlim = c(0,100), ylim = c(0,17))
# cl1 = rgb(255,0,0,maxColorValue = 255)
# y = rep(1,length(x))
# lines(x = x, y = y, type = "l", col = cl1, lwd = 3)
# y = rep(3,length(x))
# cl2 = rgb(255,50,100,maxColorValue = 255)
# lines(x = x, y = y, type = "l", col = cl2, lwd = 3)
# 
# 


