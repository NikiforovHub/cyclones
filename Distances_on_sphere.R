k1 = 1
l1 = 1
k2 = 325
l2 = 577



theta1 = data_tmp$lat[k1]
theta2 = data_tmp$lat[k2]
phi1 = data_tmp$lon[l1]
phi2 = data_tmp$lon[l2]
theta1 = theta1/180*pi
theta2 = theta2/180*pi
phi1 = phi1/180*pi
phi2 = phi2/180*pi
Lmy = R*acos(sin(theta1)*sin(theta2) + 
              cos(theta1)*cos(theta2)*cos(phi1-phi2))




lat1 = data_tmp$lat[k1]
lon1 = data_tmp$lon[l1]
p1 = c(lon1, lat1)
lat2 = data_tmp$lat[k2]
lon2 = data_tmp$lon[l2]
p2 = c(lon2, lat2)
LCosine = distCosine(p1, p2)/1000
LHavershine = distHaversine(p1,p2)/1000
LVincentySphere = distVincentySphere(p1,p2)/1000
LVincentyEllipsoid = distVincentyEllipsoid(p1,p2)/1000
LMeeus = distMeeus(p1,p2)/1000
LGeo = distGeo(p1,p2)/1000

print(c("Lmy",Lmy))
print(c("Lcosine", LCosine))
print(c("LHavershine", LHavershine))
print(c("LVincentySphere", LVincentySphere))
print(c("LVincentyEllipsoid", LVincentyEllipsoid))
print(c("LMeeus", LMeeus))
print(c("LGeo", LGeo))
print(c("dLmy/LGeo", (Lmy-LGeo)/Lmy*100,"%"))