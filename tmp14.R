km = kmeans(clust_data_ver1, 5)
dissE <- daisy(clust_data_ver1) 
dE2   <- dissE^2
sk2   <- silhouette(km$cl, dE2)
plot(sk2)



data(pottery)
km    <- kmeans(pottery,3)
dissE <- daisy(pottery) 
dE2   <- dissE^2
sk2   <- silhouette(km$cl, dE2)
plot(sk2)

k1 <- kmeans(x=iris[, 1:4], centers=3)
plot(k1)
plot(k1, data=iris)



cl2 = kmeans(clust_data_ver1, 6)
cl3 = kmeans(clust_data_ver1, 7)
cl4 = kmeans(clust_data_ver1, 8)
cl5 = kmeans(clust_data_ver1, 9)
cl6 = kmeans(clust_data_ver1, 10)
plot(clust_data_ver1, col = cl2$cluster)
plot(clust_data_ver1, col = cl3$cluster)
plot(clust_data_ver1, col = cl4$cluster)
plot(clust_data_ver1, col = cl1$cluster)