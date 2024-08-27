ilustrasikm <- read.csv ("F:/UNM/Stats/04 Data Mining/Kuliah/[labs] Data Mining/ilustrasikm.csv")
str(ilustrasikm) 

library(cluster)

cluster <- kmeans(ilustrasikm, 3)
plot(ilustrasikm[,1], ilustrasikm[,2], col=cluster$cluster, xlab = 'X1', ylab = 'X2', las = 1)
points(cluster$centers, pch=9)

banyak.klp <- 2:10 #Criteria Pembuatan Kelompok
banyak.klp
kriteria <- NULL
for (i in banyak.klp) {set.seed(1000)
  kriteria[i-1] <- kmeans(ilustrasikm, i)$tot.withins / kmeans(ilustrasikm, i)$betweenss
}
elbow <- plot (banyak.klp, kriteria, type = "b", las=1) #Elbow
kriteria

str(cluster)

d = dist(ilustrasikm)
siluet_km = NULL
for (k in 2:5) {
  labels = kmeans(ilustrasikm, centers = k, iter.max = 100)$cluster
  sils = silhouette(labels, d)
  siluet_km[k] = mean(sils[,3])
}
plot(siluet_km, type = "b", xlab = "Banyaknya gerombol", cex = 1.5, pch = 19, bty = "l", las = 1)


#



1. Elmira (seed d elbow atau di kmeansnya)
2. Khairun Nisa, SH (nice, pembakuan data)

