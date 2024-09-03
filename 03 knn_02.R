#Import data white_wine2.csv
ww = read.csv('white_wine2.csv')
dim(ww)

#02 membuat kelas baru:
    #skor quality lebih dari 6 dikelaskan menjadi kelas=1, 
    #selainnya dikelaskan menjadi kelas=0
quality2 <- as.factor(ifelse(ww$quality>6,1,0))

#03 membagi dataset menjadi dua bagian secara acak
    #pemilihan amatan secara acak sbg data training
set.seed (1001)
acak <- sample(1:nrow(ww), 3000)  

    #data training: fitur
data.training <- data.frame(ww$alcohol, ww$density)[acak,]
head(data.training,2)
colnames(data.training) <- c("alcohol","density")

kelas.training <- quality2[acak]
head(kelas.training)

    #data testing
data.testing <- data.frame(ww$alcohol, ww$density)[-acak,]
colnames(data.testing) <- c("alcohol","density")
kelas.testing <- quality2[-acak]
head(data.testing,2)
head(kelas.testing)

#04 membakukan data:
    #data training
min <- matrix(apply(data.training,2,min), nrow=nrow(data.training),
              ncol = 2, byrow=TRUE)
max <- matrix(apply(data.training,2,max), nrow=nrow(data.training),
              ncol = 2, byrow=TRUE)
data.baku.training <- (data.training - min)/(max - min)

    #data testing
min <- matrix(apply(data.training,2,min), nrow=nrow(data.testing),
              ncol = 2, byrow=TRUE)
max <- matrix(apply(data.training,2,max), nrow=nrow(data.testing),
              ncol = 2, byrow=TRUE)
data.baku.testing <- (data.testing - min)/(max - min)

#05 mengaplikasikan algoritma k-NN
    #Pemilihan nilai k optimum
    #k = 1-50
library(class)
set.seed(1001)

akurasi <- NULL
a1 <- seq(1, 50, by=1)
for (k in a1){
  prediksi <- knn(data.baku.training, data.baku.testing, kelas.training, k = k)
  akurasi[k]= mean(ifelse(prediksi==kelas.testing,1,0))*100
}
matrix(akurasi, 50,1)

#06 Prediksi data testing sebagai validasi model
pred_kNN <- knn(data.baku.training, data.baku.testing, kelas.training, k = 11)

#07 mengevaluasi hasil prediksi
cm = table(kelas.testing, pred_kNN)

akurasi = (cm[1]+cm[4])/sum(cm)
akurasi*100

sens = cm[4]/(cm[2]+cm[4])
sens*100

spes = cm[1]/(cm[1]+cm[3])
spes*100
