raw = read.csv("DT.csv", sep = ";")
str(raw)

library(dplyr)
ds <- raw %>% 
  mutate(across(c(Jenis.Kelamin,
                  Single,
                  Tinggal.di.Kota,
                  Perokok,
                  Budget,
                  Kesukaan,
                  Tertarik
  ), as.factor))
str(ds)


#membagi gugus data menjadi dua bagian secara acak
set.seed (1000)
acak <- sample(1:nrow(ds), .80*nrow(ds))
data.training <- data.frame(ds[,-1])[acak,]
data.testing <- data.frame(ds[,-1])[-acak,]


library(rpart)
library(rpart.plot)

pohon <- rpart(Tertarik~., data= data.training, 
               method="class", 
               control = rpart.control(minsplit= 36))
rpart.plot(pohon,extra= 3, digits = 3, type = 4) #, 

printcp(pohon) #cp optimum

prediksi<- predict(pohon, data.testing, type = 'prob')
pred.response <- colnames(prediksi)[max.col(prediksi, ties.method = c("random"))] 
cm <- table(data.testing$Tertarik, pred.response)

accs  <- (cm[1] + cm[4])/(sum(cm))
sensi <- (cm[4])/(cm[2]+cm[4])
spesi <- (cm[1])/(cm[1]+cm[3])
print(list(accs, sensi, spesi))


# minsplit dengan validasi silang
set.seed(100)
akurasi.semua = NULL

for (ulangan in 1:30){
  acak <- sample(1:nrow(ds), .75*nrow(ds))
  data.training <- data.frame(ds[,-1])[acak,]
  data.testing <- data.frame(ds[,-1])[-acak,]
  
  for (k in 1:40){
    pohon1 <- rpart(Tertarik~., 
                    data= data.training, method="class", 
                    control = rpart.control(minsplit= k, cp = 0))
    
    pel.pred =  predict(pohon1, data.testing)
    pred = ifelse(pel.pred > 0.5, '1', '0')[,2]
    akur = mean(pred == data.testing$Tertarik)
    akurasi.semua = rbind(akurasi.semua, c(k,akur))
  }
  
}
mean.akurasi = tapply(akurasi.semua[,2], akurasi.semua[,1], mean)

plot(names(mean.akurasi), mean.akurasi, type = 'b', pch = 19, bty = "l", las =1, 
     xlab = "minsplit", ylab = "rata-rata akurasi testing")
########


















