#Import data white_wine2.csv
ww = read.csv('white_wine2.csv')
dim(ww)

#02 membuat kelas baru:
#skor quality lebih dari 6 dikelaskan menjadi kelas=1, selainnya dikelaskan menjadi kelas=0
ww$quality <- as.factor(ifelse(ww$quality>6,1,0))

#03 membagi dataset menjadi dua bagian secara acak
#pemilihan amatan secara acak sbg data training
set.seed (1001)
acak <- sample(1:nrow(ww), 0.8*nrow(ww))  

#data training
data.training <- data.frame(ww$quality, ww$alcohol, ww$density)[acak,]
head(data.training)
colnames(data.training) <- c("quality","alcohol","density")
head(data.training)


#data testing
data.testing <- data.frame(ww$quality, ww$alcohol, ww$density)[-acak,]
colnames(data.testing) <- c("quality", "alcohol","density")
head(data.testing)

# Uji ragam-peragam
bartlett.test(data.training[,-1], data.training[,1], data.training)

#Linear Discriminant
library(MASS)
lda.fit = lda(quality~., data =  data.training)
lda.pred = predict(lda.fit, data.testing)$class

#Ukuran kebaikan prediksi
cm <- table(data.testing$quality, lda.pred)

accs  <- (cm[1] + cm[4])/(sum(cm))
sensi <- (cm[4])/(cm[2]+cm[4])
spesi <- (cm[1])/(cm[1]+cm[3])
print(list(accs, sensi, spesi))


#Quadratic Disriminant

qda.fit = qda(quality~., data =  data.training)
qda.pred = predict(qda.fit, data.testing)$class

#Ukuran kebaikan prediksi
cm1 <- table(data.testing$quality, qda.pred)

acc  <- (cm1[1] + cm1[4])/(sum(cm1))
sens <- (cm1[4])/(cm1[2]+cm1[4])
spes <- (cm1[1])/(cm1[1]+cm1[3])
print(list(acc, sens, spes))



