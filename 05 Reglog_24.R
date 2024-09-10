#Import data white_wine2.csv
ww = read.csv('white_wine2.csv')
str(ww)

#02 membuat kelas baru:
#skor quality lebih dari 6 dikelaskan menjadi kelas=1, selainnya dikelaskan menjadi kelas=0
ww$quality <- as.factor(ifelse(ww$quality>6,1,0))

#03 membagi dataset menjadi dua bagian secara acak
#pemilihan amatan secara acak sbg data training
set.seed (1001)
acak <- sample(1:nrow(ww), 0.8*nrow(ww))  

#data training
data.training <- data.frame(ww$alcohol, 
                            ww$density,
                            ww$quality)[acak,]
head(data.training)
colnames(data.training) <- c("alcohol",
                             "density",
                             "quality")

#data testing
data.testing <- data.frame(ww$alcohol, 
                            ww$density,
                            ww$quality)[-acak,]
head(data.testing)
colnames(data.testing) <- c("alcohol",
                             "density",
                             "quality")

reg1 = glm(quality ~ alcohol + density, 
           data = data.training, family = 'binomial' )
summary(reg1)

pred_1 = predict(reg1, subset(data.testing, 
                              select=c(1:2)), 
                 type = 'response')

hasil = (cbind(data.testing[,3], pred_1, 
               ifelse(pred_1 >= 0.5, '1', '0')))
head(hasil)

cm = table(data.testing[,3], hasil[,3])
akurasi = (cm[1] + cm[4])/sum(cm)
sens = cm[4] / (cm[2] + cm[4])
spes = cm[1] / (cm[1] + cm[3])
print(c(akurasi, sens, spes))
