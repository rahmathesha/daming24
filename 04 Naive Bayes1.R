raw <- read.csv("DT.csv", sep = ';')
str(raw)

raw$Jenis.Kelamin <- as.factor(raw$Jenis.Kelamin)
raw$Single <- as.factor(raw$Single)
raw$Tinggal.di.Kota <- as.factor(raw$Tinggal.di.Kota)
raw$Perokok <- as.factor(raw$Perokok)
raw$Budget <- as.factor(raw$Budget)
raw$Kesukaan <- as.factor(raw$Kesukaan)
raw$Tertarik <- as.factor(raw$Tertarik)

set.seed (1001)
acak <- sample(1:nrow(raw), 0.8*nrow(raw)) #80:20


train <- raw[acak,]
test <- raw[-acak,]

library(e1071)

nb.fit = naiveBayes(Tertarik~., data =  train)
nb.pred = predict(nb.fit, test)


cm = table(nb.pred, test[,1])
akurasi = (cm[1] + cm[4])/sum(cm)
sens = cm[4] / (cm[2] + cm[4])
spes = cm[1] / (cm[1] + cm[3])
print(c(akurasi, sens, spes))
