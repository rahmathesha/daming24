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
names(ds)


#membagi gugus data menjadi dua bagian secara acak
set.seed (1000)
acak <- sample(1:nrow(ds), .80*nrow(ds))
train <- data.frame(ds)[acak,]
test <- data.frame(ds)[-acak,]

#### Bagging
library(ipred)
set.seed(1001)
bag1 = bagging(Tertarik ~ ., data = train)

pred_bag1 = predict(bag1,test[,-8])

guide <- table(test$Tertarik, pred_bag1)

akurasi <- (guide[1]+guide[4])/(sum(guide))
sensitifitas <- (guide[4])/(guide[2]+guide[4])
spesifisitas <- (guide[1])/(guide[1]+guide[3])
print(list(akurasi, sensitifitas, spesifisitas))


# Pemodelan RF
library(randomForest)
set.seed(1001)
rf1 = randomForest(Tertarik ~., data = train, 
                   ntree = 500, mtry = 3, 
                   nodesize = 21, 
                   importance = TRUE)
print(rf1)

# variabel yg penting
importance(rf1)
varImpPlot(rf1, type = 1)

pred_rf1 = predict(rf1, test[,-8])

cm <- table(test$Tertarik, pred_rf1)

accs  <- (cm[1] + cm[4])/(sum(cm))
sensi <- (cm[4])/(cm[2]+cm[4])
spesi <- (cm[1])/(cm[1]+cm[3])
print(list(accs, sensi, spesi))
