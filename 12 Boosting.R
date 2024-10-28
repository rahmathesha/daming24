library(adabag)

raw = read.csv("DT.csv", sep = ';')
summary(raw)
str(raw)

# penyesuaian variabel
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

# Pemisahan data training dan testing
set.seed (1001)
acak <- sample(1:nrow(ds), .75*nrow(ds))
train <- data.frame(ds)[acak,]
test <- data.frame(ds)[-acak,]

#AdaBoost
set.seed(1001)
boost1 = boosting(Tertarik~., data = train, mfinal = 5,
                  control = rpart.control(maxdepth = 1), 
                  coeflearn = 'Freund')
boost1$trees[3]

pred_boost1 = predict(boost1, test)$class

guide <- table(test$Tertarik, pred_boost1)

#Perbaikan dari minggu sebelumnya
acc <- (guide[1]+guide[4])/(sum(guide))
sens <- (guide[4])/(guide[2]+guide[4])
spe <- (guide[1])/(guide[1]+guide[3])
print(list(acc, sens, spe))

#####################











