raw <- read.csv("DT.csv", sep = ';')
str(raw)

library(dplyr)

raw <- raw %>% 
  mutate(across(c(Jenis.Kelamin,
                  Single,
                  Tinggal.di.Kota,
                  Perokok,
                  Budget,
                  Kesukaan,
                  Tertarik), as.factor))
str(raw)

# Variabel dummy
library(fastDummies)
dummi <- raw %>% dummy_cols(c("Jenis.Kelamin", 
                              "Single",
                              "Tinggal.di.Kota",
                              "Perokok",
                              "Budget",
                              "Kesukaan", auto_values= T)) 
names(dummi)

dummi_fix <- dummi %>% 
  select(-c("Jenis.Kelamin", "Jenis.Kelamin_0",
            "Single", "Single_0",
            "Tinggal.di.Kota", "Tinggal.di.Kota_0",
            "Perokok", "Perokok_0",
            "Budget", "Budget_low",
            "Kesukaan", "Kesukaan_Lainnya"))

names(dummi_fix)
head(dummi_fix)

set.seed (1001)
acak <- sample(1:nrow(dummi_fix), 0.8*nrow(dummi_fix)) #80:20


train <- dummi_fix[acak,]
test <- dummi_fix[-acak,]

library(e1071)

# Kernel linear
sv1 = svm(Tertarik ~., data= train, kernel = "linear", 
          scale = F)
summary(sv1)
# plot(sv1, train)

set.seed(1001)
tune.out = tune(svm, Tertarik~.,data= train,
                kernel = "linear", 
                ranges =list(cost=c(0.001 , 0.01, 0.1, 1,5,10,100)))

summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)

pred1 = predict(bestmod, test)

#Ukuran kebaikan prediksi
cm <- table(test$Tertarik, pred1)

accs  <- (cm[1] + cm[4])/(sum(cm))
sensi <- (cm[4])/(cm[2]+cm[4])
spesi <- (cm[1])/(cm[1]+cm[3])
print(list(accs, sensi, spesi))

