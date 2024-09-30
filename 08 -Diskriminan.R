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
dummi_fix <- dummi%>% 
  select(-c("Jenis.Kelamin", "Jenis.Kelamin_0",
            "Single", "Single_0",
            "Tinggal.di.Kota", "Tinggal.di.Kota_0",
            "Perokok", "Perokok_0",
            "Budget", "Budget_low",
            "Kesukaan", "Kesukaan_Lainnya",
            ))

dim(dummi_fix)
# head(raw)

set.seed (1001)
acak <- sample(1:nrow(dummi_fix), 0.75*nrow(dummi_fix)) #80:20


train <- dummi_fix[acak,]
test <- dummi_fix[-acak,]

# Uji ragam-peragam
library(MVTests)
BoxM(data= train[,-2], group= train[,2]) #kolom Tertarik

#Linear Discriminant
library(MASS)
qda.fit = qda(Tertarik~., data =  train)
print(qda.fit)
qda.pred = predict(qda.fit, test)$class

cmq = table(qda.pred, test$Tertarik)
akurasi_q = (cmq[1] + cmq[4])/sum(cmq)
sens_q = cmq[4] / (cmq[2] + cmq[4])
spes_q = cmq[1] / (cmq[1] + cmq[3])
print(c(akurasi_q, sens_q, spes_q))


#Linear Discriminant
lda.fit = lda(Tertarik~., data =  train)
lda.pred = predict(lda.fit, test)$class


cm = table(lda.pred, test$Tertarik)
akurasi = (cm[1] + cm[4])/sum(cm)
sens = cm[4] / (cm[2] + cm[4])
spes = cm[1] / (cm[1] + cm[3])
print(c(akurasi, sens, spes))

