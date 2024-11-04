library(tidyverse)
library(ROSE)
library(e1071)
library(lattice)
library(adabag)
library(caret)

ds = read.csv("DT.csv", sep = ';')
ds = as_tibble(ds)
dim(ds)
str(ds)

# penyesuaian variabel
ds$Jenis.Kelamin = as.factor(ds$Jenis.Kelamin)
ds$Single = as.factor(ds$Single)
ds$Tinggal.di.Kota = as.factor(ds$Tinggal.di.Kota)
ds$Perokok = as.factor(ds$Perokok)
ds$Budget = as.factor(ds$Budget)
ds$Kesukaan = as.factor(ds$Kesukaan)
ds$Tertarik = as.factor(ds$Tertarik)

str(ds)

# Persiapan Data

## Missing values
colSums(is.na(ds))

## Proporsi kelas target (Kelas tak seimbang)
ty = data.frame(round(prop.table(table(ds$Tertarik)),2)) 
colnames(ty) = c("Tertarik", "proporsi")

warnacu = c("blue", "red")
ggplot(ty, aes(y= proporsi, x= Tertarik)) + 
  geom_bar(stat="identity") + scale_fill_brewer(palette = 'Set2') +
  theme_classic()
  #scale_fill_manual(values = warnacu)

ty %>% ggplot(aes(x= Tertarik, y= proporsi)) + 
  geom_col(aes(fill= Tertarik),colour="white", position="dodge") + 
  geom_text(aes(label= format(proporsi, digits = 2)), size = 4,
            position = position_dodge(width=0.1), vjust= -0.5, angle= 0,hjust= 0.3) +
  theme_classic()+ 
  scale_fill_brewer(palette = "Set2") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ xlab("Tertarik") + 
  scale_y_continuous(expand = c(0,0),limit=c(0,1)) + 
  theme(legend.justification=c(1,1), legend.position=c(1,1))


## Resampling & SMOTE

### Partisi data set

set.seed (1001)
acak <- sample(1:nrow(ds), .75*nrow(ds))
train <- data.frame(ds)[acak,]
test <- data.frame(ds)[-acak,]

prop.train = data.frame(round(prop.table(table(train$Tertarik)),2)) 
colnames(prop.train) = c("Tertarik", "proporsi")

prop.train %>% ggplot(aes(x= Tertarik, y= proporsi)) + 
  geom_col(aes(fill= Tertarik),colour="white", position="dodge") + 
  geom_text(aes(label= format(proporsi, digits = 2)), size = 4,
            position = position_dodge(width=0.1), vjust= -0.5, angle= 0,hjust= 0.3) +
  theme_classic()+ 
  scale_fill_brewer(palette = "Set2") +
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+ xlab("Tertarik") + 
  scale_y_continuous(expand = c(0,0),limit=c(0,1)) + 
  theme(legend.justification=c(1,1), legend.position=c(1,1))



### Oversampling (2 kali kelas mayor di data train)
over <- ovun.sample(Tertarik~., train, method = "over", N = 1112)$data
summary(over)

o_boost <- boosting(Tertarik~., data = over, mfinal = 5,
                    control = rpart.control(maxdepth = 1), 
                    coeflearn = 'Freund')

o_perf <- confusionMatrix(as.factor(predict(o_boost, test)$class), test$Tertarik,
                          positive = "1")
o_perf

### Undersampling (2 kali kelas minor di data train)
under <- ovun.sample(Tertarik~., train, method = "under", N = 514)$data
summary(under)

u_boost <- boosting(Tertarik~., data = under, mfinal = 5,
                    control = rpart.control(maxdepth = 1), 
                    coeflearn = 'Freund')

pred_u_boost <- as.factor(predict(u_boost, test)$class)
u_perf <- confusionMatrix(pred_u_boost, test$Tertarik, positive = "1")
u_perf

### Under-Over Sampling
both <- ovun.sample(Tertarik~., train, method = "both",
                    p = 0.5,
                    seed = 1001,
                    N = 813)$data
summary(both)

b_boost <- boosting(Tertarik~., data = both, mfinal = 5,
                    control = rpart.control(maxdepth = 1), 
                    coeflearn = 'Freund')

b_perf <- confusionMatrix(as.factor(predict(b_boost, test)$class), test$Tertarik, 
                          positive = "1")
b_perf


### SMOTE
rose <- ROSE(Tertarik~., data = train, 
             N = 1500, seed = 1001)$data
summary(rose)

r_boost <- boosting(Tertarik~., data = rose, mfinal = 5,
                    control = rpart.control(maxdepth = 1), 
                    coeflearn = 'Freund')

r_perf <- confusionMatrix(as.factor(predict(r_boost, test)$class), test$Tertarik, 
                          positive = "1")
r_perf

# Ukuran 
confusionMatrix(data = as.factor(predict(r_boost, test)$class), 
                reference = test$Tertarik, 
                mode = "prec_recall")

roc.curve(test$Tertarik, as.factor(predict(b_boost, test)$class),
          xlab = "1-Spesifitas",
          ylab = "Sensitivitas")


