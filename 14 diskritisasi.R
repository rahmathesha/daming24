x <-c(15, 4, 21, 11, 16, 18, 24, 26, 28)

library(classInt)

#equal width
eqwid<-classIntervals(x, 4, style = 'equal')
eqwid$brks

x.eqwid<-cut(x, breaks=eqwid$brks, include.lowest=TRUE)
cbind(x, x.eqwid)


#equal freq
eqfreq<-classIntervals(x, 4, style = 'quantile')
eqfreq$brks

x.eqfreq<-cut(x, breaks=eqfreq$brks, include.lowest=TRUE)
cbind(x, x.eqwid, x.eqfreq)

# Ilustrasi
data <- read.csv("05 disk01.csv")
str(data)
data$class <- factor(data$class)

# Sebelum diskritisasi
model.asli <-glm(class ~ x, data=data, family="binomial")
maudiprediksi <-data.frame(data$x)
colnames(maudiprediksi) <-c("x")
prediksi.prob.asli <-predict(model.asli, newdata=maudiprediksi, 
                             type="response")
prediksi.asli <-ifelse(prediksi.prob.asli > 0.5, 1, 0)
table(data$class, prediksi.asli)
mean(data$class == prediksi.asli)

# Setelah diskritisasi
eqwid <-classIntervals(data$x, 10, style = 'equal')
x.eqwid <-cut(data$x, breaks=eqwid$brks, include.lowest=TRUE)
head(cbind(data$x, x.eqwid))

model.disk <-glm(data$class ~ factor(x.eqwid), family="binomial")
prediksi.prob.disk <-predict(model.disk, newdata= factor(x.eqwid), 
                             type="response")
prediksi.disk <-ifelse(prediksi.prob.disk> 0.5, 1, 0)
table(data$class, prediksi.disk)
mean(data$class == prediksi.disk)

table(x.eqwid, data$class)
prop.table(table(x.eqwid, data$class), margin=1)
proporsi <-prop.table(table(x.eqwid, data$class), margin=1)
barplot(t(proporsi))

#################
boxplot(dt_graph$x ~ dt_graph$class,
        col = c("coral", "skyblue"),
        xlab = "kelas nasabah",
        ylab = "x",
        las = 1)

library(ggridges)
library(ggplot2)
 
ggplot(data, aes(x = x, y = class, fill = class)) +
  geom_density_ridges() +
  theme_ridges()

ggplot(data, aes(x = x, fill = class, after_stat(count))) +
  geom_density()
