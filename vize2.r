library(caret)
library(tidyverse)
library(AppliedPredictiveModeling)
library(pls)
library(elasticnet)
library(broom)
library(glmnet)
library(MASS)
library(ISLR)
library(PerformanceAnalytics)
library(funModeling)
library(Matrix)
library(readxl)
library(olsrr)
library(asympTest)
dt1 <- vize
attach(vize)
dt1
attach(dt1)
set.seed(123)
attach(dt1)
head(dt1)
#soru1)
model1 <- glm(DFREE~AGE+BECK+factor(IVHX)+NDRUGTX+RACE+TREAT+SITE,family = binomial)
summary(model1)
1-pchisq(34.48,8)
#653.73-619.25=34.48
#null deviance: 653.73
#Residual deviance: 619.25
#kikare (0.05)(8)=15.507 model anlamlıdır.
#Null deviance ve residual deviance farkının chi-square tablo değerinden büyük olması gerekmektedir. df = 8 serbestlik derecesi ve 0.05 değeri için chi-square tablo değeri: 15.507 (tablo: 
#Model anlamlıdır.
exp(model1$coefficients)
#age için yorum
#Diğer değişkenler modelde ve sabitken, yası bir yas arttıgında 12 ay boyunca uysturucudan uzak kalması
#oddsu bir yas genc olan kisinin 1.051 katıdır.

#Race için yorum
#Diğer değişkenler sabitken, ırkı beyaz olmayanların beyaz olanlara göre 12 ay boyunca uysturucudan 
#uzak kalması oddsu 1.25 kattır.


#3)AGE
#H0: ß1 = 0
#H1: ß1 != 0
#p = 5.59e-05     H0 hipotezi reddedilir. Değişken anlamlıdır.

#BECK
#H0: ß2 = 0
#H1: ß2 != 0
#p = 0.97961     H0 hipotezi reddedilemez. Değişken anlamlı değildir.

#as.factor(IVHX)2 
#H0: ß3 = 0
#H1: ß3 != 0
#p = 0.03581      H0 hipotezi reddedilir. Değişken anlamlıdır.

#as.factor(IVHX)3
#H0: ß4 = 0
#H1: ß4 != 0
#p = 0.00401     H0 hipotezi reddedilir. Değişken anlamlıdır.

#NDRUGTX
#H0: ß5 = 0
#H1: ß5 != 0
#p = 0.01642     H0 hipotezi reddedilir. Değişken anlamlıdır.

#RACE
#H0: ß6 = 0
#H1: ß6 != 0
#p = 0.31159    H0 hipotezi reddedilemez. Değişken anlamlı değildir.

#TREAT
#H0: ß7 = 0
#H1: ß7 != 0
#p = 0.02640     H0 hipotezi reddedilir. Değişken anlamlıdır.

#SITE
#H0: ß8 = 0
#H1: ß8 != 0
#p = 0.49375     H0 hipotezi reddedilemez. Değişken anlamlı değildir.



#4)
exp(confint(model1))

#yorum: Diğer değişkenler sabitken, yas 1 yıl arttıgında 12 ay boyunca uysturucudan uzak kalması
#oddsu, suanda 12 ay boyunca uysturucudan uzak kalması oddsunun %95 güvenle 1.016 ve 1.088 katı arasındadır.

#Diğer değişkenler sabitken, ırkı beyaz olmayanların beyaz olanlara göre 12 ay boyunca uysturucudan 
#uzak kalması oddsu  0.8050679  ve 1.9351600 kat arasındadır.
#BECK depresyon skoru bir birim arttığında 12 ay boyunca uyuşturucu kullanmama oddsunun artma oranı %95 güven seviyesi ile 0.9791804 1.0216018 arasındadır.


#5)
library ("MASS")
step.model<-model1 %>% stepAIC(trace=FALSE)
step.model
model2<-glm(formula = DFREE ~ AGE + factor(IVHX) + NDRUGTX + TREAT, family = binomial)
#step(glm(DFREE~AGE+BECK+factor(IVHX)+NDRUGTX+RACE+TREAT+SITE,data=dt1),direction="both")
#k=step(glm(DFREE~AGE+BECK+factor(IVHX)+NDRUGTX+RACE+TREAT+SITE,data=dt1),direction="both")
summary(model2)
# (Intercept)            AGE  factor(IVHX)2  factor(IVHX)3        NDRUGTX          TREAT  
#  2.1e-05      0.009378      -0.120841      -0.154414      -0.008505       0.081927 

model1_temiz <- glm(DFREE~AGE+factor(IVHX)+NDRUGTX+TREAT,family = binomial)

summary(model1_temiz)

shapiro.test(residuals(model1_temiz))
#H0:Artıklar normal dağılıyor.
#H1:Artıklar Normal Dağılmıyor.
#p-value < 2.2e-16 olduğu için hipotez red edilemez. Artıklar normal dağılmamaktadır.

hist(DFREE)
?var.test
?asymp.test
#asymp.test(DFREE ~TREAT , data = dt1,na.action = na.omit, parameter = "dVar")
asymp.test(DFREE ~SITE , data = dt1,na.action = na.omit,parameter="dVar")
var.test(DFREE ~SITE , data = dt1,na.action = na.omit)

#Null deviance: 653.73
#Residual deviance: 620.59
#653.73-620.59=33.14>5.9 model anlamlıdır.
1-pchisq(33.14,5) 
cutpoint <- sum(DFREE)/length(DFREE)
#5)
p.tahmin.dfree<-fitted(model1_temiz)  
p.tahmin.dfree[p.tahmin.dfree>cutpoint]=1
p.tahmin.dfree[p.tahmin.dfree<=cutpoint]=0
print(p.tahmin.dfree)
library(caret)

#6)
cutpoint <- sum(DFREE)/length(DFREE)
xtab.dfree<-table(as.factor(p.tahmin.dfree),as.factor(dt1$DFREE))
sens.dfree<-sensitivity(as.factor(p.tahmin.dfree),as.factor(DFREE),positive=levels(as.factor(DFREE))[2])

spec.dfree<-specificity(as.factor(p.tahmin.dfree),as.factor(DFREE),negative=levels(as.factor(DFREE))[1])
spec.dfree
sens.dfree
#0.6326531 yorum:duyarlılık:  gercekte 12 ay boyunca uysturucudan uzak kalması durumunda iken
#model tahminiyle de 12 ay boyunca uysturucudan uzak kalması tahmini yapmamızın olasılığı %63 dir.

# 0.6168224 yorum:secicilik: gercekte 12 ay boyunca uysturucudan uzak kalmaması durumunda iken
#model tahminiyle de 12 ay boyunca uysturucudan uzak kalmaması tahmini yapmamızın olasılığı %62 dir.


#7)
#yası 37, gecmişinde uyusturucu kullanmış(2),3 kere ilaç tedavisi almış ve treat(0) short
exp(-2.33276+0.05259*40-0.62366*1-0.06376 *20+0.45134*0)
#0.1190728'dir bu kişi için 0 degeri yani 12 ay boyunca uyusturucudan uzak kalamayacaktır yorumu yapılabilir. çünkü 0.1190728<cutpoint


newdata <- data.frame("AGE"= 55, "BECK"= 35, "IVHX"= 2, "NDRUGTX"= 1, "RACE"=1, "TREAT" = 1, "SITE" = 0, "DFREE" = 1 )
predict(model1_temiz,newdata = newdata,type ="response" )
