####################REGRESI COX PROPORTIONAL HAZARD#############################
#Panggil data
library(survival)
data(package="survival") #mencari dataset dalam R
dataUTS.rini=colon   #panggil data
View(colon) #melihat data
attach(colon)
?colon 
summary(colon) # minimum,quartile dan lain-lain

#model cox
# cara tulis modelnya 
response= Surv(time,status)
model.coxph = coxph(response ~ rx+sex+age+obstruct+perfor+adhere+nodes+differ
                    +extent+surg+node4+etype, data=colon)
model.coxph

#Uji GOF
residual= cox.zph(model.coxph)
residual

###############################KAPLA-MEIER######################################
#Library
library(ggplot2)
library(dplyr)
library(ggfortify)

#Model 1
km.model1<-survfit(Surv(time, status)~sex,
                   data=dataUTS.rini,type="kaplan-meier")
km.model1

#Plot 1
autoplot(km.model1,main="KM CURVE BERDASARKAN JENIS KELAMIN")

#Model 2
km.model2<-survfit(Surv(time, status)~surg,
                   data=dataUTS.rini,type="kaplan-meier")
km.model2

#Plot 2
autoplot(km.model2,main="KM CURVE BERDASARKAN WAKTU PEMBEDAHAN")

#Model 3
km.model3<-survfit(Surv(time, status)~etype,
                   data=dataUTS.rini,type="kaplan-meier")
km.model3

#Plot 3
autoplot(km.model3,main="KM CURVE BERDASARKAN JENIS KEJADIAN")

##############################UJI LOG-RANK######################################
mydata=Surv(dataUTS.rini$time,dataUTS.rini$status)
mydata

#log rank
log.rank = survdiff(mydata~dataUTS.rini$etype)
log.rank

mydata=Surv(dataUTS.rini$time,dataUTS.rini$status)
mydata

#log rank
log.rank = survdiff(mydata~dataUTS.rini$surg)
log.rank
