#install.packages("survival")
library(survival)
data(package="survival") #mencari dataset dalam R
datasurv.rini=survival::gbsg 
attach(gbsg)

View(gbsg) 
summary(gbsg)
#membuat Model Cox PH
response= Surv(rfstime, status)
model.coxph = coxph(response~pid+age+meno+size+grade+nodes+pgr+er+hormon, data=datasurv.rini)
model.coxph

#Hipotesis Serempak
#H0 : variabel bebas secara bersama-sama tidak berpengaruh terhadap variabel terikat
#H1 : setidaknya ada satu variabel bebas yang berpengaruh terhadap variabel terikat

#Hipoteisi partial
#H0: variabel grade tidak berpengaruh terhadap waktu kelangsungan hidup pasien aknker usus besar
#H1 : variabel grade berpengaruh terhadap waktu kelangsungan hidup pasien aknker usus besar

#UJI ASUMSI
residual=cox.zph(model.coxph)
residual
#H0 : variabel memenuhi asumsi Proportional Hazard
#H1 : variabel tidak memenuhi asumsi Proportional Hazard

#UNTUK MEMENUHI ASUMSI HAZARD SALAH SATUNYA BISA DIGUNAKAN MODEL STRATIFIED COX 

#PERBANDINGAN DENGAN STRATIFIED COX
response=Surv(rfstime, status)
cox.strata= coxph(response~pid+size+nodes+hormon+strata(age+meno+grade+pgr+er),data=rats)
cox.strata

#UJI ASUMSI PH
residual2= cox.zph(cox.strata)
residual2

