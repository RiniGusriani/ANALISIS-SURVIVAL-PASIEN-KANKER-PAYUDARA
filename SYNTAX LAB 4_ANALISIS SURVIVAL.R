############## REGRESI COX PROPORTIONAL HAZARD ################

#Panggil data
library(survival)
data(package="survival") #mencari dataset dalam R
datalap.rini=gbsg   #panggil data
View(gbsg) #melihat data
attach(gbsg)
?gbsg 
summary(gbsg) # minimum,quartile dan lain-lain

#model cox
# cara tulis modelnya 
response= Surv(rfstime,status)
model.coxph = coxph(response ~ pid+age+meno+size+grade+nodes+pgr+er+hormon, data=gbsg)
model.coxph

#Uji GOF
residual= cox.zph(model.coxph)
residual
