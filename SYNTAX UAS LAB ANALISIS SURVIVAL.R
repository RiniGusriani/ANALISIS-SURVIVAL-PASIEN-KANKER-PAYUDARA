############## UAS PRAKTIKUM ANALISIS SURVIVAL ################

#Panggil data
library(survival)
data(package="survival") #mencari dataset dalam R
dataUAS.rini=lung   #panggil data
View(lung) #melihat data
attach(lung)
?lung
summary(lung) # minimum,quartile dan lain-lain

#model cox
#RINI
response= Surv(time,status)
model.coxph = coxph(response ~ inst+age+sex+ph.ecog+ph.karno+pat.karno+meal.cal+wt.loss, data=lung)
model.coxph

#UJI ASUMSI
#RINI
residual=cox.zph(model.coxph)
residual

#PERBANDINGAN DENGAN STRATIFIED COX
#RINI
response=Surv(time, status)
cox.strata= coxph(response~inst+age+sex+ph.ecog+pat.karno+wt.loss+strata(ph.karno+meal.cal),data=lung)
cox.strata

#UJI ASUMSI PH
#RINI
residual2= cox.zph(cox.strata)
residual2

#Membuat model cox expended
#RINI
lung.cp = survSplit(Surv(lung$time, lung$status)~. , data = lung , cut = 255.5 , event = "status", start = "start", end = "time")
lung.cp
lung.cp$hv = lung.cp$ph.karno*(lung.cp$start <255.5)

new_response = Surv(lung.cp$start, lung.cp$time, lung.cp$status)

extended.cox = coxph(new_response ~ inst+age+sex+ph.ecog+pat.karno+wt.loss+ph.karno+hv, 
                     data = lung.cp, method = "breslow")
extended.cox

residual.extend = cox.zph(extended.cox)
residual.extend

#Membuat model cox expended
#RINI
lung.cp = survSplit(Surv(lung$time, lung$status)~. , data = lung , cut = 255.5 , event = "status", start = "start", end = "time")
lung.cp
lung.cp$hv = lung.cp$ph.karno*(lung.cp$start <255.5)

new_response = Surv(lung.cp$start, lung.cp$time, lung.cp$status)

extended.cox = coxph(new_response ~ sex+ph.ecog+ph.karno+hv, 
                     data = lung.cp, method = "breslow")
extended.cox
