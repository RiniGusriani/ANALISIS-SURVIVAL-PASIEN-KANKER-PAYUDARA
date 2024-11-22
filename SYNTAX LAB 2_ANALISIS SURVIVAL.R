#LAPORAN 2 ANALISIS SURVIVAL#

#Input data
data.Lab2 <- read.delim("clipboard")
data.Lab2
attach(data.Lab2)

#Library
library(survival)
library(ggplot2)
library(dplyr)
library(ggfortify)

#Model
km.model1<-survfit(Surv(Survival.Time, Status)~Jenis.Kelamin,
                   data=data.Lab2,type="kaplan-meier")
km.model1

#Plot
autoplot(km.model1,main="KM CURVE BERDASARKAN JENIS KELAMIN")
