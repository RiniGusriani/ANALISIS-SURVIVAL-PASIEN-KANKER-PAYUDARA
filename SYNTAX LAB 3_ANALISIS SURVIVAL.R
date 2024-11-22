####UJI LOG RANK####

library(survival)
data.rini=read.delim("clipboard")
data.rini
attach(data.rini)

mydata=Surv(data.rini$Time..x.,data.rini$Event)
mydata

#log rank
log.rank = survdiff(mydata~data.rini$Grup)
log.rank
