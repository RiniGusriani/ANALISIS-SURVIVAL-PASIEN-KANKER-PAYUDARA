library(survival)

data_Rini=pbcseq
View(pbcseq)
data_Rini=survival::pbcseq
attach(pbcseq)
summary(pbcseq)
summary(pbcseq$trt,pbcseq$edema, pbcseq$bili, pbcseq$ast, pbcseq$protime, pbcseq$futime, pbcseq$status)
?pbcseq

#membuat Model Cox PH
response= Surv(futime, status)
model.coxph = coxph(response~trt+edema+bili+ast+protime, data=data_Rini)
model.coxph

#UJI GOF
residual = cox.zph(model.coxph)
residual

#Membuat model cox expended
pbcseq.cp = survSplit(Surv(pbcseq$futime, pbcseq$status)~. , data = pbcseq , cut = 2963 , event = "status", start = "start", end = "futime")
pbcseq.cp
pbcseq.cp$hv = pbcseq.cp$bili*(pbcseq.cp$start <2963)

new_response = Surv(pbcseq.cp$start, pbcseq.cp$futime, pbcseq.cp$status)

extended.cox = coxph(new_response ~ trt+edema+bili+ast+protime+hv, 
                     data = pbcseq.cp)
extended.cox

residual.extend = cox.zph(extended.cox)
residual.extend
