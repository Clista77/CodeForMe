library(survival)
library(survminer)
data(veteran)
head(veteran)
km=survfit(Surv(time,status)~trt,data=veteran)
summary(km)
ggsurvplot(fit=km,risk.table=TRUE,cumevents=TRUE,cumcensor=TRUE,
           tables.height=0.2)
wb=survreg(Surv(time,status)~trt,data=veteran)
cx=coxph(Surv(time,status)~trt,data=veteran)
ggforest(cx)

library(TH.data)
library(tidyverse)
data(GBSG2)
head(GBSG2)

#Kaplan-Meier
km=survfit(Surv(time,cens)~1,data=GBSG2)
ggsurvplot(fit=km,risk.table=TRUE,cumevents=TRUE,cumcensor=TRUE,
           tables.height=0.2)

wb=survreg(Surv(time,cens)~1,data=GBSG2)
surv=seq(0.99,0.01,by=-0.01)
time=predict(wb,p=1-surv,type='quantile',newdata=data.frame(1))
wb_df=data.frame(time=time,surv=surv)
ggsurvplot_df(fit=wb_df,surv.geom=geom_line)

#Weibull Distribution
wb=survreg(Surv(time,cens)~horTh+tsize,data=GBSG2)
coef(wb)
newdata=expand.grid(horTh=levels(GBSG2$horTh),
                    tsize = quantile(GBSG2$tsize)[c(2,3,4)])
newdata
surv=seq(0.99,0.01,by=-0.01)
time=predict(wb,p=1-surv,type="quantile",newdata=newdata)
wb_bind=cbind(newdata,time)
wb_df=wb_bind%>%gather(key=id,value=time,-c(horTh,tsize))
wb_df$surv=surv[as.numeric(wb_df$id)]
ggsurvplot_df(fit=wb_df,surv.geom=geom_line,
              linetype='horTh',color='tsize')

#Cox Proportional Hazard
cx=coxph(Surv(time,cens)~horTh+tsize,data=GBSG2)
coef(cx)
cx_fit=survfit(cx,data=GBSG2,newdata=newdata)
head(cx_fit$surv)
head(cx_fit$time)
cx_fit_summary=surv_summary(cx_fit)
head(cx_fit_summary)
cx_fit_bind=cbind(cx_fit_summary,
                  newdata[as.character(cx_fit_summary$strata),])
ggsurvplot_df(cx_fit_bind,linetype='horTh',color='tsize',censor=F)

#Kidney Data
data(kidney)
head(kidney)
library(survival)
kidney.cox <- coxph(Surv(time, status)~sex+disease, method="breslow", data=kidney)
summary(kidney.cox)
kidney.coxzph <- cox.zph(kidney.cox)
kidney.coxzph
par(mfrow=c(2, 2))
plot(kidney.coxzph, df=2)
kidney.fit <- survfit(kidney.cox)
plot(kidney.fit)
kidney.fit <- survfit(Surv(time, status)~disease, data=kidney)
plot(kidney.fit,xlab='Time',ylab='Survival Rate',col=1:4)
kidney.lr.sex <- survdiff(Surv(time, status)~sex, 
                          subset=(sex %in% c("1","2")),data=kidney) 
kidney.lr.sex




