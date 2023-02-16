
library(ggplot2)
require(car)
library(alr4)
library(boot)

attach(UN11)
x=UN11$fertility
y=UN11$lifeExpF


bootLS<-function(x,y,conf=0.95,B=1000){
  fit=lm(y~x)
  beta0=fit$coefficients[1]
  beta1=fit$coefficients[2]
  sebeta0=summary(fit)$coefficients[,2][1]
  sebeta1=summary(fit)$coefficients[,2][2]
  
  N=length(x)
  beta0_boot = rep(NA,N)
  beta1_boot = rep(NA,N)
  t0_boot = rep(NA,N)
  t1_boot = rep(NA,N)
  
  set.seed(241)
  for (i in 1:B){
    indices = sample(1:N,N,replace=TRUE)
    x_boot = x[indices]
    y_boot = y[indices]
    fit_boot=lm(y_boot~x_boot)
    beta0_boot[i] = fit_boot$coefficients[1]
    beta1_boot[i] = fit_boot$coefficients[2]
    sebeta0_boot = summary(fit_boot)$coefficients[,2][1]
    sebeta1_boot = summary(fit_boot)$coefficients[,2][2]
    t0_boot[i]=(beta0_boot[i]-beta0)/(sebeta0_boot)
    t1_boot[i]=(beta1_boot[i]-beta1)/(sebeta1_boot)
  }
  c1=(1-conf)*100/2
  c2=(100-(c1))
  boot_int = matrix(c(beta0 + quantile(t0_boot,c((1-conf)/2,(1+conf)/2))*sebeta0),ncol = 2)
  colnames(boot_int) = c(paste(c1,"%"),paste(c2,"%"))
  boot_slp = matrix(c(beta1 + quantile(t1_boot,c((1-conf)/2,(1+conf)/2))*sebeta1),ncol = 2)
  colnames(boot_slp) = c(paste(c1,"%"),paste(c2,"%"))
  return(c(boot_int,boot_slp))
  
  
}


op<-bootLS(x,y)
intercept=op[1:2]
slope=op[3:4]
conf=0.95
c1=(1-conf)*100/2
c2=(100-(c1))
print("Intercept and slope values for studentized bootstrap confidence interval:")
print(paste(c1,"%",c2,"%"))
print(intercept)
print(paste(c1,"%",c2,"%"))
print(slope)

fit=lm(y~x)
beta0=fit$coefficients[1]
beta1=fit$coefficients[2]
sebeta0=summary(fit)$coefficients[,2][1]
sebeta1=summary(fit)$coefficients[,2][2]
confint(fit)[1,]
confint(fit)[2,]
