
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
#################################

summary()

################################

sebeta0=summary(fit)$coefficients[,2][1]
sebeta1=summary(fit)$coefficients[,2][2]
confint(fit)[1,]
confint(fit)[2,]


##############
sample_size = c(50,100,500, 1000)
conf_level = c(0.8,0.9, 0.95, 0.99)
N = 1000


N = 100
intercept = numeric(N)
slope = numeric(N)
n = 500                 # remember  to  modify the sample size to be {50, 100, 200, 500}
set.seed(2022)
x = runif(n, -1, 1)
err = rnorm(n, 0, 0.5)      
err = rexp(n, rate = 0.5) 
y = 1 + 2 * x + err





#########################################################

fit = lm(y ~ x)
interval= bootLS(x,y, B=n)
intercept[j] = interval[4]-interval[3]
slope[j] = interval[1]-interval[2]


######################################################







for (j in 1:N){
  fit = lm(y ~ x)
  interval= bootLS(x,y, B=n)
  intercept[j] = interval[4]-interval[3]
  slope[j] = interval[1]-interval[2]
}

plot(intercept)

##############################################
sample_size = c(50)                                   #c(50,100,500)
conf_level = c(0.8,0.9, 0.95, 0.99)
N_sim = 100

n = 50
x = runif(n, -1, 1)
#n_err = rnorm(n, 0, 0.5)
e_err = rexp(n, rate = 0.5)
#e_err = e_err - mean(e_err)
y = 1 + 2 * x + n_err

intercept_len_boot = rep(NA,4)
slope_len_boot = rep(NA,4)
intercept_len_classic = rep(NA,4)
slope_len_classic = rep(NA,4)
intercept_per_sim = rep(NA,N_sim)
slope_per_sim = rep(NA,N_sim)



for (i in 1:4){
  conf = conf_level[i]
  print(conf)
  fit = lm(y ~ x)
  
  for (j in 1:N_sim){
    interval= bootLS(x,y,conf = conf, B=n)
    intercept_per_sim[j] = interval[4]-interval[3]
    slope_per_sim[j] = interval[2]-interval[1]  
  }
  intercept_len_boot[i] = mean(intercept_per_sim)
  slope_len_boot[i] = mean(slope_per_sim)
  intercept_len_classic[i] = confint(fit, level = conf)[1,][2]-confint(fit, level = 0.9)[1,][1]
  slope_len_classic[i] = confint(fit, level = conf)[2,][2]-confint(fit, level = 0.9)[2,][1]  
} 


print(intercept_len_boot)
df_1 <- data.frame(x = c(0.8,0.9,0.95,0.99), y1 = intercept_len_boot, y2 = intercept_len_classic)

print(ggplot(df_1, aes(x)) +
        geom_point(aes(y = y1, color = "BootStrap")) +
        geom_point(aes(y = y2, color = "Classical")) +
        scale_color_manual(values = c("BootStrap" = "red", "Classical" = "blue")) +
        labs(title = paste("Sample size:", n), x = "confidence level", y = "confidence length") +
        theme_bw())
  


######################################################
# working code
#########################################################
sample_size = c(50)                                   #c(50,100,500)
conf_level = c(0.8,0.9, 0.95, 0.99)
N_sim = 100

for (n in sample_size){
  x = runif(n, -1, 1)
  #n_err = rnorm(n, 0, 0.5)
  e_err = rexp(n, rate = 0.5)
  #e_err = e_err - mean(e_err)
  y = 1 + 2 * x + n_err
  
  intercept_len_boot = rep(NA,4)
  slope_len_boot = rep(NA,4)
  intercept_len_classic = rep(NA,4)
  slope_len_classic = rep(NA,4)
  intercept_per_sim = rep(NA,N_sim)
  slope_per_sim = rep(NA,N_sim)
  
  
  print(n)
  for (i in 1:4){
    conf = conf_level[i]
    print(conf)
    fit = lm(y ~ x)
    
    for (j in 1:N_sim){
      interval= bootLS(x,y,conf = conf, B=n)
      intercept_per_sim[j] = interval[4]-interval[3]
      slope_per_sim[j] = interval[2]-interval[1]  
    }
    intercept_len_boot[i] = mean(intercept_per_sim)
    slope_len_boot[i] = mean(slope_per_sim)
    intercept_len_classic[i] = confint(fit, level = conf)[1,][2]-confint(fit, level = 0.9)[1,][1]
    slope_len_classic[i] = confint(fit, level = conf)[2,][2]-confint(fit, level = 0.9)[2,][1]  
  } 
  
  
  print(intercept_len_boot)
  df_1 <- data.frame(x = c(0.8,0.9,0.95,0.99), y1 = intercept_len_boot, y2 = intercept_len_classic)
  df_2 <- data.frame(x = c(0.8,0.9,0.95,0.99), y1 = slope_len_boot, y2 = slope_len_classic)
  
  print(ggplot(df_1, aes(x)) +
          geom_point(aes(y = y1, color = "BootStrap")) +
          geom_point(aes(y = y2, color = "Classical")) +
          scale_color_manual(values = c("BootStrap" = "red", "Classical" = "blue")) +
          labs(title = paste("Sample size:", n), x = "confidence level", y = "confidence length") +
          theme_bw())
  
  
  print(ggplot(df_2, aes(x)) +
          geom_point(aes(y = y1, color = "BootStrap")) +
          geom_point(aes(y = y2, color = "Classical")) +
          scale_color_manual(values = c("BootStrap" = "red", "Classical" = "blue")) +
          labs(title = paste("Sample size:", n), x = "confidence level", y = "confidence length") +
          theme_bw())
}


###############################################################################

