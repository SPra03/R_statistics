install.packages('rmarkdown')

install.packages("tinytex")
install.packages('tinytex')
tinytex::install_tinytex()  # install TinyTeX

load("04cars.rda")
data = dat[,c(13,15)]
names(data) = c("hp", "mpg")

confBand <- function(x,y, conf = 0.95){
  
  n = 100
  #Plotting the linear model
  linear_model = lm(y~x)
  p=1
  df_1 = p+1 #degree of freedom 1
  df_2=n-p-1  #degree of freedom 2
  fquartile = sqrt((p+1) * qf(conf, df_1,df_2))
  
  #Calculating y bar
  y_ = predict(linear_model, data.frame(x=x), se =  T)
  
  
  cb_upper =  (y_$fit + ((fquartile)*y_$se.fit))
  cb_lower =  (y_$fit - ((fquartile)*y_$se.fit))
  
  plot(x,y, type = 'p')
  lines(x,cb_upper, type = 'l', col ="green")
  lines(x,cb_lower, type = 'l', col ="green")
  abline(linear_model, col = 'red', lwd = 2)
  #ggplot(linear_model$model, aes_string(x = names(linear_model$model)[2], y = names(linear_model$model)[1])) + 
  # geom_point() +
  #stat_smooth(method = "lm", col = "red") 
  #return (list(upb = cb_upper, lwb = cb_lower, y_pred = y_$fit, pred_se = y_$se.fit, Fvalue = fquartile))
}

confBand(data$hp, data$mpg)



# question 2

x = runif(100, 0,1)
y_true = 1.000+x
contains_true_line = rep(NA, 1000)

for (j in 1:1000){
  
  e = rnorm(100, 0, 0.2)
  y = 1.000+x+e
  
  CI = data.frame(lower = rep(NA,100),upper = rep(NA,100),contain_true_value = rep(NA,100))
  
  linear_model= lm(y~x)
  
  n = 100
  p=1
  df_1 = p+1
  df_2=n-p-1
  conf = 0.99
  F_quartile = sqrt((p+1) * qf(conf, df_1,df_2))
  pred_op = predict(linear_model, data.frame(hp=x), se =  T)
  
  
  CI$lower = (pred_op$fit - ((F_quartile)*pred_op$se.fit))
  CI$upper = (pred_op$fit + ((F_quartile)*pred_op$se.fit))
  CI$contain_true_value = (CI$lower <=y_true & CI$upper >= y_true) 
  
  #contains_true_line[j] = ((mean(CI$contain_true_value)) ==1)
  contains_true_line[j] = ((sum(CI$contain_true_value)) ==n)
}


#answer = (sum(contains_true_line)/1000)
answer = (sum(contains_true_line)/1000)






load("04cars.rda")
data = dat[,c(13,15)]
names(data) = c("hp", "mpg")
n = 100
#Plotting the linear model
linear_model = lm(data$mpg~data$hp)
p=1
df_1 = p+1 #degree of freedom 1
df_2=n-p-1  #degree of freedom 2
fquartile = sqrt((p+1) * qf(0.95, df_1,df_2))

#Calculating y bar
y_ = predict(linear_model, data.frame(x=data$mpg), se =  T)

#Calculating the upper and lower confidence bounds
cb_upper =  (y_$fit + ((fquartile)*y_$se.fit))
cb_lower =  (y_$fit - ((fquartile)*y_$se.fit))


plot(x,y, type = 'p')

lines(x,cb_upper, type = 'l', col ="green")
lines(x,cb_lower, type = 'l', col ="green")
