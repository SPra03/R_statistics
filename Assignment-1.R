setwd("D:/projects/R_Projects/Sandbox/Stats_model_assignments") 
getwd()


load("04cars.rda")
data = dat[,c(13,15)]
names(data) = c("hp", "mpg")

confBand <- function(x,y, conf = 0.95){
  
  n = 100
  m = lm(y~x)
  p=1
  df_1 = p+1
  df_2=n-p-1
  fquartile = sqrt((p+1) * qf(conf, df_1,df_2))
  
  y_ = predict(m, data.frame(x=x), se =  T)

  
  cb_upper =  (y_$fit + ((fquartile)*y_$se.fit))
  cb_lower =  (y_$fit - ((fquartile)*y_$se.fit))
  
  plot(x,y, type = 'p')
  lines(x,cb_upper, type = 'l', col ="green")
  lines(x,cb_lower, type = 'l', col ="green")
  abline(m, col = 'red', lwd = 2)
  
  return (list(upb = cb_upper, lwb = cb_lower, y_pred = y_$fit, pred_se = y_$se.fit, Fvalue = fquartile))
}

confBand(data$hp, data$mpg)

