library(car)
library(ggplot2)
n_sample_size = function(n) {
  x= runif(n,-1,1)
  e = rnorm(n,0,0.5)
  y = 1+2*x+e
  model_fit = lm(y~x)
  temp = coef(model_fit)
  intercept = temp[1]
  slope = temp[2]
  
  
  list_temp = list("int"=intercept, "slope"=slope)
  return(list_temp)
}

t = n_sample_size(50)

iterations = list(50,100,200,500)
final_list = list()
final_list_int = list()
final_list_slopes = list()
