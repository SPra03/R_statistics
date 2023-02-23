
fun <- function(row,col) row^(col-1)
final_list = c()


for (i in list(30,50,100,200,500,1000)){
  list_k_X = c()
  for (j in 1:20){
    x = seq(from = 0, to = 1, by = (1/(i+1)))[2:i+1]
    rows = x
    cols = 1:j+1
    X = outer(rows,cols,FUN=fun)
    svd_X = svd(X)
    k_X = max(svd_X$d)/min(svd_X$d) 
    list_k_X = append(list_k_X, log(k_X))
  }
  final_list = append(final_list,list_k_X)
}

final_list = c()
conditional_number = function(n){
  list_k_X = c()
  i = n
  for (j in 1:20){
    x = seq(from = 0, to = 1, by = (1/(i+1)))[2:i+1]
    rows = x
    cols = 1:j+1
    X = outer(rows,cols,FUN=fun)
    svd_X = svd(X)
    k_X = max(svd_X$d)/min(svd_X$d) 
    list_k_X = append(list_k_X, log(k_X))
    plot(list_k_X, col=("blue"))
  }
  final_list = append(final_list,list_k_X)
}


#plot(,1:40, pch = 16, main="Piecewise constant fit", cex = 1, xlab="Horsepower", ylab="MPG")

plot.new()
conditional_number(30)
conditional_number(50)
conditional_number(100)
conditional_number(200)
conditional_number(500)
conditional_number(1000)
plot(final_list)  
print(final_list)





x = runif(100, min=0, max=1)
y = x^3

# Q)2 

piecewiseConstant = function(x,y, L, plot=TRUE){
  n = 2^L
  #K = quantile(x, seq(0, 1, len = n+1), type=1)             # for quantile
  quotient = (range(x)[2]-range(x)[1])/n
  K = seq(from = range(x)[1], to = range(x)[2], by =quotient ) # for equal split
  pts = rep(0,2*n)
  val = rep(0,2*n)
  for (j in 1:n){
    I = (K[j] < x)&(x <= K[j+1])
    if (length(I[I==TRUE]) !=0){
      fit = lm(y[I] ~ 1)
      pts[2*j-1] = K[j]
      pts[2*j] = K[j+1]
      val[2*j-1] = coef(fit)
      val[2*j] = coef(fit)  
    }
    else{
      pts[2*j-1] = K[j]
      pts[2*j] = K[j+1]
      val[2*j-1] = 0
      val[2*j] = 0
    }
    
  }
  if (plot){
    if (L==2){
      lines(pts, val, col="blue", lwd = 3)  
    }
    else if (L==3){
      lines(pts, val, col="green", lwd = 3)  
    }
    if (L==4){
      lines(pts, val, col="red", lwd = 3)  
    }
  }
}

setwd("D:/projects/Quarter-2/Stats_model") 
load("04cars.rda")

data_ = dat
dat = data_[complete.cases(data_),]

plot(dat$Horsepower,dat$City_MPG, pch = 16, main="Piecewise constant fit", cex = 1, xlab="Horsepower", ylab="MPG")
piecewiseConstant(dat$Horsepower,dat$City_MPG,2, TRUE)
piecewiseConstant(dat$Horsepower,dat$City_MPG,3, TRUE)
piecewiseConstant(dat$Horsepower,dat$City_MPG,4, TRUE)
legend(450, 60, legend=c("L = 2", "L = 3", "L = 4"),col=c("blue", "green", "red"), lty=1, cex=0.8, box.lty = 0, bg='lightgrey')

#piecewiseConstant(x,y,4, TRUE)



##########################################
x = dat$Horsepower
y = dat$City_MPG  

n = 16
#K = quantile(x, seq(0, 1, len = n+1), type=1)             # for quantile
quotient = (range(x)[2]-range(x)[1])/n
K = seq(from = range(x)[1], to = range(x)[2], by = quotient ) # for equal split
pts = rep(0,2*n)
val = rep(0,2*n)

for (j in 1:n){
  I = (K[j] < x)&(x <= K[j+1])
  print(length(I[I==TRUE]))
  if (length(I[I==TRUE]) !=0){
    print("inside if")
    fit = lm(y[I] ~ 1)
  }
  else{
    print("inside else")
  }
  
}

j=2
I = (K[j] < x)&(x <= K[j+1])
print(length(is.na(I)))  
####################################################


a = c(1,2,3)
(length(a)) != 0


