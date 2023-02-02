
fun <- function(row,col) row^(col-1)
final_list = c()

conditional_number = function(n, color="blue"){
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
    plot(list_k_X, col=color)
  }
  final_list = append(final_list,list_k_X)
}




plot.new()
conditional_number(30, "blue")
conditional_number(50, "green")
conditional_number(100, "cyan")
conditional_number(200, "red")
conditional_number(500, "magenta")
conditional_number(1000, "black")








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
      val[2*j-1] = val[2*j-3]
      val[2*j] = val[2*j-2]
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


#a)
x = runif(100, min=0, max=1)
y = x^3
plot(y~x)
piecewiseConstant(x,y,2, TRUE)


#b)

setwd("D:/projects/Quarter-2/Stats_model") 
load("04cars.rda")

data_ = dat
dat = data_[complete.cases(data_),]

plot(dat$Horsepower,dat$City_MPG, pch = 16, main="Piecewise constant fit", cex = 1, xlab="Horsepower", ylab="MPG")
piecewiseConstant(dat$Horsepower,dat$City_MPG,2, TRUE)
piecewiseConstant(dat$Horsepower,dat$City_MPG,3, TRUE)
piecewiseConstant(dat$Horsepower,dat$City_MPG,4, TRUE)
legend(435, 60, legend=c("L = 2", "L = 3", "L = 4"),col=c("blue", "green", "red"), lty=1, cex=0.8, box.lty = 0, bg='lightgrey')






