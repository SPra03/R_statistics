

# Load the data
library(MASS)
dat = Boston[, -c(4, 9)]
dat = dat[,c("crim","zn","nox","rm","dis","ptratio","black","lstat","medv")]
x = dat[,!names(dat) %in% c('medv')]
y = dat$medv

set.seed(241)

cv.lm <- function(x,y, k) {
  # x is a matrix of predictors
  # y is a vector of response
  # k is the number of folds
  # returns the mean squared error of the model
  
  
  #dat_cv = dat[sample(n),]
  n = nrow(x)
  dat_cv = data.frame(x,y)
  dat_cv = dat_cv[sample(n),]
  #print(dat_cv)
  folds <- cut(seq(1,n),breaks=k,labels=FALSE)
  cv_error = rep(NA, k)
  
  # construct formula string
  formula_string <- paste(paste(colnames(y), collapse = " + "),
                          " ~", paste(colnames(x), collapse = " + "))
  # convert formula string to formula object
  formula_object <- as.formula(formula_string)
  print(formula_object)
  
  #k is the number of folds
  for (i in 1:k) 
  {
    #Creating train and validation subsets from folds
    dat_train = dat_cv[folds != i,]
    dat_val = dat_cv[folds == i,]
    
    #training model    
    train_model = lm(formula_object, data = dat_train)
    #train_model = lm(medv ~ crim + zn + nox + rm + dis + ptratio + black + lstat, data = dat_train)
    pred_val = predict(train_model, newdata = dat_val)
    
    #Calculating cross validation error at ith fold
    cv_error[i] = sqrt(mean((dat_val$medv - pred_val)^2))
  }
  
  #return mean of CV error
  return(mean(cv_error))
}

cv.lm(x, y, 5)


fun <- function(x, y, k) {
  n = nrow(x)
  features = c(colnames(x), colnames(y))
  formula_string <- paste("y ~", paste(features, collapse = " + "))
  formula_object <- as.formula(formula_string)
  
  folds = cut(seq_len(n), breaks = k, labels = FALSE)
  cv_mse = rep(0, k)
  print(formula_object)
  
  for (i in 1:k) {
    val = which(folds == i, arr.ind = TRUE)
    train = which(folds != i, arr.ind = TRUE)
    x_train = x[train,]
    y_train = y[train,]
    model = lm(formula_object, data =data.frame(x_train, y_train))
    yhat = predict(model, x[val,])
    cv_mse[i] = sqrt(mean((y[val] - yhat)^2))
  }
  print(cv_mse)
  return(mean(cv_mse))
}
fun(x, y, 5)



############# to compare
library(caret)
cv.lm <- function(x, y, k){
  # x is a matrix of predictors
  # y is a vector of response
  # k is the number of folds
  # returns the mean squared error of the model
  n = nrow(x)
  
  dat_cv = data.frame(y = y, x = x)

  
  train_control <- trainControl(method = "cv",number = k)
  m_cv <- train(y ~ ., data = dat_cv,
                method = "lm",
                trControl = train_control)
  
  print(m_cv)
  return(m_cv)
}


# Estimate the prediction error of the linear regression model with y as response using 10-fold cross-validation
m_cv = cv.lm(x, y, 5)
print(m_cv)



## 1 (b)
###########################################################################################################################################
SequentialSelection <- function(x, y, method) {

  # initialize variables
  p <- ncol(x)
  n <- nrow(x)
  included <- c() # columns included in the model
  cols = c(colnames(x))
  excluded <- c(colnames(x)) # columns excluded from the model

  intercept <- rep(1, n) # intercept only model
  models <- list(intercept)
  scores <- c()
  prev_best_score = NULL
  
  if(method == "ADJR2"){
    
    # loop over columns
    for (i in 1:p) {
      best_score <- NULL
      best_col <- NULL
      
      # loop over excluded columns
      for (j in excluded) {
        
        # fit model with j-th column included
        features = c(included, j)
        
        # construct formula string
        formula_string <- paste("y ~", paste(features, collapse = " + "))
        
        # convert formula string to formula object
        formula_object <- as.formula(formula_string)
        
        model <- lm(formula_object, data = x)
        score <- summary(model)$adj.r.squared
        
        # update best score and column
        if (is.null(best_score) || score > best_score) {
          best_score <- score
          best_col <- j
        }
      }
      
      # if no new column to add, then exit
      if (!is.null(prev_best_score) && prev_best_score> best_score){
        break
      }
      prev_best_score = best_score
    
      # add best column to included columns and remove from excluded columns
      included <- c(included, best_col)
      excluded <- setdiff(excluded, best_col)
      
      ## saving the best model
      formula_string <- paste("y ~", paste(included, collapse = " + "))
      formula_object <- as.formula(formula_string)
      models[i] <- list(lm(formula_object, data = x))
      scores[i] <- best_score
    }
  }
  else {
    for (i in 1:p) {
      best_score <- NULL
      best_col <- NULL
      # loop over excluded columns
      for (j in excluded) {
        
        # fit model with j-th column included
        features = c(included, j)
        
        # construct formula string
        formula_string <- paste("y ~", paste(features, collapse = " + "))
        
        # convert formula string to formula object
        formula_object <- as.formula(formula_string)
        
        model <- lm(formula_object, data = x)
        # compute score based on method
        if (method == "AIC") {
          score <- AIC(model)
        } else if (method == "CV5") {
          score <- mean(sapply(split(1:n, rep(1:5, each = n/5)), function(ind){
            mean((y[ind] - predict(model, newdata = x[ind,]))^2)
          }))
        } else {
          stop("Invalid method")
        }
      
        # update best score and column
        if (is.null(best_score) || score < best_score) {
          best_score <- score
          best_col <- j
        }
      }
      
      # if no new column to add, then exit
      if (!is.null(prev_best_score) && prev_best_score< best_score){
        break
      }
      prev_best_score = best_score
      
      # add best column to included columns and remove from excluded columns
      included <- c(included, best_col)
      excluded <- setdiff(excluded, best_col)
      
      ## saving the best model
      formula_string <- paste("y ~", paste(included, collapse = " + "))
      formula_object <- as.formula(formula_string)
      models[i] <- list(lm(formula_object, data = x))
      scores[i] <- best_score
    }
  }

  # choose best model based on method
  
  
  if (method == "ADJR2") {
    best_model <- models[[which.max(scores)]]
  } else if (method == "AIC") {
    best_model <- models[[which.min(scores)]]
  } else if (method == "CV5") {
    best_model <- models[[which.min(scores)]]
  } else {
    stop("Invalid method")
  }
  
  best_score = 0
  # return results
  if (method == "ADJR2"){
    best_score = max(scores)
  }
  else{
    best_score = min(scores)
  }
  
  #print("Best Score")
  #print(best_score)
  
  return(list(features = included, best_model = best_model, best_score = best_score))
}


dat_ = Boston
#dat = dat[,c("crim","zn","nox","rm","dis","ptratio","black","lstat","medv")]
x = dat_[,!names(dat_) %in% c('medv')]
y = dat_$medv
typeof(x)



result = SequentialSelection(x,y,'ADJR2')

result$features
predict(result$best_model, x)


model = lm(y~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black +lstat, data  = dat_)   # 
predict(model, x)
AIC(model)
summary(model)$adj.r.squared

lst = rep(0,2)
lst[1] <- list(model)
lst[2] <- list(model)
lst[[1]]
 
temp = lst[[2]]
abc = temp

predict(abc,x)

3027.609
0.7348058
21.91567

colnames(dat_)



###################################################################
#Q)2 

data_generation <-function(degree = 20){
  n = 200
  x <- runif(n, 0, 2*pi)
  y <- rnorm(n, mean = sin(3*x) + x, sd = 1)
  return (list(x=x,y=y))
}

max_degree = 20
methods = c("ADJR2", "AIC", "CV5")
counter=0

nsim = 100
plots <- list()
adj_deg =  rep(0, nsim)
adj_result = NULL
adj_deg_every_iter = c()
aic_deg =  rep(0, nsim)
aic_result = NULL
aic_deg_every_iter = c()
cv_deg =  rep(0, nsim)
cv_result = NULL
cv_deg_every_iter = c()
###################################################################

data = data_generation()
x_ = data$x
y_= data$y
X <- matrix(rep(x_, 20), ncol = 20)
x=NULL


for (i in 2:20) {
  X[,i] <- X[,i-1] * x_ 
  # Create a data frame with the matrix and set column names
  x <- data.frame(X)
  names(x) <- paste0("x", 1:20)
}

x1 = x[,1:5]
colnames(x1)

adj_deg_every_iter

aic_deg_every_iter
###########################################################################################

#final working 2

#################

counter=0
for(n in 1:100){
  
  print(paste("Sim: ", n))
  data = data_generation()
  x_ = data$x
  y_= data$y
  X <- matrix(rep(x_, 20), ncol = 20)
  x=NULL
  
  for (i in 2:20) {
    counter = counter+1
    X[,i] <- X[,i-1] * x_ 
    # Create a data frame with the matrix and set column names
    x <- data.frame(X)
    names(x) <- paste0("x", 1:20)
  }
  
  for (method in methods){
    best_result = NULL
    if (method == "ADJR2"){
      best_score = -5
      best_deg = -5 
    }else{
      best_score =Inf
      best_deg = Inf
    }
    
    for (deg in 2:max_degree){
      result = SequentialSelection(x[,1:deg],y_, method = method)
      max_deg = as.numeric(gsub("x", "", tail(result$features,1)))
      #print(paste("features of method",method, "degree", deg," = ",result$features ))
      
      
      if (method == "ADJR2"){
        adj_deg_every_iter = append(adj_deg_every_iter, max_deg)
        #print(paste("max degree: ",max_deg, "in Adjr"))
        if (best_score < result$best_score){
          best_score =result$best_score
          best_deg = max_deg  
          best_result = result
        }
      }
      else{
        if(method=="AIC"){
          aic_deg_every_iter=append(aic_deg_every_iter, max_deg)
          #print(paste("max degree: ",max_deg, "in AIC"))
        }
        else{
          cv_deg_every_iter=append(cv_deg_every_iter, max_deg)
          #print(paste("max degree: ",max_deg, "in cv"))
        }
        if (best_score > result$best_score){
          best_score = result$best_score
          best_deg = max_deg
          best_result = result
        }
      }
      
    }
    if (method == "ADJR2") {
      adj_deg[n] <- best_deg
      adj_result <- best_result
    } else if (method == "AIC") {
      aic_deg[n] <- best_deg
      aic_result <- best_result
    } else if (method == "CV5") {
      cv_deg[n] <- best_deg
      cv_result <- best_result
    } else {
      stop("Invalid method")
    }
  }
  
  if(n<4){
    data = data.frame(x_,y_)
    data$y_pred_adj <- predict(adj_result$best_model, x)
    data$y_pred_aic <- predict(aic_result$best_model, x)
    data$y_pred_cv <- predict(cv_result$best_model, x)
    
    p <- ggplot(data, aes(x = x_, y = y_)) +
      # add points
      geom_point() +
      # add linear regression lines with custom colors and labels
      geom_line(aes(y = y_pred_adj, color = "Model 1"), size = 0.6, alpha = 0.7) +
      geom_line(aes(y = y_pred_aic, color = "Model 2"), size = 2.0,alpha = 0.7) +
      geom_line(aes(y = y_pred_cv, color = "Model 3"), size = 1.0, alpha = 0.7) +
      # add title in the middle of the plot
      ggtitle(paste("Linear Regression- ", nsim)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      # add axis labels
      xlab("X") +
      ylab("Y") +
      # add legend with custom colors and labels
      scale_color_manual(values = c("Model 1" = "red", "Model 2" = "green", "Model 3" = "blue"),
                         labels = c("Adjusted R-squared", "AIC", "Cross-validation"),
                         name = "Model") +
      # adjust legend position
      theme(legend.position = "top")
    plots[[i]] = p
    print(p)
  }
}







ggplot(data.frame(adj_deg), aes(x = adj_deg)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 20) +
  labs(title = "Distribution of Polynomial Degrees of best model for Adjusted R Squared",
       x = "Polynomial Degree",
       y = "Frequency")

ggplot(data.frame(aic_deg), aes(x = aic_deg)) +
  geom_histogram(color = "black", fill = "lightgreen", bins = 20) +
  labs(title = "Distribution of Polynomial Degrees of best model for AIC",
       x = "Polynomial Degree",
       y = "Frequency")

ggplot(data.frame(cv_deg), aes(x = cv_deg)) +
  geom_histogram(color = "black", fill = "mistyrose", bins = 20) +
  labs(title = "Distribution of Polynomial Degrees of best model for CV",
       x = "Polynomial Degree",
       y = "Frequency")


ggplot(data.frame(adj_deg_every_iter), aes(x = adj_deg_every_iter)) +
  geom_histogram(color = "black", fill = "lightblue", bins = 20) +
  labs(title = "Distribution of Polynomial Degrees of every model for Adjusted R Squared",
       x = "Polynomial Degree",
       y = "Frequency")

ggplot(data.frame(aic_deg_every_iter), aes(x = aic_deg_every_iter)) +
  geom_histogram(color = "black", fill = "lightgreen", bins = 20) +
  labs(title = "Distribution of Polynomial Degrees of every model for AIC",
       x = "Polynomial Degree",
       y = "Frequency")

ggplot(data.frame(cv_deg_every_iter), aes(x = cv_deg_every_iter)) +
  geom_histogram(color = "black", fill = "mistyrose", bins = 20) +
  labs(title = "Distribution of Polynomial Degrees of every model for CV",
       x = "Polynomial Degree",
       y = "Frequency")





for(nsim in 1:3){
  
  print(paste("Sim: ", nsim))
  data = data_generation()
  x_ = data$x
  y_= data$y
  X <- matrix(rep(x_, 20), ncol = 20)
  x=NULL
  
  for (i in 2:20) {
    counter = counter+1
    X[,i] <- X[,i-1] * x_ 
    # Create a data frame with the matrix and set column names
    x <- data.frame(X)
    names(x) <- paste0("x", 1:20)
  }
  
  for (method in methods){
    best_result = NULL
    if (method == "ADJR2"){
      best_score = 0
      best_deg = 0  
    }else{
      best_score = Inf
      best_deg = Inf
    }
    
    for (deg in 2:max_degree){
      result = SequentialSelection(x[,1:deg],y_, method = method)
      max_deg = as.numeric(gsub("x", "", tail(result$features,1)))
      if (method == "ADJR2"){
        adj_deg_every_iter[counter] = max_deg
        if (best_score< result$best_score){
          best_score =result$best_score
          best_deg = max_deg  
          best_result = result
        }
      }
      else{
        if(method=="AIC"){
          aic_deg_every_iter[counter] = max_deg
        }
        else{
          cv_deg_every_iter[counter] = max_deg
        }
        if (best_score> result$best_score){
          best_score = result$best_score
          best_deg = max_deg
          best_result = result
        }
      }
      
    }
    if (method == "ADJR2") {
      adj_deg[nsim] <- best_deg
      adj_result <- best_result
    } else if (method == "AIC") {
      aic_deg[i] <- best_deg
      aic_result <- best_result
    } else if (method == "CV5") {
      cv_deg[nsim] <- best_deg
      cv_result <- best_result
    } else {
      stop("Invalid method")
    }
  }
  data = data.frame(x_,y_)
  data$y_pred_adj <- predict(adj_result$best_model, x)
  data$y_pred_aic <- predict(aic_result$best_model, x)
  data$y_pred_cv <- predict(cv_result$best_model, x)
  
  p <- ggplot(data, aes(x = x_, y = y_)) +
    # add points
    geom_point() +
    # add linear regression lines with custom colors and labels
    geom_line(aes(y = y_pred_adj, color = "Model 1"), size = 0.6, alpha = 0.7) +
    geom_line(aes(y = y_pred_aic, color = "Model 2"), size = 2.0,alpha = 0.7) +
    geom_line(aes(y = y_pred_cv, color = "Model 3"), size = 1.0, alpha = 0.7) +
    # add title in the middle of the plot
    ggtitle(paste("Linear Regression- ", nsim)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    # add axis labels
    xlab("X") +
    ylab("Y") +
    # add legend with custom colors and labels
    scale_color_manual(values = c("Model 1" = "red", "Model 2" = "green", "Model 3" = "blue"),
                       labels = c("Adjusted R-squared", "AIC", "Cross-validation"),
                       name = "Model") +
    # adjust legend position
    theme(legend.position = "top")
  plots[[i]] = p
  print(p)
}

grid.arrange(grobs = plots, ncol = 2) +
  theme(plot.margin = unit(c(1, 1, 0, 0), "lines"))






###################################

#plots working before 3pm, it does not capture the best models and all
###################################
library(ggplot2)

for(nsim in 1:3){
  data = data_generation()
  x_ = data$x
  y_= data$y
  X <- matrix(rep(x_, 20), ncol = 20)
  x=NULL
  
  for (i in 2:20) {
    X[,i] <- X[,i-1] * x_ 
    # Create a data frame with the matrix and set column names
    x <- data.frame(X)
    names(x) <- paste0("x", 1:20)
  }
  
  for (method in methods){
    best_score = NULL
    best_deg = NULL
    best_result = NULL
      
    for (deg in 2:max_degree){
      result = SequentialSelection(x[,1:deg],y_, method = method)
      if (method == "ADJR2"){
        best_score = max(best_score, result$best_score)
        best_deg = deg
        best_result = result
      }
      else{
        best_score = min(best_score, result$best_score)
        best_deg = deg
        best_result = result
      }
    }
    if (method == "ADJR2") {
      adj_deg[nsim] <- best_deg
      adj_result <- best_result
    } else if (method == "AIC") {
      aic_deg[i] <- best_deg
      aic_result <- best_result
    } else if (method == "CV5") {
      cv_deg[nsim] <- best_deg
      cv_result <- best_result
    } else {
      stop("Invalid method")
    }
  }
  
  
  data = data.frame(x_,y_)
  data$y_pred_adj <- predict(adj_result$best_model, x)
  data$y_pred_aic <- predict(aic_result$best_model, x)
  data$y_pred_cv <- predict(cv_result$best_model, x)
  
  p <- ggplot(data, aes(x = x_, y = y_)) +
    # add points
    geom_point() +
    # add linear regression lines with custom colors and labels
    geom_line(aes(y = y_pred_adj, color = "Model 1"), size = 0.6, alpha = 0.7) +
    geom_line(aes(y = y_pred_aic, color = "Model 2"), size = 2.0,alpha = 0.7) +
    geom_line(aes(y = y_pred_cv, color = "Model 3"), size = 1.0, alpha = 0.7) +
    # add title in the middle of the plot
    ggtitle(paste("Linear Regression- ", nsim)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    # add axis labels
    xlab("X") +
    ylab("Y") +
    # add legend with custom colors and labels
    scale_color_manual(values = c("Model 1" = "red", "Model 2" = "green", "Model 3" = "blue"),
                       labels = c("Adjusted R-squared", "AIC", "Cross-validation"),
                       name = "Model") +
    # adjust legend position
    theme(legend.position = "top")
  plots[[i]] = p
  print(p)
}

grid.arrange(grobs = plots, ncol = 2) +
  theme(plot.margin = unit(c(1, 1, 0, 0), "lines"))



###########################################################################################


#working code 
#########################################
for(i in 1:1){
  
  data = data_generation()
  x_ = data$x
  y_= data$y
  x=NULL
  
  j=0
  for (method in methods){
    j= j+1
    if (method == "ADJR2"){
      best_score = 0
      best_deg = 0  
    }
    else{
      best_score = Inf
      best_deg = Inf
    }
    for (deg in 1:max_degree){
      x = list(x_,x_^deg)
      names(x)<- c("x", paste0("x^", 2))
      x <- data.frame(x)
      result = SequentialSelection(x,y, method = method)
      if (method == "ADJR2"){
        if (best_score< result$best_score){
          best_score =result$best_score
          best_deg = deg  
          best_model[j] = result$best_model
        }
      }
      else{
        if (best_score> result$best_score){
          best_score = result$best_score
          best_deg = deg
          best_model[j] = result$best_model
        }
      }
    }
    if (method == "ADJR2") {
      adj_deg[i] <- best_deg
    } else if (method == "AIC") {
      aic_deg[i] <- best_deg
    } else if (method == "CV5") {
      cv_deg[i] <- best_deg
    } else {
      stop("Invalid method")
    }
  }
  
  #plot(x_, y_, pch = 16, cex = 0.8, xlab = "x", ylab = "y", main = "Model Fits")
  
  xplot=x_
  
  # Add lines for the model fits
  colour = c("blue", "green", "red")
  
  print("Best model")
  print(best_model)
  print(class(best_model))
  k=0
  for (model_ in best_model){
    k = k+1
    features = names(model_)
    from_idx <- 2  # Starting index
    features <- features[from_idx:length(features)]
    formula_string <- paste("y ~", paste(features, collapse = " + "))
    formula_object <- as.formula(formula_string)
    formula_object
    model = lm(formula_object, data = x)  
    #lines(x_, predict(model), col = colour[k], lwd = 2)
    lines(xplot,predict(model,newdata=data.frame(x=xplot)), col=colour[k])
  }
  
  #lines(x_, predict(model[1], x_), col = "blue", lwd = 2)
  #lines(x_, predict(model[2], x_), col = "red", lwd = 2)
  #lines(x_, predict(model[3],x_), col = "green", lwd = 2)

}
(best_model[[1]])
names(best_model[[1]])

plot(x_, y_, pch = 16, cex = 0.8, xlab = "x", ylab = "y", main = "Model Fits")

model = rep(NULL,3)
k=0
for (model_ in best_model){
  print(model_)
  k = k+1
  features = names(model_)
  from_idx <- 2  # Starting index
  features <- features[from_idx:length(features)]
  formula_string <- paste("y ~", paste(features, collapse = " + "))
  formula_object <- as.formula(formula_string)
  formula_object
  temp_m = lm(formula_object, data = x)  
  model[k] = temp_m
  print(summary(model[[k]]))
}

summary(model[1])
model[1]
lines(x_, predict(model[1], x_), col = "blue", lwd = 2)
#lines(x_, predict(model[2], x_), col = "red", lwd = 2)
#lines(x_, predict(model[3],x_), col = "green", lwd = 2)


####################################################

k=1
features = names(model[1])
from_idx <- 2  # Starting index
features <- features[from_idx:length(features)]
formula_string <- paste("y ~", paste(features, collapse = " + "))
formula_object <- as.formula(formula_string)
formula_object
temp_m = lm(formula_object, data = x)  
model[k] = temp_m

a = temp_m

summary(temp_m)
print("######")
summary(model[[k]])
print("######")
summary(a)
lines(x_, predict(a), col = "blue", lwd = 2)




###############################################

adj_deg
aic_deg
cv_deg




### step in for

for(i in 1:100){
  data = data_generation()
  x_ = data$x
  y_= data$y
  for (method in c("AIC")){
    best_score = NULL
    best_deg = NULL
    for (deg in 1:max_degree){
      x = list(x_,x_^deg)
      names(x)<- c("x1", "x2")
      x <- data.frame(x)
      m0 <- lm(y ~ 1, data=x) 
      step_model = step(m0,scope= ~ x1 + x2, direction="forward")
      result = AIC(step_model, k = length(coef(step_model)))
      
      if (method == "ADJR2"){
        best_score = max(best_score, result)
        best_deg = deg
      }
      else{
        best_score = min(best_score, result)
        best_deg = deg
      }
    }
    if (method == "ADJR2") {
      adj_deg[i] <- best_deg
    } else if (method == "AIC") {
      aic_deg[i] <- best_deg
    } else if (method == "CV5") {
      cv_deg[i] <- best_deg
    } else {
      stop("Invalid method")
    }
  }
  
  
}


############################################
#for x with 1 to 20 degrees

for (i in 100){
  data = data_generation()
  x = data$x
  y= data$y  
  formula_string = c()
  formula = c()
  
  for (deg in 2:max_degree){
    formula_string = c(formula_string, paste0("I(x^", deg, ")"))
  }
  formula = as.formula(paste("y ~", paste(formula_string, collapse = " + ")))
  print(formula)
  
  
  
  
  }

paste0("I(x^", 2)




