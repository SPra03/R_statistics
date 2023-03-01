#setwd("D:/projects/Quarter-2/Stats_model") 
#load("04cars.rda")
#head(dat)

# Load the data
library(MASS)
dat = Boston[, -c(4, 9)]
dat = dat[,c("crim","zn","nox","rm","dis","ptratio","black","lstat","medv")]
x = dat[,!names(dat) %in% c('medv')]
y = dat$medv

set.seed(241)

fun <- function(x, y, k) {
  # x is a matrix of predictors
  # y is a vector of response
  # k is the number of folds
  # returns the mean squared error of the model
  n = nrow(x)
  
  dat_cv = data.frame(y = y, x = x)
  folds = cut(seq_len(n), breaks = k, labels = FALSE)
  cv_mse = rep(0, k)
  
  for (i in 1:k) {
    val = which(folds == i, arr.ind = TRUE)
    train = which(folds != i, arr.ind = TRUE)
    x_val = x[val,]
    y_val = y[val]  
    model = lm(y ~ x.crim + x.zn + x.nox + x.rm + x.dis + x.ptratio + x.black + x.lstat , data = dat_cv[train,])
    yhat = predict(model, dat_cv[val,])
    cv_mse[i] = sqrt(mean((y_val - yhat)^2))
  }
  print(cv_mse)
  return(mean(cv_mse))
}


#############################################



#model <- lm(y ~ x[, c(included, j)])
# fit model with features from list
#print("formula_object")
#print(formula_object)
model <- lm(formula_object, data = x)
###############################################


fun <- function(x, y, k) {
  # x is a matrix of predictors
  # y is a vector of response
  # k is the number of folds
  # returns the mean squared error of the model
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

x[, 1:3]
model <- lm(y ~ . + x[, 2], data = data.frame(x, y))

?step
summary(model)adj.r.squared

m0 <- lm(y ~ 1, data=dat) 
step(m0,scope= ~ crim + zn + nox + rm + dis + ptratio + black + lstat, direction="forward")

?step

library(leaps)
X = dat[,!names(dat) %in% c('medv')]
L = leaps(X, dat$medv)
L
ind = which.min(L$Cp)
plot(L$size,L$Cp)
points(L$size[ind],L$Cp[ind],col = 'darkred',pch = 19)
points(aggregate(L$Cp, by = list(L$size), min),lwd = 2,col = 'darkred',type = 'b',pch = 19)



included = c("crim "," zn "," nox "," rm",  "dis")
features = c(included, "lstat")

# construct formula string
formula_string <- paste("y ~", paste(features, collapse = " + "))

# convert formula string to formula object
formula_object <- as.formula(formula_string)

# fit model with features from list
model <- lm(formula_object, data = dat)
model_2 = lm(medv~ crim + zn + nox + rm + dis+ lstat, data = dat)

summary(model_2)

summary(model)$adj.r.squared
AIC(model)

cols_ = c(colnames(dat))
for (i in cols_){
  print(i)
}

j = "ab" 
included = c()
features = c(included, j)

# construct formula string
formula_string <- paste("y ~", paste(features, collapse = " + "))
formula_string



###########################################################################################################################################
SequentialSelection <- function(x, y, method) {
  # x is the design matrix
  # y is the response variable
  # method is a string indicating the selection criteria
  
  # initialize variables
  p <- ncol(x)
  n <- nrow(x)
  included <- c() # columns included in the model
  #excluded <- 1:p # columns excluded from the model
  cols = c(colnames(x))
  excluded <- c(colnames(x)) # columns excluded from the model
  
  print("formula_object")
  print(cols)
  
  intercept <- rep(1, n) # intercept only model
  models <- list(intercept)
  scores <- c()
  
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
        
        
        #model <- lm(y ~ x[, c(included, j)])
        # fit model with features from list
        #print("formula_object")
        #print(formula_object)
        model <- lm(formula_object, data = x)
        score <- summary(model)$adj.r.squared
        
        # update best score and column
        if (is.null(best_score) || score > best_score) {
          best_score <- score
          best_col <- j
        }
      }
      
      # add best column to included columns and remove from excluded columns
      included <- c(included, best_col)
      excluded <- setdiff(excluded, best_col)
      
      ## saving the best model
      formula_string <- paste("y ~", paste(included, collapse = " + "))
      formula_object <- as.formula(formula_string)
      models[i] <- lm(formula_object, data = x)
      scores[i] <- best_score
    }
  }
  else {
    print("inside else")
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
        
        
        #model <- lm(y ~ x[, c(included, j)])
        # fit model with features from list
        #print("formula_object")
        #print(formula_object)
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
      
      # add best column to included columns and remove from excluded columns
      included <- c(included, best_col)
      excluded <- setdiff(excluded, best_col)
      
      ## saving the best model
      formula_string <- paste("y ~", paste(included, collapse = " + "))
      formula_object <- as.formula(formula_string)
      models[i] <- lm(formula_object, data = x)
      scores[i] <- best_score
    }
  }
  
  
  print("scores")
  print(scores)
  print("models")
  print(models)
  print("models end")
  
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
  
  print("Best Score")
  print(best_score)
  
  return(list(included = included, best_model = best_model, best_score = best_score))
}



mod = SequentialSelection(x,y, "CV5" ) 
mod
mod$best_model[]


dat_ = Boston
#dat = dat[,c("crim","zn","nox","rm","dis","ptratio","black","lstat","medv")]
x = dat_[,!names(dat_) %in% c('medv')]
y = dat_$medv


model = lm(y~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black +lstat, data  = dat_)
AIC(model)
summary(model)$adj.r.squared

3027.609
0.7348058
21.91567

colnames(dat_)


m0 <- lm(y ~ 1, data=dat_) 
step(m0,scope= ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+black +lstat, direction="forward")
AIC(model_2)







scores_ = c(2,4,5,6,1,1,2,9,1)



models_[]
which.max(scores_)


