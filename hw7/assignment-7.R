
library(ggplot2)


setwd("D:/projects/Quarter-2/Stats_model") 
data = read.csv("Placekick.csv")

m=glm(good~distance,family = binomial(link = logit),data=data)
summary(m)

plot(data$distance ,data$good)



# Create a scatter plot with fitted logistic regression curve
ggplot(data, aes(x = distance, y = good)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)

#1 b)

# Fit the initial intercept-only model
model <- glm(good ~ 1, data = data, family = binomial(link = logit))

# Perform forward selection with AIC
step(model, scope = ~ distance + change + elap30 + PAT + type + field + wind + 
                      week, direction = "forward")

model = glm(formula = good ~ distance + PAT + change + wind, family = binomial(link = logit), data = data)
# Print the final model summary
summary(model)

# 1c)
model = glm(formula = good ~ distance + PAT + change + wind, family = binomial(link = logit), data = data)

#calculating for good
# distance = (0.5 - β0 - β2(PAT_mean) - β3(change_mean) - β4(wind_mean)) / β1


distance <- (0.5 - 4.751 - 1.229 * mean(data$PAT) - (-0.3350) * mean(data$change) - (-0.523) * mean(data$wind)) / (-0.087)
distance


# q2


bootGLM <- function(x, y, B = 1000){
  # Define a logistic model formula with all predictors
  formula <- as.formula(paste("y ~", paste(names(x), collapse = " + ")))
  
  # Fit the full logistic model
  full_model <- glm(formula, data = cbind(x, y), family =binomial(link = logit))
  
  # Get the names of the predictor variables
  predictors <- names(x)
  
  # Initialize a matrix to store the bootstrap results
  boot_results <- matrix(0, nrow = B, ncol = length(predictors),
                         dimnames = list(NULL, predictors))
  
  # Perform B bootstrap replicates
  for (i in 1:B) {
    # Resample the observations with replacement
    indices <- sample(nrow(x), replace = TRUE)
    x_boot <- x[indices, ]
    y_boot <- y[indices]
    
    # Fit the logistic model with resampled data
    boot_model <- glm(formula, data = cbind(x_boot, y_boot), family = "binomial")
    
    # Compute the standard errors of the predictor variables
    se <- summary(boot_model)$coefficients[, "Std. Error"]
    
    # Store the standard errors in the boot_results matrix
    boot_results[i, ] <- se[-1] 
  }
  # Compute the standard error estimates and return them as a data frame
  se_estimates <- apply(boot_results, 2, sd)
  se_df <- data.frame(variable = predictors,se_estimate = se_estimates)  
  return(se_df)
}

  
  
summary(model)$coefficients  
  
  
  
  
x = data[,!names(data) %in% c('good')]
y = data$good  
  
result = bootGLM(x,y)

result


#2b)

# Fit the model selected by AIC
model <- glm(good ~ distance + PAT + change + wind, family = binomial, data = data)

# Compute the standard errors using the bootGLM function
se_boot <- bootGLM(data[, c("distance", "PAT", "change", "wind")], data$good)

# Get the standard errors from the summary of the model
se_summary <- summary(model)$coefficients[-1, "Std. Error"]

se_boot 
se_summary


# Print the standard errors from both methods side by side
cbind(se_boot, se_summary)

