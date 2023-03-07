library(ggplot2)

set.seed(2022)

bootLS = function(x,y, conf = 0.95, B=1000){
  N = length(x)
  beta0_boot = rep(NA,N)
  beta1_boot = rep(NA,N)
  t0_boot = rep(NA,N)
  t1_boot = rep(NA,N)
  
  fit = lm(y~x)
  beta0 = fit$coefficients[1]
  beta1 = fit$coefficients[2]
  sebeta0=summary(fit)$coefficients[,2][1]
  sebeta1=summary(fit)$coefficients[,2][2]
  
  
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
  
  c1 = ((1 - conf)/2)*100
  c2 = (100 - c1)
  boot_int = matrix(c(beta0 + quantile(t0_boot,c((1-conf)/2,(1+conf)/2))*sebeta0),ncol = 2)
  colnames(boot_int) = c(paste(c1,"%"),paste(c2, "%"))
  boot_slp = matrix(c(beta1 + quantile(t1_boot,c((1-conf)/2,(1+conf)/2))*sebeta1),ncol = 2)
  colnames(boot_slp) = c(paste(c1,"%"),paste(c2, "%"))
  return(c(boot_int,boot_slp))
  
}



setwd("D:/projects/Quarter-2/Stats_model") 
load("04cars.rda")
intervals = bootLS(dat$Weight, dat$City_MPG)


fit = lm(dat$City_MPG~dat$Weight)
#beta0 = fit$coefficients[1]
#beta1 = fit$coefficients[2]
#sebeta0=summary(fit)$coefficients[,2][1]
#sebeta1=summary(fit)$coefficients[,2][2]
confint(fit)[1,]
confint(fit)[2,]


# Q) 2

sample_size = c(50,100,500, 1000)
conf_level = c(0.8,0.9, 0.95, 0.99)

#x = runif(100, min=0, max=1)
#y = x

#x_norm = rnorm(1000,mean = 0,sd =2)
#y_norm = x_norm + runif(1000, min=0, max=1)

#for (size_ in sample_size){
#  interval = bootLS(x_norm,y_norm, B = size_)
#  print(interval)
#}

#interval = bootLS(dat$Weight, dat$City_MPG, B = 500)
#interval[2]


N = 1000

#' ## Part A - Simulation over 1000 iterations
intercept = numeric(N)
slope = numeric(N)
n = 500 # modify the sample size to be {50, 100, 200, 500}
set.seed(2022)
x = runif(n, -1, 1)
for (j in 1:N){
  err = rnorm(n, 0, 0.5)
  #err = rexp(n, rate = 0.5)
  y = 1 + 2 * x + err
  fit = lm(y ~ x)
  interval= bootLS(x,y, B=n)
  intercept[j] = interval[4]-interval[3]
  slope[j] = interval[1]-interval[2]
}

plot(intercept)

#' Q-Q plot 
qqnorm(intercept, main = paste("Q-Q Plot for Intercept / n =", n))
qqline(intercept, col = "red")

#' Q-Q plot for the slope to check for marginal normality
qqnorm(slope, main = paste("Q-Q Plot for Slope / n =", n))
qqline(slope, col = "red")  



sample_size = c(50,100,500, 1000)
conf_level = c(0.8,0.9, 0.95, 0.99)
N_sim = 10



for (n in sample_size){
  x = runif(n, -1, 1)
  n_err = rnorm(n, 0, 0.5)
  #e_err = rexp(n, rate = 0.5)
  y = 1 + 2 * x + e_err
  
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
    for (j in N_sim){
      fit = lm(y ~ x)
      print(bootLS(x,y,conf = conf, B=n))
      interval= bootLS(x,y,conf = conf, B=n)
      intercept_per_sim[j] = interval[4]-interval[3]
      slope_per_sim[j] = interval[2]-interval[1]
    }
    
    intercept_len_boot[i] = mean(intercept_per_sim)
    slope_len_boot[i] = mean(slope_per_sim)
    intercept_len_classic[i] = confint(fit, level = conf)[1,][2]-confint(fit, level = 0.9)[1,][1]
    slope_len_classic[i] = confint(fit, level = conf)[2,][2]-confint(fit, level = 0.9)[2,][1]
  } 
  
  
  
  df_1 <- data.frame(x = c(0.8,0.9,0.95,0.99), y1 = intercept_len_boot, y2 = intercept_len_classic)
  df_2 <- data.frame(x = c(0.8,0.9,0.95,0.99), y1 = slope_len_boot, y2 = slope_len_classic)
  
  ggplot(df_1, aes(x)) +
    geom_point(aes(y = y1), color = "red" ) +
    geom_point(aes(y = y2), color = "blue") +
    labs(title = "Multiple Scatterplots in One Plot", x = "X Axis", y = "Y Axis") +
    theme_bw()
  
  ggplot(df_2, aes(x)) +
    geom_point(aes(y = y1), color = "red") +
    geom_point(aes(y = y2), color = "blue") +
    labs(title = "Multiple Scatterplots in One Plot", x = "X Axis", y = "Y Axis") +
    theme_bw()  
}



n = 50
x = runif(n, -1, 1)
#n_err = rnorm(n, 0, 0.5)
e_err = rexp(n, rate = 0.5)
y = 1 + 2 * x + e_err

intercept_len_boot = rep(NA,4)
slope_len_boot = rep(NA,4)
intercept_len_classic = rep(NA,4)
slope_len_classic = rep(NA,4)

print(n)
for (i in 1:4){
  conf = conf_level[i]
  print(conf)
  fit = lm(y ~ x)
  print(bootLS(x,y,conf = conf, B=n))
  interval= bootLS(x,y,conf = conf, B=n)
  intercept_len_boot[i] = interval[4]-interval[3]
  slope_len_boot[i] = interval[2]-interval[1]
  intercept_len_classic[i] = confint(fit, level = conf)[1,][2]-confint(fit, level = 0.9)[1,][1]
  slope_len_classic[i] = confint(fit, level = conf)[2,][2]-confint(fit, level = 0.9)[2,][1]
} 



df_1 <- data.frame(x = c(0.8,0.9,0.95,0.99), y1 = intercept_len_boot, y2 = intercept_len_classic)
df_2 <- data.frame(x = c(0.8,0.9,0.95,0.99), y1 = slope_len_boot, y2 = slope_len_classic)

ggplot(df_1, aes(x)) +
  geom_point(aes(y = y1), color = "red" ) +
  geom_point(aes(y = y2), color = "blue") +
  labs(title = "Multiple Scatterplots in One Plot", x = "X Axis", y = "Y Axis") +
  theme_bw()

ggplot(df_2, aes(x)) +
  geom_point(aes(y = y1), color = "red") +
  geom_point(aes(y = y2), color = "blue") +
  labs(title = "Multiple Scatterplots in One Plot", x = "X Axis", y = "Y Axis") +
  theme_bw()


?rexp

# Set up simulation parameters
set.seed(123)
num_sims <- 1000
sample_sizes <- c(10, 30, 50, 100)
distributions <- list("Normal" = rnorm, "Exponential" = rexp)

# Define studentized bootstrap confidence interval function
studentized_boot_ci <- function(x, B) {
  n <- length(x)
  boot_mean <- replicate(B, mean(sample(x, n, replace = TRUE)))
  boot_se <- sd(boot_mean)
  t_star <- qt(1 - 0.025, n - 1)
  lower <- mean(x) - t_star * boot_se
  upper <- mean(x) + t_star * boot_se
  return(c(lower, upper))
}

# Run simulation
results <- list()
for (dist_name in names(distributions)) {
  dist <- distributions[[dist_name]]
  for (n in sample_sizes) {
    lengths <- c()
    coverages <- c()
    if (n < 2) {
      # Skip this scenario if the sample size is too small
      next
    }
    for (i in 1:num_sims) {
      x <- dist(n, 0)
      # Compute student confidence interval
      if (n < 3) {
        t_ci <- c(NA, NA)
      } else {
        t_ci <- t.test(x)$conf.int
      }
      # Compute studentized bootstrap confidence interval
      boot_ci <- studentized_boot_ci(x, 1000)
      # Record results
      lengths <- c(lengths, diff(boot_ci), diff(t_ci))
      coverages <- c(coverages, !is.na(t_ci[1]) && t_ci[1] < 0 && t_ci[2] > 0,
                     boot_ci[1] < 0 && boot_ci[2] > 0)
    }
    # Record results for this scenario
    results[[paste(dist_name, n, sep = "_")]] <- list(
      method = c(rep("Studentized Bootstrap", num_sims), rep("Student", num_sims)),
      length = lengths,
      coverage = coverages,
      avg_length = c(mean(lengths[1:num_sims]), mean(lengths[(num_sims+1):(2*num_sims)])),
      coverage_rate = c(mean(coverages[1:num_sims]), mean(coverages[(num_sims+1):(2*num_sims)]))
    )
  }
}

# Plot results
library(ggplot2)
library(gridExtra)
plots <- list()
for (name in names(results)) {
  df <- data.frame(results[[name]])
  df$method <- factor(df$method, levels = c("Studentized Bootstrap", "Student"))
  plot1 <- ggplot(df, aes(x = method, y = length)) +
    geom_boxplot() +
    ggtitle(paste("Distribution of Interval Lengths -", name))
  plot2 <- ggplot(df, aes(x = method, y = coverage)) +
    geom_boxplot() +
    ggtitle(paste("Distribution of Coverage Rates -", name))
  plots[[name]] <- grid.arrange(plot1, plot2, ncol = 2)
}
grid.arrange(grobs = plots, ncol = 2)
