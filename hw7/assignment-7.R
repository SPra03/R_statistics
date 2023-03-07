
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
model <- glm(good ~ 1, data = data, family = binomial)

# Perform forward selection with AIC
step(model, scope = ~ distance + change + elap30 + PAT + type + field + wind + 
                      week, direction = "forward")

model = glm(formula = good ~ distance + PAT + change + wind, family = binomial, data = data)
# Print the final model summary
summary(model)

# 1c)
model = glm(formula = good ~ distance + PAT + change + wind, family = binomial, data = data)

#calculating for good
# distance = (0.5 - β0 - β2(PAT_mean) - β3(change_mean) - β4(wind_mean)) / β1


distance <- (0.5 - 4.751 - 1.229 * mean(data$PAT) - (-0.3350) * mean(data$change) - (-0.523) * mean(data$wind)) / (-0.087)
distance
