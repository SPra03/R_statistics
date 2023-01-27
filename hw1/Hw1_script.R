library(ggplot2)

load("04cars.rda")
head(dat)
tmp = dat[,c(13,15)]
head(tmp)
tmp = tmp[complete.cases(tmp),] # extracts complete cases
tmp = as.data.frame(tmp)
names(tmp) = c("hp","mpg")
head(tmp)

confband<-function(x, y, conf=0.95)
{
  linear_model = lm(y ~ x)
  
  ggplot(tmp,aes(x, y)) +geom_point() +  geom_smooth(linear_model) 
}
#data(tmp)
confband(tmp$hp, tmp$mpg)
