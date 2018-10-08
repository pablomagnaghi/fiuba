library(MASS)
library(ggplot2)

names(Boston)

str(boston)

# using the lm() function to fit a simple linear regression model, 
# with medv as the response and lstat as the predictor
# The basic lm() syntax is lm(y∼x,data), where y is the response, 
# x is the predictor, and data is the data set 
# in which these two variables are kept.

lm.fit = lm(medv ~ lstat, data=boston)
class(lm)
str(lm.fit)
lm.fit
lm.fit$residuals
lm.fit$coefficients
summary(lm.fit)

names(lm.fit)
lm.fit[[1]]
coef(lm.fit)

# In order to obtain a confidence interval for the coefficient estimates, 
# we can use the confint() command.
confint(lm.fit, level = 0.95)
summary(lm.fit)$coefficients

# We will now plot medv and lstat along with the least 
# squares regression line using the plot() and abline() functions.
plot(medv ~ lstat, data = Boston)
abline(lm.fit)
plot(boston$lstat, boston$medv)
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")

plot(medv ~ lstat, data = Boston, pch = 20)

plot(medv ~ lstat, data = Boston, pch = "+")

ggplot(data = Boston, aes(x = lstat, y = medv)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  theme_bw()
ggplot(data = Boston, aes(x = lstat, y = medv)) +
  geom_point() + 
  geom_smooth(method = "lm") + 
  theme_bw()

# second sample
lm.fit2 = lm(medv + lstat ~age, data=boston)
summary(lm.fit2)
