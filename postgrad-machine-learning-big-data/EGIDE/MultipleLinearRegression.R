# Multiple Regression
library(MASS)

ls.fit <- lm(medv ~ lstat + age, data = Boston)
summary(ls.fit)

# The Boston data set contains 13 variables, and so it 
# would be cumbersome to have to type all of these in order 
# to perform a regression using all of the predictors. 
# Instead, we can use the following short-hand:

ls.fit <- lm(medv ~ ., data = Boston)
summary(ls.fit)

# The vif() function, part of the  car package, can be 
# used to compute variance inflation factors. Most VIF's 
# are low to moderate for this data

library(car)
vif(ls.fit)


# For example, in the above regression output,  age has a 
# high p-value. So we may wish to run a regression 
# excluding this predictor. The following syntax results 
# in a regression using all predictors except age.

ls.fit1 <- lm(medv ~ . - age, data = Boston)
summary(ls.fit1)

# Or
ls.fit2 <- update(ls.fit, .~. - age)
summary(ls.fit2)

