# Linear Regression with Cars

# For this analysis, we will use the cars dataset that comes 
# with R by default. cars is a standard built-in dataset, 
# that makes it convenient to demonstrate linear regression 
# in a simple and easy to understand fashion. 
head(cars)

###############################################################################################
# Scatter Plot
scatter.smooth(x=cars$speed, 
               y=cars$dist, 
               main="Dist ~ Speed")  # scatterplot
# The scatter plot along with the smoothing line above suggests 
# a linearly increasing relationship between the ‘dist’ and 
# ‘speed’ variables. This is a good thing, because, one of the 
# underlying assumptions in linear regression is that the 
# relationship between the response and predictor variables 
# is linear and additive.

###############################################################################################
# BoxPlot – Check for outliers
par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(cars$speed, 
        main="Speed", 
        sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'
boxplot(cars$dist, 
        main="Distance", 
        sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'
# Generally, any datapoint that lies outside the 
# 1.5 * interquartile-range (1.5 * IQR) is considered an outlier, 
# where, IQR is calculated as the distance between the 25th 
# percentile and 75th percentile values for that variable.

###############################################################################################
# Density plot – Check if the response variable is close to normality
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(cars$speed), main="Density Plot: Speed", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(cars$speed), 2)))  # density plot for 'speed'
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main="Density Plot: Distance", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(cars$dist), 2)))  # density plot for 'dist'
polygon(density(cars$dist), col="red")

###############################################################################################
# Correlation
# Correlation is a statistical measure that suggests the level of linear 
# dependence between two variables, that occur in pair – just like what 
# we have here in speed and dist. Correlation can take values 
# between -1 to +1. If we observe for every instance where speed increases, 
# the distance also increases along with it, then there is a high positive 
# correlation between them and therefore the correlation between them will 
# be closer to 1. The opposite is true for an inverse relationship, 
# in which case, the correlation between the variables will be close to -1.
# A value closer to 0 suggests a weak relationship between the variables. 
# A low correlation (-0.2 < x < 0.2) probably suggests that much of 
# variation of the response variable (Y) is unexplained by the 
# predictor (X), in which case, we should probably look for 
# better explanatory variables.
cor(cars$speed, cars$dist)

###############################################################################################
# Build Linear Model
# Now that we have seen the linear relationship pictorially in the 
# scatter plot and by computing the correlation, lets see the syntax 
# for building the linear model. The function used for building linear 
# models is lm(). The lm() function takes in two main arguments, 
# namely: 1. Formula 2. Data. The data is typically a data.frame 
# and the formula is a object of class formula. 
linearMod <- lm(dist ~ speed, data=cars) 
print(linearMod)
# Now that we have built the linear model, we also have established 
# the relationship between the predictor and response in the form 
# of a mathematical formula for Distance (dist) as a function 
# for speed. For the above output, you can notice the ‘Coefficients’ 
# part having two components: Intercept: -17.579, speed: 3.932 
# These are also called the beta coefficients. In other words, 
# dist = Intercept + (β ∗ speed)
# => dist = −17.579 + 3.932∗speed

###############################################################################################
# Linear Regression Diagnostics
# Now the linear model is built and we have a formula that we 
# can use to predict the dist value if a corresponding speed 
# is known. Is this enough to actually use this model? NO! 
# Before using a regression model, you have to ensure that it 
# is statistically significant. How do you ensure this? Lets 
# begin by printing the summary statistics for linearMod.
summary(linearMod) 


###############################################################################################
# The p Value: Checking for statistical significance

# The summary statistics above tells us a number of things. 
# One of them is the model p-Value (bottom last line) and 
# the p-Value of individual predictor variables (extreme right 
# column under ‘Coefficients’). The p-Values are very important 
# because, We can consider a linear model to be statistically 
# significant only when both these p-Values are less that 
# the pre-determined statistical significance level, which is ideally 0.05. 
# This is visually interpreted by the significance stars at the 
# end of the row. The more the stars beside the variable’s 
# p-Value, the more significant the variable.

# Null and alternate hypothesis
# When there is a p-value, there is a hull and alternative 
# hypothesis associated with it. In Linear Regression, the Null 
# Hypothesis is that the coefficients associated with the 
# variables is equal to zero. The alternate hypothesis is 
# that the coefficients are not equal to zero (i.e. there 
# exists a relationship between the independent variable 
# in question and the dependent variable).

# t-value
# We can interpret the t-value something like this. A larger 
# t-value indicates that it is less likely that the coefficient 
# is not equal to zero purely by chance. 
# So, higher the t-value, the better.
# Pr(>|t|) or p-value is the probability that you get a t-value 
# as high or higher than the observed value when the Null 
# Hypothesis (the β coefficient is equal to zero or that 
# there is no relationship) is true. So if the Pr(>|t|) is low, 
# the coefficients are significant (significantly different from 
# zero). If the Pr(>|t|) is high, the coefficients are not significant.

# What this means to us? when p Value is less than significance 
# level (< 0.05), we can safely reject the null hypothesis that 
# the co-efficient β of the predictor is zero. In our case, 
# linearMod, both these p-Values are well below the 0.05 threshold, 
# so we can conclude our model is indeed statistically significant.
# It is absolutely important for the model to be statistically 
# significant before we can go ahead and use it to predict (or estimate) 
# the dependent variable, otherwise, the confidence in predicted values 
# from that model reduces and may be construed as an event of chance.

###############################################################################################
# How to calculate the t Statistic and p-Values?
  
modelSummary <- summary(linearMod)  # capture model summary as an object
modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["speed", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["speed", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

###############################################################################################
# R-Squared and Adj R-Squared
# What R-Squared tells us is the proportion of variation in the 
# dependent (response) variable that has been explained by this model.
# We don’t necessarily discard a model based on a low R-Squared value. 
# Its a better practice to look at the AIC and prediction accuracy on 
# validation sample when deciding on the efficacy of a model.
# Now thats about R-Squared. What about adjusted R-Squared? 
# As you add more X variables to your model, the R-Squared 
# value of the new bigger model will always be greater than that 
# of the smaller subset. This is because, since all the variables 
# in the original model is also present, their contribution to 
# explain the dependent variable will be present in the super-set 
# as well, therefore, whatever new variable we add can only add 
# (if not significantly) to the variation that was already 
# explained. It is here, the adjusted R-Squared value comes 
# to help. Adj R-Squared penalizes total value for the number 
# of terms (read predictors) in your model. Therefore when 
# comparing nested models, it is a good practice to look 
# at adj-R-squared value over R-squared.

###############################################################################################
# Standard Error and F-Statistic
# Both standard errors and F-statistic are measures of goodness of fit.

###############################################################################################
# AIC and BIC
# The Akaike’s information criterion - AIC (Akaike, 1974) and the 
# Bayesian information criterion - BIC (Schwarz, 1978) are measures 
# of the goodness of fit of an estimated statistical model and 
# can also be used for model selection. Both criteria depend 
# on the maximized value of the likelihood function L for 
# the estimated model.

# The AIC is defined as:
# AIC = (−2) × ln(L) + (2×k)
# where, k is the number of model parameters and the BIC is 
# defined as:

# BIC = (−2) × ln(L) + k × ln(n)
# where, n is the sample size.

# For model comparison, the model with the lowest AIC and BIC 
# score is preferred.
AIC(linearMod)  # AIC => 419.1569
BIC(linearMod)  # BIC => 424.8929


