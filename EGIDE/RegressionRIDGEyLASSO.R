install.packages("glmnet")
library(glmnet)
library(ISLR)

summary(Hitters)

# esta libreria no usa el objeto lenguaje de formulas para los modelos,
# asi que generaremos la matriz

Hitters <- na.omit(Hitters)


names(Hitters)

# con todos los campos

x_t <- model.matrix(Salary ~., data=Hitters)
y <- Hitters$Salary

fit.ridge <- glmnet(x_t,y, alpha=0)

# el parametro alpha=0 es RIDGE Y alpha=1 es LASSO
fit.ridge <- glmnet(x_t,y, alpha=0)

# grafiquemos como evolucionan los parametros con el lambda
plot(fit.ridge, xvar="lambda", label="TRUE" )

preds <- predict(fit.ridge, x_t)

str(x_t)
class(x_t)
colnames(x_t)

# con menos predictores
formula = as.formula(Salary ~ AtBat + Hits + HmRun + Runs)

x <- model.matrix(formula , data=Hitters)
y <- Hitters$Salary

# el parametro alpha=0 es RIDGE Y alpha=1 es LASSO
fit.ridge_menos <- glmnet(x, y, alpha=0)

# grafiquemos como evolucionan los parametros con el lambda
plot(fit.ridge_menos, xvar="lambda",label="TRUE")


coef(fit.ridge_menos, s=1)
coef(fit.ridge_menos, s=100)

coef(fit.ridge_menos,s=seq(1,10000, 1000))


str(x)
class(x)
head(x)

# con un dato nuevo
x_m <- data.frame(Intercept=1, AtBat=325, Hits=89, HmRun=18, Runs=26, Salary=2555)
ncol(x_m)

X_form_n <- model.matrix(formula, data=x_m)

predict(fit.ridge_menos, newx=X_form_n, s=1)
coef(fit.ridge_menos, s=1)
