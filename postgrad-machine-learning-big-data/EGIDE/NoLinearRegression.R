install.packages("ISLR")
library(ISLR)
attach(Wage)

# Polynomial Regression and Step Functions
# Polinomio de grado 4 sobre un solo predictor age

fit=lm(wage ~ poly(age, 4), data = Wage)
# el objeto fit es una lista de clase "lm"

# le aplico la funcion summary al objeto lm

summary(fit)
# el objeto summary es de m�s f�cil acceso y brinda informacion mas sintetizada

coef(summary(fit))
# al ver los p values vemos que seria suficiente con los que tiene p significativos
# AL incluir poly estamos generando un ajuste en base a polinomios ortogonales, ello implica que NO HAY
# CORRELACION (O COVARIANZA) entre las variables, o monomios.
# vamos a graficar EL MODELO de regresion polinomial

agelims = range(age)
age.grid <- seq(agelims[1], agelims[2])

# voy a correr el modelo sobre esta malla de edades, incluyendo los desvios estandares de cada prediccion
# o valor ajustado
preds=predict(fit,newdata=list(age=age.grid),se=TRUE)

# ahora preparo una matriz con dos columnas que sean las dos bandas un intervalo de confianza a 2 sigmas
se.bands <- cbind((preds$fit - 2 * preds$se.fit),
                  (preds$fit + 2 * preds$se.fit),
                  (preds$fit - 3 * preds$se.fit),
                  (preds$fit + 3 * preds$se.fit))

# hago el grafico con todos los datos del data set wages vs age
plot(age, wage, col = "grey")

# Voy a dibujar arriba el polinomio de grado 4 que calculamos sobre la grilla
lines(age.grid, preds$fit, lwd = 4, col = "red")

# ahora le agrego las l�neas con los intervalos de confianza a dos sigmas
matlines(age.grid, se.bands, col  =c("blue"), lty = 3)


# ANOVA
# Veamos algunos modelos anidados

fita = lm(wage ~ education, data = Wage)
fitb = lm(wage ~ education + age, data = Wage)
fitc = lm(wage ~ education + poly(age,2), data = Wage)
fitd = lm(wage ~ education + poly(age,3), data = Wage)

# corro el anova vs el modelo con eduication (el cual es
# el campo mas simple incuidos en todos los demas)
anova(fita, fitb, fitc, fitd)

#############
#  SPLINES
############

library(splines)
fit <- lm(wage ~ bs(age, knots = c(25,40,60)), data = Wage)

# corro sobre los datos de entrenamiento
pred <- predict(fit, list(age = sort(age)))


# grafico con los datos de edad wage
plot(age, wage, col = "blue")

# dibuja los splines!!!!!
lines(sort((age)), pred, col = "red", lw =3)

# dibujo los limites de los splines
abline(v = c(25,40,60), col="green")

summary(fit)

# GENERALIZE ADDITIVE MODEL
install.packages("gam")
library(gam)

gam1 <- gam(wage ~ s(age, df=4) + s(year, df=4) + education, data=Wage)
par(mfrow = c(1,3))
plot(gam1, se=T)

gam2 <- gam(wage ~ s(age, df=4) + year + education, data=Wage)
plot(gam2, se=T)


# vamos a compararlos con un anova
anova(gam2, gam1)

# el p value 0.35 nos dice que no es necesario hacer no lineal

summary(gam1)
summary(gam2)

# splines naturales combinados pero en vez de GAM uso lm en regresion multiple
lm1 <- lm(wage ~ ns(age, df=4) + ns(year, df=4) + education, data=Wage)

# puedo usar la metodo de graficacion de GAM
par(mfrow=c(2,2))
plot.Gam(lm1, se=T)
