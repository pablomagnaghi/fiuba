# Multiple linear regression with Boston dataset
library(MASS)
modelo_multiple <- lm(formula = medv ~ ., data = Boston)
# Tambien se pueden especificar una a una 
summary(modelo_multiple)

# El p-value obtenido para el estadistico F es muy chuico (< 2.2e-16) 
# lo que indica que al menos uno de los predictores introducidos en el 
# modelo esta relacionado con la variable respuesta medv. 
# El modelo es capaz de explicar el 74% de la variabilidad 
# observada en el precio de la vivienda (R2=0.74)

# En el summary se puede observar que algunos predictores tienen 
# p-values muy altos, sugiriendo que no contribuyen al modelo por 
# lo que deben ser excluidos, por ejemplo age e indus. La exclusion 
# de predictores basandose en p-values no es aconsejable, en su lugar 
# se recomienda emplear metodos de best subset selection, stepwise 
# selection (forward, backward e hybrid) o Shrinkage/regularization

step(modelo_multiple, direction = "both", trace = 0)

# La seleccion de predictores empleando stepwise selection (hybrid/doble) 
# ha identificado como mejor modelo el formado por los predictores crim, 
# zn, chas, nox, rm, dis, rad, tax, ptratio, black, lstat.


modelo_multiple <- lm(formula = medv ~ crim + zn + chas +  nox + rm +  dis +
                        rad + tax + ptratio + black + lstat, data = Boston)
# Tambien se pueden indicar todas las variables de un data.frame y exluir algunas
# modelo_multiple <- lm(formula = medv~. -age -indus, data = Boston)
summary(modelo_multiple)

# En los modelos de regresion lineal con multiples predictores, 
# ademas del estudio de los residuos vistos en el modelo simple, 
# es necesario descartar colinealidad o multicolinealidad entre variables.

par(mfrow = c(1,2))
plot(modelo_multiple)

par(mfrow = c(1,1))

# Para la colinealidad se recomienda calcular el coeficiente de 
# correlacion entre cada par de predictores incluidos en el modelo:

library(corrplot)
require(corrplot)
corrplot.mixed(corr = cor(Boston[,c("crim", "zn", "nox", "rm", "dis", "rad", 
                                    "tax", "ptratio", "black", "lstat", "medv")],
                          method = "pearson"))

# El analisis muestra correlaciones muy altas entre los 
# predictores rad y tax (positiva) y entre dis y nox (negativa).


attach(Boston)
par(mfrow = c(2,2))
plot(x = tax, y = rad, pch = 20)
plot(x = tax, y = nox, pch = 20)
plot(x = dis, y = nox, pch = 20)
plot(x = medv, y = rm, pch = 20)

par(mfrow = c(1,1))

# Si la correlacion es alta y por lo tanto las variables aportan informacion redundante, 
# es recomendable analizar si el modelo mejora o no empeora excluyendo alguno de estos predictores.

# Para el estudio de la multicolinealidad una de las medidas mas 
# utilizadas es el factor de inflacion de varianza VIF. Puede 
# calcularse mediante la funcion vif() del paquete car.

require(car)
vif(modelo_multiple)
# Los indices VIF son bajos o moderados, valores entre 5 y 10 indican
# posibles problemas y valores mayores o iguales a 10 se consideran muy problematicos. 



