# La Regresión Polinomial, aunque permite describir relaciones no lineales, se trata de un
# modelo lineal en el que se incorporan nuevos predictores elevando el valor de los ya
# existentes a diferentes potencias.

# Cuando se intenta predecir el valor de la vivienda en función del estatus de la población,
# el modelo lineal generado no se ajusta del todo bien debido a que las observaciones muestran
# una relación entre ambas variables con cierta curvatura.
require(MASS)
require(ISLR)
data("Boston")
attach(Boston)

modelo_simple <- lm(data = Boston, formula = medv ~ lstat)


plot(x = lstat, y = medv, main = "medv vs lstat", pch = 20, col = "grey30")
abline(modelo_simple, lwd = 3, col = "red")

# La curvatura descrita apunta a una posible relación cuadrática, por lo que un polinomio de
# segundo grado podría capturar mejor la relación entre las variables. En R se pueden generar
# modelos de regresión polinómica de diferentes formas:

# Identificando cada elemento del polinomio:
# modelo_pol2 <- lm(formula = medv ~ lstat + I(lstat^2), data = Boston)
# El uso de I() es necesario ya que el símbolo ^ tiene otra función dentro de las formula de R.

#Con la función poly(): lm(formula = medv ~ poly(lstat, 2), data = Boston)

modelo_pol2 <- lm(formula = medv ~ poly(lstat, 2), data = Boston)
summary(modelo_pol2)

# El p-value próximo a 0 del predictor cuadrático de lstat indica que contribuye a mejorar el modelo.

attach(Boston)
plot(x = lstat, y = medv, main = "medv vs lstat", pch = 20, col = "grey30")
points(lstat, fitted(modelo_pol2), col = 'red', pch = 20)

# A la hora de comparar dos modelos se pueden evaluar sus R2. En este caso el modelo cuadrático es
# capaz de explicar un 64% de variabilidad frente al 54% del modelo lineal.

# Cuando se comparan dos modelos anidados (el modelo de menor tamaño está formado por un subset
# de predictores del modelo mayor), se puede saber si el modelo mayor aporta una mejora sustancial
# estudiando si los coeficientes de regresión de los predictores adicionales son distintos a cero.
# El test estadístico empleado para hacerlo es el ANOVA.

# La hipótesis a contrastar es que todos los coeficientes de regresión de los predictores
# adicionales son igual a cero, frente a la hipótesis alternativa de que al menos uno es distinto

# Dado que un polinomio de orden n siempre va a estar anidado a uno de orden n+1, se pueden
# comparar modelos polinómicos dentro un rango de grados haciendo comparaciones secuenciales.
anova(modelo_simple, modelo_pol2)

# El p-value obtenido para el estadístico F confirma que el modelo cuadrático es superior.

