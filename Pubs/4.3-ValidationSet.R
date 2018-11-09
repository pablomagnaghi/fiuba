# Simple validation set

# El primer paso del metodo validation set requiere dividir aleatoriamente las
# observaciones disponibles en training set y test set. En R se pueden conseguir
# una division aleatoria empleando indices aleatorios.

set.seed(1)
datos <- na.omit(Hitters)
# Se emplean como training aproximadamente 2/3 de las observaciones
# Se seleccionan indices aleatorios que forman el training dataset
train <- sample(x = 1:263, size = 180, replace = FALSE)
# Los restantes forman el test dataset

# Empleando el training dataset se identifica el mejor modelo para cada posible
# tamaño. En este paso pueden estudiarse todas las posibles combinaciones de
# predictores (best subset selection) o solo parte de ellos (stepwise selection).

library(leaps)
# En este caso se emplea forward stepwise selection, pero podrían emplearse
# los otros metodos.
# Dado que hay 19 predictores disponibles y se quieren estudiar todos los
# posibles tamaños de modelo, nvmax=19
mejores_modelos <- regsubsets(Salary~., data = datos[train,], nvmax = 19,
                              method = "forward")
# Mejores_modelos almacena los 19 modelos seleccionados

# Como resultado del proceso de evaluacion de regsubsets() se han seleccionado
# 19 modelos, el mejor para cada tamaño. Para poder compararlos se procede a
# estimar el validation test error empleando las observaciones que se han
# excluido del training y que se han designado como test.

Se genera un vector que almacenará el test-error de cada modelo
validation_error <- rep(NA, 19)
# No existe una función predefinida para obtener el validation-test-error, se
# tiene que programar. Para facilitar el proceso es conveniente trabajar con
# matrices. Los valores de los predictores de cada observación se almacenan en
# forma de matriz. La función  model.matrix() devuelve una matriz formada con
# los predictores indicados en la fórmula e introduce para todas las
# observaciones un intercept con valor 1, así al multiplicar por los
# coeficientes se obtiene el valor de la predicción (producto matricial).
test_matrix <- model.matrix(Salary~., data = datos[-train, ])

# Para cada uno de los modelos almacenados en la variable mejores modelos
for (i in 1:19) {
   # Se extraen los coeficientes del modelo
   coeficientes <- coef(object = mejores_modelos, id = i)

   # Se identifican los predictores que forman el modelo y se extraen de la
   # matriz modelo
   predictores <- test_matrix[, names(coeficientes)]

   # Se obtienen las predicciones mediante el producto matricial de los
   # predictores extraídos y los coeficientes del modelo
   predicciones <- predictores %*% coeficientes

   # Finalmente se calcula la estimación del test error como el promedio de
   # los residuos al cuadrado (MSE)
   validation_error[i] <- mean((datos$Salary[-train] - predicciones)^2)
}

which.min(validation_error)

# Gráfico
p <- ggplot(data = data.frame(n_predictores = 1:19,
                              Estimacion_MSE = validation_error),
            aes(x = n_predictores, y = Estimacion_MSE)) +
    geom_line() +
    geom_point()

# Se identifica en rojo el mínimo
p <- p + geom_point(aes(x = n_predictores[which.min(validation_error)],
                        y = validation_error[which.min(validation_error)]),
                        colour = "red", size = 3)

p <- p +  scale_x_continuous(breaks = c(0:19)) +
          theme_bw() +
          labs(title = 'validation MSE vs número de predictores',
               x =  'número predictores')
p


# El modelo con menor validation test error es el que contiene 5 predictores.
# Se puede considerar que 5 es el número de predictores que debe contener el
# modelo para alcanzar la mayor precision predictiva. Por ultimo, una vez
# identificada la cantidad de predictores que debe contener el modelo y para
# obtener las estimaciones de los coeficientes de la forma más precisa posible,
# se vuelven a ajustar los posibles modelos con 5 predictores empleando todas
# las observaciones (training + test). Es importante volver a seleccionar el
# mejor de entre los modelos con 5 predictores ya que al incluir nuevas
# observaciones puede que esos 5 predictores no sean los mismos que los
# seleccionados previamente con el training dataset.

mejores_modelos <- regsubsets(Salary~., data = datos, nvmax = 19,
                              method = "forward")
coef(object = mejores_modelos, id = 5)

# A pesar de que la validación simple es un método muy sencillo, tiene el
# inconveniente de sufrir mucha varianza. Los resultados pueden variar mucho
# dependiendo de como se repartan las observaciones entre tranining set y
# test set. Por ejemplo, si se establece set.seed(1388), el reparto generado
# da como resultado que el mejor modelo es el formado por 7 predictores.

set.seed(1388)
datos <- na.omit(Hitters)
train <- sample(x = 1:263, size = 180, replace = FALSE)
mejores_modelos <- regsubsets(Salary~., data = datos[train,], nvmax = 19,
                              method = "forward")
validation_error <- rep(NA, 19)
test_matrix <- model.matrix(Salary~., data = datos[-train, ])
for (i in 1:19) {
    coeficientes <- coef(object = mejores_modelos, id = i)
    predictores <- test_matrix[, names(coeficientes)]
    predicciones <- predictores %*% coeficientes
    validation_error[i] <- mean((datos$Salary[-train] - predicciones)^2)
}

which.min(validation_error)

p <- ggplot(data = data.frame(n_predictores = 1:19,
                              Estimacion_MSE = validation_error),
            aes(x = n_predictores, y = Estimacion_MSE)) +
     geom_line() +
     geom_point()
p <- p + geom_point(aes(x = n_predictores[which.min(validation_error)],
                        y = validation_error[which.min(validation_error)]),
                        colour = "red", size = 3)

p <- p +  scale_x_continuous(breaks = c(0:19)) +
    theme_bw() +
    labs(title = 'validation MSE vs número de predictores',
         x =  'número predictores')
p
