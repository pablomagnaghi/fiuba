# El set de datos Boston contiene información sobre viviendas de la ciudad de Boston
# así como información sobre el barrio en el que se encuentran. Se pretende generar
# modelo basado en árboles de regresión que permita predecir el precio medio de una
# vivienda (medv) en función de las variables disponibles. Para mejorar la capacidad
# predictiva del modelo se recurre al proceso de bagging.

# La función randomForest() del paquete randomForest permite ajustar modelos ensemble
# basados en árboles mediante el proceso de bagging y random forest. La misma función
# puede emplearse para ambos procesos, ya que, como se describe más adelante, random
# forest es un caso particular de bagging. Para obtener un modelo básico de bagging
# se indica que el número de predictores empleados por los árboles es igual al número
# de predictores disponibles (mtry = p).

library(MASS)
library(randomForest)
data("Boston")
ncol(Boston)


# El data set tiene 14 columnas: 1 variables respuesta y 13 predictores.
# 13 es el valor que se tiene que indicar en el argumento mtry.
set.seed(1)
train <- sample(1:nrow(Boston), size = nrow(Boston)/2)
modelo_bagging <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 13)
modelo_bagging

# Al imprimir el resultado por pantalla se muestra: el número de árboles generados en el
# proceso, el número de predictores evaluados en cada división (en este caso todos los
# disponibles), el Mean of Squared Residuals y el porcentaje de varianza que el modelo
# es capaz de explicar. El término Mean of Squared Residuals hace referencia al out-of-bag MSE,
# que como se ha indicado anteriormente, puede interpretarse como una estimación del test error.

# Para comprobar la capacidad del modelo al aplicarlo a nuevos datos, se predicen las
# observaciones no utilizadas en el ajuste del modelo.
predicciones <- predict(object = modelo_bagging, newdata = Boston[-train,])
test_mse     <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del modelo obtenido por bagging es:",
      round(test_mse,2))

# El test-MSE asociado al árbol de regresión obtenido por bagging es de 13.18, casi la mitad
# del obtenido anteriormente con un árbol generado por pruning (23). Esto pone de manifiesto
# la superioridad de los modelos obtenidos por bagging en comparación a los obtenidos por pruning.
# Gracias a la estimación del out-of-bag MSE y su similitud con el leave-one-out error,
# realmente no es necesario dividir las observaciones disponibles en training y test para
# evaluar el modelo. Se pueden emplear todas ellas en el proceso de bagging y considerar
# el out-of-bag MSE como una estimación de test-MSE. Esto es una ventaja importante,
# sobretodo, cuando no se dispone de muchas observaciones. Sin embargo, para comparar
# un modelo de bagging con otros modelos distintos, sí se necesita # disponer de un set
# de test para que las comparaciones sean estrictamente válidas.
modelo_bagging <- randomForest(medv ~ ., data = Boston, mtry = 13, ntree = 500,
                               importance = TRUE)
modelo_bagging

# Como se ha comentado en las definiciones teóricas anteriores, una de las principales
# ventajas de los modelos de bagging es su capacidad para cuantificar la importancia
# de los predictores. La función  importance() permite extraer esta informacion de
# objetos creados mediante randomforest(). El resultado es una matriz que contiene
# dos medidas de importancia: %IncMSE y IncNodePurity.
install.packages('tidyverse')
library(tidyverse)
library(ggpubr)
importancia_pred <- as.data.frame(importance(modelo_bagging, scale = TRUE))
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
p1 <- ggplot(data = importancia_pred, aes(x = reorder(variable, `%IncMSE`),
                                          y = `%IncMSE`,
                                          fill = `%IncMSE`)) +
    labs(x = "variable", title = "Reducción de MSE") +
    geom_col() +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")

p2 <- ggplot(data = importancia_pred, aes(x = reorder(variable, IncNodePurity),
                                          y = IncNodePurity,
                                          fill = IncNodePurity)) +
    labs(x = "variable", title = "Reducción de pureza") +
    geom_col() +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")
ggarrange(p1, p2)

# Los resultados muestran que, teniendo en cuenta todos los árboles generados en el
# proceso de bagging, las variables lstat y rm son con diferencia las más importantes
# en el modelo.
# Por defecto, la funcion randomForest() crea 500 árboles. Este valor se puede modificar
# mediante el argumento ntree, siendo recomendable no sustituirlo por valores muy
# pequeños para que todas las observaciones participen un mínimo de veces en el proceso.
# Una forma de identificar el numero óptimo de arboles es representando la evolución del
# out-of-bag-error en función del número de árboles

oob_mse <- data.frame(oob_mse = modelo_bagging$mse,
                      arboles = seq_along(modelo_bagging$mse))
ggplot(data = oob_mse, aes(x = arboles, y = oob_mse )) +
  geom_line() +
  labs(title = "Evolución del out-of-bag-error vs número árboles",
       x = "nº árboles") +
  theme_bw()

# En este caso, a partir de aproximadamente 100 árboles la precisión del modelo se estabiliza.
# De hecho, tanto el out-of-bag-MSE como el test error que se obtienen si en lugar de 500
# arboles se emplean solo 100 es casi el mismo.

modelo_bagging <- randomForest(medv ~ ., data = Boston, subset = train,
                               mtry = 13, ntree = 100)
modelo_bagging

# Calculo del test error
predicciones <- predict(object = modelo_bagging, newdata = Boston[-train,])
test_mse <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del modelo obtenido por bagging es:",
      round(test_mse,2))
      
