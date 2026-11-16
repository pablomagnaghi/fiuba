# La cuantificación del contenido en grasa de la carne pude hacerse mediante técnicas de
# analítica química, sin embargo, este proceso es costoso en tiempo y recursos. Una posible
# alternativa para reducir costes y optimizar tiempo es emplear un espectrofotómetro (instrumento
# capaz de detectar la absorbancia que tiene un material a diferentes tipos de luz en función
# de sus características). Para comprobar su efectividad se mide el espectro de absorbancia
# de 100 longitudes de onda en 215 muestras de carne, cuyo contenido en grasa se obtiene
# también por análisis químico para poder comparar los resultados. El set de datos meatspec
# del paquete faraway contiene toda la información.
library(faraway)
data(meatspec)
dim(meatspec)

# El set de datos contiene 101 columnas. Las 100 primeras, nombradas como V1, …, V100 recogen el
# valor de absorbancia para cada una de las 100 longitudes de onda analizadas, y la columna fat
# el contenido en grasa medido por técnicas químicas.

# Para poder evaluar la capacidad predictiva del modelo, se dividen las observaciones disponibles
# en dos grupos: uno de entrenamiento para ajustar el modelo (80% de los datos) y uno de test
# (20% de los datos).
training <- meatspec[1:172, ]
test     <- meatspec[173:215, ]

# En primer lugar se ajusta un modelo incluyendo todas las longitudes de onda como predictores.

modelo <- lm(fat ~ ., data = training)
summary(modelo)

# El valor R2ajustado obtenido es muy alto (0.9928) lo que indica que el modelo es capaz de
# predecir con gran exactitud el contenido en grasa de las observaciones con las que se ha
# entrenado. El hecho de que el modelo en conjunto sea significativo (p-value: < 2.2e-16),
# pero que muy pocos de los predictores lo sean a nivel individual, es un indicativo de una
# posible redundancia entre los predictores (colinealidad).
# MSE empleando las observaciones de entrenamiento
training_mse <- mean((modelo$fitted.values - training$fat)^2)
training_mse

# MSE empleando nuevas observaciones
predicciones <- predict(modelo, newdata = test)
test_mse <- mean((predicciones - test$fat)^2)
test_mse

# Se observa que el modelo tiene un MSE muy bajo (0.48) cuando predice las mismas observaciones
# con las que se ha entrenado, pero 30 veces más alto (14.54) al predecir nuevas observaciones.
# Esto significa que el modelo no es útil, ya que el objetivo es aplicarlo para predecir el
# contenido en grasa de futuras muestras de carne. A este problema se le conoce como overfitting.
# Una de las causas por las que un modelo puede sufrir overfitting es la incorporación de
# predictores innecesarios, que no aportan información o que la información que aportan es redundante.

# Se recurre en primer lugar a la selección de predictores mediante stepwise selection empleando

# el AIC como criterio de evaluación:
modelo_step_selection <- step(object = modelo, trace = FALSE)

# Número de predictores del modelo resultante
length(modelo_step_selection$coefficients)

# Training-MSE
training_mse <- mean((modelo_step_selection$fitted.values - training$fat)^2)
training_mse

# Test-MSE
predicciones <- predict(modelo_step_selection, newdata = test)
test_mse <- mean((predicciones - test$fat)^2)
test_mse

# El proceso de stepwise selection devuelve como mejor modelo el formado por 73 de los 100
# predictores disponibles. Al haber eliminado predictores del modelo, el training MSE
# siempre aumenta, en este caso de 0.48 a 0.05, pero el test-MSE se ha reducido a 12.88986.

# Véase ahora el resultado si se ajusta el modelo empleando las componentes principales:

# Cálculo de componentes principales. Se excluye la columna con la variable
# respuesta *fat*
pca <- prcomp(training[, -101], scale. = TRUE)

# Se muestra la proporción de varianza explicada y acumulada de las 9 primeras
# componentes
summary(pca)$importance[, 1:9]

# El estudio de la proporción de varianza explicada muestra que la primera componente
# recoge la mayor parte de la información (98.5%), decayendo drásticamente la varianza
# en las sucesivas componentes.
# Una vez obtenido el valor de las componentes para cada observación (principal component
# scores), puede ajustarse el modelo lineal empleando dichos valores junto con la variable
# respuesta que le corresponde a cada observación. Con la función pcr() del paquete pls
# se evita tener que codificar cada uno de los pasos intermedios.

# Acorde a la proporción de varianza acumulada, emplear las 4 primeras componentes podría
# ser una buena elección, ya que en conjunto explican el 99.99100 % de varianza.
library(pls)
modelo_pcr <- pcr(formula = fat ~ ., data = training, scale. = TRUE, ncomp = 4)

# Test-MSE
predicciones <- predict(modelo_pcr, newdata = test, ncomp = 4)
test_mse <- mean((predicciones - test$fat)^2)
test_mse

# El test-MSE obtenido (20.56) para el modelo que emplea como predictores las 4 primeras
# componentes es mucho mayor que el obtenido con el modelo generado por stepwise selection
# (12.89) e incluso que el obtenido incluyendo todos los predictores (14.54659). Esto
# significa que, o bien el hecho de emplear componentes principales como predictores no
# es útil para este caso, o que el número de componentes incluido no es el adecuado.

# La función pcr() incluye la posibilidad de recurrir a cross validation para identificar
# el número óptimo de componentes con el que se minimiza el MSE.
set.seed(123)
modelo_pcr <- pcr(formula = fat ~ ., data = training, scale. = TRUE,
                  validation = "CV")
modelo_pcr_CV <- MSEP(modelo_pcr, estimate = "CV")
which.min(modelo_pcr_CV$val)

par(mfrow = c(1,2))
plot(modelo_pcr_CV$val, main = "MSE vs nº componentes", type = "l",
     ylab = "MSE",
     col = "blue", xlab = "Componentes")
plot(modelo_pcr_CV$val, main = "zoom", type = "l", ylab = "MSE",
     xlab = "Componentes", col = "blue", ylim = c(0,20))

# Test-MSE
predicciones <- predict(modelo_pcr, newdata = test, ncomp = 18)
test_mse <- mean((predicciones - test$fat)^2)
test_mse

# El número óptimo de componentes principales identificado por cross validation es de 18.
# Empleando este número en la PCR se consigue reducir el test-MSE a 4.52, un valor muy
# por debajo del conseguido con los otros modelos.
