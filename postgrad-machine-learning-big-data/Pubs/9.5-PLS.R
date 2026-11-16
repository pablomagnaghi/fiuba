# El método Partial Least Squares (PLS) es muy similar al PCR en cuanto que ambos emplean las
# componentes principales resultantes de un análisis PCA como predictores. La diferencia reside
# en que, mientras PCR ignora la variable respuesta Y para determinar las combinaciones lineales,
# PLS busca aquellas que, además de explicar la varianza observada, predicen Y lo mejor posible.
# Puede considerarse como una versión supervised de PCR.
library(faraway)
data(meatspec)

# Igual que en el ejemplo anterior de PCR, para poder evaluar la capacidad predictiva del modelo,
# se dividen las observaciones disponibles en dos grupos: uno de entrenamiento para ajustar el
# modelo (80% de los datos) y uno de test (20% de los datos).
training <- meatspec[1:172, ]
test     <- meatspec[173:215, ]

# La función plsr() del paquete pls ajusta modelos por Partial Least Squares e incluye la
# posibilidad de recurrir a cross validation para identificar el número óptimo de componentes
# con el que se minimiza el MSE.
library(pls)
set.seed(123)
modelo_pls <- plsr(formula = fat ~ ., data = training, scale. = TRUE, validation = "CV")
modelo_pls_CV <- MSEP(modelo_pls, estimate = "CV")
which.min(modelo_pls_CV$val)

par(mfrow = c(1,2))
plot(modelo_pls_CV$val, main = "MSE vs nº componentes", type = "l",
     ylab = "MSE",
     col = "blue", xlab = "Componentes")
plot(modelo_pls_CV$val, main = "zoom", type = "l", ylab = "MSE",
     xlab = "Componentes", col = "blue", ylim = c(0,20))

# Test-MSE
predicciones <- predict(modelo_pls, newdata = test, ncomp = 15)
test_mse <- mean((predicciones - test$fat)^2)
test_mse

# Si se comparan los resultados obtenidos por PCR y PLS se observa que el número de componentes
# óptimo es inferior en PLS. Esto suele ser así ya que en el proceso de PLS se está incluyendo
# información adicional a través de la variable respuesta. Para este ejemplo, el método PLS
# consigue un test-MSE ligeramente inferior al obtenido por PCR. 
