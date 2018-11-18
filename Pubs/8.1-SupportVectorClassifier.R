# Para mostrar el uso de un Support Vector Classifier como clasificador binario, se simulan
# observaciones en un espacio bidimensional que pertenecen a dos clases. Este ejemplo se ha
# obtenido de los videos asociados al libro Introduction to Statistical Learning, que no es
# igual al presentado en el libro.

# En los siguientes ejemplos, se emplea la función svm() contenida en el paquete e1071. Esta
# función ajusta Support Vector Classifier si se le especifica el argumento kernel="linear"
# (como se describe más adelante, el método de Support Vector Machines es equivalente al Support
# Vector Classifier cuando el kernel utilizado es lineal). El argumento cost determina la
# penalización aplicada por violar el margen, es el nombre que emplea esta función para el
# hiperparámetro C.
set.seed(10111)
coordenadas <- matrix(rnorm(40), 20, 2)
colnames(coordenadas) <- c("X1", "X2")
y <- c(rep(-1, 10), rep(1, 10))
coordenadas[y == 1, ] <- coordenadas[y == 1, ] + 1
datos <- data.frame(coordenadas, y)
ggplot(data = datos, aes(x = X1, y = X2, color = as.factor(y))) + geom_point(size = 6) +
    theme_bw() + theme(legend.position = "none")

# La representación gráfica de los datos muestra que los grupos no son linealmente separables.

# La función svm() identifica automáticamente si se trata de un problema de clasificación,
# la variable respuesta es de tipo factor, o de regresión, la variable respuesta es tipo numérico.

library(e1071)

# Se convierte la variable respuesta a factor
datos$y <- as.factor(datos$y)

# Para que la función svm() calcule el Support Vector Classifier, se tiene
# que indicar que la función kernel es lineal.
modelo_svm <- svm(formula = y ~ X1 + X2, data = datos, kernel = "linear", cost = 10,
    scale = FALSE)

# IMPORTANTE: En este caso, ambos predictores (X1, X2) tienen la misma escala por lo que no
# es necesario estandarizarlos. En aquellas situaciones en las que las escalas son distintas,
# sí hay que estandarizarlos, de lo contrario, los predictores de mayor magnitud eclipsarán
# a los de menor magnitud.

# El summary del modelo muestra que hay un total de 6 vectores soporte, 3 pertenecen a una
# clase y 3 a la otra.
summary(modelo_svm)

# Índice de las observaciones que actúan como vector soporte
modelo_svm$index

# Empleando la función plot() con un objeto de tipo svm junto con las observaciones de
# entrenamiento, se obtiene una representación del límite de decisión y las dos regiones
# en las que queda dividido el espacio muestral.
plot(modelo_svm, datos)

# A pesar de que el clasificador es de tipo lineal (kernel = "linear"), la baja resolución
# del gráfico hace parecer que no lo es. Además, esta representación no muestra el margen
# ni el hiperplano. A continuación, se muestra una alternativa basada en ggplot2 para
# obtener una representación de los resultados de svm() cuando el espacio es bidimensionales.
# SI AL AJUSTAR EL MODELO SE INDICA scale = true, SE TIENEN QUE ESTANDARIZAR
# TAMBIÉN LAS OBSERVACIONES PARA QUE COINCIDAN LAS COORDENADAS.

# Se interpolar puntos dentro del rango de los dos predictores X1 y X2.
# Estos nuevos puntos se emplean para predecir la variable respuesta acorde
# al modelo y así colorear las regiones que separa el hiperplano.

# Rango de los predictores
rango_X1 <- range(datos$X1)
rango_X2 <- range(datos$X2)

# Interpolación de puntos
new_x1 <- seq(from = rango_X1[1], to = rango_X1[2], length = 75)
new_x2 <- seq(from = rango_X2[1], to = rango_X2[2], length = 75)
nuevos_puntos <- expand.grid(X1 = new_x1, X2 = new_x2)

# Predicción según el modelo
predicciones <- predict(object = modelo_svm, newdata = nuevos_puntos)

# Se almacenan los puntos predichos para dar color a las regiones
color_regiones <- data.frame(nuevos_puntos, y = predicciones)

# Para extraer la ecuación del hiperplano y del margen es necesario aplicar
# algebra lineal.
beta <- drop(t(modelo_svm$coefs) %*% as.matrix(datos[, c("X1", "X2")])[modelo_svm$index,
    ])
beta0 <- modelo_svm$rho


ggplot() + # Representación de las 2 regiones empleando los puntos y coloreándolos
# según la clase predicha por el modelo
geom_point(data = color_regiones, aes(x = X1, y = X2, color = as.factor(y)),
    size = 0.5) + # Se añaden las observaciones
geom_point(data = datos, aes(x = X1, y = X2, color = as.factor(y)), size = 6) +
    # Se identifican aquellas observaciones que son vectores soporte del modelo
geom_point(data = datos[modelo_svm$index, ], aes(x = X1, y = X2, color = as.factor(y)),
    shape = 21, colour = "black", size = 6) + # Se añaden las rectas del hiperplano y los márgenes
geom_abline(intercept = beta0/beta[2], slope = -beta[1]/beta[2]) + geom_abline(intercept = (beta0 -
    1)/beta[2], slope = -beta[1]/beta[2], linetype = "dashed") + geom_abline(intercept = (beta0 +
    1)/beta[2], slope = -beta[1]/beta[2], linetype = "dashed") + theme_bw() +
    theme(legend.position = "none")

# En el ajuste anterior se ha empleado un valor del hiperparámetro de penalización cost = 10.
# Este hiperparámetro determina el balance bias-varianza y por lo tanto, es crítico para la
# capacidad predictiva del modelo. El paquete e1071 incluye la función tune() que realiza
# 10-cross-validation para identificar el valor óptimo de penalización. Entre sus argumentos
# están: el modelo svm y un vector  ranges con los valores de los hiperparámetros que se
# quieren evaluar.
set.seed(1)
svm_cv <- tune("svm", y ~ X1 + X2, data = datos, kernel = "linear", ranges = list(cost = c(0.001,
    0.01, 0.1, 1, 5, 10, 20, 50, 100, 150, 200)))
summary(svm_cv)

ggplot(data = svm_cv$performances, aes(x = cost, y = error)) + geom_line() +
    geom_point() + labs(title = "Error de clasificación vs hiperparámetro C") +
    theme_bw()


# El proceso de cross-validation muestra que el valor de penalización con el que se consigue menor
# error rate es 20 o superior. La función tune() almacena el mejor modelo de entre todos los que
# se han comparado:
mejor_modelo <- svm_cv$best.model


# Una vez obtenido el modelo final, se puede predecir la clase a la que pertenecen nuevas
# observaciones empleando la función predict().

# Datos de test simulados
set.seed(19)
coordenadas <- matrix(rnorm(40), 20, 2)
colnames(coordenadas) <- c("X1", "X2")
y <- sample(c(-1, 1), 20, rep = TRUE)
coordenadas[y == 1, ] <- coordenadas[y == 1, ] + 1
test <- data.frame(coordenadas, y)

# Predicciones
predicciones <- predict(object = mejor_modelo, test)
paste("Error de test:", 100 * mean(test$y != predicciones), "%")

table(predicción = predicciones, valor_real = test$y)

# 18 de las 20 observaciones han sido clasificadas correctamente por el modelo.
# El test error es del 10 %. 
