# El primer paso en el proceso de K-Cross-Validation es dividir las observaciones
# de forma aleatoria en k grupos de aproximadamente el mismo tamaño. En este caso
# se va a emplear un valor de k=10.

library(ISLR)
library(leaps)
datos <- na.omit(Hitters)
set.seed(11)
# Sample() mezcla aleatoriamente las posiciones
# Es importante que la asignación sea aleatoria
grupo <- sample(rep(x = 1:10, length = nrow(datos)))

#Se comprueba que la distribución es aproximadamente equitativa
table(grupo)

# A continuación se inicia un proceso iterativo con k ciclos en el que:

# 1. Se identifica el mejor modelo para cada tamaño (1 predictor,…, 19 predictores),
# empleando como trainig set las observaciones de todos los k grupos excepto uno y
# evaluandolos acorde a menor RSS. Esto puede hacerse empleando la funcion regsubsets().

# 2. Se estima y almacena el test error (MSE) para cada uno de los modelos seleccionados
# empleando las observaciones del grupo que se excluyó en el paso anterior.

# 3. Se repite el proceso k veces, excluyendo en cada iteracion un grupo distinto del training set.

# 4. Se calcula el promedio de los k test error (MSE) para cada tamaño de modelo. A este
# valor promedio se le conoce como estimación del Mean-Cross-Validation Test Error.
# Cuanto menor sea, mejor precision predictiva tiene el modelo.

# 5. Identificacion del tamaño de modelo que consigue el menor Mean-Cross-Validation Test Error.

# 6. Reajuste e identificacion del mejor modelo con el número de predictores
# obtenido en el paso 5, empleando todas las observaciones como training. Esto
# puede hacerse empleando la función  regsubsets().

# En este ejemplo, dado que hay 19 modelos y 10 grupos de validación (k), para
# cada uno de los 19 modelos se calculan 10 estimaciones de test error haciendo
# un total de 190 valores. La mejor forma de almacenarlos es en formato de
# matriz. El mejor modelo de todos ellos será aquel que tenga un promedio de
# error menor (mean cv-test error).

# Función descrita en libro ISLR p.250
# Dado un objeto creado por la función regsubsets(), que es una lista de
# modelos, y un nuevo set de observaciones, la función predict.regsubsets()
# devuelve las predicciones para cada uno de los modelos.

predict.regsubsets  <- function(object, newdata, id, ...){
    # Extraer la formula del modelo (variable dependiente ~ predictores)
    form <- as.formula(object$call[[2]])
    # Generar una matriz modelo con los nuevos datos y la fórmula
    mat <- model.matrix(form, newdata)
    # Extraer los coeficientes del modelo
    coefi <- coef(object , id = id)
    # Almacenar el nombre de las variables predictoras del modelo
    xvars <- names(coefi)
    # Producto matricial entre los coeficientes del modelo y los valores de
    # los predictores de las nuevas observaciones para obtener las
    # predicciones
    mat[ , xvars] %*% coefi
}


# Matriz que almacena los test-error estimados. Cada columna representa un
# modelo. Cada fila representa uno de los 10 grupos en los que se han dividido
# las observaciones.
error_matrix <- matrix(data = NA, nrow = 10, ncol = 19,
                       dimnames = list(NULL, c(1:19)))

# Loop en el que se excluye en cada iteración un grupo distinto
# ESTE LOOP ESTA HECHO PARA UN DATA FRAME CON 19 PREDICTORES
num_validaciones <- 10
num_predictores <- 19

for (k in 1:num_validaciones) {
    # Identificacion de datos empleados como training
    train <- datos[grupo != k, ]
    # Selección de los mejores modelos para cada tamaño basándose en RSS
    mejores_modelos <- regsubsets(Salary~., data = train, nvmax = 19,
                                  method = "forward")

    # Para cada uno de los modelos "finalistas" se calcula el test-error con
    # el grupo excluido
    for (i in 1:num_predictores) {
        test <- datos[grupo == k, ]
        # Las predicciones del modelo i almacenado en el objeto regsubsets se
        # extraen mediante la función predict.regsubsets() definida arriba
        predicciones <- predict.regsubsets(object = mejores_modelos,
                                           newdata = test, id = i)
        # Calculo y almacenamiento del MSE para el modelo i
        error_matrix[k,i] <- mean((test$Salary - predicciones)^2)
        }
}

# Cada columna de la matriz error_matrix contiene los 10 valores de error
# calculados por cv
mean_cv_error <- apply(X = error_matrix, MARGIN = 2, FUN = mean)
# plot(sqrt(mean_cv_error), type = "b", pch = 19)
which.min(x = mean_cv_error)

ggplot(data = data.frame(n_predictores = 1:19, mean_cv_error = mean_cv_error),
       aes(x = n_predictores, y = mean_cv_error)) +
  geom_line() +
  geom_point() +
  geom_point(aes(x = n_predictores[which.min(mean_cv_error)],
                 y = mean_cv_error[which.min(mean_cv_error)]),
             colour = "red", size = 3) +
  scale_x_continuous(breaks = c(0:19)) +
  theme_bw() +
  labs(title = "Cross-validation mean error vs número de predictores",
       x = "número predictores")

# El mejor modelo identificado mediante 10-Cross-Validation es el formado por
# 12 predictores. Finalmente se identifica el mejor modelo formado por 12
# predictores empleando todas las observaciones (training + test).

modelo_final <- regsubsets(Salary~., data = datos, nvmax = 19,
                           method = "forward")
coef(object = modelo_final, 12)

# Si bien el modelo con 12 predictores es el que menor cv test error estimado
# tiene, el grafico muestra que a partir de 6 predictores la mejora es mínima.
# Acorde al principio de parsimonia, segun el cual se recomienda emplear de
# entre los modelos buenos el mas simple, el modelo más adecuado es el de 6 predictores.
