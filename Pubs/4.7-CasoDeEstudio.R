 # Se dispone del set de datos hitters que contiene 19 variables sobre jugadores
 # de la liga de beisbol. Se quiere crear un modelo lineal multiple que permita
 # predecir el salario de los jugadore

 # En primer lugar se divide aleatoriamente el set de datos en dos grupos, uno
 # se empleara para entrenar los modelos y el otro para validarlos.

 require(ISLR)
 data("Hitters")
 Hitters <- na.omit(Hitters)
 dim(Hitters)

 set.seed(1)
indices_entrenamiento <- sample(x = 1:nrow(Hitters),
                                size = round(nrow(Hitters) * (2/3)))
# 2/3 de las observaciones
indices_test <- (1:nrow(Hitters))[-indices_entrenamiento]
Hitters_1 <- Hitters[indices_entrenamiento,]
Hitters_2 <- Hitters[indices_test,]

# Ordinary least square (regresion por minimos cuadrados)

modelo_OLS   <- lm(formula = Salary ~ ., data = Hitters_1)
test_MSE_OLS <- mean((predict(modelo_OLS, Hitters_2) - Hitters_2$Salary)^2)
test_MSE_OLS

# Best subset selection mediante k-cross-validation

set.seed(3553)
require(leaps)
require(ggplot2)
grupo <- sample(rep(x = 1:10, length = nrow(Hitters_1)))
# Se comprueba que la distribucion es aproximadamente equitativa
table(grupo)

predict.regsubsets  <- function(object, newdata, id, ...){
    # Extraer la fórmula del modelo (variable dependiente ~ predictores)
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
# modelo. Cada fila es uno de los 10 grupos en los que se han dividido las
# observaciones
error_matrix <- matrix(data = NA, nrow = 10, ncol = 19,
                       dimnames = list(NULL, c(1:19)))

# Loop en el que se excluye en cada iteración un grupo distinto
for (k in 1:10) {
    # Identificación de Hitters empleados como training
    train <- Hitters_1[grupo != k, ]
    # Selección de los mejores modelos para cada tamaño basándose en RSS
    mejores_modelos <- regsubsets(Salary~., data = train, nvmax = 19,
                                  method = "backward")

    # Para cada uno de los modelos "finalistas" se calcula el test-error con
    # el grupo excluido
    for (i in 1:19) {
        test <- Hitters_1[grupo == k, ]
        # Las predicciones del modelo i almacenado en el objeto regsubsets se
        # extraen mediante la función predict.regsubsets() definida arriba
        predicciones <- predict.regsubsets(object = mejores_modelos,
                                           newdata = test, id = i)
        # Cálculo y almacenamiento del MSE para el modelo i
        error_matrix[k,i] <- mean((test$Salary - predicciones)^2)
        }
}

mean_cv_error <- apply(X = error_matrix, MARGIN = 2, FUN = mean)
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

# El mejor modelo identificado mediante 10-Cross-Validation es el formado por 5
# predictores. Finalmente se identifica el mejor modelo formado por 5 predictores,
# esta vez empleando todas las observaciones de Hitters_1, y se calcula el test-MSE
# empleando el set de datos Hitters_2.

modelo_final <- regsubsets(Salary ~ ., data = Hitters_1, nvmax = 19,
                           method = "backward")
coef(object = modelo_final, 5)

# Como el modelo esta dentro de un objeto regsubsets, en predict() se tiene
# que identificar el id
test_MSE_subset <- mean((predict(modelo_final, Hitters_2, id = 5) -
                         Hitters_2$Salary)^2)
test_MSE_subset

# Ridge regression

# La funcion glmnet() requiere pasar los predictores como matriz y la variable
# dependiente como vector.
x_Hitters_1 <- model.matrix(Salary~., data = Hitters_1)[, -1]
y_Hitters_1 <- Hitters_1$Salary

x_Hitters_2 <- model.matrix(Salary~., data = Hitters_2)[, -1]
y_Hitters_2 <- Hitters_2$Salary

library(glmnet)
set.seed(1)
# Se identifica mediante k-cross-validation el mejor valor de lambda para
# ridge regresion
cv_error_ridge <- cv.glmnet(x = x_Hitters_1, y = y_Hitters_1, alpha = 0,
                            nfolds = 10,
                            type.measure = "mse")
# Para obtener un ajuste mediante *ridge regression* se indica argumento
# alpha=0
modelo_ridge <- glmnet(x = x_Hitters_1, y = y_Hitters_1, alpha = 0,
                       lambda = cv_error_ridge$lambda.1se)
# Se almacenan las predicciones en una variable separada para no concatenar
# tanto código
predicciones <- predict(object = modelo_ridge, newx = x_Hitters_2,
                      s = cv_error_ridge$lambda.1se, exact = TRUE)
test_MSE_ridge <- mean((predicciones - Hitters_2$Salary)^2)
test_MSE_ridge

# Lasso

library(glmnet)
set.seed(1)
# Se identifica mediante k-cross-validation el mejor valor de lambda para
# lasso regresion
cv_error_lasso <- cv.glmnet(x = x_Hitters_1, y = y_Hitters_1, alpha = 1, nfolds = 10,
    type.measure = "mse")
# Para obtener un ajuste mediante *lasso regression* se indica argumento
# alpha=1
modelo_lasso <- glmnet(x = x_Hitters_1, y = y_Hitters_1, alpha = 1, lambda = cv_error_ridge$lambda.1se)
# Se almacenan las predicciones en una variable separada para no concatenar
# tanto código
predicciones <- predict(object = modelo_ridge, newx = x_Hitters_2, s = cv_error_ridge$lambda.1se,
    exact = TRUE)

test_MSE_lasso <- mean((predicciones - Hitters_2$Salary)^2)
test_MSE_lasso
