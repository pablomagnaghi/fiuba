# El set de datos Boston contiene información sobre viviendas de la ciudad de Boston
# así como información sobre el barrio en el que se encuentran. Se pretende generar
# modelo basado en árboles de regresión que permita predecir el precio medio de una
# vivienda (medv) en función de las variables disponibles. Para mejorar la capacidad
# predictiva del modelo se recurre al proceso de bagging.

# Con la función randomForest() se puede generar un modelo random forest de la misma
# forma descrita en el ejemplo anterior de bagging. La única diferencia es que, en el
# argumento mtry, se indica un número menor que el total de predictores. Por defecto,
# la función emplea p/3 predictores cuando aplica random forest a árboles de regresión
# y raiz de p cuando son árboles de clasificación. Sin embargo, en lugar de emplear estos
# valores, es mejor identificar el valor óptimo estudiando el out-of-bag-MSE en función
# del número de predictores evaluados en cada división.

### Optimización de hiperparámetros
tuning_rf_mtry <- function(df, y, ntree = 500){
  # Esta función devuelve el out-of-bag-MSE de un modelo RandomForest en función
  # del número de predictores evaluados (mtry)

  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   ntree = número de árboles creados en el modelo randomForest

  require(dplyr)
  max_predictores <- ncol(df) - 1
  n_predictores   <- rep(NA, max_predictores)
  oob_mse         <- rep(NA, max_predictores)
  for (i in 1:max_predictores) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = i, ntree = ntree)
    n_predictores[i] <- i
    oob_mse[i] <- tail(modelo_rf$mse, n = 1)
  }
  results <- data_frame(n_predictores, oob_mse)
  return(results)
}

hiperparametro_mtry <-  tuning_rf_mtry(df = Boston, y = "medv")
hiperparametro_mtry %>% arrange(oob_mse)


ggplot(data = hiperparametro_mtry, aes(x = n_predictores, y = oob_mse)) +
  scale_x_continuous(breaks = hiperparametro_mtry$n_predictores) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_mtry %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs mtry",
       x = "nº predictores empleados") +
  theme_bw()

# Se identifica 5 como número óptimo de predictores a evaluar en cada división.
# Además del número de predictores evaluados en cada división, el método de ranfom
# forest tiene otros dos hiperparámetros: el número mínimo de observaciones que deben
# tener los nodos terminales (nodesize) y el número de árboles ajustados (ntree).
# La función randomForest() emplea como valores por defecto nodesize = 1 en problemas
# de clasificación, nodesize = 5 en regresión y  ntree = 500, sin embargo, ambos
# hiperparámetros pueden optimizarse empleando el out of bag error.
tuning_rf_nodesize <- function(df, y, size = NULL, ntree = 500){
  # Esta función devuelve el out-of-bag-MSE de un modelo randomForest en función
  # del tamaño mínimo de los nodos terminales (nodesize).

  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   sizes = tamaños evaluados
  #   ntree = número de árboles creados en el modelo randomForest

  require(dplyr)
  if (is.null(size)){
    size <- seq(from = 1, to = nrow(df), by = 5)
  }
  oob_mse <- rep(NA, length(size))
  for (i in seq_along(size)) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = 5, ntree = ntree,
                              nodesize = i)
    oob_mse[i] <- tail(modelo_rf$mse, n = 1)
  }
  results <- data_frame(size, oob_mse)
  return(results)
}

hiperparametro_nodesize <-  tuning_rf_nodesize(df = Boston, y = "medv",
                                               size = c(1:20))
hiperparametro_nodesize %>% arrange(oob_mse)

ggplot(data = hiperparametro_nodesize, aes(x = size, y = oob_mse)) +
  scale_x_continuous(breaks = hiperparametro_nodesize$size) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_nodesize %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs nodesize",
       x = "nº observaciones en nodos terminales") +
  theme_bw()

# Se identifica el 3 como número óptimo de observaciones mínimas que deben contener
# los nodos terminales.
# Por último, se estudia la evolución del out of bag error en función del número de
# árboles. En el caso de random forest, este hiperparámetro no afecta al overfiting,
# por lo que el principal objetivo de su optimización es evitar consumir recursos
# computacionales innecesarios.
modelo_randomforest <- randomForest(medv ~ ., data = Boston, subset = train,
                                    mtry = 5 , ntree = 500, nodesize = 3,
                                    importance = TRUE)
oob_mse <- data.frame(oob_mse = modelo_randomforest$mse,
                      arboles = seq_along(modelo_randomforest$mse))
ggplot(data = oob_mse, aes(x = arboles, y = oob_mse )) +
  geom_line() +
  labs(title = "Evolución del out-of-bag-error vs número árboles",
       x = "nº árboles") +
  theme_bw()

# A partir de aproximadamente 100 árboles el error del modelo se estabiliza.
# Nota: La búsqueda de mejores hiperparámetros no debe hacerse de forma secuencial, ya que
# cada hiperparámetro interacciona con los demás. Sin embargo, para no añadir una capa de
# complejidad extra he optado por la búsqueda más sencilla. El paquete caret implementa
# múltiples formas adecuadas de encontrar los hiperparámetros óptimos.

### Ajuste final y error de test
set.seed(123)
modelo_randomforest <- randomForest(medv ~ ., data = Boston, subset = train,
                                    mtry = 5 , ntree = 100, nodesize = 3,
                                    importance = TRUE)
modelo_randomforest

predicciones <- predict(object = modelo_randomforest, newdata = Boston[-train, ])
test_mse <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del modelo:", round(test_mse, 2))

# Empleando todas las observaciones en el proceso de randomforest
set.seed(123)
modelo_randomforest <- randomForest(medv ~ ., data = Boston, mtry = 5, ntree = 100,
                                    nodesize = 3, importance = TRUE)
modelo_randomforest

# El test MSE (11.19) obtenido con el test set y el out-of-bag-MSE (9.62) estimado
# empleando todas las observaciones, son similares y ambos menores los obtenidos
# por bagging (13.18 y 10.63). Gracias a la decorrelación, el método random forest
# suele superar al de bagging.

### Identificación de los predictores más influyentes
# Con la función importance() se extrae la importancia de las variables.
library(tidyverse)
library(ggpubr)
importancia_pred <- as.data.frame(importance(modelo_randomforest, scale = TRUE))
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

# Al igual que con bagging, las variables lstat y rm son con diferencia las más
# importantes en el modelo. 
