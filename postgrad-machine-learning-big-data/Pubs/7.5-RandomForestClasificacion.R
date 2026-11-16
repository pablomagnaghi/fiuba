# El set de datos fgl del paquete MASS contiene información sobre 214 muestras de cristal.
# Para cada una de ellas se han registrado 9 variables relacionadas con su composición y
# una (type) que indica su procedencia. Se desea ajustar un árbol de clasificación mediante
# random forest que permita predecir la procedencia del cristal en función de su composición.
library(MASS)
data("fgl")
head(fgl)

### Optimización de hiperparámetros
# Identificación del valor óptimo del hiperparámetro mtry.
tuning_rf_mtry <- function(df, y, ntree = 500){
  # Esta función devuelve el out-of-bag clasification error de un modelo RandomForest
  # en función del número de predictores evaluados (mtry)

  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   ntree = número de árboles creados en el modelo randomForest

  require(dplyr)
  max_predictores <- ncol(df) - 1
  n_predictores   <- rep(NA, max_predictores)
  oob_err_rate    <- rep(NA, max_predictores)
  for (i in 1:max_predictores) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = i, ntree = ntree)
    n_predictores[i] <- i
    oob_err_rate[i] <- tail(modelo_rf$err.rate[, 1], n = 1)
  }
  results <- data_frame(n_predictores, oob_err_rate)
  return(results)
}

hiperparametro_mtry <-  tuning_rf_mtry(df = fgl, y = "type")
hiperparametro_mtry %>% arrange(oob_err_rate)

ggplot(data = hiperparametro_mtry, aes(x = n_predictores, y = oob_err_rate)) +
  scale_x_continuous(breaks = hiperparametro_mtry$n_predictores) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_mtry %>% arrange(oob_err_rate) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs mtry",
       x = "nº predictores empleados") +
  theme_bw()

# Se identifica el valor 2 como número óptimo de predictores a evaluar en cada división.
# Identificación del valor óptimo del hiperparámetro nodesize.

tuning_rf_nodesize <- function(df, y, size = NULL, ntree = 500){
  # Esta función devuelve el out-of-bag clasification error de un modelo RandomForest
  # en función del tamaño mínimo de los nodos terminales (nodesize).

  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   sizes = tamaños evaluados
  #   ntree = número de árboles creados en el modelo randomForest

  require(dplyr)
  if (is.null(size)){
    size <- seq(from = 1, to = nrow(df), by = 5)
  }
  oob_err_rate <- rep(NA, length(size))
  for (i in seq_along(size)) {
    set.seed(321)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = 2, ntree = ntree,
                              nodesize = i)
    oob_err_rate[i] <- tail(modelo_rf$err.rate[, 1], n = 1)
  }
  results <- data_frame(size, oob_err_rate)
  return(results)
}

hiperparametro_nodesize <-  tuning_rf_nodesize(df = fgl, y = "type",
                                               size = c(1:20))
hiperparametro_nodesize %>% arrange(oob_err_rate)


ggplot(data = hiperparametro_nodesize, aes(x = size, y = oob_err_rate)) +
  scale_x_continuous(breaks = hiperparametro_nodesize$size) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_nodesize %>% arrange(oob_err_rate) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs nodesize",
       x = "nº observaciones en nodos terminales") +
  theme_bw()

# Se identifica el 1 como número óptimo de observaciones mínimas que deben contener los nodos terminales.
modelo_randomforest <- randomForest(type ~ ., data = fgl, mtry = 2, ntree = 500,
                                    importance = TRUE, nodesize = 1)

oob_err_rate <- data.frame(oob_err_rate = modelo_randomforest$err.rate[, 1],
                           arboles = seq_along(modelo_randomforest$err.rate[, 1]))
ggplot(data = oob_err_rate, aes(x = arboles, y = oob_err_rate )) +
  geom_line() +
  labs(title = "Evolución del out-of-bag-error vs número árboles",
       x = "nº árboles") +
  theme_bw()

# Alcanzados en torno a 300 árboles el error del modelo se estabiliza.

### Ajuste final y out-of-bag (OOB) error

# En este caso, en lugar de calcular el error de test con un conjunto de datos
# independiente, se emplean todas las observaciones para entrenar el modelo final
# y se extrae el OOB-error que, como se ha descrito anteriormente, es una buena
# estimación del error de test.
set.seed(17)
modelo_randomforest <- randomForest(type ~ ., data = fgl, mtry = 2, ntree = 300,
                                    importance = TRUE, nodesize = 1,
                                    norm.votes = TRUE )
modelo_randomforest

# Al tratarse de un problema de clasificación, es conveniente no solo mostrar la clase
# predicha (la más votada) sino también la fracción de votos para cada clase.

head(modelo_randomforest$votes)

# Dentro del objeto randomforest, el elemento $err.rate contiene el OOB-error (en este
# caso error de clasificación) obtenido tras añadir cada nuevo árbol. El valor de interés
# es el obtenido con el modelo completo
# Se extrae el último valor OOB
tail(modelo_randomforest$err.rate[, 1], 1)

### Identificación de los predictores más influyentes
library(tidyverse)
library(ggpubr)
importancia_pred <- as.data.frame(importance(modelo_randomforest, scale = TRUE))
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
p1 <- ggplot(data=importancia_pred, aes(x=reorder(variable, MeanDecreaseAccuracy),
                                        y = MeanDecreaseAccuracy,
                                        fill = MeanDecreaseAccuracy)) +
    labs(x = "variable", title = "Reducción de Accuracy") +
    geom_col() +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")

p2 <- ggplot(data = importancia_pred, aes(x = reorder(variable, MeanDecreaseGini),
                                          y = MeanDecreaseGini,
                                          fill = MeanDecreaseGini)) +
    labs(x = "variable", title = "Reducción de pureza (Gini)") +
    geom_col() +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom")
ggarrange(p1, p2)

# El modelo obtenido tiene un OOB estimated error rate del 20.56%. De entre todos los
# predictores empleados, RI, Mg, Al y Ca son los más influyentes. De hecho, si se
# genera un modelo más simple que solo incluya estos predictores, la estimación del
# error aumenta poco.
set.seed(17)
arbol_randomforest <- randomForest(type ~ RI + Mg + Al + Ca, data = fgl, mtry = 2,
                                   nodesize = 1, ntree = 300, importance = TRUE)
arbol_randomforest
