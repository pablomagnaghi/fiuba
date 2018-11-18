# En el siguiente ejemplo se emplea el dataset Auto del paquete ISLR para mostrar
# el método de validación simple consistente en dividir las observaciones de
# forma aleatoria en dos grupos, uno de ellos se emplea como set de entrenamiento
# y otro como set de validación. Se pretende generar un modelo que permita predecir
# el consumo de un vehículo (mpg) a partir de la potencia del motor (horse power),
# estimar su test error rate y el grado de flexibilidad (polinomio) más adecuado.

library(ISLR)
require(knitr)
data(Auto)
kable(head(Auto, n = 3), align = "c")

dim(Auto)

# En primer lugar se separan aleatoriamente las observaciones en dos grupos de
# 196 observaciones (mitad de las observaciones para cada set).


# Se seleccionan 196 índices aleatorios que formarán el training set.
set.seed(1)
train <- sample(x = 1:392, 196)

# Se genera un modelo lineal que relacione el consumo (mpg) con la potencia (horsepower)
# empleando unicamente los datos de training. Se recurre a la funcion lm() para generar
# el modelo y el argumento subset para identificar las observaciones que se tienen
# que emplear como entrenamiento.

modelo <- lm(mpg~horsepower, data = Auto, subset = train)
summary(modelo)

# Una vez generado el modelo se emplea la funcion predict() para estimar el consumo
# de las 196 observaciones restantes no empleadas como entrenamiento.

predicciones <- predict(object = modelo, newdata = Auto[-train, ])

# Dado que se conoce el valor real de consumo de los 196 coches empleados como test,
# se puede estimar el error de prediccion del modelo. Al tratarse de una variable
# continua se emplea como medida de error el MSE (mean square error).

mean((Auto$mpg[-train] - predicciones)^2)

# La estimacion de test error rate del modelo de regresion lineal creado es de 26.14.
# Como se ha comentado previamente, una de las principales desventajas de
# cross-validation simple es que la estimación de error varia mucho dependiendo de
# como se hayan repartido los datos entre set de entrenamiento y set de validacion.
# A continuacion se calcula el test error rate para 100 repeticiones de la validacion,
# repartiendo en cada una de ellas los datos de forma aleatoria.

library(ggplot2)
library(gridExtra)
cv_MSE <- rep(NA,100)
for (i in 1:100) {
  train <- sample(x = 1:392, 196)
  modelo <- lm(mpg ~ horsepower, data = Auto, subset = train)
  predicciones <- predict(object = modelo,newdata = Auto[-train, ])
  cv_MSE[i] <- mean((Auto$mpg[-train] - predicciones)^2)
}
p1 <- ggplot(data = data.frame(cv_MSE = cv_MSE), aes(x = 1, y = cv_MSE)) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(colour = c("firebrick3"), width = 0.1) +
      theme_bw() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())

p2 <- ggplot(data = data.frame(cv_MSE = cv_MSE), aes(cv_MSE)) +
      geom_histogram(colour = "firebrick3") +
      theme_bw()
grid.arrange(p1, p2, ncol = 2)

# Se observa que la estimacion del test error rate oscila entre 19.59 y 27.91, con media de 24.2 y sd  1.8.
# La representacion grafica de las observaciones y del modelo, muestra que la
# relacion entre las variables mpg y horsepower no es del todo lineal.
ggplot(data = Auto, aes(x = horsepower, y = mpg)) +
geom_point(colour = c("firebrick3")) +
geom_smooth(method = "lm", colour = "black") +
theme_bw() +
labs(title  =  'mpg ~ horsepower') +
theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# A continuacion se ajustan 10 modelos distintos empleando polinomios de grado 1
# hasta 10 y se registra el cv-MSE para cada uno. La función poly() permite
# determinar el grado de polinomio empleado.

cv_MSE <- rep(NA,10)
set.seed(1)
train <- sample(x = 1:392, 196)

for (i in 1:10) {
  modelo <- lm(mpg ~ poly(horsepower,i), data = Auto, subset = train)
  predicciones <- predict(object = modelo, newdata = Auto[-train, ])
  cv_MSE[i] <- mean((Auto$mpg[-train] - predicciones)^2)
}
ggplot(data = data.frame(polinomio = 1:10, cv_MSE = cv_MSE),
       aes(x = polinomio, y = cv_MSE)) +
geom_point(colour = c("firebrick3")) +
geom_path() +
scale_x_continuous(breaks = c(0:10)) +
theme_bw() +
labs(title  =  'Test Error ~ Grado del polinomio') +
theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Mediante validacion cruzada simple se identifica que un modelo que emplee una
# funcion cuadratica o cubica de horsepower minimiza el test error, es decir,
# captura mejor la relacion existente entre las variables y por lo tanto tiene
# mayor precision en sus predicciones. Dada la minima mejoria que consiguen los
# polinomios de grado 3 o superior, siguiendo el principio de parsimonia, el
# de grado 2 es el mas adecuado.

ggplot(data = Auto, aes(x = horsepower, y = mpg)) + geom_point(colour = c("firebrick3")) +
    stat_smooth(method = "lm", formula = y ~ poly(x, 2), color = "black") +
    theme_bw() + labs(title = "mpg ~ horsepower^2") + theme(plot.title = element_text(hjust = 0.5,
    face = "bold"))

