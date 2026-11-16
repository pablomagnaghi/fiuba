# En este ejemplo se emplea el set de datos Hitters del paquete ISLR que contiene
# 19 variables con informacion tecnica sobre jugadores de beisbol. El objetivo es
# encontrar el modelo que permita predecir con mayor precisión el salario de un jugador.

library(ISLR)
data("Hitters")
names(Hitters)

str(Hitters)

1 - (sum(complete.cases(Hitters)) / nrow(Hitters))

# El dataset contiene aproximadamente un 18% de observaciones a las que les falta
# al menos una variable (missing value). Existen tecnicas para manejar missing
# values pero en este caso simplemente se van a excluir los registros que no esten completos.

datos <- na.omit(Hitters)
dim(datos)

# El numero de observaciones supera en más de 10 veces al numero de predictores.
# Esto es importante a la hora de elegir el metodo con el que se va a ajustar el
# modelo. Como se ha descrito previamente, si el número de predictores se aproxima
# o supera al de observaciones, la regresion por mínimos cuadrados no es adecuada.

# La funcion regsubsets() del paquete leaps realiza best subset selection de
# modelos ajustados mediante regresion lineal por mínimos cuadrados. En primer
# lugar identifica el mejor modelo de cada tamaño (mejor modelo con 1 predictor,
# mejor modelo con 2 predictores…), entendiendo por mejor modelo aquel que tiene menor RSS.


require(leaps)
mejores_modelos <- regsubsets(Salary~., data = datos, nvmax = 19)
# El argumento nvmax determina el tamaño máximo de los modelos a inspeccionar.
# Si se quiere realizar best subset selection evaluando todos los posibles
# modelos, nvmax tiene que ser igual al numero de variables disponibles


# La funcion summary() aplicada a un objeto regsubsets() devuelve una tabla en la
# que se muestra, para cada posible tamaño de modelo, cual es el mejor. El nombre
# de la fila indica el tamaño del modelo (el número de predictores que contiene).
# El “∗” indica qué variables se han incluido. Por ejemplo, el mejor modelo
# formado por dos predictores incorpora las variables Hits y CRBI.

summary(mejores_modelos)

# Una vez identificado el mejor modelo de cada tamaño, se tiene que escoger el
# mejor de entre todos ellos.

names(summary(mejores_modelos))

summary(mejores_modelos)$adjr2

#se identifica que modelo tiene el valor maximo de R ajustado
which.max(summary(mejores_modelos)$adjr2)

# El modelo que ocupa la posición 11 es el que mayor R2 ajustado alcanza. Dado que las
# posiciones en la tabla creada por regsubsets() se corresponden con el numero de
# predictores, el mejor modelo es el que contiene 11 predictores.

# Una representacion grafica del estadístico escogido para comparar los modelos,
# en este caso R2 ajustado, frente al numero de predictores permite evaluar la
# evolucion de la precision del modelo en funcion del tamaño y si la mejora es sustancial.

library(ggplot2)
p <- ggplot(data = data.frame(n_predictores = 1:19,
                              R_ajustado = summary(mejores_modelos)$adjr2),
            aes(x = n_predictores, y = R_ajustado)) +
    geom_line() +
    geom_point()

#Se identifica en rojo el máximo
p <- p + geom_point(aes(
                    x = n_predictores[which.max(summary(mejores_modelos)$adjr2)],
                    y = R_ajustado[which.max(summary(mejores_modelos)$adjr2)]),
                    colour = "red", size = 3)
p <- p +  scale_x_continuous(breaks = c(0:19)) +
          theme_bw() +
          labs(title = 'R2_ajustado vs número de predictores',
               x =  'número predictores')
p

# Para conocer cuales son los coeficientes del mejor modelo y su estimacion:
coef(object = mejores_modelos, id = 11)


# Si bien el modelo con mayor R2ajustado es el formado por 11 predictores,
# la mejora conseguida a partir de 6 predictores es minima:

summary(mejores_modelos)$adjr2[11]

summary(mejores_modelos)$adjr2[6]

# Acorde al principio de parsimonia, el modelo que se debe seleccionar como adecuado
# es el que contiene entre 6-8 predictores.
