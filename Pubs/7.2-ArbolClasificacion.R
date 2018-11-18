# El set de datos Carseats contiene informacion sobre la venta de sillas infantiles en
# 400 tiendas distintas. Para cada una de las 400 tiendas se han registrado 11 variables.
# Se pretende generar un modelo de clasificacion que permita predecir si una tienda
# tiene ventas altas (Sales > 8) o bajas (Sales <= 8) en función de todas las variables disponibles.
library(ISLR)
data("Carseats")
head(Carseats)

# Como Sales es una variable continua y el objetivo del estudio es clasificar las tiendas
# segun si venden mucho o poco, se crea una nueva variable dicotomica (Si, No) llamada ventas_altas.
Carseats$ventas_altas <- ifelse(test = Carseats$Sales > 8,
                                yes = "Si", no = "No")

### Ajuste del modelo
# Se ajusta un arbol de clasificacion empleando como variable respuesta ventas_altas y como
# predictores todas las variables disponibles excepto Sales.
library(tree)
# Conversión de la variable respuesta a tipo factor
Carseats$ventas_altas <- as.factor(Carseats$ventas_altas)

arbol_clasificacion <- tree(formula = ventas_altas ~ CompPrice + Income +
                            Advertising + Population + Price + ShelveLoc +
                            Age + Education + Urban + US, data = Carseats)
# equivalente a tree( ventas_altas ~ . - Sales, Carseats)
summary(arbol_clasificacion)

# La función summary() muestra que el arbol ajustado tiene un total de 27 nodos terminales y
# un classification error rate del 9%.

# El termino Residual mean deviance mostrado en el summary es simplemente la deviance dividida
# entre (número de observaciones - número de nodos terminales). Cuanto menor es la deviance
# mejor es el ajuste del árbol a las observaciones de entrenamiento.
plot(x = arbol_clasificacion, type = "proportional")
text(x = arbol_clasificacion, splits = TRUE, pretty = 0,
     cex = 0.8, col = "firebrick")

# Analizando la representacion del arbol, el predictor más influyente sobre las ventas
# es el lugar que ocupa el producto en los estantes de las tiendas (ShelveLoc). Este
# hecho queda reflejado en la primera división del árbol, que separa las posiciones
# buenas de las malas e intermedias.
arbol_clasificacion

# En la practica, no solo es interesante ajustar el arbol sino tambien evaluar su
# capacidad predictiva mediante la estimación del test error. La forma más sencilla
# de hacerlo es dividiendo las observaciones disponibles en dos grupos, uno se emplea
# para ajustar el arbol training data set y el otro para evaluarlo test data set. La
# función predict() puede aplicarse a modelos de tipo arbol. En el caso de arboles
# de clasificacion, hay que indicar el argumento type="class" para que se devuelva
# la clase predicha.
set.seed(2)
train <- sample(1:nrow(Carseats), nrow(Carseats) / 2, replace = FALSE)
Carseats_train <- Carseats[train,]
Carseats_test <- Carseats[-train,]
arbol_clasificacion <- tree(formula =  ventas_altas ~ . -Sales,
                            data = Carseats_train)
predicciones <- predict(arbol_clasificacion, newdata = Carseats_test,
                        type = "class")
table(predicciones, Carseats_test$ventas_altas)

# El test error es 28.5%, El modelo es capaz de predecir correctamente un 71.5 % de las
# observaciones del test set.

### Podado del arbol (pruning)
# Con la finalidad de reducir la varianza del modelo y así disminuir el test error, se
# somete al arbol a un proceso de pruning. A diferencia del ejemplo anterior, al ser
# este un arbol de clasificacion se indica en la función cv.tree() que FUN = prune.misclass.
# De esta manera se emplea el classification error rate para guiar el proceso de pruning.
set.seed(3)
cv_arbol <- cv.tree(arbol_clasificacion, FUN = prune.misclass, K = 10)
cv_arbol

library(ggplot2)
library(ggpubr)

resultados_cv <- data.frame(n_nodos = cv_arbol$size, clas_error = cv_arbol$dev,
                            alpha = cv_arbol$k)
p1 <- ggplot(data = resultados_cv, aes(x = n_nodos, y = clas_error)) +
      geom_line() +
      geom_point() +
      labs(title = " Error de clasificación vs \n tamaño del árbol") + theme_bw()


p2 <- ggplot(data = resultados_cv, aes(x = alpha, y = clas_error)) +
      geom_line() +
      geom_point() +
      labs(title = " Error de clasificación vs \n hiperparámetro alpha") + theme_bw()
ggarrange(p1, p2)

cv_arbol$size[which.min(cv_arbol$dev)]

# En este caso, un arbol con 9 nodos terminales minimiza el test error.
# Con la funcion prune.missclas() se obtiene el mejor árbol de clasificación del tamaño
# identificado como óptimo (no confundir con la función prune.tree() para arboles de
# regresion). A esta funcion también se le puede indicar el valor de α optimo en lugar del tamaño.
arbol_pruning <- prune.misclass(tree = arbol_clasificacion, best = 9)
plot(x = arbol_pruning, type = "proportional")
text(x = arbol_pruning, splits = TRUE, pretty = 0,
     cex = 0.8, col = "firebrick")

### prediccion
predicciones <- predict(arbol_pruning, newdata = Carseats_test, type = "class")
table(predicciones, Carseats_test$ventas_altas)

paste("El porcentaje de acierto es de",
      100 * ((94 + 60) / (94 + 24 + 22 + 60)), "%")

# Gracias al proceso de pruning el porcentaje de acierto ha pasando de 71.5% a 77%.
