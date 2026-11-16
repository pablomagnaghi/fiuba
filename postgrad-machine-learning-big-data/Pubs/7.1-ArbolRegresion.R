# El set de datos Boston contiene informacion sobre viviendas de la ciudad de Boston,
# asi como informacion sobre el barrio en el que se encuentran. Se pretende ajustar
# un arbol de regresion que permita predecir el precio medio de una vivienda (medv)
# en funcion de las variables disponibles.

library(MASS)
data("Boston")
head(Boston)

### Ajuste del modelo
# La funcion tree() del paquete tree permite ajustar arboles de prediccion. La eleccion
# entre arbol de regresion o arbol de clasificacion se hace automaticamente dependiendo
# de si la variable respuesta es de tipo numeric o factor. Es importante tener en cuenta
# que solo estos dos tipos de vectores están permitidos, si se pasa uno de tipo character
# se devuelve un error.
# A continuacion, se ajusta un árbol de regresion empleando como variable respuesta medv y
# como predictores todas las variables disponibles. La funcion tree() crece el arbol hasta
# que encuentra una condicion de stop. Por defecto, estas condiciones son:
# # mincut = 5: número mínimo de observaciones que debe de tener al menos uno de los nodos
# # hijos para que se produzca la división.
# # minsize = 10: número mínimo de observaciones que debe de tener un nodo para que pueda dividirse.
# # deepth = 31: profundidad máxima que puede alcanzar el árbol.
# Esto implica que, no todas las variables pasadas como predictores en el argumento formula,
# necesariamente tienen que acabar incluidas en el arbol.

# Como en cualquier estudio de regresión, no solo es importante ajustar el modelo, sino
# tambien cuantificar su capacidad para predecir nuevas observaciones. Para poder hacer
# la posterior evaluación, se dividen los datos en dos grupos, uno se emplea como training
# data set y el otro como test data set.

library(tree)
set.seed(1)
train <- sample(1:nrow(Boston), size = nrow(Boston)/2)
arbol_regresion <- tree(formula = medv ~ ., data = Boston, subset = train,
                        split = "deviance")

# La funcion summary() muestra que el arbol ajustado tiene un total de 8 nodos terminales y
# que se han empleado como predictores las variables lstat, rm y dis. En el contexto de
# arboles de regresion, el termino Residual mean deviance es simplemente la suma de cuadrados
# residuales dividida entre (número de observaciones - número de nodos terminales). Cuanto
# menor es la deviance, mejor es el ajuste del arbol a las observaciones de entrenamiento.

# Una vez creado el arbol, se puede representar mediante la combinacion de las funciones
# plot() y  text(). La funcion plot() dibuja la estructura del arbol: las ramas y los nodos.
# Mediante su argumento type se puede especificar si se quiere que todas las ramas tengan
# el mismo tamaño (type = "uniform") o que su longitud sea proporcional a la reducción de
# impureza (heterogeneidad) de los nodos terminales (type = "proportional"). Esta segunda
# opcion permite identificar visualmente el impacto de cada división en el modelo. La funcion
# text() añade la descripcion de cada nodo interno y el valor de cada nodo terminal.

plot(x = arbol_regresion, type = "proportional")
text(x = arbol_regresion, splits = TRUE, pretty = 0,
     cex = 0.8, col = "firebrick")

# La variable lstat, que mide el porcentaje de personas en estado de pobreza en la zona, ha
# resultado ser el predictor más importante (primer nodo). El arbol indica que, valores
# bajos de lstat, se corresponden con precios de vivienda mas elevados. Por ejemplo, el
# modelo predice un precio de 46380 dolares para viviendas que están en una zona con un
# porcentaje lstat<9.715 y un número de habitaciones rm≥7.437.

# Para obtener una descripción más detallada del arbol, se puede imprimir el objeto por
# pantalla. R muestra el criterio de division de cada nodo, el número de observaciones
# que hay en esa rama (antes de dividirse), la deviance, la predicción promedio de esa
# rama (en el caso de clasificación se muestra el grupo más frecuente) y la proporción
# de cada grupo. Cuando se trata de nodo terminal, se indica con un asterisco.

arbol_regresion

### Podado del arbol (pruning)
# Con la finalidad de reducir la varianza del modelo y así disminuir el test error, se
# somete al arbol a un proceso de pruning. Tal como se describió con anterioridad, el
# proceso de pruning intenta encontrar el arbol mas sencillo (menor tamaño) que consigue
# explicar las observaciones. La función cv.tree() emplea cross validation para identificar
# el valor optimo de penalización α. Por defecto, esta función emplea la deviance para
# guiar el proceso de pruning.
set.seed(3)
cv_arbol <- cv.tree(arbol_regresion, K = 10)
cv_arbol


# El objeto devuelto por cv.tree() contiene:
# # size: el tamaño (número de nodos terminales) de cada arbol.
# # dev: la estimación de cross-validation test error para cada tamaño de árbol.
# # k: El rango de valores de penalización α evaluados.
# # method: El criterio empleado para seleccionar el mejor árbol.
# El objetivo del proceso es encontrar el valor α con el que se consigue el menor
# cross-validation test error. Dado que α es quien determina el tamaño del arbol,
# la frase anterior equivale a decir que se busca el tamaño del arbol que minimiza
# el cross-validation test error. En este caso, el mejor arbol está formado por 8
# nodos, por lo que no es necesario reducir el tamaño original.

library(ggplot2)
library(ggpubr)

resultados_cv <- data.frame(n_nodos = cv_arbol$size, deviance = cv_arbol$dev,
                            alpha = cv_arbol$k)
p1 <- ggplot(data = resultados_cv, aes(x = n_nodos, y = deviance)) +
      geom_line() +
      geom_point() +
      labs(title = "Error vs tamaño del árbol") + theme_bw()


p2 <- ggplot(data = resultados_cv, aes(x = alpha, y = deviance)) +
      geom_line() +
      geom_point() +
      labs(title = "Error vs hiperparámetro alpha") + theme_bw()

ggarrange(p1, p2)

# Con la funcion prune.tree() se obtiene el mejor arbol de regresion del tamaño identificado
# como optimo. Esta función también acepta el valor de α optimo en lugar del tamaño.

arbol_pruning <- prune.tree(tree = arbol_regresion, best = 8)
plot(x = arbol_pruning, type = "proportional")
text(x = arbol_pruning, splits = TRUE, pretty = 0,
     cex = 0.8, col = "firebrick")

### Evaluacion del modelo
# Por ultimo, se evalúa la precision del arbol empleando el test data set.
predicciones <- predict(arbol_pruning, newdata = Boston[-train,])
test_mse     <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del árbol de regresión tras podado:", round(test_mse,2))

# El Mean Square Test Error asociado con el árbol de regresion es de 25.05 unidades. La
# raiz cuadrada del Mean Square Test Error es 5.005, lo que significa que las predicciones
# se alejan en promedio 5.005 unidades (5005 dólares) del valor real.
