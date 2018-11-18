# Los siguientes datos simulados contienen observaciones que pertenecen a cuatro grupos distintos.
# Se pretende aplicar K-means-clustering con el fin de identificarlos.

library(tidyverse)
library(ggpubr)
set.seed(101)
# Se simulan datos aleatorios con dos dimensiones
datos <- matrix(rnorm(n = 100*2), nrow = 100, ncol = 2,
                dimnames = list(NULL,c("x", "y")))
datos <- as.data.frame(datos)

# Se determina la media que va a tener cada grupo en cada una de las dos
# dimensiones. En total 2*4 medias. Este valor se utiliza para separar
# cada grupo de los demás.
media_grupos <- matrix(rnorm(n = 8, mean = 0, sd = 4), nrow = 4, ncol = 2,
                       dimnames = list(NULL, c("media_x", "media_y")))
media_grupos <- as.data.frame(media_grupos)
media_grupos <- media_grupos %>% mutate(grupo = c("a","b","c","d"))

# Se genera un vector que asigne aleatoriamente cada observación a uno de
# los 4 grupos
datos <- datos %>% mutate(grupo = sample(x = c("a","b","c","d"),
                                         size = 100,
                                         replace = TRUE))

# Se incrementa el valor de cada observación con la media correspondiente al
# grupo asignado.
datos <- left_join(datos, media_grupos, by = "grupo")
datos <- datos %>% mutate(x = x + media_x,
                          y = y + media_y)

ggplot(data = datos, aes(x = x, y = y, color = grupo)) +
  geom_point(size = 2.5) +
  theme_bw()


# La función kmeans() del paquete stats realiza K-mean-clustering. Entre sus argumentos
# destacan: centers, que determina el número K de clusters que se van a generar y nstart,
# que determina el número de veces que se va a repetir el proceso, cada vez con una
# asignación aleatoria inicial distinta. Es recomendable que este último valor sea alto,
# entre 25-50, para no obtener resultados malos debido a una iniciación poco afortunada del proceso.

# Como los datos se han simulado considerando que todas las dimensiones tienen aproximadamente
# la misma magnitud, no es necesario escalarlos ni centrarlos. De no ser así, sí que habría que hacerlo.
set.seed(101)
km_clusters <- kmeans(x = datos[, c("x", "y")], centers = 4, nstart = 50)
km_clusters

# El objeto devuelto por la función kmeans() contiene entre otros datos: la media de cada una de
# las variables para cada cluster (centers), un vector indicando a que cluster se ha asignado
# cada observación (cluster), la suma de cuadrados interna de cada cluster (withinss) y la suma
# total de cuadrados internos de todos los clusters (tot.withinss). Al imprimir el resultado
# también se muestra y el ratio de la suma de cuadrados entre-clusters y la suma de cuadrados
# totales. Este ratio es equivalente al R2 de los modelos de regresión, indica el porcentaje de
# varianza explicada por el modelo respecto al total de varianza observada. Puede utilizarse para
# evaluar el clustering obtenido pero, al igual que ocurre con R2 al incrementar el número de
# predictores, el ratio between_SS / total_SS aumenta con el número de clusters creados. Esto
# último se debe de tener en cuenta para evitar problemas de overfitting.

# Al tratarse de una simulación, se conoce el número real de grupos (4) y a cuál de ellos pertenece
# cada observación. Esto no sucede en la mayoría de casos prácticos, pero es útil para evaluar cómo
# de bueno es el método de K-means-clustering clasificando las observaciones.

# Se representa el número de cluster al que se ha asignado cada observación y
# se muestra con un código de color el grupo real al que pertenece.

datos <- datos %>% mutate(cluster = km_clusters$cluster)
datos <- datos %>% mutate(cluster = as.factor(cluster),
                          grupo   = as.factor(grupo))

ggplot(data = datos, aes(x = x, y = y, color = grupo)) +
  geom_text(aes(label = cluster), size = 5) +
  theme_bw() +
  theme(legend.position = "none")


# El gráfico muestra que solo dos observaciones se han agrupado incorrectamente. Este tipo de
# visualización es muy útil e informativa, sin embargo, solo es posible cuando se trabaja con
# dos dimensiones y se conocen los verdaderos grupos a los que pertenecen las observaciones.
# Si los datos contienen más de dos variables (dimensiones), una posible solución es utilizar
# las dos primeras componentes principales obtenidas en un PCA previo.

# El número de aciertos y errores puede representarse también en modo de matriz de confusión.
# A la hora de interpretar estas matrices, es importante recordar que el clustering asigna
# las observaciones a clusters cuyo identificador no tiene que por qué coincidir con la
# nomenclatura empleada para los grupos reales. Por ejemplo, el grupo b podría haberse
# llamado en su lugar grupo 2 y haberse asignado al cluster 1. Así pues, por cada fila de
# la matriz cabe esperar un valor alto (coincidencias) para una de las posiciones y valores
# bajos en las otras (errores de clasificación), pero no tienen por qué coincidir los nombres.

table(km_clusters$cluster, datos[, "grupo"],
      dnn = list("cluster", "grupo real"))

# En este análisis, solo 2 de las 100 observaciones se han agrupado erróneamente. De nuevo
# repetir que, en la realidad, no se suelen conocer los verdaderos grupos en los que se
# dividen las observaciones, de lo contrario no se necesitaría aplicar clustering.

# Supóngase ahora que se trata de un caso real, en el que se desconoce el número de grupos
# en los que se subdividen las observaciones. El investigador tendría que probar con diferentes
# valores de K y decidir cuál parece más razonable (en los siguientes apartados se describen
# métodos más sofisticados). A continuación, se muestran los resultados para K=2 y K=6.


# Resultados para K = 2
datos <- datos %>% select(x, y)
set.seed(101)
km_clusters_2 <- kmeans(x = datos, centers = 2, nstart = 50)
datos <- datos %>% mutate(cluster = km_clusters_2$cluster)
p1 <- ggplot(data = datos, aes(x = x, y = y, color = as.factor(cluster))) +
      geom_point(size = 3) +
      labs(title = "Kmenas con k=2") +
      theme_bw() +
      theme(legend.position = "none")


# Resultados para K = 6
datos <- datos %>% select(x, y)
set.seed(101)
km_clusters_6 <- kmeans(x = datos, centers = 6, nstart = 50)
datos <- datos %>% mutate(cluster = km_clusters_6$cluster)
p2 <- ggplot(data = datos, aes(x = x, y = y, color = as.factor(cluster))) +
      geom_point(size = 3) +
      labs(title = "Kmenas con k=6") +
      theme_bw() +
      theme(legend.position = "none")

ggarrange(p1, p2)


# Al observar los resultados obtenidos para K = 2, es intuitivo pensar que el grupo que se
# encuentra entorno a las coordenadas x=−1,y=6 (mayoritariamente considerado como azul)
# debería ser un grupo separado. Para K = 6 no parece muy razonable la separación de los
# grupos rojo y verde. Este ejemplo muestra la principal limitación del método de K-means,
# el hecho de tener que escoger de antemano el número de clusters que se generan. El siguiente
# ejemplo, se muestra una de las estrategias para identificar posibles valores de K óptimos.
