# El set de datos USArrests contiene información sobre el número de delitos (asaltos, asesinatos
# y secuestros) junto con el porcentaje de población urbana para cada uno de los 50 estados de USA.
# Se pretende estudiar si existe una agrupación subyacente de los estados empleando K-means-clustering.

# El paquete factoextra creado por Alboukadel Kassambara contiene funciones que facilitan en gran
# medida la visualización y evaluación de los resultados de clustering.

# Si se emplea K-means-clustering con distancia euclídea hay que asegurarse de que las variables
# empleadas son de tipo continuo, ya que trabaja con la media de cada una de ellas.

data("USArrests")
head(USArrests)

str(USArrests)

# Como la magnitud de los valores difiere notablemente entre variables, se procede a escalarlas
# antes de aplicar el clustering.
datos <- scale(USArrests)

# Una forma sencilla de estimar el número K óptimo de clusters cuando no se dispone de información
# adicional en la que basarse, es aplicar el algoritmo de K-means para un rango de valores de K e
# identificar aquel valor a partir del cual la reducción en la suma total de varianza intra-cluster
# deja de ser sustancial. A esta estrategia se la conoce como método del codo o elbow method (en
# los siguientes apartados se detallan otras opciones).

# La función fviz_nbclust() automatiza este proceso y genera una representación de los resultados.
library(factoextra)
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "wss", k.max = 15,
             diss = get_dist(datos, method = "euclidean"), nstart = 50)

# Este mismo análisis también puede realizarse sin recurrir a la función fviz_nbclust().
calcular_totwithinss <- function(n_clusters, datos, iter.max=1000, nstart=50){
  # Esta función aplica el algoritmo kmeans y devuelve la suma total de
  # cuadrados internos.
  cluster_kmeans <- kmeans(centers = n_clusters, x = datos, iter.max = iter.max,
                           nstart = nstart)
  return(cluster_kmeans$tot.withinss)
}

# Se aplica esta función con para diferentes valores de k
total_withinss <- map_dbl(.x = 1:15,
                          .f = calcular_totwithinss,
                          datos = datos)
total_withinss


data.frame(n_clusters = 1:15, suma_cuadrados_internos = total_withinss) %>%
  ggplot(aes(x = n_clusters, y = suma_cuadrados_internos)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = 1:15) +
    labs(title = "Evolución de la suma total de cuadrados intra-cluster") +
    theme_bw()

# En este análisis, a partir de 4 clusters la reducción en la suma total de cuadrados internos
# parece estabilizarse, indicando que K = 4 es una buena opción.

# El paquete factoextra también permite obtener visualizaciones de las agrupaciones resultantes.
# Si el número de variables (dimensionalidad) es mayor de 2, automáticamente realiza un PCA y
# representa las dos primeras componentes principales.

set.seed(123)
km_clusters <- kmeans(x = datos, centers = 4, nstart = 50)

# Las funciones del paquete factoextra emplean el nombre de las filas del
# dataframe que contiene los datos como identificador de las observaciones.
# Esto permite añadir labels a los gráficos.
fviz_cluster(object = km_clusters, data = datos, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

# Otra forma de representar los resultados obtenidos con un clustering k-means es mediante una
# network que muestre las observaciones conectadas por* clusters*. Es importante tener en cuenta
# que, a diferencia de la proyección PCA, en esta representación las distancias son arbitrarias,
# solo importan las conexiones.
set.seed(123)
km_clusters <- kmeans(x = datos, centers = 4, nstart = 50)
resultados <- data.frame(ciudad  = names(km_clusters$cluster),
                         cluster = as.factor(km_clusters$cluster)) %>%
              arrange(cluster)
library(igraph)
library(tidygraph)
library(ggraph)
datos_graph <- graph_from_data_frame(d = resultados, directed = TRUE)
datos_graph <- as_tbl_graph(datos_graph)

# Se añade información sobre a que cluster pertenece cada observacion
datos_graph <- datos_graph %>%
  activate(nodes) %>%
  left_join(resultados, by = c("name" = "ciudad"))

ggraph(graph = datos_graph) +
  geom_edge_link(alpha = 0.5) +
  geom_node_point(aes(color = cluster)) +
  geom_node_text(aes(label = name), repel = TRUE, alpha = 0.5, size = 3) +
  labs(title = "Resultados clustering K-means") +
  theme_graph()

