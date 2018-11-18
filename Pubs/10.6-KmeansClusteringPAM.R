# El set de datos USArrests contiene información sobre el número de delitos (asaltos,
# asesinatos y secuestros) junto con el porcentaje de población urbana para cada uno de
# los 50 estados de USA. Se pretende estudiar si existe una agrupación subyacente de los
# estados mediante clustering. Dado que se sospecha de la presencia de outliers se recurre a K-medoids.

# El proceso a seguir en R para aplicar el método de K-medoids es igual al seguido en K-means
# (ejemplo 2), pero en este caso empleando la función pam() del paquete cluster.
data("USArrests")
str(USArrests)

# Como la magnitud de los valores difiere notablemente entre variables, se procede a escalarlas
# antes de aplicar el clustering.
datos <- scale(USArrests)

# Se evalúa la reducción de varianza total intra-cluster para un rango de valores K con el objetivo
# de identificar el número óptimo de clusters (elbow method). En este caso, dado que se sospecha
# de la presencia de outliers, se emplea la distancia de Manhattan como medida de similitud.

library(cluster)
library(factoextra)
fviz_nbclust(x = datos, FUNcluster = pam, method = "wss", k.max = 15,
             diss = dist(datos, method = "manhattan"))

# Mismo análisis pero sin recurrir a factoextra
# ==============================================================================
calcular_suma_dif_interna <- function(n_clusters, datos, distancia = "manhattan"){
  # Esta función aplica el algoritmo pam y devuelve la suma total de las
  # diferencias internas
  cluster_pam <- cluster::pam(x = datos, k = n_clusters, metric = distancia)
  # El objeto pam almacena la suma de las diferencias respecto a los medoides en
  # $objective["swap"]
  return(cluster_pam$objective["swap"])
}

# Se aplica esta función con para diferentes valores de k
suma_dif_interna <- map_dbl(.x = 1:15,
  .f = calcular_suma_dif_interna,
  datos = datos)
  data.frame(n_clusters = 1:15, suma_dif_interna = suma_dif_interna) %>%
  ggplot(aes(x = n_clusters, y = suma_dif_interna)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = 1:15) +
    labs(title = "Evolución de la suma total de diferencias intra-cluster") +
    theme_bw()

# Al igual que ocurría al aplicar K-means a estos datos, a partir de 4 clusters la
# reducción en la suma total de diferencias internas parece estabilizarse, indicando que
# K = 4 es una buena opción.

set.seed(123)
pam_clusters <- pam(x = datos, k = 4, metric = "manhattan")
pam_clusters

# El objeto devuelto por pam() contiene entre otra información: las observaciones que finalmente
# se han seleccionado como medoids ($medoids) y el cluster al que se ha asignado cada observación
# ($clustering).

fviz_cluster(object = pam_clusters, data = datos, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")

  # Como en k-medoids no hay centroides, no se muestran en la representación ni
  # tampoco las distancias desde este al resto de observaciones

  # La función fviz_cluster() no permite resaltar las observaciones que actúan como medoids,
  # sin embargo, al tratarse de un objeto ggplot2, es sencillo conseguirlo.

  # Como hay más de 2 variables, se están representando las 2 primeras componentes
  # de un PCA. Se tienen que calcular el PCA y extraer las proyecciones almacenadas
  # en el elemento x.
  medoids <- prcomp(datos)$x

  # Se seleccionan únicamente las proyecciones de las observaciones que son medoids
  medoids <- medoids[rownames(pam_clusters$medoids), c("PC1", "PC2")]
  medoids <- as.data.frame(medoids)

  # Se emplean los mismos nombres que en el objeto ggplot
  colnames(medoids) <- c("x", "y")

  # Creación del gráfico
  fviz_cluster(object = pam_clusters, data = datos, ellipse.type = "t",
               repel = TRUE) +
    theme_bw() +
    # Se resaltan las observaciones que actúan como medoids
    geom_point(data = medoids, color = "firebrick", size = 2) +
    labs(title = "Resultados clustering PAM") +
    theme(legend.position = "none")

  