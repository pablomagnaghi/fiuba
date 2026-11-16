# El set de datos USArrests contiene información sobre el número de delitos (asaltos,
# asesinatos y secuestros) junto con el porcentaje de población urbana para cada uno
# de los 50 estados de USA. Se pretende estudiar si existe una agrupación subyacente
# de los estados empleando Hierarchical K-means clustering.
data("USArrests")

# Como la magnitud de los valores difiere notablemente entre variables, se procede a
# escalarlas antes de aplicar el clustering.
datos <- scale(USArrests)

# La función hkmeans() del paquete factoextra permite aplicar el método hierarchical
# K-means clustering de forma muy similar a la función estándar kmeans().

library(factoextra)
# Se obtiene el dendrograma de hierarchical clustering para elegir el número de
# clusters.
set.seed(101)
hc_euclidea_completo <- hclust(d = dist(x = datos, method = "euclidean"),
                               method = "complete")
fviz_dend(x = hc_euclidea_completo, cex = 0.5, main = "Linkage completo",
          sub = "Distancia euclídea") +
  theme(plot.title =  element_text(hjust = 0.5, size = 15))

# Empleando la representación del dendrograma se considera que existen 4 grupos.

hkmeans_cluster <- hkmeans(x = datos, hc.metric = "euclidean",
                           hc.method = "complete", k = 4)
hkmeans_cluster


fviz_cluster(object = hkmeans_cluster, pallete = "jco", repel = TRUE) +
  theme_bw() + labs(title = "Hierarchical k-means Clustering")

