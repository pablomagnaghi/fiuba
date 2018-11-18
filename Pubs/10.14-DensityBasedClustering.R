# El set de datos multishape del paquete factoextra contiene observaciones que pertenecen
# a 5 grupos distintos junto con cierto ruido (outliers). Como se espera que la distribución
# espacial de los grupos no sea esférica, se aplica el método de clustering DBSCAN.

# En R existen dos paquetes con funciones que permiten aplicar el algoritmo DBSCAN: fpc y
# dbscan. El segundo contiene una modificación del algoritmo original que lo hace más rápido.
# La función kNNdistplot del paquete dbscan calcula y representa las k-distancias para ayudar a
# identificar el valor óptimo de epsilon.
library(fpc)
library(dbscan)
library(factoextra)
data("multishapes")
datos <- multishapes[, 1:2]

# Selección del valor óptimo de epsilon. Como valor de minPts se emplea 5.
dbscan::kNNdistplot(datos, k = 5)

# La curva tiene el punto de inflexión en torno a 0.15, por lo que se escoge este valor como
# epsilon para DBSCAN.
set.seed(321)

# DBSCAN con epsilon = 0.15 y minPts = 5
dbscan_cluster <- fpc::dbscan(data = datos, eps = 0.15, MinPts = 5)

# Resultados de la asignación
head(dbscan_cluster$cluster)

# Visualización de los clusters
fviz_cluster(object = dbscan_cluster, data = datos, stand = FALSE,
             geom = "point", ellipse = FALSE, show.clust.cent = FALSE,
             pallete = "jco") +
  theme_bw() +
  theme(legend.position = "bottom")

