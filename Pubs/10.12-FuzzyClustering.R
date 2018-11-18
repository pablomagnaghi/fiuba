# El set de datos USArrests contiene información sobre el número de delitos (asaltos, asesinatos
# y secuestros) junto con el porcentaje de población urbana para cada uno de los 50 estados de USA.
# Se pretende estudiar si existe una agrupación subyacente de los estados empleando fuzzy clustering.

data("USArrests")

# Como la magnitud de los valores difiere notablemente entre variables, se procede a escalarlas antes
# de aplicar el clustering.

datos <- scale(USArrests)

# La función fanny() (fuzzy analysis) del paquete cluster permite aplicar el algoritmo c-means clustering.

library(cluster)
fuzzy_cluster <- fanny(x = datos, diss = FALSE, k = 3, metric = "euclidean",
                       stand = FALSE)

# El objeto devuelto fanny() incluye entre sus elementos: una matriz con el grado de pertenencia de
# cada observación a cada cluster (las columnas son los clusters y las filas las observaciones).

head(fuzzy_cluster$membership)

# El coeficiente de partición Dunn normalizado y sin normalizar. Valores normalizados próximos a 0
# indican que la estructura tiene un alto nivel fuzzy y valores próximos a 1 lo contrario.
fuzzy_cluster$coeff

# El cluster al que se ha asignado mayoritariamente cada observación.

head(fuzzy_cluster$clustering)

# Para obtener una representación gráfica del clustering se puede emplear la función fviz_cluster().

library(factoextra)
fviz_cluster(object = fuzzy_cluster, repel = TRUE, ellipse.type = "norm",
             pallete = "jco") + theme_bw() + labs(title = "Fuzzy Cluster plot")
