# El set de datos diabetes del paquete mclust contiene 3 parámetros sanguíneos medidos en 145
# pacientes con 3 tipos distintos de diabetes. Se pretende emplear model-based-clustering para
# encontrar las agrupaciones.

library(mclust)
data("diabetes")
head(diabetes)

# Estandarización de variables
datos <- scale(diabetes[, -1])

# Model-based-clustering
model_clustering <- Mclust(data = datos, G = 1:10)
summary(model_clustering)

# El algoritmo de ajuste selecciona como mejor modelo el formado por 3 clusters, cada uno con
# forma elipsoidal y con volume, shape y orientation propias.

# El clustering basado en modelos es de tipo fuzzy, es decir, para cada observación se calcula
# un grado de pertenencia a cada cluster y se asigna finalmente al que mayor valor tiene.

# Grado de asignación a cada cluster
head(model_clustering$z)

# Clasificación final
head(model_clustering$classification)

# La visualización del clustering puede hacerse mediante la función plot.Mclust() o mediante fviz_mclust().

library(factoextra)
# Curvas del valor BIC en función del número de clusters para cada modelo.
# Atención al orden en el que se muestra la variable horizontal, por defecto es
# alfabético.
fviz_mclust(object = model_clustering, what = "BIC", pallete = "jco") +
  scale_x_discrete(limits = c(1:10))

# Clusters
fviz_mclust(model_clustering, what = "classification", geom = "point",
              pallete = "jco")

# Certeza de las clasificaciones. Cuanto mayor el tamaño del punto menor la
# seguridad de la asignación
fviz_mclust(model_clustering, what = "uncertainty", pallete = "jco")

