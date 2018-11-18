# Para ilustrar la aplicación del método CLARA se simula un set de datos bidimensional
# (dos variables) con 500 observaciones, de las cuales 200 pertenecen a un grupo y 300 a otro
# (número de grupos reales = 2)
set.seed(1234)
grupo_1 <- cbind(rnorm(n = 200, mean = 0, sd = 8),
                 rnorm(n = 200, mean = 0, sd = 8))
grupo_2 <- cbind(rnorm(n = 300, mean = 30, sd = 8),
                 rnorm(n = 300, mean = 30, sd = 8))
datos <- rbind(grupo_1, grupo_2)
colnames(datos) <- c("x", "y")
head(datos)

# La función clara() del paquete cluster permite aplicar el algoritmo CLARA. Entre sus argumentos
# destaca: una matriz numérica x donde cada fila es una observación, el número de clusters k,
# la medida de distancia empleada metric (euclídea o manhattan), si los datos se tienen que
# estandarizar stand, el número de partes samples en las que se divide el set de datos (recomendable 50)
# y si se utiliza el algoritmo PAM pamLike.

library(cluster)
library(factoextra)
clara_clusters <- clara(x = datos, k = 2, metric = "manhattan", stand = TRUE,
                        samples = 50, pamLike = TRUE)
clara_clusters

fviz_cluster(object = clara_clusters, ellipse.type = "t", geom = "point",
             pointsize = 2.5) +
  theme_bw() +
  labs(title = "Resultados clustering CLARA") +
  theme(legend.position = "none")

