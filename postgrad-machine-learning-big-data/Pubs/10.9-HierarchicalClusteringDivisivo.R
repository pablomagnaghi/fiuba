# Con los mismos datos que en el ejemplo anterior, se emplea la función diana() de paquete
# cluster para crear un heirarchical clustering divisivo con diatancia euclídea.
set.seed(101)
# Se simulan datos aleatorios con dos dimensiones
datos <- matrix(rnorm(n = 100*2), nrow = 100, ncol = 2,
                dimnames = list(NULL,c("x", "y")))
datos <- as.data.frame(datos)


library(cluster)
library(dendextend)
library(factoextra)
library(mclust)

matriz_distancias <- dist(x = datos[, c("x", "y")], method = "euclidean")
hc_diana <- diana(x = matriz_distancias, diss = TRUE, stand = FALSE)
fviz_dend(x = hc_diana, cex = 0.5) +
  labs(title = "Hierarchical clustering divisivo",
       subtitle = "Distancia euclídea")

