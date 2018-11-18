# Los siguientes datos simulados contienen observaciones que pertenecen a cuatro grupos
# distintos. Se pretende aplicar hierarchical clustering aglomerativo con el fin de identificarlos.
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
datos <- datos %>% select(grupo, x, y)
ggplot(data = datos, aes(x = x, y = y, color = grupo)) +
  geom_point(size = 2.5) +
  theme_bw()

# Al aplicar un hierarchical clustering aglomerativo se tiene que escoger una medida de
# distancia (1-similitud) y un tipo de linkage. En este caso, se emplea la función hclust(),
# a la que se pasa como argumento una matriz de distancia euclidea y el tipo de linkages. Se
# comparan los resultados con los linkages complete, single y average. Dado que los datos
# se han simulado considerando que las dos dimensiones tienen aproximadamente la misma
# magnitud, no es necesario escalarlos ni centrarlos.
# Se calculan las distancias
matriz_distancias <- dist(x = datos[, c("x", "y")], method = "euclidean")
set.seed(567)
hc_euclidea_completo <- hclust(d = matriz_distancias, method = "complete")
hc_euclidea_single   <- hclust(d = matriz_distancias, method = "single")
hc_euclidea_average  <- hclust(d = matriz_distancias, method = "average")

# Los objetos devueltos por hclust() pueden representarse en forma de dendrograma con
# la función  plot() o con la función fviz_dend() del paquete factoextra.

par(mfrow = c(3,1))
plot(x = hc_euclidea_completo, cex = 0.6, xlab = "", ylab = "", sub = "",
     main = "Distancia euclídea, Linkage complete")
plot(x = hc_euclidea_single, cex = 0.6, xlab = "", ylab = "", sub = "",
     main = "Distancia euclídea, Linkage single")
plot(x = hc_euclidea_average, cex = 0.6, xlab = "", ylab = "", sub = "",
     main = "Distancia euclídea, Linkage average")

# El conocer que existen 4 grupos en la población permite evaluar qué linkage consigue
# los mejores resultados. En este caso, los tres tipos identifican claramente 4 clusters,
# si bien esto no significa que en los 3 dendrogramas los clusters estén formados por
# exactamente las mismas observaciones.

# Una vez creado el dendrograma, se tiene que decidir a qué altura se corta para generar
# los clusters. La función cutree() recibe como input un dendrograma y devuelve el
# cluster al que se ha asignado cada observación dependiendo del número de clusters
# especificado (argumento k) o la altura de corte indicada (argumento h).

cutree(hc_euclidea_completo, k = 4)

cutree(hc_euclidea_completo, h = 6)

# Una forma visual de comprobar los errores en las asignaciones es indicando en el argumento
# labels el grupo real al que pertenece cada observación. Si la agrupación resultante coincide
# con los grupos reales, entonces, dentro de cada clusters las labels serán las mismas.

plot(x = hc_euclidea_completo, cex = 0.6, sub = "",
     main = "Distancia euclídea, Linkage complete, k=4",
     xlab = "", ylab = "", labels = datos[, "grupo"])
abline(h = 6, lty = 2)

table(cutree(hc_euclidea_completo, h = 6), datos[, "grupo"])

# El método de hierarchical clustering aglomerativo con linkage completo y k=4 ha sido capaz de
# agrupar correctamente todas las observaciones. 
