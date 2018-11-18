# El set de datos USArrests del paquete básico de R contiene el porcentaje de asaltos (Assault),
# asesinatos (Murder) y secuestros (Rape) por cada 100,000 habitantes para cada uno de los 50
# estados de USA (1973). Además, también incluye el porcentaje de la población de cada estado
# que vive en zonas rurales (UrbanPoP).

data("USArrests")
head(USArrests)


# El promedio de los datos muestra que hay tres veces más secuestros que asesinatos y 8 veces
# más asaltos que secuestros.

apply(X = USArrests, MARGIN = 2, FUN = mean)

# La varianza es muy distinta entre las variables, en el caso de Assault, la varianza es varios
# órdenes de magnitud superior al resto.
apply(X = USArrests, MARGIN = 2, FUN = var)

# Si no se estandarizan las variables para que tengan media cero y desviación estándar 1 antes
# de realizar el estudio PCA, la variable Assault dominará la mayoría de las componentes principales.

# La función prcomp() es una de las múltiples funciones en R que realizan PCA. Por defecto,
# prcomp() centra las variables para que tengan media cero, pero si se quiere además que su
# desviación estándar sea de uno, hay que indicar scale = TRUE.
pca <- prcomp(USArrests, scale = TRUE)
names(pca)

# Los elementos center y scale almacenados en el objeto pca contienen la media y desviación
# típica de las variables previa estandarización (en la escala original).
pca$center

pca$scale

# rotation contiene el valor de los loadings ϕ para cada componente (eigenvector). El número
# máximo de componentes principales se corresponde con el mínimo(n-1,p), que en este caso
# es min(49,4)=4.

pca$rotation
# Analizar con detalle el vector de loadings que forma cada componente puede ayudar a interpretar
# que tipo de información recoge cada una de ellas. Por ejemplo, la primera componente es el
# resultado de la siguiente combinación lineal de las variables originales:

# PC1=−0.5358995 Murder−0.5831836 Assault−0.2781909 UrbanPop−0.5434321 Rape

# Los pesos asignados en la primera componente a las variables Assault, Murder y Rape son
# aproximadamente iguales entre ellos y bastante superiores al asignado a UrbanPoP, esto
# significa que la primera componente recoge mayoritariamente la información correspondiente
# a los delitos. En la segunda componente, es la variable UrbanPoP la que tiene con diferencia
# mayor peso, por lo que se corresponde principalmente con el nivel de urbanización del estado.
# Si bien en este ejemplo la interpretación de las componentes es bastante clara, no en todos
# los casos ocurre lo mismo.

# La función prcomp() calcula automáticamente el valor de las componentes principales para cada
# observación (principal component scores) multiplicando los datos por los vectores de loadings.
# El resultado se almacena en la matriz x.
head(pca$x)

dim(pca$x)

# Mediante la función biplot() se puede obtener una representación bidimensional de las dos
# primeras componentes. Es recomendable indicar el argumento scale = 0 para que las flechas
# estén en la misma escala que las componentes.
biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

# La imagen especular, cuya interpretación es equivalente, se puede obtener invirtiendo el
# signo de los loadings y de los principal component scores.
pca$rotation <- -pca$rotation
pca$x        <- -pca$x
biplot(x = pca, scale = 0, cex = 0.6, col = c("blue4", "brown3"))

# Una vez calculadas las componentes principales, se puede conocer la varianza explicada por
# cada una de ellas, la proporción respecto al total y la proporción de varianza acumulada.
library(ggplot2)
pca$sdev^2

prop_varianza <- pca$sdev^2 / sum(pca$sdev^2)
prop_varianza

ggplot(data = data.frame(prop_varianza, pc = 1:4),
       aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")


prop_varianza_acum <- cumsum(prop_varianza)
prop_varianza_acum


ggplot(data = data.frame(prop_varianza_acum, pc = 1:4),
       aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. varianza explicada acumulada")

# En este caso, la primera componente explica el 62% de la varianza observada en los datos y
# la segunda el 24.7%. Las dos últimas componentes no superan por separado el 1% de varianza
# explicada. Si se empleasen únicamente las dos primeras componentes se conseguiría explicar
# el 86.75% de la varianza observada. 
