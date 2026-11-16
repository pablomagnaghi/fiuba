# Se quiere estudiar la relacion entre el tamaño de diferentes
# partes de las flores. Para ello, se dispone del set de datos
# iris que consta de cuatro variables numericas.
data(iris)
str(iris)
# Se seleccionan unicamente las variables numericas
datos <- iris[,c(1,2,3,4)]
head(datos)

pairs(x = datos, lower.panel = NULL)

# No se muestra la diagonal inferior ya que es lo mismo que la superior
cor(x = datos, method = "pearson")
# install.packages('psych')'
require(psych)
# dev.new()
multi.hist(x = datos, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")
# dev.off()

# La funcion corrplot() del paquete corrplot recibe como argumento la
# matriz de correlaciones generada por la función cor() y genera
# diferentes tipos de heat maps mucho mas visuales que la matriz numerica.
# install.packages('corrplot')
require(corrplot)
corrplot(corr = cor(x = datos, method = "pearson"), method = "number")

# Otros paquetes permiten representar a la vez los diagramas de dispersion
# y los valores de correlación para cada par de variables. Ademas de la
# distribucion de cada una de las variables.

require(psych)
pairs.panels(x = datos, ellipses = FALSE, lm = TRUE, method = "pearson")

# La funcion ggpairs() del paquete GGally basada en ggplot2 representa los
# diagramas de dispersion, el valor de la correlacion e incluso interpreta
# el tipo de variable para que, en caso de ser categorica, representarla en
# forma de boxplot.
# install.packages('GGally')
require(GGally)
ggpairs(iris, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

