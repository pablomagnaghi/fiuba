# El set de datos NCI60 contiene información genética de 64 líneas celulares cancerígenas.
# Para cada una de ellas, se ha cuantificado la expresión de 6830 genes mediante tecnología
# microarray. Los investigadores conocen el tipo de cáncer (histopatología) al que pertenece
# cada línea celular y quieren utilizar esta información para evaluar si el método de clustering
# (agglomerative hierarchical clustering) es capaz de agrupar correctamente las líneas empleando
# los niveles de expresión génica.

# Los métodos de clustering son unsupervised, lo que significa que al aplicarlos no se hace
# uso de la variable respuesta, en este caso el tipo de cáncer. Una vez obtenidos los resultados
# se añade esta información para determinar si es posible agrupar a las líneas celulares empleando
# su perfil de expresión.
library(ISLR)
data(NCI60)
str(NCI60)

# Los datos están almacenados en forma de lista, un elemento contiene los niveles
# de expresión y otro el tipo de cáncer
expresion   <- NCI60$data
tipo_cancer <- NCI60$labs

# Una exploración inicial de los datos permite conocer el número de líneas celulares que hay de
# cada tipo de cáncer.
table(tipo_cancer)

# El siguiente paso antes de aplicar el método de clustering es decidir cómo se va a cuantificar
# la similitud. Los perfiles de expresión génica son un claro ejemplo de que es necesario comprender
# el problema en cuestión para hacer la elección adecuada.

# Una vez aplicada la estandarización, las distancias obtenidas en las comparaciones tienen más
# sentido dentro del contexto de los perfiles de expresión génica. La similitud entre el tumor A
# y el de referencia es total (distancia 0), y el tumor B pasa a ser el más distinto. El mismo
# resultado se hubiese obtenido empleando como medida de similitud la correlación lineal de Pearson.

# Se procede a escalar la matriz de expresión NCI60 y se aplica hierarchical clustering con
# linkage completo, average y single.

expresion <- scale(expresion, center = TRUE, scale = TRUE)
matriz_distancias <- dist(x = expresion, method = "euclidean")
hc_completo <- hclust(d = matriz_distancias, method = "complete")
hc_average  <- hclust(d = matriz_distancias, method = "average")
hc_single   <- hclust(d = matriz_distancias, method = "single")
par(mfrow = c(3, 1))
plot(hc_completo, labels = tipo_cancer, ylab = "", xlab = "", sub = "",
     main = "Linkage completo", cex = 0.8)
plot(hc_average, labels = tipo_cancer, ylab = "", xlab = "", sub = "",
     main = "Linkage average", cex = 0.8)
plot(hc_single, labels = tipo_cancer, ylab = "", xlab = "", sub = "",
     main = "Linkage single", cex = 0.8)

# La elección del tipo de linkage influye de forma notable en los dendrogramas obtenidos. Por lo
# general, single linkage tiende a formar clusters muy grandes en los que las observaciones
# individuales se unen una a una. El completo y average suele generar dendrogramas más compensados
# con clusters más definidos, tal como ocurre en este ejemplo.

# A pesar de que la agrupación no es perfecta, los clusters tienden a segregar bastante bien las
# líneas celulares procedentes de leukemia, melanoma y renal. Véase los aciertos cuando el
# dendrograma se corta a una altura tal que genera 4 clusters.
clusters <- cutree(tree = hc_completo, k = 4)
table(clusters, tipo_cancer, dnn = list("clusters", "tipo de cáncer"))


# Todas las líneas celulares de leukemia caen en el cluster 3, todas las de melanoma y ovarian
# en el 1. Las líneas celulares de breast son las más distribuidas (heterogéneas en su perfil
# genético), ya que están presentes en los clusters 1, 2 y 4.

# Nota: de los 6830 genes medidos, muchos pueden estar aportando información redundante o puede
# que no varíen lo suficiente como para contribuir al modelo. Con el fin de eliminar todo ese
# ruido y mejorar los resultados del clustering, es aconsejable filtrar aquellos genes cuya
# expresión no supere un mínimo de varianza. Del mismo modo, puede ser útil evaluar si aplicando
# un Principal Component Analysis se consigue capturar la mayor parte de la información en unas
# pocas componentes y utilizarlas como variables de clustering. Este es un problema muy importante
# en la aplicación del clustering a perfiles genéticos. En la siguiente sección se describe con más
# detalle cómo solucionarlo. 
