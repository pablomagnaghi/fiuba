# El siguiente es un ejemplo que muestra cómo se pueden calcular las componentes principales
# de un set de datos mediante la identificación de eigenvectors y eigenvalues por el método
# de covarianza. Con el objetivo de ser un ejemplo sencillo, el set de datos empleado tiene
# dos dimensiones (variables). La mayoría de programas de análisis, entre ellos R, disponen
# de funciones que devuelven directamente el valor de las componentes principales (ver ejemplo siguiente).
datos <- data.frame(X1 = c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2, 1, 1.5, 1.1),
                    X2 = c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7, 1.6, 1.1, 1.6, 0.9))
datos

# En primer lugar se resta a cada valor la media de la variable a la que pertenece. Con esto se
# consigue centralizar las variables y que su media sea 0.

datos_centrados <- datos
datos_centrados$X1 <- datos$X1 - mean(datos$X1)
datos_centrados$X2 <- datos$X2 - mean(datos$X2)
datos_centrados

# A continuación se calcula la matriz de correlaciones entre cada par de variables. Como en
# este ejemplo hay dos variables, el resultado es una matriz simétrica 2x2.

matriz_cov <- cov(datos_centrados)
matriz_cov

# Dado que la matriz de covarianzas es cuadrada, se pueden obtener sus correspondientes eigenvectors
# y eigenvalues. La función eigen() calcula ambos y los almacena en una lista bajo el nombre de
# vectors y values. Los eigenvalues se devuelven en orden decreciente y los eigenvectors
# (estandarizados) se ordenan de izquierda a derecha acorde a sus eigenvalues asociados.

eigen <- eigen(matriz_cov)
eigen$values

eigen$vectors

# Los eigenvectors ordenados de mayor a menor eigenvalues se corresponden con las componentes principales.
# Una vez obtenidos los eigenvectors (componentes principales) se calcula el valor que
# toma cada componente para cada observación en función de las variables originales
# (principal component scores). Para ello, simplemente se tienen que multiplicar los
# eigenvectors transpuestos por los datos originales centrados y también transpuestos.

t_eigenvectors <- t(eigen$vectors)
t_eigenvectors

t_datos_centrados <- t(datos_centrados)
t_datos_centrados

# Producto matricial
pc_scores <- t_eigenvectors %*% t_datos_centrados
rownames(pc_scores) <- c("PC1", "PC2")

# Se vuelve a transponer para que los datos estén en modo tabla
t(pc_scores)

# En este ejemplo, al partir inicialmente de un espacio muestral con 2 dimensiones y haber
# calculado sus 2 componentes principales, no se ha reducido la dimensionalidad ni se ha
# perdido nada de información presente en los datos originales.

# Cuando se calculan y conservan todos los eigenvectors (componentes principales), es posible
# recuperar de nuevo los valores iniciales.
datos_recuperados <- t(eigen$vectors %*% pc_scores)
datos_recuperados[, 1] <- datos_recuperados[, 1] + mean(datos$X1)
datos_recuperados[, 2] <- datos_recuperados[, 2] + mean(datos$X2)

datos_recuperados

datos

# En la mayoría de programas estadísticos, entre ellos R, existen funciones que calculan
# directamente las componentes principales y los principal component scores de cada observación
# sin tener que ir paso por paso. 
