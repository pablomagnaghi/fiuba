# El siguiente es un ejemplo de como PCA puede emplearse para encontrar patrones cuando se
# dispone de muchos de predictores.

# Supóngase un equipo de investigación que se encarga de clasificar los tumores de pacientes
# en 3 subtipos. Dependiendo del subtipo, el paciente recibe una medicación diferente. El
# proceso de caracterización se hace mediante tinciones y observaciones al microscopio.
# Este proceso es muy laborioso y lento, lo que incrementa mucho el tiempo de respuesta de
# los médicos. Nuevos estudios apuntan a que cuantificando la expresión de un grupo de 9 genes
# se podría clasificar los tumores con una alta precisión. Se quiere determinar si tal patrón
# existe dentro de los datos.

# Se trata de un ejemplo muy simplificado (en la realidad suele disponerse de varios miles
# de genes). El método de PCA puede combinarse con técnicas de clustering para crear
# agrupaciones automáticamente. En este caso, simplemente se muestra como PCA puede ayudar
# a la identificación de patrones que facilitarán posibles agrupamientos posteriores.
#Se crean 3 perfiles modelo de expresión génica que pueden tener los tumores
tumor_1 <- c(1,1,0,-0.5,-1,-1,0,1,1)
tumor_2 <- c(-1,-1,-1 ,-0.5,0,0,1,1,1)
tumor_3 <- c(1,1,1 ,1,1,1,-1,-1,-1)

#Añadiendo ruido aleatorio se generan 3 muestras a partir de cada perfil modelo
set.seed(755)
tumor_a <- tumor_1 + rnorm(n = 9, mean = 0, sd = 0.2)
tumor_b <- tumor_1 + rnorm(n = 9, mean = 0, sd = 0.2)
tumor_c <- tumor_1 + rnorm(n = 9, mean = 0, sd = 0.2)
tumor_d <- tumor_2 + rnorm(n = 9, mean = 0, sd = 0.2)
tumor_e <- tumor_2 + rnorm(n = 9, mean = 0, sd = 0.2)
tumor_f <- tumor_2 + rnorm(n = 9, mean = 0, sd = 0.2)
tumor_g <- tumor_3 + rnorm(n = 9, mean = 0, sd = 0.2)
tumor_h <- tumor_3 + rnorm(n = 9, mean = 0, sd = 0.2)
tumor_i <- tumor_3 + rnorm(n = 9, mean = 0, sd = 0.2)

# Representación de los perfiles
plot(0,0, xlim = c(0,10), ylim = c(-2.5, 2.5), type = "n",
     main = "Perfil de expresión génica de 9 tumores")
lines(x = 1:9, y = tumor_a, type = "b", col = "blue")
lines(x = 1:9, y = tumor_b, type = "b", col = "blue")
lines(x = 1:9, y = tumor_c, type = "b", col = "blue")

lines(x = 1:9, y = tumor_d, type = "b", col = "firebrick")
lines(x = 1:9, y = tumor_e, type = "b", col = "firebrick")
lines(x = 1:9, y = tumor_f, type = "b", col = "firebrick")

lines(x = 1:9, y = tumor_g, type = "b", col = "cyan3")
lines(x = 1:9, y = tumor_h, type = "b", col = "cyan3")
lines(x = 1:9, y = tumor_i, type = "b", col = "cyan3")
legend("topright", legend = c("Tumor tipo 1", "Tumor tipo 2", "Tumor tipo 3"),
       col = c("blue", "firebrick", "cyan3"), lty = 1, cex = 0.65)

# La representación del perfil de expresión muestra que, aunque con variabilidad, cada
# grupo tiene su propio perfil característico.

# Se crea un data.frame con los datos para poder empelar ggplot2

library(ggplot2)
library(tidyr)
library(dplyr)
datos <- data.frame(gene = paste("gene_", 1:9, sep = ""), tumor_a, tumor_b,
                                 tumor_c, tumor_d, tumor_e, tumor_f, tumor_g,
                                 tumor_h, tumor_i)
datos_tidy <- gather(data = datos, key = tumor, value = expresion, -1)
ggplot(data = datos_tidy, aes(x = gene, y = expresion, color = tumor)) +
  geom_path(aes(group = tumor)) +
  geom_point() +
  theme_bw() +
  facet_grid(tumor~.) +
  theme(legend.position = "none")


# PCA es dependiente de la escala y varianza de los predictores. En este caso se va a suponer
# que los genes se han medido con el mismo instrumento y la misma escala. Solo queda entonces
# comprobar que ninguno de ellos tiene una varianza mucho mayor al resto.
datos_tidy %>% group_by(gene) %>% summarise(varianza = var(expresion)) %>%
  ggplot(aes(x = gene, y = varianza)) +
  geom_col() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# No hay ningún gen cuya varianza sea mucho mayor que la del resto, lo que significa que
# ninguno de ellos va dirigir el patrón resultante más que los demás.

### PCA
# Dado que se quieren estudiar patrones empleando la expresión de los genes,
# cada gen debe de estar en una columna (predictores)

datos_trans <- datos_tidy %>% spread(key = gene, value = expresion)
rownames(datos_trans) <- datos_trans$tumor
datos_trans <- datos_trans[, -1]
head(datos_trans)

pca <- prcomp(datos_trans)

# Cálculo de la varianza explicada acumulada
prop_varianza <- pca$sdev^2/sum(pca$sdev^2)
prop_varianza_acum <- cumsum(prop_varianza)
ggplot(data = data.frame(prop_varianza_acum, pc = factor(1:9)),
       aes(x = pc, y = prop_varianza_acum, group = 1)) +
  geom_point() +
  geom_line() +
  geom_label(aes(label = round(prop_varianza_acum,2))) +
  theme_bw() +
  labs(x = "Componentes principales",
       y = "Prop. varianza explicada acumulada")

# El estudio de la varianza explicada acumulada muestra que con solo dos componentes se
# puede capturar el 97% de la varianza, prácticamente la totalidad de la información
# contenida en la expresión de los 9 genes. Viendo en detalle el valor de los loadings
# de las dos primeras componentes se observa que en la primera los 9 genes tienen la
# misma importancia y lo mismo ocurre en la segunda, a excepción del gen 4 que tiene un peso mucho menor.
pca$rotation[, 1:2]

# Finalmente, la proyección de los datos en las dos primeras componentes muestra que el
# PCA ha sido capaz de encontrar un claro patrón que diferencia los 3 tipos de tumores.

biplot(x = pca, scale = 0, cex = 0.7, col = c("blue4", "brown3"))

