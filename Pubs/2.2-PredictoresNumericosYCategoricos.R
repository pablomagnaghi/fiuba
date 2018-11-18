# Se dispone de un dataset que contiene informacion de 30 libros. Se conoce del
# peso total de cada libro, el volumen que tiene y el tipo de tapas (duras o
# blandas). Se quiere generar un modelo lineal múltiple que permita predecir
# el peso de un libro en funcion de su volumen y del tipo de tapas.

datos <- data.frame(peso = c(800, 950, 1050, 350, 750, 600, 1075, 250, 700,
                             650, 975, 350, 950, 425, 725),
                    volumen = c(885, 1016, 1125, 239, 701, 641, 1228, 412, 953,
                                929, 1492, 419, 1010, 595, 1034),
                    tipo_tapas = c("duras", "duras", "duras", "duras", "duras",
                                   "duras", "duras", "blandas", "blandas",
                                   "blandas", "blandas", "blandas", "blandas",
                                   "blandas", "blandas"))
head(datos, 4)

# 1. Analizar la correlacion entre cada par de variables cuantitativas y
# diferencias del valor promedio entre las categoricas

# Se enfrentan cada par de variables cuantitativas mediante un diagrama de
# dispersion multiple (pairwise scatterplot) para intuir si existe relacion
# lineal o monotonica con la variable respuesta. Si no la hay, no es adecuado
# emplear un modelo de regresión lineal. Además, se estudia la relación entre
# variables para detectar posible colinealidad. Para las variables de tipo
# categorico se genera un boxplot con sus niveles para intuir su influencia
# en la variable dependiente.
pairs(datos)

cor.test(datos$peso, datos$volumen, method = "pearson")

ggplot(data = datos, mapping=aes(x = tipo_tapas, y = peso, color=tipo_tapas)) +
geom_boxplot() +
geom_jitter(width = 0.1) +
theme_bw() + theme(legend.position = "none")

# El analisis grafico y de correlacion muestran una relacion lineal significativa
# entre la variable peso y volumen. La variable tipo_tapas parece influir de
# forma significativa en el peso. Ambas variables pueden ser buenos predictores
# en un modelo lineal multiple para la variable dependiente peso.

# 2. Generar el modelo lineal multiple
modelo <- lm(peso ~ volumen + tipo_tapas, data = datos)
summary(modelo)

confint(modelo)

# Cada una de las pendientes de un modelo de regresión lineal multiple se define
# del siguiente modo: Si el resto de variables se mantienen constantes, por cada
# unidad que aumenta el predictor en cuestion, la variable Y varia en promedio
# tantas unidades como indica la pendiente. En el caso del predictor volume,
# si el resto de variables no varian, por cada unidad de volumen que aumenta el
# libro el peso se incrementa en promedio 0.71795 unidades.

# Cuando un predictor es cualitativo, uno de sus niveles se considera de
# referencia (el que no aparece en la tabla de resultados) y se le asigna el
# valor de 0. El valor de la pendiente de cada nivel de un predictor cualitativo
# se define como el promedio de unidades que dicho nivel está por encima o
# debajo del nivel de referencia. Para el predictor tipo_tapas, el nivel de
# referencia es tapas blandas por lo que si el libro tiene este tipo de tapas
# se le da a la variable el valor 0 y si es de tapas duras el valor 1. Acorde
# al modelo generado, los libros de tapa dura son en promedio 184.04727 unidades
# de peso superiores a los de tapa blanda.

# Peso libro=13.91557+0.71795 volumen+184.04727 tipotapas

# El modelo es capaz de explicar el 92.75% de la variabilidad observada en el
# peso de los libros (R-squared: 0.9275). El valor de R2-ajustado es muy alto y
# cercano al R2 (Adjusted R-squared: 0.9154) lo que indica que el modelo
# contiene predictores utiles. El test F muestra un p-value de 1.455e-07 por lo
# que el modelo en conjunto es significativo. Esto se corrobora con el p-value
# de cada predictor, en ambos casos significativo.

# 3. Eleccion de los predictores
# En este caso, al solo haber dos predictores, a partir del summary del modelo
# se identifica que ambas variables incluidas son importantes.

# 4. Condiciones para la regresion multiple lineal

# 4.1. Relacion lineal entre los predictores numericos y la variable dependiente:
library(ggplot2)
ggplot(data = datos, aes(x = volumen, y = modelo$residuals)) +
geom_point() +
geom_smooth(color = "firebrick") +
geom_hline(yintercept = 0) +
theme_bw()

# Se satisface la condicion de linealidad. Se aprecia un posible dato atipico.

# 4.2. Distribucion normal de los residuos:
qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

# La condicion de normalidad no se satisface, posiblemente debido a un dato
# atipico. Se repite el analisis excluyendo la observacion a la que pertenece
# el residuo atipico.

which.max(modelo$residuals)

shapiro.test(modelo$residuals[-13])

# Se confirma que los residuos sí se distribuyen de forma normal a excepcion de
# un dato extremo. Es necesario estudiar en detalle la influencia de esta
# observacion para determinar si el modelo es más preciso sin ella.

# 4.3. Variabilidad constante de los residuos:
ggplot(data = data.frame(predict_values = predict(modelo),
                         residuos = residuals(modelo)),
       aes(x = predict_values, y = residuos)) +
    geom_point() +
    geom_smooth(color = "firebrick", se = FALSE) +
    geom_hline(yintercept = 0) +
    theme_bw()

library(lmtest)
bptest(modelo)

# No hay evidencias que indiquen falta de homocedasticidad.

# 4.4. No multicolinialidad:

# Dado que solo hay un predictor cuantitativo no se puede dar colinialidad.

# 4.5. Autocorrelacion:

library(car)
dwt(modelo,alternative = "two.sided")

# No hay evidencia de autocorrelación

# 4.6. Tamaño de la muestra:
# No existe una condicion establecida para el número minimo de observaciones
# pero, para prevenir que una variable resulte muy influyente cuando realmente
# no lo es, se recomienda que la cantidad de observaciones sea entre 10 y 20
# veces el numero de predictores. En este caso deberia haber como minimo 20
# observaciones y se dispone de 15 por lo que se deberia considerar incrementar
# la muestra.

# 5. Identificacion de posibles valores atipicos o influyentes
library(car)
outlierTest(modelo)

# Tal como se aprecio en el estudio de normalidad de los residuos, la
# observacion 13 tiene un residuo estandarizado >3 (mas de 3 veces la desviacion
# estandar de los residuos) por lo que se considera un dato atipico.
# El siguiente paso es determinar si es influyente.

summary(influence.measures(modelo))

influencePlot(modelo)

# El analisis muestran varias observaciones influyentes aunque ninguna excede
# los limites de preocupacion para los valores de Leverages hat
# (>2.5(2+1)/15=0.5) o Distancia Cook (>1). Estudios mas exhaustivos
# consistirian en rehacer el modelo sin las observaciones y ver el impacto.

# 6. Conclusion
# El modelo lineal multiple
# Peso libro=13.91557+0.71795 volumen+184.04727 tipotapas
# es capaz de explicar el 92.75% de la variabilidad observada en el peso de los
# libros (R-squared: 0.9275, Adjusted R-squared: 0.9154). El test F muestra
# que es significativo (1.455e-07). Se satisfacen todas las condiciones
# para este tipo de regresion. 
