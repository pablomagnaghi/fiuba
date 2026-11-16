require(MASS)
require(ISLR)
data("Boston")

# El dataset Boston del paquete MASS recoge la mediana del valor de la vivienda en 506 áreas
# residenciales de Boston. Junto con el precio, se han registrado 13 variables adicionales.

# crim: ratio de criminalidad per cápita de cada ciudad.
# zn: Proporción de zonas residenciales con edificaciones de más de 25.000 pies cuadrados.
# indus: proporción de zona industrializada.
# chas: Si hay río en la ciudad (= 1 si hay río; 0 no hay).
# nox: Concentración de óxidos de nitrógeno (partes per 10 millón).
# rm: promedio de habitaciones por vivienda.
# age: Proporción de viviendas ocupadas por el propietario construidas antes de 1940.
# dis: Media ponderada de la distancias a cinco centros de empleo de Boston.
# rad: Índice de accesibilidad a las autopistas radiales.
# tax: Tasa de impuesto a la propiedad en unidades de $10,000.
# ptratio: ratio de alumnos/profesor por ciudad.
# black: 1000(Bk - 0.63)^2 donde Bk es la proporción de gente de color por ciudad.
# lstat: porcentaje de población en condición de pobreza.
# medv: Valor mediano de las casas ocupadas por el dueño en unidades de $1000s.

require(psych)
# La variable chas es una variable categórica por lo que se transforma a factor
Boston$chas <- as.factor(Boston$chas)
summary(Boston)

# En primer lugar se realiza un análisis básico de los datos de forma numérica y gráfica.

# Dado que hay muchas variables, se grafican por grupos de 4, excluyendo las
# categóricas
multi.hist(x = Boston[,1:3], dcol = c("blue","red"), dlty = c("dotted", "solid"),
           main = "")


multi.hist(x = Boston[,5:9], dcol = c("blue","red"), dlty = c("dotted", "solid"),
           main = "")

multi.hist(x = Boston[,10:14], dcol = c("blue","red"), dlty = c("dotted", "solid"),
           main = "")

# Se pretende predecir el valor de la vivienda en función del porcentaje de pobreza de la
# población. Empleando la función lm() se genera un modelo de regresión lineal por mínimos
# cuadrados en el que la variable respuesta es medv y el predictor lstat.

modelo_simple <- lm(data = Boston, formula = medv ~ lstat)

# La función lm() genera un objeto que almacena toda la información del modelo, para ver su
# contenido se emplea la función names() y para visualizar los principales parámetros del
# modelo generado se utiliza summary().

names(modelo_simple)

summary(modelo_simple)

# En la información devuelta por el summary se observa que el p-value del estadístico F es muy
# pequeño, indicando que al menos uno de los predictores del modelo está significativamente
# relacionado con la variable respuesta. Al tratarse de un modelo simple, el p-value de estadístico
# F es el mismo que el p-value del estadístico t del único predictor incluido en el modelo (lstat).
# La evaluación del modelo en conjunto puede hacerse a partir de los valores RSE o del valor R2
# devuelto en el summary.

# Residual standar error (RSE): En promedio, cualquier predicción del modelo se aleja 6.216 unidades
# del verdadero valor. Teniendo en cuenta que el valor promedio de la variable respuesta medv es
# de 22.53, RSE es de 6.21622.53=27%.

# R2: El predictor lstatus empleado en el modelo es capaz de explicar el 54.44% de la variabilidad
# observada en el precio de las viviendas.

# La ventaja de R2 es que es independiente de la escala en la que se mida la variable respuesta,
# por lo que su interpretación es más sencilla.

# Los dos coeficientes de regresión (β0 y β1) estimados por el modelo son significativos y se
# pueden interpretar como:

# Intercept(β0): El valor promedio del precio de la vivienda cuando el lstatus es 0 es de
# 34.5538 unidades.
# Predictor lstat(β1): por cada unidad que se incrementa el predictor lstat el precio de la
# vivienda disminuye en promedio 0.9500 unidades.
# La estimación de todo coeficiente de regresión tiene asociada un error estándar, por lo
# tanto todo coeficiente de regresión tiene su correspondiente intervalo de confianza.

confint(modelo_simple, level = 0.95)


# Como era de esperar dado que el p-value del predictor lstat ha resultado significativo para
# un α=0.05, su intervalo de confianza del 95% no contiene el valor 0.

# Una vez generado el modelo, es posible predecir el valor de la vivienda sabiendo el estatus
# de la población en la que se encuentra. Toda predicción tiene asociado un error y por lo
# tanto un intervalo. Es importante diferenciar entre dos tipos de intervalo:

# Intervalo de confianza: Devuelve un intervalo para el valor promedio de todas las viviendas
# que se encuentren en una población con un determinado porcentaje de pobreza, supóngase lstat=10.

predict(object = modelo_simple, newdata = data.frame(lstat = c(10)),
        interval = "confidence", level = 0.95)

# Intervalo de predicción: Devuelve un intervalo para el valor esperado de una vivienda en
# particular que se encuentre en una población con un determinado porcentaje de pobreza.

predict(object = modelo_simple, newdata = data.frame(lstat = c(10)),
        interval = "prediction", level = 0.95)

# Como es de esperar ambos intervalos están centrados en torno al mismo valor. Si bien ambos
# parecen similares, la diferencia se encuentra en que los intervalos de confianza se aplican
# al valor promedio que se espera de y para un determinado valor de x, mientras que los
# intervalos de predicción no se aplican al promedio. Por esta razón, los segundos siempre
# son más amplios que los primeros.

# La creación de un modelo de regresión lineal simple suele acompañarse de una representación
# gráfica superponiendo las observaciones con el modelo. Además de ayudar a la interpretación,
# es el primer paso para identificar posibles violaciones de las condiciones de la regresión lineal.

attach(Boston)
plot(x = lstat, y = medv, main = "medv vs lstat", pch = 20, col = "grey30")
abline(modelo_simple, lwd = 3, col = "red")

# La representación gráfica de las observaciones muestra que la relación entre ambas variables
# estudiadas no es del todo lineal, lo que apunta a que otro tipo de modelo podría explicar
# mejor la relación. Aun así la aproximación no es mala.

# Una de las mejores formas de confirmar que las condiciones necesarias para un modelo de regresión
# lineal simple por mínimos cuadrados se cumplen es mediante el estudio de los residuos del modelo.

# En R, los residuos se almacenan dentro del modelo bajo el nombre de residuals. R genera automáticamente
# los gráficos más típicos para la evaluación de los residuos de un modelo.

par(mfrow = c(1,2))
plot(modelo_simple)

par(mfrow = c(1,1))

# Los residuos confirman que los datos no se distribuyen de forma lineal, ni su varianza constante
# (plot1). Además se observa que la distribución de los residuos no es normal (plot2). Algunas
# observaciones tienen un residuo estandarizado absoluto mayor de 3 (1.73 si se considera la raíz
# cuadrada) lo que es indicativo de observación atípica (plot3). Valores de Leverages (hat) mayores
# que 2.5x((p+1)/n), siendo p el número de predictores y n el número de observaciones, o valores de
# Cook mayores de 1 se consideran influyentes (plot4). Todo ello reduce en gran medida la robustez
# de la estimación del error estándar de los coeficientes de correlación estimados y con ello la del
# modelo es su conjunto.

# Otra forma de identificar las observaciones que puedan ser outliers o puntos con alta influencia
# (leverage) es emplear las funciones rstudent() y hatvalues().
plot(x = modelo_simple$fitted.values, y = abs(rstudent(modelo_simple)),
     main = "Absolute studentized residuals vs predicted values", pch = 20,
     col = "grey30")
abline(h = 3, col = "red")

plot(hatvalues(modelo_simple), main = "Medición de leverage", pch = 20)
# Se añade una línea en el threshold de influencia acorde a la regla
# 2.5x((p+1)/n)
abline(h = 2.5*((dim(modelo_simple$model)[2]-1 + 1)/dim(modelo_simple$model)[1]),
       col = "red")

# En este caso muchos de los valores parecen posibles outliers o puntos con alta influencia porque
# los datos realmente no se distribuyen de forma lineal en los extremos.

# Modelo
# precio medio vivienda=34.55−0.95lstat
