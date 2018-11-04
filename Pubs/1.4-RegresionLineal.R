# Un analista de deportes quiere saber si existe una relacion
# entre el numero de bateos que realiza un equipo de beisbol
# y el numero de runs que consigue. En caso de existir y de
# establecer un modelo, podria predecir el resultado del partido.
equipos <- c("Texas","Boston","Detroit","Kansas","St.","New_S.","New_Y.",
             "Milwaukee","Colorado","Houston","Baltimore","Los_An.","Chicago",
             "Cincinnati","Los_P.","Philadelphia","Chicago","Cleveland","Arizona",
             "Toronto","Minnesota","Florida","Pittsburgh","Oakland","Tampa",
             "Atlanta","Washington","San.F","San.I","Seattle")
numero_bateos <- c(5659,  5710, 5563, 5672, 5532, 5600, 5518, 5447, 5544, 5598,
                   5585, 5436, 5549, 5612, 5513, 5579, 5502, 5509, 5421, 5559,
                   5487, 5508, 5421, 5452, 5436, 5528, 5441, 5486, 5417, 5421)
runs <- c(855, 875, 787, 730, 762, 718, 867, 721, 735, 615, 708, 644, 654, 735,
          667, 713, 654, 704, 731, 743, 619, 625, 610, 645, 707, 641, 624, 570,
          593, 556)
datos <- data.frame(equipos,numero_bateos,runs)
head(datos)

# 1. Representacion grafica de las observaciones
# Si en este paso no se detecta la posible relacion lineal, no tiene sentido
# seguir adelante generando un modelo lineal
require(ggplot2)
ggplot(data = datos, mapping = aes(x = numero_bateos, y = runs)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'Diagrama de dispersión', x  =  'número  de bateos') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

cor.test(x = datos$numero_bateos, y = datos$runs, method = "pearson")

 # El grafico y el test de correlacion muestran una relacion lineal, de intensidad
 # considerable (r = 0.61) y significativa (p-value = 0.0003388). Tiene sentido
 # intentar generar un modelo de regresion lineal que permita predecir el numero de
 # runs en funcion del numero de bateos del equipo.

 # 2. Calculo del modelo de regresion lineal simple

 modelo_lineal <- lm(runs ~ numero_bateos, datos)
 # lm() devuelve el valor de la variable y para x=0 (interseccion) junto
 # con la pendiente de la recta.
 # Para ver la informacion del modelo se requiere summary().
 summary(modelo_lineal)

# Para el modelo generado, tanto la ordenada en el origen como la pendiente son
# significativas (p-values < 0.05).
# El valor de R2 indica que el modelo calculado explica el 37.29% de la variabilidad
# presente en la variable respuesta (runs) mediante la variable independiente
# (numero de bateos).
# El p-value obtenido en el test F (0.0003388) determina que si es significativamente
# superior la varianza explicada por el modelo en comparacion a la varianza total. Es
# el parametro que determina si el modelo es significativo y por lo tanto se puede aceptar.

# El modelo lineal generado sigue la ecuacion runs = -2789.2429 + 0.6305 bateos.
# Por cada unidad que se incrementa el numero de bateos, el numero de runs aumenta
# en promedio 0.6305 unidades.

# 3. Intervalos de confianza para los parámetros del modelo

confint(modelo_lineal)

# 4. Representación gráfica del modelo

ggplot(data = datos, mapping = aes(x = numero_bateos, y = runs)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'Runs ~ número de bateos', x  =  'número  de bateos') +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))


# Para poder representar el intervalo de confianza a lo largo de todo el
# modelo se recurre a la funcion  predict() para predecir valores que
# abarquen todo el eje X.

# Se genera una secuencia de valores x_i que abarquen todo el rango de las
# observaciones de la variable X
puntos <- seq(from = min(datos$numero_bateos), to = max(datos$numero_bateos),
             length.out = 100)
# Se predice el valor de la variable Y junto con su intervalo de confianza para
# cada uno de los puntos generados. En la funcion predict() hay que nombrar a
# los nuevos puntos con el mismo nombre que la variable X del modelo.
# Devuelve una matriz.
limites_intervalo <- predict(object = modelo_lineal,
                            newdata = data.frame(numero_bateos = puntos),
                            interval = "confidence", level = 0.95)
head(limites_intervalo,3)


# Finalmente se añaden al grafico las líneas formadas por los limites
# superior e inferior.
plot(datos$numero_bateos, datos$runs, col = "firebrick", pch = 19, ylab = "runs",
     xlab = "número de bateos", main = 'Runs ~ número de bateos')
abline(modelo_lineal, col = 1)
lines(x = puntos, y = limites_intervalo[,2],type = "l", col = 2, lty = 3)
lines(x = puntos, y = limites_intervalo[,3],type = "l", col = 3, lty = 3)

ggplot(data = datos, mapping = aes(x = numero_bateos, y = runs)) +
  geom_point(color = "firebrick", size = 2) +
  labs(title  =  'Diagrama de dispersión', x  =  'número  de bateos') +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
# Por defecto incluye la region de 95% de confianza.

# 5. Verificar condiciones para poder aceptar un modelo lineal
# Relacion lineal entre variable dependiente e independiente:
# Se calculan los residuos para cada observacion y se representan (scatterplot).
# Si las observaciones siguen la línea del modelo, los residuos se deben
# distribuir aleatoriamente entorno al valor 0.

#La funcion lm() calcula y almacena los valores predichos por el modelo y los residuos.
datos$prediccion <- modelo_lineal$fitted.values
datos$residuos <- modelo_lineal$residuals
head(datos)

ggplot(data = datos, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_hline(yintercept = 0) +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")


# Los residuos se distribuyen de forma aleatoria entorno al 0 por lo que se
# acepta la linealidad.

# Distribucion normal de los residuos:
# Los residuos se deben distribuir de forma normal con media 0. Para comprobarlo
# se recurre a histogramas, a los cuantiles normales o a un test de contraste
# de normalidad.

ggplot(data = datos, aes(x = residuos)) +
  geom_histogram(aes(y = ..density..)) +
  labs(title = "histograma de los residuos") +
  theme_light()

qqnorm(modelo_lineal$residuals)
qqline(modelo_lineal$residuals)

shapiro.test(modelo_lineal$residuals)

# Tanto la representacion grafica como el contraste de hipotesis confirman la
# distribucion normal de los residuos.

# Varianza constante de los residuos (Homocedasticidad):
# La variabilidad de los residuos debe de ser constante a lo largo del eje X.
# Un patron conico es indicativo de falta de homogeneidad en la varianza.

ggplot(data = datos, aes(x = prediccion, y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_segment(aes(xend = prediccion, yend = 0), alpha = 0.2) +
  geom_smooth(se = FALSE, color = "firebrick") +
  labs(title = "Distribución de los residuos", x = "predicción modelo",
       y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Test de Breush-Pagan
# install.packages('lmtest')
library(lmtest)
bptest(modelo_lineal)

# Ni la representacion grafica ni el contraste de hipotesis muestran evidencias
# que haga sospechar falta de homocedasticidad.

# Cuando se trabaja con intervalos de tiempo, es muy importante comprobar que no
# existe aoutocorrelacion de los residuos, es decir que son independientes.
# Esto puede hacerse detectando visualmente patrones en la distribución de
# los residuos cuando se ordenan segúu se han registrado o con el test de
# Durbin-Watson dwt() del paquete Car.
ggplot(data = datos, aes(x = seq_along(residuos), y = residuos)) +
  geom_point(aes(color = residuos)) +
  scale_color_gradient2(low = "blue3", mid = "grey", high = "red") +
  geom_line(size = 0.3) +
  labs(title = "Distribución de los residuos", x = "index", y = "residuo") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# En este caso, la representacion de los residuos no muestra ninguna tendencia.

# 6. Identificacion de valores atipicos: outliers, leverage y
# observaciones influyentes
# install.packages('ggrepel')
# install.packages('dplyr')
library(ggrepel)
library(dplyr)
datos$studentized_residual <- rstudent(modelo_lineal)
ggplot(data = datos, aes(x = prediccion, y = abs(studentized_residual))) +
  geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
# se identifican en rojo observaciones con residuos studentized absolutos > 3
geom_point(aes(color = ifelse(abs(studentized_residual) > 3, 'red', 'black'))) +
  scale_color_identity() +
#se muestra el equipo al que pertenece la observacion atipica
geom_text_repel(data = filter(datos, abs(studentized_residual) > 3),
                  aes(label = equipos)) +
labs(title="Distribucion de los residuos studentized", x="prediccion modelo") +
theme_bw() +
theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

datos %>% filter(abs(studentized_residual) > 3)

which(abs(datos$studentized_residual) > 3)
# El estudio de los residuos studentized identifica al equipo de New_Y.
# como una posible observacion atipica. Esta observacion ocupa la posicion
# 7 en la tabla de datos.
# En R se dispone de la funcion outlierTest() del paquete car y de las
# funciones  influence.measures(), influencePlot() y hatvalues() para
# identificar las observaciones mas influyentes en el modelo.
# install.packages('car')
library(car)
summary(influence.measures(model = modelo_lineal))
influencePlot(model = modelo_lineal)

# Las funciones influence.measures() e influencePlot() detectan la observacion
# 7 como atipica pero no significativamente influyente. Sí detectan como influyente
# la observacion que ocupa la segunda posicion. Para evaluar hasta que punto
# condiciona el modelo, se recalcula la recta de mínimos cuadrados excluyendo
# esta observacion.

ggplot(data = datos, mapping = aes(x = numero_bateos, y = runs)) +
  geom_point(color = "grey50", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  #se resalta el valor excluido
  geom_point(data = datos[2,], color = "red", size = 2) +
  #se añade la nueva recta de mínimos cuadrados
  geom_smooth(data = datos[-2,], method = "lm", se = FALSE, color = "blue") +
  labs(title  =  'Diagrama de dispersión', x  =  'número  de bateos') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# La eliminacion del valor identificado como influyente apenas cambia la
# recta de minimos cuadrados. Para conocer con exactitud el resultado de e
# xcluir la observacion se comparan las pendientes de ambos modelos.

lm(formula = runs ~ numero_bateos, data = datos)$coefficients


# Conclusion
# Dado que se satisfacen todas las condiciones para considerar valido
# un modelo de regresion lineal por minimos cuadrados y que el p-value
# indica que el ajuste es significativo, se puede aceptar el modelo lineal.
# A pesar de ello, el valor de R2 no es muy alto por lo que el numero de
# bateos no es muy buen predictor del número de runs.


# Evaluacion de los residuos de un modelo lineal simple mediante graficos R
par(mfrow = c(1,2))
plot(modelo_lineal)

par(mfrow = c(1,1))

