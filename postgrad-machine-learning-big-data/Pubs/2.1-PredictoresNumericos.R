# Un estudio quiere generar un modelo que permita predecir la
# esperanza de vida media de los habitantes de una ciudad en
# funcion de diferentes variables. Se dispone de informacion
# sobre: habitantes, analfabetismo, ingresos, esperanza de vida,
# asesinatos, universitarios, heladas, area y densidad poblacional.

# El data set empleado es el state.x77
# Para facilitar su interpretacion se renombra y se modifica
# install.packages('dplyr')
library(dplyr)
datos <- as.data.frame(state.x77)
datos <- rename(habitantes = Population, analfabetismo = Illiteracy,
                ingresos = Income, esp_vida = `Life Exp`, asesinatos = Murder,
                universitarios = `HS Grad`, heladas = Frost, area = Area,
                .data = datos)
datos <- mutate(.data = datos, densidad_pobl = habitantes * 1000 / area)

# 1. Analizar la relación entre variables
# Las dos formas principales de hacerlo son mediante representaciones graficas
# (graficos de dispersion) y el calculo del coeficiente de correlacion de
# cada par de variables.
round(cor(x = datos, method = "pearson"), 3)

# install.packages('dplyr')
library(psych)
multi.hist(x = datos, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")

# Otros paquetes permiten representar a la vez los diagramas de dispersion,
# los valores de correlacion para cada par de variables y la distribucion
# de cada una de las variables.
# install.packages('GGally')
library(GGally)
ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

# Del analisis preliminar se pueden extraer las siguientes conclusiones:
# Las variables que tienen una mayor relacion lineal con la esperanza de
# vida son: asesinatos (r= -0.78), analfabetismo (r= -0.59) y universitarios (r= 0.58).
# Asesinatos y analfabetismo estan medianamente correlacionados (r = 0.7)
# por lo que posiblemente no sea útil introducir ambos predictores en el modelo.
# Las variables habitantes, area y densidad poblacional muestran una distribucion
# exponencial, una transformacion logaritmica posiblemente haria mas normal su distribucion.

# 2. Generar el modelo
# En este caso se va a emplear el metodo mixto iniciando el modelo con todas las variables
# como predictores y realizando la seleccion de los mejores predictores con la medicion Akaike(AIC).

modelo <- lm(esp_vida ~ habitantes + ingresos + analfabetismo + asesinatos +
               universitarios + heladas + area + densidad_pobl, data = datos )
summary(modelo)

# El modelo con todas las variables introducidas como predictores tiene un R2 alta (0.7501), es capaz
# de explicar el 75,01% de la variabilidad observada en la esperanza de vida. El p-value del modelo
# es significativo (3.787e-10) por lo que se puede aceptar que el modelo no es por azar, al menos
# uno de los coeficientes parciales de regresion es distinto de 0. Muchos de ellos no son
# significativos, lo que es un indicativo de que podrian no contribuir al modelo.

# 3. Seleccion de los mejores predictores
# En este caso se van a emplear la estrategia de stepwise mixto. El valor matematico empleado
# para determinar la calidad del modelo va a ser Akaike(AIC).

step(object = modelo, direction = "both", trace = 1)

# El mejor modelo resultante del proceso de seleccion ha sido:

modelo <- (lm(formula = esp_vida ~ habitantes + asesinatos + universitarios +
              heladas, data = datos))
summary(modelo)

# Es recomendable mostrar el intervalo de confianza para cada uno de los coeficientes
# parciales de regresion:

confint(lm(formula = esp_vida ~ habitantes + asesinatos + universitarios +
            heladas, data = datos))

# Cada una de las pendientes de un modelo de regresion lineal multiple (coeficientes
# parciales de regresion de los predictores) se define del siguiente modo: Si el
# resto de variables se mantienen constantes, por cada unidad que aumenta el
# predictor en cuestion, la variable (Y) varia en promedio tantas unidades como
# indica la pendiente. Para este ejemplo, por cada unidad que aumenta el predictor
# universitarios, la esperanza de vida aumenta en promedio 0.04658 unidades, manteniendose
# constantes el resto de predictores.


# 4. Validacion de condiciones para la regresion multiple lineal
# Relacion lineal entre los predictores numericos y la variable respuesta:
library(ggplot2)
library(gridExtra)
plot1 <- ggplot(data = datos, aes(habitantes, modelo$residuals)) +
    geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
    theme_bw()
plot2 <- ggplot(data = datos, aes(asesinatos, modelo$residuals)) +
    geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
    theme_bw()
plot3 <- ggplot(data = datos, aes(universitarios, modelo$residuals)) +
    geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
    theme_bw()
plot4 <- ggplot(data = datos, aes(heladas, modelo$residuals)) +
    geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
    theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)

# Se cumple la linealidad para todos los predictores
# Distribucion normal de los residuos:
qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)
# Tanto el analisis grafico como es test de hipotesis confirman la normalidad.

# Variabilidad constante de los residuos (homocedasticidad):
# Al representar los residuos frente a los valores ajustados por el modelo, los primeros
# se tienen que distribuir de forma aleatoria en torno a cero, manteniendo aproximadamente
# la misma variabilidad a lo largo del eje X. Si se observa algún patrón específico,
# por ejemplo forma conica o mayor dispersión en los extremos, significa que la
# variabilidad es dependiente del valor ajustado y por lo tanto no hay homocedasticidad.


ggplot(data = datos, aes(modelo$fitted.values, modelo$residuals)) +
geom_point() +
geom_smooth(color = "firebrick", se = FALSE) +
geom_hline(yintercept = 0) +
theme_bw()

library(lmtest)
bptest(modelo)

# No hay evidencias de falta de homocedasticidad.
# No multicolinialidad:
# Matriz de correlacion entre predictores.

library(corrplot)
corrplot(cor(dplyr::select(datos, habitantes, asesinatos,universitarios,heladas)),
         method = "number", tl.col = "black")

# Analisis de Inflacion de Varianza (VIF):
library(car)
vif(modelo)

# No hay predictores que muestren una correlacion lineal muy alta ni inflacion de varianza.

# autocorrelacion
library(car)
dwt(modelo, alternative = "two.sided")

# No hay evidencia de autocorrelacion
# Tamaño de la muestra:
# No existe una condicion establecida para el número minimo de observaciones, pero para
# prevenir que una variable resulte muy influyente cuando realmente no lo es, se
# recomienda que la cantidad de observaciones sea entre 10 y 20 veces el número de
# predictores. En este caso debería haber como mínimo 40 observaciones y se dispone
# de 50 por lo que es apropiado.

# 5. Identificación de posibles valores atípicos o influyentes
library(dplyr)
datos$studentized_residual <- rstudent(modelo)
ggplot(data = datos, aes(x = predict(modelo), y = abs(studentized_residual))) +
geom_hline(yintercept = 3, color = "grey", linetype = "dashed") +
# se identifican en rojo observaciones con residuos estandarizados absolutos > 3
geom_point(aes(color = ifelse(abs(studentized_residual) > 3, 'red', 'black'))) +
scale_color_identity() +
labs(title = "Distribución de los residuos studentized",
     x = "predicción modelo") +
theme_bw() + theme(plot.title = element_text(hjust = 0.5))

which(abs(datos$studentized_residual) > 3)

# No se identifica ninguna observacion atipica.

summary(influence.measures(modelo))

# En la tabla generada se recogen las observaciones que son significativamente influyentes en
# al menos uno de los predictores (una columna para cada predictor). Las tres ultimas columnas
# son 3 medidas distintas para cuantificar la influencia. A modo de guia se pueden considerar
# excesivamente influyentes aquellas observaciones para las que:

# Leverages (hat): Se consideran observaciones influyentes aquellas cuyos valores hat
# superen 2.5((p+1)/n), siendo p el numero de predictores y n el numero de observaciones.
# Distancia Cook (cook.d): Se consideran influyentes valores superiores a 1.
# La visualizacion grafica de las influencias se obtiene del siguiente modo:
influencePlot(modelo)

# Los analisis muestran varias observaciones influyentes (posición 5 y 11) que exceden los limites
# de preocupacion para los valores de Leverages o Distancia Cook. Estudios mas
# exhaustivos consistirian en rehacer el modelo sin las observaciones y ver el impacto.

# 6. Conclusion
# El modelo lineal multiple
# Esperanza de vida=5.014e−05habitantes−3.001e−01asesinatos+4.658e−02universitarios−5.943e−03heladas

# es capaz de explicar el 73.6% de la variabilidad observada en la esperanza de vida (R2: 0.736,
# R2-Adjusted: 0.7126). El test F muestra que es significativo (p-value: 1.696e-12).
# Se satisfacen todas las condiciones para este tipo de regresion multiple.
# Dos observaciones (posición 5 y 11) podrian estar influyendo de forma notable en el modelo.
