# Un estudio quiere establecer un modelo que permita calcular la probabilidad de
# obtener una matricula de honor al final del bachillerato en funcion de la nota
# que se ha obtenido en matematicas. La variable matricula está codificada como
# 0 si no se tiene matrícula y 1 si se tiene.

matricula <- as.factor(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1,
                         0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1,
                         0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0,
                         0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
                         1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0,
                         1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1,
                         1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1,
                         0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
                         0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0,
                         0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0,
                         0, 0, 0, 0, 1, 0, 0, 0, 1, 1))
matematicas <- c(41, 53, 54, 47, 57, 51, 42, 45, 54, 52, 51, 51, 71, 57, 50, 43,
                 51, 60, 62, 57, 35, 75, 45, 57, 45, 46, 66, 57, 49, 49, 57, 64,
                 63, 57, 50, 58, 75, 68, 44, 40, 41, 62, 57, 43, 48, 63, 39, 70,
                 63, 59, 61, 38, 61, 49, 73, 44, 42, 39, 55, 52, 45, 61, 39, 41,
                 50, 40, 60, 47, 59, 49, 46, 58, 71, 58, 46, 43, 54, 56, 46, 54,
                 57, 54, 71, 48, 40, 64, 51, 39, 40, 61, 66, 49, 65, 52, 46, 61,
                 72, 71, 40, 69, 64, 56, 49, 54, 53, 66, 67, 40, 46, 69, 40, 41,
                 57, 58, 57, 37, 55, 62, 64, 40, 50, 46, 53, 52, 45, 56, 45, 54,
                 56, 41, 54, 72, 56, 47, 49, 60, 54, 55, 33, 49, 43, 50, 52, 48,
                 58, 43, 41, 43, 46, 44, 43, 61, 40, 49, 56, 61, 50, 51, 42, 67,
                 53, 50, 51, 72, 48, 40, 53, 39, 63, 51, 45, 39, 42, 62, 44, 65,
                 63, 54, 45, 60, 49, 48, 57, 55, 66, 64, 55, 42, 56, 53, 41, 42,
                 53, 42, 60, 52, 38, 57, 58, 65)
datos <- data.frame(matricula, matematicas)
head(datos, 4)

### Representacion de las observaciones
# Representar las observaciones es útil para intuir si la variable independiente
# escogida esta relacionada con la variable respuesta y por lo tanto puede ser
# un buen predictor.

require(ggplot2)
table(datos$matricula)

ggplot(data = datos, aes(x = matricula, y = matematicas, color = matricula)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")

# Parece existir una diferencia entre la nota de las personas con matricula y sin matricula.

### Generar el modelo de regresión logistica
modelo <- glm(matricula ~ matematicas, data = datos, family = "binomial")
summary(modelo)

# El coeficiente estimado para la interseccion es el valor esperado del logaritmo de
# odds de que un estudiante obtenga matricula teniendo un 0 en en matematicas.
# Como es de esperar, los odds son muy bajos

# Acorde al modelo, el logaritmo de los odds de que un estudiante tenga matrícula
# está positivamente relacionado con la puntuación obtenida en matemáticas
# (coeficiente de regresión = 0.1563404). Esto significa que, por cada unidad que
# se incrementa la variable matemáticas, se espera que el logaritmo de odds de la
# variable matrícula se incremente en promedio 0.1563404 unidades. Aplicando la
# inversa del logaritmo natural se obtiene que (1.169) por cada unidad
# que se incrementa la variable matemáticas los odds de obtener matrícula se
# incremente en promedio 1.169 unidades. No hay que confundir esto último con que
# la probabilidad de matricula se incremente un 1.169 %.

# A diferencia de la regresión lineal en la que β1 se corresponde con el cambio
# promedio en la variable dependiente Y debido al incremento en una unidad del
# predictor X, en regresión logistica, β1 indica el cambio en el logaritmo de
# odds debido al incremento de una unidad de X, o lo que es lo mismo, multiplica
# los odds por e[expβ1]. Dado que la relacion entre p(Y) y X no es lineal, β1 no se
# corresponde con el cambio en la probabilidad de Y asociado con el incremento
# de una unidad de X. Cuanto se incremente la probabilidad de Y por unidad de X
# depende del valor de X, es decir, de la posición en la curva logística en la
# que se encuentre.

# Ademas del valor de las estimaciones de los coeficientes parciales de correlacion
# del modelo, es conveniente calcular sus correspondientes intervalos de confianza.
# En el caso de regresión logística, estos intervalos suelen calcularse empleando
# el método de profile likelihood (en R es el método por defecto si se tiene
# instalado el paquete MASS).

confint(object = modelo, level = 0.95 )

# Grafico del modelo
# Dado que un modelo logistico modela el logaritmo de ODDs, estas son las unidades
# en las que se devuelven las predicciones. Es necesario convertirlas de nuevo en
# probabilidad mediante la función logit. En R, la función predict() puede devolver
# directamente las probabilidades en lugar de los logODDs si se indica el argumento
# type="response". Sin embargo, si se quieren calcular intervalos de confianza y
# que estos no se salgan del rango [0, 1] es necesario emplear los logODDs y una
# vez que se les ha sustraido o sumado el margen de error (Z x SE) se transforman
# en probabilidades.

# MEDIANTE BASE GRAPHICS SIN INTERVALOS DE CONFIANZA

# Codificación 0,1 de la variable respuesta
datos$matricula <- as.character(datos$matricula)
datos$matricula <- as.numeric(datos$matricula)

plot(matricula ~ matematicas, datos, col = "darkblue",
     main = "Modelo regresión logística matrícula ~ nota matemáticas",
     ylab = "P(matrícula=1 | matemáticas)",
     xlab = "matemáticas", pch = "I")

# type = "response" devuelve las predicciones en forma de probabilidad en lugar de en log_ODDs
curve(predict(modelo, data.frame(matematicas = x), type = "response"),
      col = "firebrick", lwd = 2.5, add = TRUE)

# MEDIANTE GGPLOT2 INCLUYENDO INTERVALOS DE CONFIANZA

datos$matricula <- as.character(datos$matricula)
datos$matricula <- as.numeric(datos$matricula)

# Se crea un vector con nuevos valores interpolados en el rango de observaciones.
nuevos_puntos <- seq(from = min(datos$matematicas), to = max(datos$matematicas),
                           by = 0.5)

# Predicciones de los nuevos puntos según el modelo.
# Si se indica se.fit = TRUE se devuelve el error estándar de cada predicción
# junto con el valor de la predicción (fit).
predicciones <- predict(modelo, data.frame(matematicas = nuevos_puntos),
                              se.fit = TRUE)

# Mediante la función logit se transforman los log_ODDs a probabilidades.
predicciones_logit <- exp(predicciones$fit) / (1 + exp(predicciones$fit))

# Se calcula el límite inferior y superior del IC del 95% sustrayendo e
# incrementando el logODDs de cada predicción 1.95*SE. Una vez calculados los
# logODDs del intervalo se transforman en probabilidades con la función logit.
limite_inferior       <- predicciones$fit - 1.96 * predicciones$se.fit
limite_inferior_logit <- exp(limite_inferior) / (1 + exp(limite_inferior))
limite_superior       <- predicciones$fit + 1.96 * predicciones$se.fit
limite_superior_logit <- exp(limite_superior) / (1 + exp(limite_superior))

# Se crea un data frame con los nuevos puntos y sus predicciones
datos_curva <- data.frame(matematicas = nuevos_puntos,
                                probabilidad_matricula = predicciones_logit,
                                limite_inferior_logit = limite_inferior_logit,
                                limite_superior_logit = limite_superior_logit)

ggplot(datos, aes(x = matematicas, y = matricula)) +
            geom_point(aes(color = as.factor(matricula)), shape = "I", size = 3) +
            geom_line(data = datos_curva, aes(y = probabilidad_matricula),
                      color = "firebrick") +
            geom_line(data = datos_curva, aes(y = limite_inferior_logit),
                      linetype = "dashed") +
            geom_line(data = datos_curva, aes(y = limite_superior_logit),
                      linetype = "dashed") +
            theme_bw() +
            labs(title = "Modelo regresión logística matrícula ~ nota matemáticas",
                 y = "P(matrícula = 1 | matemáticas)", y = "matemáticas") +
            theme(legend.position = "null") +
            theme(plot.title = element_text(hjust = 0.5))

### Evaluacion del modelo
# A la hora de evaluar la validez y calidad de un modelo de regresión logistica,
# se analiza tanto el modelo en su conjunto como los predictores que lo forman.
# Se considera que el modelo es util si es capaz de mostrar una mejora explicando
# las observaciones respecto al modelo nulo (sin predictores). El test Likelihood
# ratio calcula la significancia de la diferencia de residuos entre el modelo de
# interes y el modelo nulo. El estadistico sigue una distribución chi-cuadrado
# con grados de libertad equivalentes a la diferencia de grados de libertad de los dos modelos.

# Diferencia de residuos
# En R, un objeto glm almacena la "deviance" del modelo, así como la "deviance"
# del modelo nulo.
dif_residuos <- modelo$null.deviance - modelo$deviance
# Grados libertad
df <- modelo$df.null - modelo$df.residual
# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)

paste("Diferencia de residuos:", round(dif_residuos, 4))

paste("Grados de libertad:", df)

paste("p-value:", p_value)

# El mismo calculo se puede obtener directamente con:
# anova(modelo, test = "Chisq")

# En este caso, el modelo obtenido sí es significativo.
# Para determinar si los predictores introducidos en un modelo de regresion logistica
# contribuyen de forma significativa se emplea el estadistico Z y el test Wald chi-test.
# Este es el metodo utilizado para calcular los p-values que se muestran al hacer
# summary() del modelo. El predictor matematicas sí contribuye de forma significativa
# (p-value = 1.03e-09).

### Comparacion de clasificación predicha y observaciones
# Para este estudio se va a emplear un threshold de 0.5. Si la probabilidad de que
# la variable adquiera el valor 1 (matrícula) es superior a 0.5 se asigna a este
# nivel, si es menor se asigna al 0 (no matricula).
library(vcd)
predicciones <- ifelse(test = modelo$fitted.values > 0.5, yes = 1, no = 0)
matriz_confusion <- table(modelo$model$matricula, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

# El modelo es capaz de clasificar correctamente (140+22)/(140+22+27+11)=0.81(81%)
# de las observaciones cuando se emplea el trainig data set.
### Conclusion
# El modelo logistico creado para predecir la probabilidad de que un alumno obtenga
# matricula de honor a partir de la nota de matematicas es en conjunto significativo
# acorde al Likelihood ratio (p-value = 8.717591e-14). El p-value del predictor
# matematicas es significativo (p-value = 1.03e-09).

# logit(matricula)=−9.793942+0.1563404∗nota matematicas

