# Un estudio considera que existe relacion entre el hecho de que un estudiante
# asista a clases de repaso de lectura (si = 1, no = 0), la nota que obtiene
# en un examen de lectura estandar (realizado antes de iniciar las clases de
# repaso) y el sexo (hombre = 1, mujer = 0). Se quiere generar un modelo en el
# que a partir de las variables puntuacion del examen y genero, prediga la
# probabilidad de que el estudiante tenga que asistir a clases de repaso.
datos <- data.frame(sexo = c("1", "1", "0", "0", "0", "1", "0", "1", "0", "0",
                             "1", "1", "1", "1", "0", "0", "1", "0", "1", "0",
                             "1", "0", "0", "1", "1", "0", "0", "0", "1", "1",
                             "1", "0", "1", "0", "1", "0", "0", "0", "0", "0",
                             "1", "0", "1", "0", "0", "0", "0", "1", "0", "1",
                             "0", "1", "0", "0", "1", "1", "1", "1", "1", "1",
                             "1", "1", "1", "0", "1", "1", "1", "1", "0", "1",
                             "0", "1", "1", "1", "0", "1", "0", "0", "1", "0",
                             "0", "0", "1", "1", "1", "1", "1", "0", "0", "0",
                             "0", "1", "0", "0", "0", "0", "0", "0", "0", "0",
                             "0", "0", "1", "0", "1", "1", "0", "0", "0", "1",
                             "0", "1", "0", "0", "0", "1", "0", "1", "0", "1",
                             "0", "1", "0", "0", "0", "0", "0", "0", "0", "0",
                             "1", "0", "1", "1", "1", "1", "1", "1", "1", "0",
                             "0", "0", "1", "1", "0", "0", "1", "0", "1", "1",
                             "1", "0", "0", "0", "0", "1", "1", "0", "1", "1",
                             "0", "1", "1", "1", "1", "0", "1", "1", "0", "0",
                             "1", "1", "1", "1", "1", "0", "0", "0", "0", "1",
                             "1", "1", "0", "1", "0", "1", "1", "1", "0"),
                             examen_lectura = c(91, 77.5, 52.5, 54, 53.5, 62, 59, 51.5,
                               61.5, 56.5, 47.5, 75, 47.5, 53.5, 50, 50,
                               49, 59, 60, 60, 60.5, 50, 101, 60, 60,
                               83.5, 61, 75, 84, 56.5, 56.5, 45, 60.5,
                               77.5, 62.5, 70, 69, 62, 107.5, 54.5, 92.5,
                               94.5, 65, 80, 45, 45, 66, 66, 57.5, 42.5,
                               60, 64, 65, 47.5, 57.5, 55, 55, 76.5,
                               51.5, 59.5, 59.5, 59.5, 55, 70, 66.5,
                               84.5, 57.5, 125, 70.5, 79, 56, 75, 57.5,
                               56, 67.5, 114.5, 70, 67, 60.5, 95, 65.5,
                               85, 55, 63.5, 61.5, 60, 52.5, 65, 87.5,
                               62.5, 66.5, 67, 117.5, 47.5, 67.5, 67.5,
                               77, 73.5, 73.5, 68.5, 55, 92, 55, 55, 60,
                               120.5, 56, 84.5, 60, 85, 93, 60, 65, 58.5,
                               85, 67, 67.5, 65, 60, 47.5, 79, 80, 57.5,
                               64.5, 65, 60, 85, 60, 58, 61.5, 60, 65,
                               93.5, 52.5, 42.5, 75, 48.5, 64, 66, 82.5,
                               52.5, 45.5, 57.5, 65, 46, 75, 100, 77.5,
                               51.5, 62.5, 44.5, 51, 56, 58.5, 69, 65,
                               60, 65, 65, 40, 55, 52.5, 54.5, 74, 55,
                               60.5, 50, 48, 51, 55, 93.5, 61, 52.5,
                               57.5, 60, 71, 65, 60, 55, 60, 77, 52.5,
                               95, 50, 47.5, 50, 47, 71, 65),
                               clases_repaso = c("0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "0", "0", "0", "0", "0", "0",
                               "0", "0", "1", "1", "1", "1", "1", "1",
                               "1", "1", "1", "1", "1", "1", "1", "1",
                               "1", "1", "1", "1", "1", "1", "1", "1",
                               "1", "1", "1", "1", "1", "1", "1", "1",
                               "1", "1", "1", "1", "1", "1", "1", "1",
                               "1", "1", "1", "1", "1", "1", "1", "1",
                               "1", "1", "1", "1", "1", "1", "1", "1",
                               "1", "1", "1", "1", "1"))

### Analisis de las observaciones
# Las tablas de frecuencia así como representaciones graficas de las observaciones
# son utiles para intuir si las variables independientes escogidas estan relacionadas
# con la variable respuesta y por lo tanto ser buenos predictores.
require(ggplot2)
tabla <- table(datos$clases_repaso, datos$sexo,
               dnn = c("clases de repaso","sexo"))
addmargins(tabla)

tabla_frecuencias <- prop.table(tabla)*100
addmargins(tabla_frecuencias)

ggplot(data = datos, aes(x = clases_repaso, y = examen_lectura, colour = sexo)) +
  geom_boxplot() +
  theme_bw()

# El numero de estudiantes en la muestra es semejante para ambos sexos (96, 93).
# Parece ser mayor el porcentaje de hombres que necesitan clases de repaso
# (19.04762, 12.16931). El promedio de las notas de lectura de los estudiantes
# que requieren de clases particulares es menor que el de los que no requieren
# clases. En vista de estos datos, tiene sentido considerar el sexo y la nota
# como posibles predictores.

### Generar el modelo de regresion logistica
modelo <- glm(clases_repaso ~ examen_lectura + sexo, data = datos,
              family = "binomial")
summary(modelo)

# Acorde al modelo, el logaritmo de odds de que un estudiante necesite clases de
# repaso esta negativamente relacionado con la puntuación obtenida en el examen
# de lectura (coeficiente parcial = -0.02617), siento significativa esta relacion
# (p-value = 0.0324). Tambien existe una relación significativa positiva entre
# el logaritmo de odds de necesitar clases de repaso y el genero del estudiante
# (p-value = 0.0462), siendo, para un mismo resultado en el examen de lectura,
# mayor si el estudiante es hombre. En concreto los odds de que un hombre requiera
# clases de repaso son e0.64749=1.910739 mayores que los de las mujeres. (Esto se
# puede ver graficamente representando el modelo para hombres y mujeres).

# Ademas del valor estimado de los coeficientes parciales de correlacion calculados
# por el modelo, es conveniente generar sus correspondientes intervalos de confianza.
# En el caso de regresion logistica, estos intervalos suelen calcularse basados en
# profile likelihood (en R es el método por defecto si se tiene instalado el paquete MASS).
confint(modelo)
# En caso de querer los intervalos basados en el error estándar
# confint.default(modelo)

### Representacion grafica del modelo
# Al tratarse de un modelo con 2 predictores, no se puede obtener una representacion
# en 2D en la que se incluyan ambos predictores a la vez. Sí es posible representar
# la curva del modelo logístico cuando se mantiene constante uno de los dos predictores.
# Por ejemplo, al representar las predicciones del modelo diferenciando entre hombres
# y mujeres (fijando el valor del predictor sexo) se aprecia que la curva de los
# hombres (sexo=1) siempre está por encima. Esto se debe a que, como indica el
# coeficiente parcial de correlación del predictor sexo, para una misma nota en el
# examen de lectura el logaritmo de ODDs de necesitar clases de repaso es 0.64749
# veces mayor en hombres.

require(ggplot2)
# Para graficar los valores en ggplot junto con la curva, la variable respuesta
# tiene que ser numérica en lugar de factor.
datos$clases_repaso <- as.numeric(as.character(datos$clases_repaso))

# Se crea un data frame que contenga la probabilidad de que se necesiten clases
# de repaso dada una determinada nota en el examen de lectura y siendo hombre
# (sex=1).

# vector con nuevos valores interpolados en el rango de observaciones
nuevos_valores_examen <- seq(from = min(datos$examen_lectura),
                             to = max(datos$examen_lectura), by = 0.5)
sexo <- as.factor(rep(x = 1, length(nuevos_valores_examen)))
# predicciones de los nuevos puntos según el modelo. type = "response" devuelve
# las predicciones en forma de probabilidad en lugar de en log_ODDs
predicciones <- predict(object = modelo,
                        newdata=data.frame(examen_lectura=nuevos_valores_examen,
                                           sexo = sexo),
                        type = "response")
# Se crea un data frame con los nuevos puntos y sus predicciones para graficar
# la curva
datos_curva_hombre <- data.frame(examen_lectura = nuevos_valores_examen,
                                 sexo = sexo, clases_repaso = predicciones)

# Mismo proceso para mujeres (sexo = 0)
nuevos_valores_examen <- seq(from = min(datos$examen_lectura),
                             to = max(datos$examen_lectura), by = 0.5)
sexo <- as.factor(rep(x = 0, length(nuevos_valores_examen)))
predicciones <- predict(object = modelo,
                        newdata=data.frame(examen_lectura=nuevos_valores_examen,
                                           sexo = sexo),
                        type = "response")
datos_curva_mujer <- data.frame(examen_lectura = nuevos_valores_examen,
                                sexo = sexo, clases_repaso = predicciones)

# Se unifican los dos dataframe
datos_curva <- rbind(datos_curva_hombre, datos_curva_mujer)

ggplot(data = datos, aes(x = examen_lectura, y = as.numeric(clases_repaso),
                         color = sexo)) +
    geom_point() +
    geom_line(data = datos_curva, aes(y = clases_repaso)) +
    geom_line(data = datos_curva, aes(y = clases_repaso)) +
    theme_bw() +
    labs(title = "Prob. clases repaso en función de la nota en lectura y sexo",
         y = "P(clase de repaso)")

# Otra opcion para graficarlo es:
# qplot(x = modelo$data$examen_lectura, y = modelo$fitted.values,
# geom = c("point","line"), colour = modelo$data$sexo, ylim = c(0,1))

### Evaluacion del modelo
# Likelihood ratio:
# Diferencia de residuos
dif_residuos <- modelo$null.deviance - modelo$deviance
# Grados libertad
df <- modelo$df.null - modelo$df.residual
# p-value
p_value <- pchisq(q = dif_residuos,df = df, lower.tail = FALSE)

paste("Diferencia de residuos:", round(dif_residuos, 4))

paste("Grados de libertad:", df)

paste("p-value:", round(p_value, 4))

# El modelo en conjunto si es significativo y acorde a los p-values mostrados en
# el summary(modelo) también es significativa la contribucion al modelo de ambos predictores.
### Comparacion de las predicciones con las observaciones
# Para este estudio se va a emplear un threshold de 0.5. Si la probabilidad predicha
# de asistir a clases de repaso es superior a 0.5 se asigna al nivel 1 (si asiste),
# si es menor se asigna al nivel 0 (no clases de repaso).
predicciones <- ifelse(test = modelo$fitted.values > 0.5, yes = 1, no = 0)
matriz_confusion <- table(modelo$model$clases_repaso, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

# El modelo es capaz de clasificar correctamente (129+3)/(129+3+56+1)=0.698(69.8%) de
# las observaciones cuando se emplea el trainig data set. Si se analiza en detalle
# como se distribuye el error, se aprecia que el modelo solo ha sido capaz de
# identificar correctamente a 3 de los 59 alumnos que realmente asisten a clases
# de repaso. El porcentaje de falsos negativos es muy alto. Seleccionar otro
# threshold puede mejorar la exactitud del modelo.
library(vcd)
predicciones <- ifelse(test = modelo$fitted.values > 0.45, yes = 1, no = 0)
matriz_confusion <- table(modelo$model$clases_repaso, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

### Conclusion
# El modelo logistico creado para predecir la probabilidad de que un alumno tenga
# que asistir a clases de repaso a partir de la nota obtenida en un examen de
# lectura y el sexo del alumno es en conjunto significativo acorde al Likelihood
# ratio (p-value = 0.0066). El p-value de ambos predictores es significativo
# (examen_lectura = 0.0324, sexo1 = 0.0462). El ratio de error obtenido empleando
# las observaciones con las que se ha entrenado el modelo muestra un porcentaje
# de falsos negativos muy alto.
# logit(clases de repaso)=0.53616−0.02617examen lectura+0.64749sexo
