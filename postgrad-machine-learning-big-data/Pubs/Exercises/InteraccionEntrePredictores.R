# Una de las asunciones del modelo de regresión lineal múltiple es la de aditividad, según la
# cual los efectos que causan sobre la variable respuesta Y variaciones en el predictor Xi son
# independientes del valor que tomen los otros predictores. Se conoce como efecto de interacción
# cuando el efecto de un predictor varía dependiendo del valor que adquiera otro predictor. Si
# esto ocurre, el modelo mejorará si se incluye dicha interacción. Si en un modelo se incorpora
# una interacción, se deben incluir también los predictores individuales que forman la interacción
# aun cuando por sí solos no sean significativos.

# Supóngase que empleando las variables age y lstat del data set Boston se quiere generar un
# modelo lineal que permita predecir el valor medio de la vivienda medv.

# El modelo lineal múltiple empleando ambos predictores resulta en:
modelo <- lm(medv ~ lstat + age, data = Boston)
summary(modelo)

# Dado que es un modelo con dos predictores continuos se puede representar el plano de regresión.

rango_lstat <- range(Boston$lstat)
nuevos_valores_lstat <- seq(from = rango_lstat[1], to = rango_lstat[2],
                            length.out = 20)
rango_age <- range(Boston$age)
nuevos_valores_age <- seq(from = rango_age[1], to = rango_age[2],
                          length.out = 20)

predicciones <- outer(X = nuevos_valores_lstat, Y = nuevos_valores_age,
                      FUN = function(lstat, age) {
                          predict(object = modelo,
                                  newdata = data.frame(lstat, age))
                          })

superficie <- persp(x = nuevos_valores_lstat, y = nuevos_valores_age,
                    z = predicciones,
                    theta = 20, phi = 5,
                    col = "lightblue", shade = 0.1,
                    zlim = range(-10,100),
                    xlab = "lstat", ylab = "age", zlab = "medv",
                    ticktype = "detailed",
                    main = "Predición precio medio ~ lstat y age"
                  )

observaciones <- trans3d(Boston$lstat, Boston$age, Boston$medv, superficie)
error <- trans3d(Boston$lstat, Boston$age, fitted(modelo), superficie)
points(observaciones, col = "red", pch = 16)
segments(observaciones$x, observaciones$y, error$x, error$y)

# En R se puede generar un modelo con interacción de dos formas: indicando de forma explícita los
# predictores individuales y entre que predictores se quiere evaluar la interacción

lm(medv ~ lstat + age + lstat:age, data = Boston)

# O bien de forma directa
modelo_interaccion <- lm(medv ~ lstat * age, data = Boston)
summary(modelo_interaccion)

# La interacción, aunque significativa, apenas aporta mejora al modelo, R-squared sin
# interacción = 0.5513 y R-squared con interacción = 0.5557. Por lo tanto, siguiendo el
# principio de parsimonia, el modelo más adecuado (dentro de lo limitado ya que solo
# explica el 55% de variabilidad) es el modelo sin interacción.
