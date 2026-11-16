# La funcion loess() del paquete base de R permite obtener modelos ajustados por regresion
# local. Por defecto se emplea un polinomio de grado 2 y el span de 0.75, pero estos
# parametros se pueden modificar para aumentar o disminuir la flexibilidad del modelo.

# AUSTE DEL MODELO
# ------------------------------------------------------------------------------
attach(Wage)
modelo_local_reg1 <- loess(wage ~ age, span = 0.75, degree = 2, data = Wage)
# Se ajusta un segundo modelo con un span menor
modelo_local_reg2 <- loess(wage ~ age, span = 0.1, degree = 2, data = Wage)

# INTERPOLACION DE PUNTOS DENTRO DEL RANGO DEL PREDICTOR
# -----------------------------------------------------------------------------
limites <- range(Wage$age)
nuevos_puntos <- seq(from = limites[1], to = limites[2], by = 1)
nuevos_puntos <- data.frame(age = nuevos_puntos)

# PREDICCION DE LA VARIABLE RESPUESTA Y DEL ERROR ESTÁNDAR
# -----------------------------------------------------------------------------
predicciones1 <- predict(modelo_local_reg1, newdata = nuevos_puntos)
predicciones2 <- predict(modelo_local_reg2, newdata = nuevos_puntos)
# No es posible calcular el error de la prediccion de un modelo loess


plot(x = age, y = wage, pch = 20, col = "darkgrey")
title("2 Modelos Loess, span = 0.75 y 0.1, degree = 2")
lines(x = nuevos_puntos$age, predicciones1, col = "red", lwd = 2)
lines(x = nuevos_puntos$age, predicciones2, col = "blue", lwd = 2)

# La representacion de los modelos muestra que, para un mismo grado de polinomio,
# cuanto menor es el valor de span más se ajusta el modelo a las observaciones.
# Con la funcion geom_smooth() del paquete gráfico ggplot2 también se pueden
# obtener representaciones graficas de un modelo ajustado por regresion local.
# En este caso, el grado del polinomio por defecto es 1.
library(ggplot2)
ggplot(data = Wage, aes(x = age, y = wage)) +
  geom_point(color = "darkgray") +
  geom_smooth(formula = y ~ poly(x, 2), span = 0.75, se = TRUE, level = 0.95,
              color = "firebrick") +
  theme_bw() +
  labs(title = "Modelo Loess, span = 0.75, degree = 1")

