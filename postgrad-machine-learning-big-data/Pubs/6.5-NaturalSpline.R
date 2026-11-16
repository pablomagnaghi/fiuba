# Para ajustar natural splines en R, el proceso es equivalente al seguido en el ajuste de
# regression splines pero empleando la función ns(). Dado que las naturals spline introducen
# dos restricciones adicionales (linealidad en los extremos), se necesitan dos grados de
# libertad menos en comparación con las regression splines para introducir el mismo numero de knots.

# AUSTE DEL MODELO
# ------------------------------------------------------------------------------
library(splines)
attach(Wage)
modelo_nsplines <- lm(wage ~ ns(age, df = 4), data = Wage)
attr(ns(age, df = 4), "knots")

# INTERPOLACIÓN DE PUNTOS DENTRO DEL RANGO DEL PREDICTOR
# -----------------------------------------------------------------------------
limites <- range(Wage$age)
nuevos_puntos <- seq(from = limites[1], to = limites[2], by = 1)
nuevos_puntos <- data.frame(age = nuevos_puntos)

# PREDICCION DE LA VARIABLE RESPUESTA Y DEL ERROR ESTANDAR
# -----------------------------------------------------------------------------
predicciones <- predict(modelo_nsplines, newdata = nuevos_puntos, se.fit = TRUE,
                        level = 0.95)

# CALCULO DEL INTERVALO DE CONFIANZA SUPERIOR E INFERIOR
# -----------------------------------------------------------------------------
intervalo_conf <- data.frame(inferior = predicciones$fit -
                                        1.96*predicciones$se.fit,
                             superior = predicciones$fit +
                                        1.96*predicciones$se.fit)

plot(x = age, y = wage, pch = 20, col = "darkgrey")
title("Natural Spline con 3 knots (df=4)")
lines(x = nuevos_puntos$age, predicciones$fit, col = "red", lwd = 2)
lines(x = nuevos_puntos$age, intervalo_conf$inferior, col = "blue",
      lwd = 2, lty = 2)
lines(x = nuevos_puntos$age, intervalo_conf$superior, col = "blue",
      lwd = 2, lty = 2)

