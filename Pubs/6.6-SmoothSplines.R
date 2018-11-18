# De nuevo, se pretende crear un modelo que permita predecir el salario de un trabajador
# en funcion de la edad que tiene, esta vez empleando smooth splines.
# La funcion smooth.spline() de R permite ajustar smooth splines de forma sencilla, con la
# ventaja añadida de que el valor óptimo de smothness (λ) puede identificarse por cross-validation.

# AUSTE DEL MODELO
# ------------------------------------------------------------------------------
attach(Wage)
modelo_smooth_splines <- smooth.spline(wage ~ age, cv = TRUE)
modelo_smooth_splines$df

modelo_smooth_splines$lambda

modelo_smooth_splines$spar

# INTERPOLACION DE PUNTOS DENTRO DEL RANGO DEL PREDICTOR
# -----------------------------------------------------------------------------
limites <- range(Wage$age)
nuevos_puntos <- seq(from = limites[1], to = limites[2], by = 1)
nuevos_puntos <- data.frame(age = nuevos_puntos)

# PREDICCION DE LA VARIABLE RESPUESTA Y DEL ERROR ESTÁNDAR
# -----------------------------------------------------------------------------
predicciones <- predict(modelo_smooth_splines, newdata = nuevos_puntos)
# predict() no devuelve el error de la predicción de un modelo smooth.spline

plot(x = age, y = wage, pch = 20, col = "darkgrey")
title("Smooth Spline df = 6.79, lambda = 0.028")
lines(x = predicciones$x, predicciones$y, col = "red", lwd = 2)

