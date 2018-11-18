# Empleando el mismo set de datos Wage que en el ejercicio anterior, se pretende crear
# un modelo de regression splines que permita predecir el salario de un trabajador en
# funcion de la edad que tiene.

# Para ajustar regression splines en R se emplea la librería splines, que viene instalada
# por defecto. La función bs(), cuyo nombre viene de B-spline, genera la matriz de
# ecuaciones necesaria para crear las splines acorde a los knots indicados. Por defecto,
# bs() se generan polinomios de grado 3 (cubic splines) pero esto puede cambiarse con el argumento degree.

# AUSTE DEL MODELO
# ------------------------------------------------------------------------------
library(splines)
modelo_splines <- lm(wage ~ bs(age, knots = c(25,40,60), degree = 3),
                     data = Wage)

# INTERPOLACION DE PUNTOS DENTRO DEL RANGO DEL PREDICTOR
# -----------------------------------------------------------------------------
limites <- range(Wage$age)
nuevos_puntos <- seq(from = limites[1], to = limites[2], by = 1)
nuevos_puntos <- data.frame(age = nuevos_puntos)

# PREDICCION DE LA VARIABLE RESPUESTA Y DEL ERROR ESTANDAR
# -----------------------------------------------------------------------------
predicciones <- predict(modelo_splines, newdata = nuevos_puntos, se.fit = TRUE,
                        level = 0.95)

# CALCULO DEL INTERVALO DE CONFIANZA SUPERIOR E INFERIOR
# -----------------------------------------------------------------------------
intervalo_conf <- data.frame(
                  inferior = predicciones$fit - 1.96*predicciones$se.fit,
                  superior = predicciones$fit + 1.96*predicciones$se.fit)
attach(Wage)
plot(x = age, y = wage, pch = 20, col = "darkgrey")
title("Cubic Spline, knots: 25, 40, 60")
lines(x = nuevos_puntos$age, predicciones$fit, col = "red", lwd = 2)
lines(x = nuevos_puntos$age, intervalo_conf$inferior, col = "blue",
      lwd = 2, lty = 2)
lines(x = nuevos_puntos$age, intervalo_conf$superior, col = "blue",
      lwd = 2, lty = 2)


# Dado que se han especificado 3 knots en los valores 25, 40 y 60, la spline resultante
# tiene 6 funciones. Esto equivale a 7 grados de libertad, uno por el intercept más 6
# por las funciones que lo forman.
# Para que los knots estén equidistribuidos, en lugar de indicar las posiciones en el
# argumento  knots, se indican los grados de libertad del spline.

# CUBIC SPLINE CON 3 KNOTS EQUIDISTRIBUIDOS
# -----------------------------------------------------------------------------
modelo_splines <- lm(wage ~ bs(age, df = 6, degree = 3), data = Wage)
attr(bs(age, df = 6, degree = 3), "knots")

# CUBIC SPLINE CON 5 KNOTS EQUIDISTRIBUIDOS
# -----------------------------------------------------------------------------
modelo_splines <- lm(wage ~ bs(age, df = 8, degree = 3), data = Wage)
attr(bs(age, df = 8, degree = 3), "knots")

# Si se aumenta el grado del polinomio y se mantienen los grados de
# libertad, el número de knots introducidos es menor.

attr(bs(age, df = 8, degree = 4), "knots")

