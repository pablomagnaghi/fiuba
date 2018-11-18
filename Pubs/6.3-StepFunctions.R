# Empleando el mismo set de datos Wage que en el ejercicio anterior, se pretende crear
# un modelo formado por 4 step functions que permita predecir el salario de un trabajador
# en funcion de la edad que tiene.

# En R, los modelos step functions se obtienen mediante una combinación de las funciones
# lm() y  cut(). Dado un vector numerico como argumento, la funcion cut() establece n
# puntos de corte y devuelve una variable cualitativa que indica el subintervalo al
# que pertenece cada observación.

# Ejemplo de la funcion cut()
 set.seed(357)
 cut(runif(n = 20, min = 0, max = 100), breaks = 3)

# Es posible indicarle a la función cut() dónde se quieren los puntos de corte

# Cuando la funcion lm() reconoce que se esta empleando cut() genera automaticamente
# las variables dummy necesarias para proceder con la regresion por minimos cuadrados.
table(cut(Wage$age, 4))

modelo_step_fun <- lm(wage ~ cut(age, 4), data = Wage)
summary(modelo_step_fun)

# La funcion cut() ha establecido los puntos de corte en los valores age = 33.5, 49, 64.5.
# En el  summary del modelo obtenido no aparece el grupo age < 33.5, lo que significa que
# este es el grupo de referencia. El intercept (94.158) se interpreta como el salario medio
# de los trabajadores con age < 33.5 y el coeficiente de regresión estimado de cada grupo
# como el incremento promedio de salario respecto al grupo de referencia.

# INTERPOLACIÓN DE PUNTOS DENTRO DEL RANGO DEL PREDICTOR
# -----------------------------------------------------------------------------
limites <- range(Wage$age)
nuevos_puntos <- seq(from = limites[1], to = limites[2], by = 1)
nuevos_puntos <- data.frame(age = nuevos_puntos)

# PREDICCIÓN DE LA VARIABLE RESPUESTA Y DEL ERROR ESTÁNDAR
# -----------------------------------------------------------------------------
predicciones <- predict(modelo_step_fun, newdata = nuevos_puntos, se.fit = TRUE,
                        level = 0.95)

# CÁLCULO DEL INTERVALO DE CONFIANZA SUPERIOR E INFERIOR
# -----------------------------------------------------------------------------
intervalo_conf <- data.frame(
                  inferior = predicciones$fit - 1.96*predicciones$se.fit,
                  superior = predicciones$fit + 1.96*predicciones$se.fit)
attach(Wage)
plot(x = age, y = wage, pch = 20, col = "darkgrey")
title("Modelo Picewise Constant con 4 step functions")
lines(x = nuevos_puntos$age, predicciones$fit, col = "red", lwd = 2)
lines(x = nuevos_puntos$age, intervalo_conf$inferior, col = "blue",
      lwd = 2, lty = 2)
lines(x = nuevos_puntos$age, intervalo_conf$superior, col = "blue",
      lwd = 2, lty = 2)
