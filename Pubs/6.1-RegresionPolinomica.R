 # El set de datos Wage del paquete ISRL contiene información sobre 3000 trabajadores.
 # Entre las 12 variables registradas se encuentra el salario (wage) y la edad (age).
 # Dada la relacion no lineal existente entre estas dos variables, se recurre a un
 # modelo polinomico de grado 4 que permita predecir el salario en funcion de la edad.

# La funcion lm() permite ajustar modelos lineales por minimos cuadrados. En el argumento
# formula se especifica la variable dependiente y los predictores, que en este caso son
# wage ~ age + age^2 + age^3 + age^4. lm() tiene flexibilidad a la hora de interpretar
# el argumento formula, por lo que existen diferentes formas de obtener el mismo ajuste.
# Todas las que se muestran a continuacion son equivalentes:
# lm(wage ~ age + I(age^2) + I(age^3) + I(age^4), data = Wage)
# lm(wage ~ cbind(age + age^2 + age^3 + age^4), data = Wage)
# lm(wage ~ poly(age, 4, raw = TRUE), data = Wage)
# lm(wage ~ poly(age, 4), data = Wage)

# Con la funcion poly() se puede generar directamente un polinomio, evitando tener que
# escribir toda la formula. Cuando se especifica que su argumento raw = TRUE, devuelve
# una matriz formada por el valor de la variable original elevada a cada una de las
# potencias del polinomio. Si el argumento  raw = FALSE se devuelve una matriz con
# polinomios ortogonales, en la que cada columna es una combinacion lineal de las otras.
# Es importante tenerlo en cuenta al emplear la función poly() en el ajuste de modelos,
# ya que, aunque no influye en el valor de las predicciones que se obtienen (las curvas
# obtenidas son iguales) sí que cambia el valor estimado de los coeficientes.

library(ISLR)
data("Wage")
Wage$age[1:5]

poly(Wage$age, degree = 4, raw = TRUE, simple = TRUE)[1:5, ]

poly(Wage$age, degree = 4, raw = FALSE, simple = TRUE)[1:5, ]

# CALCULO DEL MODELO POLINOMICO DE GRADO 4
# ----------------------------------------
modelo_poli4 <- lm(wage ~ poly(age, 4), data = Wage)
summary(modelo_poli4)

# El p-value obtenido para el estadístico F es muy bajo (< 2.2e-16), lo que indica que
# al menos uno de los predictores introducidos en el modelo está relacionado con la
# variable respuesta wage. Los p-values individuales de cada predictor son todos muy
# bajos a excepción de age^4, lo que apunta a que un polinomio de grado 3 es suficiente
# para modelar el salario en función de la edad. Acorde al R2, el modelo es capaz de
# explicar el 8.6% de la variabilidad observada en wage. (Es un % muy bajo).

# Si se quiere generar una representación gráfica del modelo ajustado
# wage=111.70+447.07age−478.32age^2+125.52age^3−77.91age^4)
# se tiene que representar la curva que describe su ecuacion. Para conseguirlo, es
# necesario predecir el valor de la variable respuesta en puntos interpolados dentro
# del rango del predictor. Cuantos más puntos se interpolen, mejor sera la representación.
# R permite obtener predicciones (junto con el intervalo de confianza) mediante la función predict().


# INTERPOLACION DE PUNTOS DENTRO DEL RANGO DEL PREDICTOR
# -----------------------------------------------------------------------------
limites <- range(Wage$age)
nuevos_puntos <- seq(from = limites[1], to = limites[2], by = 1)
nuevos_puntos <- data.frame(age = nuevos_puntos)

# PREDICCION DE LA VARIABLE RESPUESTA Y DEL ERROR ESTÁNDAR
# -----------------------------------------------------------------------------
predicciones <- predict(modelo_poli4, newdata = nuevos_puntos, se.fit = TRUE,
                        level = 0.95)

# CALCULO DEL INTERVALO DE CONFIANZA SUPERIOR E INFERIOR 95%
# -----------------------------------------------------------------------------
intervalo_conf <- data.frame(inferior = predicciones$fit -
                                        1.96*predicciones$se.fit,
                             superior = predicciones$fit +
                                        1.96*predicciones$se.fit)

attach(Wage)
plot(x = age, y = wage, pch = 20, col = "darkgrey")
title("Polinomio de grado 4: wage ~ age")
points(x = nuevos_puntos$age, predicciones$fit, col = "red", pch = 20)
points(x = nuevos_puntos$age, intervalo_conf$inferior, col = "blue", pch = 4)
points(x = nuevos_puntos$age, intervalo_conf$superior, col = "blue", pch = 4)

# El ejemplo anterior muestra que, para conseguir el efecto visual de curva continua,
# es necesario interpolar muchos datos. La función lines() facilita el proceso uniendo
# los puntos con segmentos, lo que mejora la representación gráfica de curvas.


attach(Wage)
plot(x = age, y = wage, pch = 20, col = "darkgrey")
title("Polinomio de grado 4: wage ~ age")
lines(x = nuevos_puntos$age, predicciones$fit, col = "red", lwd = 2)
lines(x = nuevos_puntos$age, intervalo_conf$inferior, col = "blue", lwd = 2)
lines(x = nuevos_puntos$age, intervalo_conf$superior, col = "blue", lwd = 2)

# El paquete grAfico ggplot2 permite obtener representaciones de modelos de forma
# rApida. Solo con especificar la fórmula del modelo, ejecuta automAticamente todos
# los clculos necesarios (ajuste, predicciOn…).
library(ggplot2)
ggplot(data = Wage, aes(x = age, y = wage)) +
  geom_point(color = "grey30", alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 4), color = "red") +
  labs(title = "Polinomio de grado 4: wage ~ age") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

### Comparacion de modelos por contraste de hipotesis ANOVA
# Se ajustan modelos polinómicos de grado 1 a 5
# ----------------------------------------------------------------------------
modelo_1 <- lm(wage ~ age, data = Wage)
modelo_2 <- lm(wage ~ poly(age, 2), data = Wage)
modelo_3 <- lm(wage ~ poly(age, 3), data = Wage)
modelo_4 <- lm(wage ~ poly(age, 4), data = Wage)
modelo_5 <- lm(wage ~ poly(age, 5), data = Wage)

anova(modelo_1, modelo_2, modelo_3, modelo_4, modelo_5)

# El p-value de la comparación entre el modelo lineal (modelo_1) y el cuadratico
# (modelo_2) es practicamente cero (< 2.2e-16), indicando que el modelo lineal
# no es suficiente. La comparacion entre el modelo cuadrático y el cúbico también
# ha resultado en un p-value muy pequeño (0.001679), evidencia suficiente de que
# el modelo cúbico es superior. El p-value obtenido al comparar el modelo cúbico
# con el de grado 4 está ligeramente por encima del 0.05 y el de comparar grado 4
# con grado 5 es muy alto (0.37). En base a los resultados del ANOVA, el modelo
# cubico es el mejor. Polinomios superiores no aportan una mejora significativa y
# polinomios inferiores pierden mucha capacidad de ajuste.

# Es importante recordar que las comparaciones por ANOVA pueden hacerse entre cualquier
# par de modelos, siempre y cuando estos sean anidados.

# Comparacion de modelos por Cross-Validation
library(boot)
cv_MSE_k10 <- rep(NA,10)

for (i in 1:10) {
  modelo <- glm(wage ~ poly(age, i), data = Wage)
  set.seed(17)
  cv_MSE_k10[i] <- cv.glm(data = Wage, glmfit = modelo, K = 10)$delta[1]
}
p4 <- ggplot(data = data.frame(polinomio = 1:10, cv_MSE = cv_MSE_k10),
             aes(x = polinomio, y = cv_MSE)) +
      geom_point(colour = c("firebrick3")) +
      geom_path()
p4 <- p4 + theme(panel.grid.major = element_line(colour  =  'gray90'))
p4 <- p4 + theme(plot.title = element_text(face  =  'bold'))
p4 <- p4 + theme(panel.background = element_rect(fill  =  'gray98'))
p4 <- p4 + labs(title  =  'Test Error ~ Grado del polinomio')
p4 <- p4 + scale_x_continuous(breaks = 1:10)
p4

# El metodo de cross-validation k=10 tambien indica que el mejor modelo es el que
# emplea un polinomio de grado 3
