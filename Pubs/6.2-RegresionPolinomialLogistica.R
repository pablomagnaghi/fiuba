 # Empleando el mismo set de datos Wage que en el ejercicio anterior, se pretende
 # crear un modelo logistico que permita predecir la probabilidad de que un trabajador
 # cobre más de 250.000 dólares en función de la edad que tiene.

 # Que la variable respuesta sea cualitativa con dos o mas niveles (la regresion logistica
 # trabaja mejor con solo dos niveles). En el ejemplo que aquí se estudia, la variable
 # respuesta de interés es si wage > 250, cuyo resultado puede ser verdadero o falso.
 # Dado que la variable disponible wage es continua, se tiene que realizar una transformación.
 # La función I(condicion) devuelve un TRUE si se cumple la condición y FALSE de lo contrario.
 set.seed(365)
 muestra <- sample(Wage$wage,6)
 muestra

 I(muestra > 250)

# Ajuste por regresion empleando una transformación logistica. Esto se consigue con la función
# glm() especificando el argumento family = binomial.

# REGRESION LOGISTICA CON POLINOMIO DE GRADO 4
# -----------------------------------------------------

# Creación de una variable binaria si wage>250
Wage$wage_superior250 <- I(Wage$wage > 250)

# Ajuste del modelo logístico
modelo_logit <- glm(wage_superior250  ~ poly(age, 4), family = "binomial",
                    data = Wage)

# Es posible obtener directamente la probabilidad de las predicciones seleccionando el argumento
# type = "response". A pesar de que esta forma es más directa, si junto al valor predicho se
# quiere obtener su intervalo de confianza y que este caiga dentro de [0, 1], se tienen que
# realizar los calculos con los Log ODDs para finalmente transformarlos a probabilidad.
# INTERPOLACIÓN DE PUNTOS DENTRO DEL RANGO DEL PREDICTOR
# -----------------------------------------------------------------------------
limites <- range(Wage$age)
nuevos_puntos <- seq(from = limites[1], to = limites[2], by = 1)
nuevos_puntos <- data.frame(age = nuevos_puntos)

# PREDICCION DE LA VARIABLE RESPUESTA Y DEL ERROR ESTÁNDAR
# -----------------------------------------------------------------------------
# Devuelve las predicciones en forma de log_ODDs
# Si se indica se.fit = TRUE se devuelve el error estándar de cada predicción
predicciones <- predict(modelo_logit, newdata = nuevos_puntos, se.fit = TRUE,
                        level = 0.95)

# CALCULO DEL INTERVALO DE CONFIANZA DEL 95%
# -----------------------------------------------------------------------------
limite_inferior <- predicciones$fit - 1.96 * predicciones$se.fit
limite_superior <- predicciones$fit + 1.96 * predicciones$se.fit

# TRANSFORMACION DE LOG ODDs A PROBABILIDADES
# -----------------------------------------------------------------------------
fit_logit <- exp(predicciones$fit) / (1 + exp(predicciones$fit))
limite_inferior_logit <- exp(limite_inferior) / (1 + exp(limite_inferior))
limite_superior_logit <- exp(limite_superior) / (1 + exp(limite_superior))

# RUG PLOT: REPRESENATACION GRAFICA CON GGPLOT2
# -----------------------------------------------------------------------------
library(ggplot2)
library(gridExtra)
datos_curva <- data.frame(age = nuevos_puntos,
                          probabilidad = fit_logit,
                          limite_inferior_logit = limite_inferior_logit,
                          limite_superior_logit = limite_superior_logit)

p1 <- ggplot(Wage, aes(x = age, y = as.numeric(wage_superior250))) +
      #La variable respuesta y se convierte de vector lógico a 1/0
      geom_jitter(aes(color = as.factor(wage_superior250)), shape = "I",
                  size = 3, height = 0) +
      geom_line(data = datos_curva, aes(y = probabilidad), color = "firebrick") +
      geom_line(data = datos_curva, aes(y = limite_inferior_logit),
                linetype = "dashed") +
      geom_line(data = datos_curva, aes(y = limite_superior_logit),
                linetype = "dashed") +
      theme_bw() +
      labs(y = "P(wage > 250 | age)") +
      theme(legend.position = "null") +
      theme(plot.title = element_text(hjust = 0.5))

# Zoom en la zona baja
p1_zoom <- p1 + lims(y = c(0,0.15)) +
  labs(title = "Zoom región [0, 15]", y = "")

grid.arrange(p1, p1_zoom, ncol = 2,
             top = "Modelo regresión logística con polinomio de grado 4")

