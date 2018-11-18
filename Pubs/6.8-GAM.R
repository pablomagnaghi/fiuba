# Empleando el mismo set de datos Wage que en el ejercicio anterior, se pretende crear un
# modelo GAM que permita predecir el salario (wage) de un trabajador en función de su edad
# (age), año en el que se registró la información (year) y nivel educativo (education).

library(ggplot2)
library(gridExtra)

p1 <- ggplot(data = Wage, aes(x = age, y = wage)) +
        geom_point(color = "darkgray") +
        theme_bw() +
        labs(title = "wage ~ age")

p2 <- ggplot(data = Wage, aes(x = year, y = wage)) +
        geom_point(color = "darkgray") +
        stat_summary(fun.y = "mean", fill = "red", size = 4,
                     geom = "point", shape = 23) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(title = "wage ~ year")

p3 <- ggplot(data = Wage, aes(x = education, y = wage)) +
        geom_point(color = "darkgray") +
        stat_summary(fun.y = "mean", fill = "red", size = 4,
                     geom = "point", shape = 23) +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(title = "wage ~ education")

p1

grid.arrange(p2, p3, ncol = 2)

# Los predictores age y year son variables continuas. La relacion entre age y wage es
# claramente no lineal, mientras que en el caso de year no queda claro a simple vista
# si es o no lineal. El predictor education es de tipo cualitativo con 5 niveles.

# Dado que un modelo GAM es en esencia un modelo de regresión lineal multiple formado
# por funciones independientes para cada predictor, se puede ajustar empleando la
# funcion lm(). Se procede a ajustar un GAM que contenga natural splines para las
# variables continuas y constantes dummy para la variable cualitativa

library(splines)
modelo_gam <- lm(wage ~ ns(year, 4) + ns(age, 5) + education, data = Wage)
# plot(modelo_gam)

# Para crear modelos GAM mas complejos, que incluyan smooth splines o local regression
# se emplean las funciones gam(), s() y lo() del paquete gam. A continuación se crea un
# modelo similar al anterior pero esta vez empleando una smooth spline con 4 grados de
# libertad para year y otra con 5 grados de libertad para age. Una vez ajustado, se
# obtiene una representación grafica de cada una de las funciones que componen el
# modelo mediante la función plot.gam(). Esto es aplicable tanto a modelos GAM obtenidos
# con la funcion gam() como con lm().
library(gam)
modelo_gam <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
plot(modelo_gam, se = TRUE, col = "red")

# El summary() de un modelo gam genera un tabla resumen del ajuste. En la seccion Anova for
# Nonparametric Effects se muestran p-values para cada predictor. Estos p-values se
# corresponden con el contraste de hipótesis de que la relación entre predictor y
# variable respuesta es lineal, frente a la alternativa de que no lo es.

summary(modelo_gam)

# El p-value obtenido para la funcion del predictor year (0.35) no muestra evidencias
# de que la relacion entre wage y year no sea lineal, lo que lleva a preguntarse si
# sería mejor emplear un ajuste lineal en lugar de una smooth spline, reduciendo así
# la complejidad del modelo. Un análisis ANOVA sirve para dar respuesta a esta pregunta.
# A continuación, se comparan 3 posibles modelos de menor a mayor complejidad: Un
# modelo (m_1) que no contiene el predictor year, un modelo (m_2) que emplea una
# funcion lineal para year y un tercer modelo (m_3) que emplea smooth spline.

library(gam)
m_1 <- gam(wage ~  s(age, 5) + education, data = Wage)
m_2 <- gam(wage ~ year + s(age, 5) + education, data = Wage)
m_3 <- gam(wage ~ s(year, 4) + s(age, 5) + education, data = Wage)
# método anova para objetos de tipo gam
anova(object = m_1, m_2, m_3, test = "F")

# El ANOVA muestra una clara evidencia (p-value = 0.00014) de que el modelo GAM con una
# funcion lineal de year es superior a un GAM que no incluya este predictor. Sin embargo,
# no hay evidencia de que una función no lineal de year sea necesaria (p-value = 0.35).
# Acorde al principio de parsimonia, el modelo m_2 es el mejor.

# Al igual que con todos los tipos de modelos anteriormente explicados, la funcion
# predict() también es aplicable a modelos generados con gam().
