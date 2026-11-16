# Empleando el mismo set de datos Wage que en el ejercicio anterior, se pretende crear
# un modelo logistico que permita predecir la probabilidad de que un trabajador
# cobre más de 250.000 dolares en funcion de la edad que tiene.

# Los modelos GAM pueden emplearse también para regresión logistica. Solo es necesario
# convertir la variable respuesta en tipo dicotómico e indicar en el argumento de la
# función gam() que  family = binomial.

# Cuando se generan modelos logisticos que incluyen un predictor cualitativo, es
# importante comprobar si hay algún nivel del predictor para el cual ninguna observacion
# es de tipo verdadero. Por ejemplo, no hay ningún trabajador con nivel educativo
# inferior a HS Grad y que tenga un salario mayor de 250.000 dolares.

table(Wage$education, I(Wage$wage > 250))

# Por lo tanto, basandose en los datos disponibles, la probabilidad de que un trabajador
# con educacion < HS Grad tenga un salario superior a 250.000 es cero. Para que estas
# observaciones, cuya prediccion esta clara, no influyan en el resto del modelo es
# conveniente excluirlas. De esta forma se suele mejorar la precisión.


library(gam)
modelo_gam_logit <- gam(I(wage > 250) ~ year + s(age, df = 5) + education,
                        family = "binomial", data = Wage,
                        subset = (education != "1. < HS Grad"))
summary(modelo_gam_logit)

# Al tratarse de un modelo con mas de 2 predictores, no se puede hacer una representacion
# grafica simultánea del modelo logistico. En lugar de eso, se suele representar la
# influencia de cada predictor.

plot(modelo_gam_logit, se = TRUE, col = "green")
