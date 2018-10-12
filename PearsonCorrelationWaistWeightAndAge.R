# Vamos a calcular el coeficiente de correlación lineal de 
# Pearson entre las variables peso y altura

peso <- c(74, 92, 63, 72, 58, 78, 85, 85, 73, 62, 80, 72)
altura <- c(168, 196, 170, 175, 162, 169, 190, 186, 176, 170, 176, 179)
cintura <- c(62, 75, 60, 71, 66, 62, 79, 74, 70, 66, 71, 69)
edad <- c(25, 31, 29, 64, 44, 41, 37, 35, 34, 29, 19, 50)

datos <- data.frame(peso, altura, cintura, edad)

# Vamos a calcular el coeficiente de correlación lineal de
# Pearson entre las variables peso y altura:
cor(peso, altura)

# También es posible calcular de forma simultánea la correlación 
# entre un conjunto de variables almacenada en un data frame. Por ejemplo:

cor(datos)


# En este caso, en lugar de un único valor numérico, la función cor 
# devuelve una matriz simétrica con las correlaciones entre las variables.
# Habitualmente, se está interesado en contrastar si la correlación 
# lineal entre pares de variables puede considerarse 0 o no. 
# Es decir, se quiere resolver el contraste
# H0 ≡ ρ = 0
# H1 ≡ ρ ≠ 0

# donde ρ representa el coeficiente de correlación lineal. Este contraste se resuelve en R usando la función cor.test

# cor.test(x, y, alternative = c("two.sided", "less", "greater"), 
#         method = c("pearson", "kendall", "spearman"), conf.level = 0.95)

# donde
# - x, y, method son los mismos parámetros que se usan en la función cor
# - alternative indica cuál va a ser la hipótesis alternativa del contraste. 
# Por defecto se considera que la hipótesis alternativa es  (two.sided), 
# aunque también pueden considerarse las hipótesis alternativas (less) o (greater).
# - conf.level indica el nivel de confianza que se usará en el contraste (por defecto, 0.95)

cor.test (peso, altura)

# El p-valor asociado a este contraste es de 0.0004106 < 0.05, por lo que 
# rechazamos la hipótesis de que la correlación lineal entre estas dos variables sea 0.


