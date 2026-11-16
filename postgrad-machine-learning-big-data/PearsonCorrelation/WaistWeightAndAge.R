# Vamos a calcular el coeficiente de correlacion lineal de 
# Pearson entre las variables peso y altura

peso <- c(74, 92, 63, 72, 58, 78, 85, 85, 73, 62, 80, 72)
altura <- c(168, 196, 170, 175, 162, 169, 190, 186, 176, 170, 176, 179)
cintura <- c(62, 75, 60, 71, 66, 62, 79, 74, 70, 66, 71, 69)
edad <- c(25, 31, 29, 64, 44, 41, 37, 35, 34, 29, 19, 50)

datos <- data.frame(peso, altura, cintura, edad)

# Vamos a calcular el coeficiente de correlacion lineal de
# Pearson entre las variables peso y altura:
cor(peso, altura)

# Tambien es posible calcular de forma simultanea la correlacion 
# entre un conjunto de variables almacenada en un data frame. Por ejemplo:

cor(datos)


# En este caso, en lugar de un unico valor numerico, la funcion cor 
# devuelve una matriz simetrica con las correlaciones entre las variables.
# Habitualmente, se esta interesado en contrastar si la correlacion 
# lineal entre pares de variables puede considerarse 0 o no. 
# Es decir, se quiere resolver el contraste
# H0 ≡ ρ = 0
# H1 ≡ ρ ≠ 0

# donde ρ representa el coeficiente de correlacion lineal. Este contraste se resuelve en R usando la funcion cor.test

# cor.test(x, y, alternative = c("two.sided", "less", "greater"), 
#         method = c("pearson", "kendall", "spearman"), conf.level = 0.95)

# donde
# - x, y, method son los mismos parametros que se usan en la funcion cor
# - alternative indica cual va a ser la hipotesis alternativa del contraste. 
# Por defecto se considera que la hipotesis alternativa es  (two.sided), 
# aunque tambien pueden considerarse las hipotesis alternativas (less) o (greater).
# - conf.level indica el nivel de confianza que se usara en el contraste (por defecto, 0.95)

cor.test(peso, altura)

# El p-valor asociado a este contraste es de 0.0004106 < 0.05, por lo que 
# rechazamos la hipotesis de que la correlacion lineal entre estas dos variables sea 0.


