# Multiple Linear Regression

peso <- c(74, 92, 63, 72, 58, 78, 85, 85, 73, 62, 80, 72)
altura <- c(168, 196, 170, 175, 162, 169, 190, 186, 176, 170, 176, 179)
cintura <- c(62, 75, 60, 71, 66, 62, 79, 74, 70, 66, 71, 69)
edad <- c(25, 31, 29, 64, 44, 41, 37, 35, 34, 29, 19, 50)

datos <- data.frame(peso, altura, cintura, edad)

# Comprobemos que, efectivamente, el data frame que hemos 
# creado contiene la información sobre las 4 variables:

head(datos)

# A continuación ajustamos el modelo de regresión lineal múltiple

reg_lin_mul <- lm(peso ~ altura + cintura + edad)
summary(reg_lin_mul)

# El modelo podría escribirse tal y como sigue:
# Peso = -78.03017 + 0.93629 * Altura - 0.13261 * Cintura - 0.09672 * Edad