# Calificaciones

# El siguiente conjunto de datos contiene informacion relativa a las
# calificaciones en Estadistica, Quimica y Geologia de 8 alumnos,
# como el numero de horas diarias que en promedio dedican al estudio

#########################
# Crea 4 vectores numericos, de manera que cada uno contenga los datos
# de una columna y un data frame llamado Notas con los 4 vectores que acabas de crear

Est <- c(9,2,5,10,6,4,7,6)

Qui <- c(8.5, 5,7,10,1,3,6,8)

Geo <- c(10,4,7,7,3,7,8,7)

Horas <- c(5,0.2,2,5.5,1.5,1.5,3,2.5)

Notas <- data.frame(Est, Qui, Geo, Horas)

head(Notas)

#########################
# Representa el diagrama de dispersion de la nota en estadistica y la nota en quimica
plot(Notas$Est, Notas$Qui)

#########################
# Escribe la recta de regresion lineal que surge de considerar
# la nota en estadistica como variable dependiente y la nota
# en quimica como variable independiente.

reg_lin <- lm(Est ~ Qui, data = Notas)
reg_lin

# Por tanto, la recta de regresion es:
# Est = 2.9643 + 0.5213 Qui

#########################
# �Son significativos los dos parametros anteriores?
# �Que puede decirse del ajuste del modelo a los datos?

summary(reg_lin)

# Puesto que los p-valores asociados al contraste t de Student para ambos
# parametros (0.168 y 0.115) son mayores que 0.05, podemos considerar
# que ambos parametros no son significativamente distintos de 0.

# Por otra parte, el valor de R2 (0.3608) nos lleva a concluir que el
# ajuste del modelo a los datos es bastante pobre.

#########################
# Cual seria la recta de regresion en el caso de que se
# considere un modelo de regresion lineal simple sin constante?

reg_lin2 <- lm(Est ~ 0 + Qui, data = Notas)
reg_lin2

# En este caso, el modelo seria:
# Est = 0.9249 * Qui

#########################
# Calcula el coeficiente de correlacion lineal de Pearson
# entre ambas variables. Es significativa esta correlacion?

cor(Notas$Est, Notas$Qui)

# La correlacion lineal de Pearson entre ambas variables es de 0.6006425

cor.test(Notas$Est, Notas$Qui)

# Puesto que el p-valor del contraste es de 0.1153 y, por lo tanto,
# mayor que 0.05, podemos concluir que la correlacion no es
# significativa, y podria considerarse 0.

#########################
# Cual seria la recta de regresion lineal si se incluye
# la nota en geologia como variable independiente?

reg_lin_mul <- lm(Est ~ Qui + Geo, data = Notas)
reg_lin_mul

# En este caso, la recta de regresion lineal multiple podria escribirse como:
# Est = 1.7276 + 0.3535 * Qui + 0.3403 * Geo
