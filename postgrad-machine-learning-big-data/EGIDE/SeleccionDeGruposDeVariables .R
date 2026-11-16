library(ISLR)
summary(Hitters)

# es un dataset de estadistas deportivas de baseball

# por practicidad vamso a eliminar los registros con NAs con esta funcion
Hitters <- na.omit(Hitters)

#####################################
### SELECCION POR FUERZA BRUTA #####
#####################################


# Vamos a hacer el mejor grupo de variables, haciendo fuerza bruta.
# Para ello utilziaremos el paquete leaps

install.packages("leaps")
library(leaps)
reg.fits <- regsubsets(Salary ~ ., data=Hitters)

summary(reg.fits)

# por default analiza todos los grupos de variables de 2 a 8
# ahora le indicaremos que tome grupos de hasta 19
# (que es la cantidad tortal de predictores del dataset)

reg.fits <- regsubsets(Salary ~ ., data=Hitters, nvmax=19)

# veo el objeto resumen y su estructura (para poder acceder a sus valores)
resumen <- summary(reg.fits)

str(resumen)

# voy a graficar los correpondientes r2 ajustados

plot(resumen$adjr2, col="blue", xlab= "Cant Variables Modelo", ylab="R2 ajustado")

# podemos ver que esta en cant predictores= 10
# hay una funcion que nos trae cu�l es la ubicacion de maximo de un campo de un
# vector o lista (que en un datset es una columna)

which.max(resumen$adjr2)



plot(reg.fits, scale="adjr2")

# es una excelente forma de ver graficamente esto:
modelos <-cbind(1:19,resumen$adjr2)

# ahora queremos ver los coeficientes de ese modelo con 11 variables.
coef(reg.fits,11)

#####################################
### SELECCION HACIA ADELANTE #####
#####################################

reg.fitf <- regsubsets(Salary ~ ., data=Hitters, nvmax=19, method="forward")

plot(reg.fitf, scale="adjr2")

# veamos el modelo con mayor adjr2
resumenf <- summary(reg.fitf)

which.max(resumenf$adjr2)

# ahora comparemos los modelos al que arribamos por cada metodo
coef(reg.fits,11)
coef(reg.fitf,11)


#####################################
### SELECCION HACIA atras #####
#####################################

reg.fitb <- regsubsets(Salary ~ ., data=Hitters, nvmax=19, method="backward")

resumenb <- summary(reg.fitb)

par(mfrow=c(1,2))
plot(reg.fits, scale="adjr2")

plot(reg.fitb, scale="adjr2")

# volvamos a la viaja forma
# voy a graficar los correpondientes r2 ajustados

plot(resumenb$adjr2, col="blue", xlab= "Cant Variables Modelo", ylab="R2 ajustado")

# veamos el modelo con mayor adjr2
resumenf <- summary(reg.fitf)

which.max(resumenb$adjr2)

# y sus coeficientes
coef(reg.fitb,11)
coef(reg.fits,11)
