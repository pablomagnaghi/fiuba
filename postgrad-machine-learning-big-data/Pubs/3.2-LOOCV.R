# En R se puede realizar LOOCV de cualquier generalized linear model creado mediante
# glm() empleando la funcion cv.glm() del paquete boot. La función glm() engloba
# diferentes tipos de modelos lineales que se especifican mediante el argumento family.
# En el caso de regresión lineal no se especifica ningún tipo de familia y es equivalente
# a emplear la función lm(). Para regresión logística se emplea family=binomial.

# La funcion cv.glm() calcula el error de prediccion mediante cross-validation.
# Si el argumento k no se especifica, se le asigna automaticamente el numero de
# observaciones empleadas para crear el modelo, por lo que se realiza un leave-one-out
# cross-validation. La funcion devuelve una lista con multiples componentes, el
# resultado de la validacion se almacena dentro del vector delta que contiene
# la estimacion de error con y sin corrección.

# Se genera el modelo lineal, dado que se va a emplear LOOCV no es necesario
# dividir las observaciones en dos grupos
modelo <- glm(mpg~horsepower, data = Auto)
# Se emplea la función cv.glm() para la validación LOOCV
library(boot)
cv_error <- cv.glm(data = Auto,glmfit = modelo)
cv_error$delta

# La estimacion de error del modelo lineal mediante LOOCV es de 24.23. Se ha
# obtenido un valor cercano al estimado mediante CV-simple pero eliminando el
# problema de la variabilidad. Al igual que se ha hecho en CV-simple, se puede
# emplear la validación LOOCV para identificar el grado de flexibilidad que
# permite obtener el mejor modelo.

cv_MSE <- rep(NA,10)
for (i in 1:10) {
  modelo <- glm(mpg ~ poly(horsepower, i), data = Auto)
  cv_MSE[i] <- cv.glm(data = Auto, glmfit = modelo)$delta[1]
}
ggplot(data = data.frame(polinomio = 1:10, cv_MSE = cv_MSE),
       aes(x = polinomio, y = cv_MSE)) +
geom_point(colour = c("firebrick3")) +
geom_path() +
scale_x_continuous(breaks = c(0:10)) +
theme_bw() +
labs(title  =  'Test Error ~ Grado del polinomio') +
theme(plot.title = element_text(hjust = 0.5, face = 'bold'))

# Los resultados de la validacion indican que polinomios superiores a grado 2 o 3 no aportan una mejora sustancial.

